#!/usr/bin/env python3
"""
org_to_tiddlers.py — Convert org-roam files to TiddlyWiki .tid files

Handles:
  - File-level nodes (#+title: with top-level :PROPERTIES:)
  - Subtree nodes (** headings with :ID:), titled by their heading text
  - ROAM_ALIASES → [[bracket]] tags on root nodes
  - ROAM_REFS → roam-refs: field on bibliography nodes
  - [[id:UUID][Label]] links → [[Target Title]] wikilinks
  - [[wikilinks]] and {{{ TW filters }}} preserved through pandoc
  - Tag filtering: allowlist + blocklist (block takes priority)
  - Incremental builds: only reprocess changed files
  - Orphan cleanup: delete tiddlers whose source node no longer exists

Requirements: Python 3.11+, pandoc in PATH
Usage: python3 org_to_tiddlers.py
"""

from __future__ import annotations

import base64
import json
import os
import re
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from datetime import datetime
from titlecase import titlecase

# ─────────────────────────────────────────────────────────────────
# CONFIG
# ─────────────────────────────────────────────────────────────────

SOURCE_DIRS: list[str] = [
    "~/resource/notes/org/roam/journal",
    "~/resource/notes/org/roam/zettels/",
    "~/resource/notes/org/roam/biblio/notes/",
]

OUTDIR: str = "./tiddlywiki/tiddlers"

# A node is included if it has ANY of these tags.
# Leave empty to allow all nodes (subject to blocklist).
ALLOW_TAGS: list[str] = []

# A node is excluded if it has ANY of these tags.
# Blocklist takes priority over allowlist.
BLOCK_TAGS: list[str] = [
    "@cred",
    "@journal",
    "kartaca",
    "genyazilim",
    "_jlog",
    "_j",
    "hopi",
    "_job",
    "_r",
    "_f",  # flashcards
    "resource",
    "contact",
    "location",
    "_p",
    "_wlog",
    "_fleeting",
    "_b",
]

TAG_COLORS: dict[str, str] = {
    "@zettel": "#000000",
    "@biblio": "#000000",
    "@moc": "#000000",
    "@query": "#000000",
    "@post": "#000000",
    "_term": "#FFD700",
    "_abbr": "#FFF176",
    "_analogy": "#CE93D8",
    "_history": "#D7B483",
    "_fact": "#80DEEA",
    "_property": "#90CAF9",
    "_mechanism": "#42A5F5",
    "_method": "#4DB6AC",
    "_comparison": "#FFB74D",
    "_usecase": "#A5D6A7",
    "_tradeoff": "#FFCA28",
    "_bestpractice": "#D4E157",
    "_warning": "#EF9A9A",
    "_fix": "#F48FB1",
    "_reference": "#B0BEC5",
}

# Manifest file path — tracks tiddler → source mapping for incremental builds
MANIFEST_PATH: str = "./.org2tid_manifest.json"


# Titles matching any of these regex patterns will not be converted.
# Useful for excluding journal files by date title format.
BLOCK_TITLE_PATTERNS: list[str] = [
    r"^\d{4}-\d{2}-\d{2}$",  # e.g. 2026-04-23
    r"^\d{2}:\d{2}$",  # e.g. 04:23
    r"^\d{4}-\d{2}-\d{2}_\d{2}:\d{2}$",  # e.g. 2026-04-23_13:10
    r"^\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}$",  # e.g. 2026-04-23 13:10
    r"^\d{4}.\d{2}.\d{2}\/\d{2}:\d{2}:\d{2}$",  # 2025.07.30/14:41:42
    r"^\[\[.*\]\]$",
    r"^index$",
    r"^TODO",
    r"^DONE",
]

_BLOCK_TITLE_RES = [re.compile(p) for p in BLOCK_TITLE_PATTERNS]

# ─────────────────────────────────────────────────────────────────
# DATA STRUCTURES
# ─────────────────────────────────────────────────────────────────
@dataclass
class Node:
    id: str
    title: str
    tags: list[str]
    body: str
    roam_refs: str
    aliases: list[str]
    is_root: bool
    source_file: str
    source_file: str
    created: str = ""


@dataclass
class ManifestEntry:
    source_file: str
    node_id: str
    source_mtime: float


# ─────────────────────────────────────────────────────────────────
# ORG PARSER
# ─────────────────────────────────────────────────────────────────

# Matches a heading line: capture (stars, heading_text)
_HEADING_RE = re.compile(r"^(\*+)\s+(.*)")
# Matches trailing inline tags on a heading: :tag1:tag2:
_INLINE_TAGS_RE = re.compile(r"\s+(:[A-Za-z0-9_@#:]+:)\s*$")
# Matches #+keyword: value
_KEYWORD_RE = re.compile(r"^#\+(\w+):\s*(.*)", re.IGNORECASE)
# Use plain string matching for properties — simpler and faster
_PROP_LINE_RE = re.compile(r"^\s*:([^:]+):\s*(.*?)\s*$")


def write_tag_tiddlers(tag_colors: dict[str, str], outdir: Path) -> None:
    for tag, color in tag_colors.items():
        safe = safe_filename(tag)
        outpath = outdir / f"{safe}.tid"
        with outpath.open("w", encoding="utf-8") as f:
            f.write(f"title: {tag}\n")
            f.write(f"color: {color}\n")
            f.write("\n")


def _parse_inline_tags(heading_text: str) -> tuple[str, list[str]]:
    """Split 'My Title :tag1:tag2:' into ('My Title', ['tag1', 'tag2'])."""
    m = _INLINE_TAGS_RE.search(heading_text)
    if not m:
        return heading_text.strip(), []
    tag_str = m.group(1)  # e.g. ':tag1:tag2:'
    tags = [t for t in tag_str.split(":") if t]
    title = heading_text[: m.start()].strip()
    return title, tags


def _parse_filetags(value: str) -> list[str]:
    """Parse ':tag1:tag2:tag3:' → ['tag1', 'tag2', 'tag3']."""
    return [t for t in value.strip().split(":") if t]


def _parse_aliases(value: str) -> list[str]:
    """Parse '"alias one" "alias two"' → ['alias one', 'alias two']."""
    return re.findall(r'"([^"]+)"', value)

def parse_org_timestamp(ts: str) -> str:
    ts = ts.strip()
    for fmt in ("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"):
        try:
            dt = datetime.strptime(ts, fmt)
            return dt.strftime("%Y%m%d%H%M%S") + "000"
        except ValueError:
            continue
    return ""

def parse_org_file(path: Path) -> list[Node]:
    """
    Parse an org file and return all org-roam nodes found in it.
    Each node with an :ID: property becomes a Node.
    """
    lines = path.read_text(encoding="utf-8").splitlines()
    nodes: list[Node] = []

    # State
    in_props = False
    first_node_done = False
    cur_created = ""
    cur_is_post = False

    # Current node being built
    cur_level = 0
    cur_id = ""
    cur_title = ""
    cur_tags: list[str] = []
    cur_body_lines: list[str] = []
    cur_roam_refs = ""
    cur_aliases: list[str] = []

    # File-level metadata (before any heading)
    filetags: list[str] = []

    def flush() -> None:
        nonlocal first_node_done
        if not cur_id:
            return
        nodes.append(
            Node(
                id=cur_id,
                title=cur_title,
                tags=cur_tags[:],
                body="\n".join(cur_body_lines),
                roam_refs=cur_roam_refs,
                aliases=cur_aliases[:],
                is_root=not first_node_done,
                source_file=str(path),
                created=cur_created,
            )
        )
        first_node_done = True

    for line in lines:
        # Heading line
        hm = _HEADING_RE.match(line)
        if hm:
            level = len(hm.group(1))
            heading_text = hm.group(2)
            title, own_tags = _parse_inline_tags(heading_text)

            if cur_is_post and level > cur_level:
                cur_body_lines.append(line)
                continue

            flush()
            all_tags = own_tags + filetags
            cur_level = level
            cur_title = titlecase(title)
            cur_tags = all_tags
            cur_id = ""
            cur_body_lines = []
            cur_roam_refs = ""
            cur_aliases = []
            cur_created = ""
            cur_is_post = "@post" in all_tags
            in_props = False
            continue

        # Properties block boundaries
        if re.match(r"^\s*:PROPERTIES:\s*$", line, re.IGNORECASE):
            in_props = True
            continue
        if re.match(r"^\s*:END:\s*$", line, re.IGNORECASE):
            in_props = False
            continue
        if in_props:
            pm = _PROP_LINE_RE.match(line)
            if pm:
                key = pm.group(1).upper()
                val = pm.group(2).strip()
                if key == "ID":
                    cur_id = val
                elif key == "ROAM_REFS":
                    cur_roam_refs = val
                elif key == "CREATED":
                    cur_created = parse_org_timestamp(val)
            continue

        # Inside properties block
        if in_props:
            pm = _PROP_LINE_RE.match(line)
            if pm:
                key = pm.group(1).upper()
                val = pm.group(2).strip()
                if key == "ID":
                    cur_id = val
                elif key == "ROAM_REFS":
                    cur_roam_refs = val
            continue

        # File-level keywords (before any heading, cur_level == 0)
        km = _KEYWORD_RE.match(line)
        if km:
            key = km.group(1).upper()
            val = km.group(2).strip()
            if key == "FILETAGS":
                filetags = _parse_filetags(val)
                if cur_level == 0:
                    cur_tags = filetags[:]
            elif key == "TITLE":
                if cur_level == 0:
                    cur_title = titlecase(val)
            elif key == "ROAM_ALIASES":
                if cur_level == 0:
                    cur_aliases = _parse_aliases(val)
            continue

        # Body line — accumulate for any node level
        if cur_level >= 0 and cur_id or cur_level > 0:
            cur_body_lines.append(line)

    flush()
    return nodes


# ─────────────────────────────────────────────────────────────────
# LINK RESOLVER — Pass 1
# ─────────────────────────────────────────────────────────────────


def build_link_map(all_nodes: list[Node]) -> dict[str, str]:
    """
    Build UUID → display title map for link rewriting.
    All nodes use their human title (not UUID) so links render meaningfully.
    """
    return {node.id: node.title for node in all_nodes if node.id}


# ─────────────────────────────────────────────────────────────────
# TAG FILTER
# ─────────────────────────────────────────────────────────────────


def should_convert(tags: list[str]) -> bool:
    """
    Returns True if this node should be converted to a tiddler.
    Blocklist takes priority over allowlist.
    """
    tag_set = set(tags)

    # Block takes priority
    for tag in BLOCK_TAGS:
        if tag in tag_set:
            return False

    # Empty allowlist = allow everything not blocked
    if not ALLOW_TAGS:
        return True

    # Handle __untagged__ sentinel
    if "__untagged__" in ALLOW_TAGS and not tags:
        return True

    # Require at least one allowlist match
    for tag in ALLOW_TAGS:
        if tag in tag_set:
            return True

    return False


# title blocker
def should_convert_title(title: str) -> bool:
    """Return False if the title matches any blocked pattern."""
    return not any(p.search(title) for p in _BLOCK_TITLE_RES)


# ─────────────────────────────────────────────────────────────────
# BODY CONVERTER
# ─────────────────────────────────────────────────────────────────

# Matches [[id:UUID][Label]] org-roam links
_ID_LINK_RE = re.compile(r"\[\[id:([0-9a-f-]+)\]\[([^\]]*)\]\]")
# Matches [[wikilinks]]
_WIKILINK_RE = re.compile(r"\[\[([^\]]+)\]\]")
# Matches {{{ TiddlyWiki filters/macros }}}
_TW_FILTER_RE = re.compile(r"\{\{\{(.*?)\}\}\}", re.DOTALL)

# Placeholder patterns (must survive pandoc unchanged — alphanumeric only)
_WL_PREFIX = "XWLX"
_TW_PREFIX = "XTWFX"


def _b64(s: str) -> str:
    return base64.b64encode(s.encode()).decode().rstrip("=")


def _unb64(s: str) -> str:
    # Restore stripped padding
    padding = 4 - len(s) % 4
    if padding != 4:
        s += "=" * padding
    return base64.b64decode(s).decode()


def _rewrite_id_links(body: str, link_map: dict[str, str]) -> str:
    """Rewrite [[id:UUID][Label]] → [[Target Title]]."""

    def replace(m: re.Match) -> str:
        uuid, label = m.group(1), m.group(2)
        target = link_map.get(uuid, label)
        return f"[[{target}]]"

    return _ID_LINK_RE.sub(replace, body)


def _protect(body: str) -> str:
    """Encode [[wikilinks]] and {{{TW filters}}} so pandoc can't touch them."""
    # Protect TW filters first (they may contain [[ ]])
    body = _TW_FILTER_RE.sub(
        lambda m: f"{_TW_PREFIX}{_b64(m.group(1))}{_TW_PREFIX}", body
    )
    # Protect wikilinks
    body = _WIKILINK_RE.sub(
        lambda m: f"{_WL_PREFIX}{_b64(m.group(1))}{_WL_PREFIX}", body
    )
    return body


def _restore(body: str) -> str:
    """Decode placeholders back to [[wikilinks]] and {{{TW filters}}}."""
    body = re.sub(
        rf"{_TW_PREFIX}([A-Za-z0-9+/]*){{0,2}}{_TW_PREFIX}",
        lambda m: "{{{" + _unb64(m.group(1)) + "}}}",
        body,
    )
    body = re.sub(
        rf"{_WL_PREFIX}([A-Za-z0-9+/]*){_WL_PREFIX}",
        lambda m: "[[" + _unb64(m.group(1)) + "]]",
        body,
    )
    return body


def convert_body(body: str, link_map: dict[str, str]) -> str:
    """Convert org body text to markdown, preserving wikilinks and TW syntax."""
    if not body.strip():
        return ""

    # Step 1: rewrite id links
    body = _rewrite_id_links(body, link_map)

    # Step 2: protect wikilinks and TW filters
    body = _protect(body)

    # Step 3: pandoc org → markdown
    result = subprocess.run(
        ["pandoc", "-f", "org", "-t", "markdown", "--wrap=none"],
        input=body,
        capture_output=True,
        text=True,
        check=True,
    )
    body = result.stdout
    body = result.stdout
    body = re.sub(r'\{\.verbatim\}', '', body)

    # Step 4: restore
    body = _restore(body)

    return body.strip()


# ─────────────────────────────────────────────────────────────────
# TIDDLER WRITER
# ─────────────────────────────────────────────────────────────────


def safe_filename(title: str) -> str:
    """Sanitize a title for use as a filename."""
    return re.sub(r"[/:\\]+", "_", title).strip("_")


def node_to_tid(
    node: Node, link_map: dict[str, str], outdir: Path, parent_node: Node | None = None
) -> Path:
    """Convert a node to a .tid file, return the output path."""
    body_md = convert_body(node.body, link_map)

    # Build tags: node tags + aliases as [[bracket]] tags
    tag_parts = node.tags[:]
    for alias in node.aliases:
        tag_parts.append(f"[[{alias}]]")
    tags_str = " ".join(tag_parts)

    # Append source reference if this is a subtree node under a @biblio root
    if parent_node and parent_node.title:
        source_line = f"\n<sup>source: [[{parent_node.title}]]</sup>"
        if body_md:
            body_md = body_md + "\n\n" + source_line
        else:
            body_md = source_line

    filename = safe_filename(node.title) + ".tid"
    outpath = outdir / filename

    with outpath.open("w", encoding="utf-8") as f:
        f.write(f"title: {node.title}\n")
        f.write(f"tags: {tags_str}\n")
        if node.id:
            f.write(f"roam-id: {node.id}\n")
        if node.roam_refs:
            f.write(f"roam-refs: {node.roam_refs}\n")
        if node.created:
            f.write(f"created: {node.created}\n")
            f.write(f"modified: {node.created}\n")
        f.write("type: text/markdown\n")
        f.write("\n")
        if body_md:
            f.write(body_md)
            f.write("\n")

    return outpath


# ─────────────────────────────────────────────────────────────────
# MANIFEST MANAGER
# ─────────────────────────────────────────────────────────────────


def load_manifest(manifest_path: Path) -> dict[str, ManifestEntry]:
    """Load manifest from disk. Returns empty dict if not found."""
    if not manifest_path.exists():
        return {}
    with manifest_path.open(encoding="utf-8") as f:
        raw = json.load(f)
    return {k: ManifestEntry(**v) for k, v in raw.items()}


def save_manifest(manifest: dict[str, ManifestEntry], manifest_path: Path) -> None:
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    with manifest_path.open("w", encoding="utf-8") as f:
        json.dump(
            {k: vars(v) for k, v in manifest.items()},
            f,
            indent=2,
        )


def needs_rebuild(
    node: Node,
    tid_filename: str,
    manifest: dict[str, ManifestEntry],
    outdir: Path,
) -> bool:
    """Return True if the tiddler needs to be (re)built."""
    tid_path = outdir / tid_filename
    if not tid_path.exists():
        return True
    if tid_filename not in manifest:
        return True
    entry = manifest[tid_filename]
    source_mtime = Path(node.source_file).stat().st_mtime
    return source_mtime > entry.source_mtime


# ─────────────────────────────────────────────────────────────────
# MAIN
# ─────────────────────────────────────────────────────────────────


def collect_org_files(source_dirs: list[str]) -> list[Path]:
    files = []
    for d in source_dirs:
        p = Path(d).expanduser()
        if not p.is_dir():
            print(f"WARNING: source dir not found, skipping: {p}", file=sys.stderr)
            continue
        files.extend(sorted(p.glob("*.org")))
    return files


def main() -> None:
    outdir = Path(OUTDIR).expanduser()
    outdir.mkdir(parents=True, exist_ok=True)

    manifest_path = Path(MANIFEST_PATH).expanduser()
    manifest = load_manifest(manifest_path)

    org_files = collect_org_files(SOURCE_DIRS)
    if not org_files:
        print("No .org files found in SOURCE_DIRS.")
        sys.exit(1)

    # ── Pass 1: parse all files, build link map ───────────────────
    print("Pass 1: scanning all org files...")
    all_nodes: list[Node] = []
    for f in org_files:
        nodes = parse_org_file(f)
        all_nodes.extend(nodes)

    link_map = build_link_map(all_nodes)
    print(f"  {len(all_nodes)} nodes found across {len(org_files)} files")

    # ── Pass 2: convert nodes to tiddlers ────────────────────────
    print("Pass 2: converting nodes...")

    # Map source file path → root (file-level) node, for parent lookup
    root_node_by_file: dict[str, Node] = {}
    for node in all_nodes:
        if node.is_root:
            root_node_by_file[node.source_file] = node

    new_manifest: dict[str, ManifestEntry] = {}
    built = 0
    skipped = 0
    errors = 0

    for node in all_nodes:
        if not should_convert(node.tags):
            continue
        if not should_convert_title(node.title):
            continue
        tid_filename = safe_filename(node.title) + ".tid"
        source_mtime = Path(node.source_file).stat().st_mtime
        new_manifest[tid_filename] = ManifestEntry(
            source_file=node.source_file,
            node_id=node.id,
            source_mtime=source_mtime,
        )

        if not needs_rebuild(node, tid_filename, manifest, outdir):
            skipped += 1
            continue

        # Attach parent for subtree nodes under a @biblio-tagged root
        parent: Node | None = None
        if not node.is_root:
            file_root = root_node_by_file.get(node.source_file)
            if file_root and "@biblio" in file_root.tags:
                parent = file_root

        try:
            outpath = node_to_tid(node, link_map, outdir, parent)
            print(f"  → {outpath.name}")
            built += 1
        except subprocess.CalledProcessError as e:
            print(f'ERROR: pandoc failed on node "{node.title}" in {node.source_file}')
            print(f"  stderr: {e.stderr}")
            sys.exit(1)
        except Exception as e:
            print(f'ERROR: failed to convert node "{node.title}" in {node.source_file}')
            print(f"  {type(e).__name__}: {e}")
            sys.exit(1)

    write_tag_tiddlers(TAG_COLORS, outdir)
    # ── Orphan cleanup ────────────────────────────────────────────
    deleted = 0
    for tid_filename, entry in manifest.items():
        if tid_filename not in new_manifest:
            orphan = outdir / tid_filename
            if orphan.exists():
                orphan.unlink()
                print(f"  ✗ deleted orphan: {tid_filename}")
                deleted += 1

    # ── Save manifest ─────────────────────────────────────────────
    save_manifest(new_manifest, manifest_path)

    # ── Summary ───────────────────────────────────────────────────
    print(f"\nDone. built={built} skipped={skipped} deleted={deleted}")

    subprocess.run(["tiddlywiki", "tiddlywiki", "--output", "docs", "--build", "index"])


if __name__ == "__main__":
    main()
