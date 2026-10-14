"""Show the current Codex session UUID without per-launch config overrides."""

import argparse
import json
from pathlib import Path
import re
import shutil
import tomllib


def configure(text: str) -> tuple[str, list[str]]:
    config = tomllib.loads(text)
    current = config.get("tui", {}).get("status_line")
    items = current if isinstance(current, list) else [
        "model-with-reasoning", "context-remaining"
    ]
    items = ["session-id", *(item for item in items if item != "session-id")]
    setting = "status_line = " + json.dumps(items)
    section = re.search(r"(?m)^\[tui\][ \t]*(?:#.*)?$", text)
    if section is None:
        result = text.rstrip() + "\n\n[tui]\n" + setting + "\n"
    else:
        start = section.end()
        next_section = re.search(r"(?m)^\[", text[start:])
        end = start + next_section.start() if next_section else len(text)
        body = text[start:end]
        existing = re.search(
            r"(?m)^status_line[ \t]*=[ \t]*(?:\[[\s\S]*?\]|null)[ \t]*(?:#.*)?$", body
        )
        if existing:
            body = body[:existing.start()] + setting + body[existing.end():]
        elif "status_line" in config.get("tui", {}):
            raise ValueError("Cannot safely replace this status_line assignment")
        else:
            body = "\n" + setting + body
        result = text[:start] + body + text[end:]
    # Validate before writing and leave all other settings intact.
    updated = tomllib.loads(result)
    original_tui = dict(config.get("tui", {}))
    original_tui["status_line"] = items
    expected = dict(config, tui=original_tui)
    if updated != expected:
        raise ValueError("Unexpected configuration changes")
    return result, items


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--path", type=Path, default=Path.home() / ".codex/config.toml")
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    source = args.path.read_text()
    updated, items = configure(source)
    if not args.dry_run and updated != source:
        backup = args.path.with_suffix(".toml.before-emacs-session-id")
        if not backup.exists():
            shutil.copy2(args.path, backup)
        if args.path.read_text() != source:
            raise RuntimeError("Configuration changed while preparing the update")
        args.path.write_text(updated)
    print("Codex status line:", ", ".join(items))


if __name__ == "__main__":
    main()
