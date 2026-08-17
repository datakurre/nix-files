#!/usr/bin/env python3

import argparse
import datetime
import re
import subprocess
import sys
from pathlib import Path


VERSION_RE = re.compile(
    r"^\s*(?P<version>\d+\.\d+\.\d+)"
    r"(?:\s*\((?P<date>[^)]*)\))?\s*$",
    re.IGNORECASE,
)
CHANGELOG_NAMES = ("CHANGES.md", "CHANGELOG.md", "CHANGES.rst", "CHANGELOG.rst")


def git(*args: str) -> str:
    try:
        return subprocess.run(
            ["git", *args],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        ).stdout
    except FileNotFoundError as error:
        raise RuntimeError("git is not available") from error
    except subprocess.CalledProcessError as error:
        message = error.stderr.strip() or "git command failed"
        raise RuntimeError(message) from error


def repository_root() -> Path:
    return Path(git("rev-parse", "--show-toplevel").strip())


def latest_tag() -> str | None:
    try:
        tag = git("describe", "--tags", "--abbrev=0", "HEAD").strip()
    except RuntimeError:
        return None
    return tag or None


def commits_since_tag(tag: str | None) -> list[tuple[str, str, str, str]]:
    range_arg = f"{tag}..HEAD" if tag else "HEAD"
    records = git(
        "log",
        "--no-color",
        f"--format=%H%x1f%an%x1f%s%x1f%b%x1e",
        range_arg,
    )
    commits = []
    for record in records.rstrip("\x1e").split("\x1e"):
        fields = record.rstrip("\n").split("\x1f")
        if len(fields) != 4:
            continue
        commits.append(tuple(fields))
    return commits


def increment_patch(version: str) -> str:
    major, minor, patch = (int(part) for part in version.split("."))
    return f"{major}.{minor}.{patch + 1}"


def version_from_text(text: str) -> str | None:
    for line in text.splitlines():
        match = VERSION_RE.match(line)
        if match:
            return match.group("version")
    return None


def version_from_tag(tag: str | None) -> str | None:
    if tag is None:
        return None
    match = re.search(r"\d+\.\d+\.\d+", tag)
    return match.group(0) if match else None


def next_version(tag: str | None, changelog: str = "") -> str:
    version = version_from_tag(tag) or version_from_text(changelog)
    return increment_patch(version) if version else "1.0.0"


def commit_block(commit: tuple[str, str, str, str]) -> str:
    _, author, subject, body = commit
    lines = [f"- {subject}"]
    body_lines = body.strip().splitlines()
    lines.extend(f"  {line}" if line else "  " for line in body_lines)
    lines.append(f"  [{author}]")
    return "\n".join(lines)


def section(version: str, commits: list[tuple[str, str, str, str]]) -> str:
    title = f"{version} (unreleased)"
    return f"{title}\n{'-' * len(title)}\n\n" + "\n\n".join(
        commit_block(commit) for commit in commits
    )


def header_lines(text: str) -> list[tuple[int, str]]:
    return [
        (index, match.group("version"))
        for index, line in enumerate(text.splitlines(keepends=True))
        if (match := VERSION_RE.match(line.rstrip("\r\n")))
    ]


def insert_into_unreleased(text: str, commits: list[tuple[str, str, str, str]]) -> str:
    lines = text.splitlines(keepends=True)
    headers = header_lines(text)
    unreleased = next(
        (
            (index, version)
            for index, version in headers
            if not (match := VERSION_RE.match(lines[index].rstrip("\r\n"))).group("date")
            or match.group("date").strip().lower() == "unreleased"
        ),
        None,
    )
    if unreleased is None:
        return prepend_new_section(text, section(next_version(None, text), commits))

    header_index, _ = unreleased
    underline_index = header_index + 1
    if underline_index >= len(lines) or not re.fullmatch(
        r"\s*-{3,}\s*", lines[underline_index].rstrip("\r\n")
    ):
        raise RuntimeError("unreleased changelog header is missing its '-' underline")

    next_header = next(
        (index for index, _ in headers if index > header_index), len(lines)
    )
    body = "".join(lines[underline_index + 1 : next_header]).lstrip("\n")
    inserted = "\n\n".join(commit_block(commit) for commit in commits)
    new_body = inserted + (f"\n\n{body}" if body else "\n")
    return "".join(lines[: underline_index + 1]) + "\n\n" + new_body + "".join(
        lines[next_header:]
    )


def prepend_new_section(text: str, new_section: str) -> str:
    lines = text.splitlines(keepends=True)
    if len(lines) >= 2 and re.fullmatch(r"\s*={3,}\s*", lines[1].rstrip("\r\n")):
        insertion = sum(len(line) for line in lines[:2])
        return text[:insertion] + "\n\n" + new_section + "\n\n" + text[insertion:].lstrip(
            "\n"
        )
    return new_section + "\n\n" + text


def find_changelog(root: Path) -> Path:
    changelog = next(
        (root / name for name in CHANGELOG_NAMES if (root / name).is_file()), None
    )
    if changelog is None:
        raise RuntimeError(
            "no changelog found (looked for " + ", ".join(CHANGELOG_NAMES) + ")"
        )
    return changelog


def update_changelog(
    root: Path, commits: list[tuple[str, str, str, str]]
) -> Path | None:
    changelog = find_changelog(root)

    content = changelog.read_text()
    new_commits = [
        commit for commit in commits if commit_block(commit) not in content
    ]
    if not new_commits:
        return None

    updated = insert_into_unreleased(content, new_commits)
    if not content.endswith("\n"):
        updated = updated.rstrip("\n") + "\n"
    changelog.write_text(updated)
    return changelog


def stamp_changelog(root: Path) -> Path:
    changelog = find_changelog(root)
    lines = changelog.read_text().splitlines(keepends=True)
    headers = header_lines("".join(lines))
    unreleased = next(
        (
            index
            for index, _ in headers
            if (match := VERSION_RE.match(lines[index].rstrip("\r\n")))
            and match.group("date")
            and match.group("date").strip().lower() == "unreleased"
        ),
        None,
    )
    if unreleased is None:
        raise RuntimeError("no unreleased changelog section found")

    header = VERSION_RE.match(lines[unreleased].rstrip("\r\n"))
    assert header is not None
    date = datetime.date.today().isoformat()
    title = f"{header.group('version')} ({date})"
    ending = "\r\n" if lines[unreleased].endswith("\r\n") else "\n"
    lines[unreleased] = title + ending
    underline = unreleased + 1
    if underline >= len(lines) or not re.fullmatch(
        r"\s*-{3,}\s*", lines[underline].rstrip("\r\n")
    ):
        raise RuntimeError("unreleased changelog header is missing its '-' underline")
    lines[underline] = "-" * len(title) + ending
    changelog.write_text("".join(lines))
    return changelog


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Show commits since the last tag in changelog format."
    )
    parser.add_argument(
        "--update",
        action="store_true",
        help="prepend the commits to the first existing changelog file",
    )
    parser.add_argument(
        "--stamp",
        action="store_true",
        help="replace the unreleased section date with today's date",
    )
    args = parser.parse_args()

    try:
        root = repository_root()
        tag = latest_tag()
        commits = commits_since_tag(tag)
        if args.stamp:
            if args.update and commits:
                update_changelog(root, commits)
            print(f"Stamped {stamp_changelog(root)}")
            return 0
        if not commits:
            return 0

        if args.update:
            changelog = update_changelog(root, commits)
            if changelog is not None:
                print(f"Updated {changelog}")
        else:
            print(section(next_version(tag), commits))
    except RuntimeError as error:
        print(f"git-changelog: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
