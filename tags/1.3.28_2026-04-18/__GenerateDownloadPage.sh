#!/usr/bin/env bash
# __GenerateDownloadPage.sh — Generate the HTML download page for a GExperts release
#
# Usage: bash __GenerateDownloadPage.sh
#
# Reads version from Projects/GExperts_version.ini, installer prefixes from
# MakeInstallers.cmd, and changelog from Documentation/CHANGES-<version>.md.
# Writes the HTML file to BuildInstall/Output/<version>_<date>/<version>.html
#
# Requires: release.conf for the download base URL.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# ── Load credentials (needed for HTTPS_BASE_URL in download links) ────────────
CREDS_FILE="$HOME/.config/gexperts/release.conf"
if [[ ! -f "$CREDS_FILE" ]]; then
  echo "ERROR: Credentials file not found: $CREDS_FILE"
  echo "Copy release-credentials-template.conf to $CREDS_FILE and fill in values."
  exit 1
fi
# shellcheck source=/dev/null
source "$CREDS_FILE"

if [[ -z "${HTTPS_BASE_URL:-}" ]]; then
  echo "ERROR: HTTPS_BASE_URL is not set in $CREDS_FILE"
  exit 1
fi

# ── Read version ──────────────────────────────────────────────────────────────
MAJOR_VER=$(grep -m1 '^MajorVer=' Projects/GExperts_version.ini | cut -d= -f2 | tr -d '\r')
MINOR_VER=$(grep -m1 '^MinorVer=' Projects/GExperts_version.ini | cut -d= -f2 | tr -d '\r')
RELEASE=$(grep -m1 '^Release=' Projects/GExperts_version.ini | cut -d= -f2 | tr -d '\r')
VERSION="${MAJOR_VER}.${MINOR_VER}.${RELEASE}"
TODAY=$(date +%Y-%m-%d)
SUBDIR="${VERSION}_${TODAY}"
RELEASE_DIR="BuildInstall/Output/${SUBDIR}"

echo "Version: $VERSION"
echo "Date:    $TODAY"

# ── Extract installer prefixes from MakeInstallers.cmd ────────────────────────
EXPECTED_PREFIXES=()
while IFS= read -r line; do
  line="${line%$'\r'}"
  [[ "$line" =~ ^[[:space:]]*rem ]] && continue
  if [[ "$line" =~ ^call\ :makeinst\ ([^[:space:]]+)(\ ([^[:space:]]+))?[[:space:]]*$ ]]; then
    prefix="${BASH_REMATCH[3]:-${BASH_REMATCH[1]}}"
    EXPECTED_PREFIXES+=("$prefix")
  fi
done < BuildInstall/MakeInstallers.cmd

echo "Found ${#EXPECTED_PREFIXES[@]} installer prefixes."

# ── Determine previous version ────────────────────────────────────────────────
PREV_VERSION=""
while IFS= read -r f; do
  v=$(basename "$f" .md | sed 's/CHANGES-//')
  [[ "$v" != "$VERSION" ]] && PREV_VERSION="$v"
done < <(ls -v Documentation/CHANGES-*.md 2>/dev/null)
[[ -z "$PREV_VERSION" ]] && PREV_VERSION="(previous version)"

CHANGES_FILE="Documentation/CHANGES-${VERSION}.md"
if [[ ! -f "$CHANGES_FILE" ]]; then
  echo "ERROR: Changelog not found: $CHANGES_FILE"
  exit 1
fi

# ── Labels ────────────────────────────────────────────────────────────────────
declare -A PREFIX_LABEL=(
  [D6]="Delphi 6"
  [D7]="Delphi 7"
  [Delphi2005]="Delphi 2005"
  [BDS2006]="Delphi/C++Builder 2006"
  [Delphi2007]="Delphi 2007"
  [RS2009]="RAD Studio 2009"
  [RS2010]="RAD Studio 2010"
  [RSXE1]="RAD Studio XE"
  [RSXE2]="RAD Studio XE2"
  [RSXE3]="RAD Studio XE3"
  [RSXE4]="RAD Studio XE4"
  [RSXE5]="RAD Studio XE5"
  [RSXE6]="RAD Studio XE6"
  [RSXE7]="RAD Studio XE7"
  [RSXE8]="RAD Studio XE8"
  [RS10]="RAD Studio 10 Seattle"
  [RS10.1]="RAD Studio 10.1 Berlin"
  [RS10.2]="RAD Studio 10.2 Tokyo"
  [RS10.3]="RAD Studio 10.3 Rio"
  [RS10.4]="RAD Studio 10.4 Sydney"
  [RS11]="RAD Studio 11 Alexandria"
  [RS12]="RAD Studio 12 Athens"
  [RS13]="RAD Studio 13"
)

# ── Convert CHANGES markdown to HTML ─────────────────────────────────────────
CHANGES_HTML=$(tr -d '\r' < "$CHANGES_FILE" | awk '
  BEGIN { in_ul = 0 }

  /^# /  { next }

  /^## / {
    if (in_ul) { print "</ul>"; in_ul = 0 }
    heading = substr($0, 4)
    gsub(/&/, "\\&amp;", heading)
    gsub(/</, "\\&lt;",  heading)
    gsub(/>/, "\\&gt;",  heading)
    print "<h3>" heading "</h3>"
    next
  }

  /^- / {
    if (!in_ul) { print "<ul>"; in_ul = 1 }
    item = substr($0, 3)
    gsub(/&/, "\\&amp;", item)
    gsub(/</, "\\&lt;",  item)
    gsub(/>/, "\\&gt;",  item)
    while (match(item, /`[^`]*`/)) {
      code = substr(item, RSTART + 1, RLENGTH - 2)
      item = substr(item, 1, RSTART - 1) "<code>" code "</code>" substr(item, RSTART + RLENGTH)
    }
    print "<li>" item "</li>"
    next
  }

  /^[[:space:]]*$/ {
    if (in_ul) { print "</ul>"; in_ul = 0 }
    next
  }

  {
    if (in_ul) { print "</ul>"; in_ul = 0 }
    line = $0
    gsub(/&/, "\\&amp;", line)
    gsub(/</, "\\&lt;",  line)
    gsub(/>/, "\\&gt;",  line)
    print "<p>" line "</p>"
  }

  END { if (in_ul) print "</ul>" }
')

# ── Write HTML file ──────────────────────────────────────────────────────────
mkdir -p "$RELEASE_DIR"
HTML_FILE="${RELEASE_DIR}/${VERSION}.html"

{
  cat <<__HTML_HEAD__
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>GExperts ${VERSION} Downloads</title>
  <style>
    body { font-family: sans-serif; max-width: 900px; margin: 2em auto; }
    table { border-collapse: collapse; width: 100%; }
    th, td { text-align: left; padding: 0.4em 0.8em; border: 1px solid #ccc; }
    th { background: #f0f0f0; }
    code { background: #f4f4f4; padding: 0.1em 0.3em; border-radius: 3px; font-size: 0.9em; }
  </style>
</head>
<body>
<h1>GExperts ${VERSION}</h1>
<p>Released: ${TODAY}</p>

<h2>Downloads</h2>
<table>
  <tr><th>Delphi Version</th><th>Installer</th></tr>
__HTML_HEAD__

  # List newest Delphi version first
  for (( i=${#EXPECTED_PREFIXES[@]}-1; i>=0; i-- )); do
    prefix="${EXPECTED_PREFIXES[$i]}"
    filename="GExperts_${prefix}_${VERSION}_${TODAY}.exe"
    label="${PREFIX_LABEL[$prefix]:-$prefix}"
    url="${HTTPS_BASE_URL}/${SUBDIR}/${filename}"
    echo "  <tr><td>${label}</td><td><a href=\"${url}\">${filename}</a></td></tr>"
  done

  cat <<__HTML_TAIL__
</table>

<h2>Changes since ${PREV_VERSION}</h2>
${CHANGES_HTML}
</body>
</html>
__HTML_TAIL__
} > "$HTML_FILE"

echo "Generated: $HTML_FILE"
