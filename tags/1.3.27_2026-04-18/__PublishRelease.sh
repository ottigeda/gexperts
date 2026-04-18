#!/usr/bin/env bash
# __PublishRelease.sh — GExperts release automation
#
# Usage: bash __PublishRelease.sh [--dry-run]
#
# Requires: lftp, curl, sha256sum, awk, svn.exe (Windows SVN via WSL PATH)
# Credentials: ~/.config/gexperts/release.conf  (chmod 600)
#              Copy release-credentials-template.conf and fill in values.
#
# Steps:
#   1. Read version from Projects/GExperts_version.ini, verify all installers present
#   2. Move installers into BuildInstall/Output/<version>_<date>/
#   3. Upload installers via SFTP (lftp)
#   4. Verify each uploaded file via HTTPS + SHA-256
#   5. Generate HTML download page
#   6. Upload HTML page via SFTP
#   7. Create SVN tag (asks for confirmation first)
#   8. Create Documentation/CHANGES-<next_version>.md and svn add
#   9. Bump Release= in GExperts_version.ini and svn commit

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# ── Parse arguments ────────────────────────────────────────────────────────────
DRY_RUN=false
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=true ;;
    *) echo "ERROR: Unknown argument: $arg"; exit 1 ;;
  esac
done

$DRY_RUN && echo "=== DRY RUN MODE — SFTP/SVN commands will NOT be executed ==="

# ── Prerequisites check ────────────────────────────────────────────────────────
for cmd in lftp curl sha256sum awk; do
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "ERROR: Required tool not found: $cmd"
    exit 1
  fi
done
if ! command -v svn.exe >/dev/null 2>&1; then
  echo "ERROR: svn.exe not found in PATH. Ensure Windows SVN is accessible from WSL."
  exit 1
fi

# ── Load credentials ───────────────────────────────────────────────────────────
CREDS_FILE="$HOME/.config/gexperts/release.conf"
if [[ ! -f "$CREDS_FILE" ]]; then
  echo "ERROR: Credentials file not found: $CREDS_FILE"
  echo "Copy release-credentials-template.conf to $CREDS_FILE and fill in values (chmod 600)."
  exit 1
fi
# shellcheck source=/dev/null
source "$CREDS_FILE"

for var in SFTP_HOST SFTP_USER SFTP_PASS SFTP_BASE_PATH SFTP_HTML_PATH \
           HTTPS_BASE_URL HTTPS_HTML_URL SVN_REPO_ROOT; do
  if [[ -z "${!var:-}" ]]; then
    echo "ERROR: $var is not set in $CREDS_FILE"
    exit 1
  fi
done

# ── Helper ─────────────────────────────────────────────────────────────────────
step() {
  echo ""
  echo "=== $* ==="
  ( printf '\033]0;GExperts Release — %s\007' "$*" > /dev/tty ) 2>/dev/null || true
}

# ══════════════════════════════════════════════════════════════════════════════
step "Step 1: Read version and verify installers"
# ══════════════════════════════════════════════════════════════════════════════

MAJOR_VER=$(grep -m1 '^MajorVer=' Projects/GExperts_version.ini | cut -d= -f2 | tr -d '\r')
MINOR_VER=$(grep -m1 '^MinorVer=' Projects/GExperts_version.ini | cut -d= -f2 | tr -d '\r')
RELEASE=$(grep -m1 '^Release=' Projects/GExperts_version.ini | cut -d= -f2 | tr -d '\r')
VERSION="${MAJOR_VER}.${MINOR_VER}.${RELEASE}"
TODAY=$(date +%Y-%m-%d)
SUBDIR="${VERSION}_${TODAY}"
RELEASE_DIR="BuildInstall/Output/${SUBDIR}"

echo "Version: $VERSION"
echo "Date:    $TODAY"

# Extract expected installer prefixes from MakeInstallers.cmd
# Logic mirrors the :makeinst subroutine: if a second arg is given it is the
# prefix, otherwise the first arg doubles as the prefix.
EXPECTED_PREFIXES=()
while IFS= read -r line; do
  line="${line%$'\r'}"                              # strip CR from CRLF files
  [[ "$line" =~ ^[[:space:]]*rem ]] && continue    # skip commented lines
  if [[ "$line" =~ ^call\ :makeinst\ ([^[:space:]]+)(\ ([^[:space:]]+))?[[:space:]]*$ ]]; then
    prefix="${BASH_REMATCH[3]:-${BASH_REMATCH[1]}}"
    EXPECTED_PREFIXES+=("$prefix")
  fi
done < BuildInstall/MakeInstallers.cmd

echo "Expected ${#EXPECTED_PREFIXES[@]} installers:"
MISSING=0
for prefix in "${EXPECTED_PREFIXES[@]}"; do
  filename="GExperts_${prefix}_${VERSION}_${TODAY}.exe"
  if [[ -f "BuildInstall/Output/$filename" ]] || [[ -f "$RELEASE_DIR/$filename" ]]; then
    echo "  OK      $filename"
  else
    echo "  MISSING $filename"
    MISSING=$((MISSING + 1))
  fi
done

if [[ $MISSING -gt 0 ]]; then
  echo "ERROR: $MISSING installer(s) missing. Run __BuildEverything.cmd first."
  exit 1
fi
echo "All ${#EXPECTED_PREFIXES[@]} installers present."

# ══════════════════════════════════════════════════════════════════════════════
step "Step 2: Move installers into versioned subdirectory"
# ══════════════════════════════════════════════════════════════════════════════

mkdir -p "$RELEASE_DIR"

if $DRY_RUN; then
  echo "[DRY RUN] Would move any installers from BuildInstall/Output/ to $RELEASE_DIR/"
else
  # Move any installers that are still in the flat Output/ directory
  shopt -s nullglob
  ROOT_INSTALLERS=("BuildInstall/Output/GExperts_"*"_${VERSION}_${TODAY}.exe")
  shopt -u nullglob
  if [[ ${#ROOT_INSTALLERS[@]} -gt 0 ]]; then
    echo "Moving ${#ROOT_INSTALLERS[@]} installer(s) to $RELEASE_DIR/"
    mv "${ROOT_INSTALLERS[@]}" "$RELEASE_DIR/"
  else
    echo "Installers already in $RELEASE_DIR/ (nothing to move)"
  fi
fi

if ! $DRY_RUN; then
  for prefix in "${EXPECTED_PREFIXES[@]}"; do
    if [[ ! -f "$RELEASE_DIR/GExperts_${prefix}_${VERSION}_${TODAY}.exe" ]]; then
      echo "ERROR: Missing from subdir: GExperts_${prefix}_${VERSION}_${TODAY}.exe"
      exit 1
    fi
  done
  echo "All ${#EXPECTED_PREFIXES[@]} installers confirmed in $RELEASE_DIR/"
fi

# ══════════════════════════════════════════════════════════════════════════════
step "Step 3: Upload installers via SFTP"
# ══════════════════════════════════════════════════════════════════════════════

REMOTE_SUBDIR="${SFTP_BASE_PATH}/${SUBDIR}"

if $DRY_RUN; then
  echo "[DRY RUN] Would create remote dir: $REMOTE_SUBDIR"
  for prefix in "${EXPECTED_PREFIXES[@]}"; do
    echo "[DRY RUN] Would upload: GExperts_${prefix}_${VERSION}_${TODAY}.exe"
  done
else
  echo "Uploading ${#EXPECTED_PREFIXES[@]} files to $REMOTE_SUBDIR/ ..."
  lftp -c "
    set sftp:auto-confirm yes
    open -u ${SFTP_USER},${SFTP_PASS} sftp://${SFTP_HOST}
    set cmd:fail-exit false
    mkdir -p ${REMOTE_SUBDIR}
    set cmd:fail-exit true
    mput -O ${REMOTE_SUBDIR} ${RELEASE_DIR}/*.exe
  "
  echo "Upload complete."
fi

# ══════════════════════════════════════════════════════════════════════════════
step "Step 4: Verify HTTPS downloads"
# ══════════════════════════════════════════════════════════════════════════════

echo "Downloading from: ${HTTPS_BASE_URL}/${SUBDIR}/"
VERIFY_FAIL=0
VERIFY_TMP="${RELEASE_DIR}/.verify_$$"
mkdir -p "$VERIFY_TMP"
# Ensure temp dir is cleaned up even on error
cleanup_verify() { rm -rf "$VERIFY_TMP"; }
trap cleanup_verify EXIT

for prefix in "${EXPECTED_PREFIXES[@]}"; do
  filename="GExperts_${prefix}_${VERSION}_${TODAY}.exe"
  url="${HTTPS_BASE_URL}/${SUBDIR}/${filename}"
  if $DRY_RUN; then
    echo "[DRY RUN] Would verify: $url"
    continue
  fi
  printf "  %-48s " "$filename"
  if curl -fsSL "$url" -o "${VERIFY_TMP}/${filename}" 2>/dev/null; then
    local_sha=$(sha256sum "${RELEASE_DIR}/${filename}" | cut -d' ' -f1)
    remote_sha=$(sha256sum "${VERIFY_TMP}/${filename}" | cut -d' ' -f1)
    if [[ "$local_sha" == "$remote_sha" ]]; then
      echo "PASS"
    else
      echo "FAIL (SHA-256 mismatch)"
      VERIFY_FAIL=$((VERIFY_FAIL + 1))
    fi
  else
    echo "FAIL (download error)"
    VERIFY_FAIL=$((VERIFY_FAIL + 1))
  fi
done

cleanup_verify
trap - EXIT

if ! $DRY_RUN; then
  if [[ $VERIFY_FAIL -gt 0 ]]; then
    echo "ERROR: $VERIFY_FAIL file(s) failed HTTPS verification."
    exit 1
  fi
  echo "All ${#EXPECTED_PREFIXES[@]} files verified successfully."
fi

# ══════════════════════════════════════════════════════════════════════════════
step "Step 5: Generate download page HTML"
# ══════════════════════════════════════════════════════════════════════════════

# Determine previous version (the last CHANGES file that is not this version)
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

HTML_FILE="${RELEASE_DIR}/${VERSION}.html"

# Installer prefix → human-readable Delphi version label
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

# Convert CHANGES markdown to HTML using awk.
# Handles: # (skip), ## (h3), - items (ul/li), blank lines, and plain paragraphs.
# Backtick-quoted spans become <code>...</code>.
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

# Write HTML file using a grouped redirect so all pieces go to one file
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

# ══════════════════════════════════════════════════════════════════════════════
step "Step 6: Upload download page HTML"
# ══════════════════════════════════════════════════════════════════════════════

if $DRY_RUN; then
  echo "[DRY RUN] Would upload: $(basename "$HTML_FILE") → ${SFTP_HTML_PATH}/${VERSION}.html"
else
  lftp -c "
    set sftp:auto-confirm yes
    open -u ${SFTP_USER},${SFTP_PASS} sftp://${SFTP_HOST}
    set cmd:fail-exit false
    mkdir -p ${SFTP_HTML_PATH}
    set cmd:fail-exit true
    put ${HTML_FILE} -o ${SFTP_HTML_PATH}/${VERSION}.html
  "
  echo "Uploaded: ${HTTPS_HTML_URL}/${VERSION}.html"
fi

# ══════════════════════════════════════════════════════════════════════════════
step "Step 7: Create SVN tag"
# ══════════════════════════════════════════════════════════════════════════════

TAG_NAME="${VERSION}_${TODAY}"
TAG_URL="${SVN_REPO_ROOT}/tags/${TAG_NAME}"
TRUNK_URL="${SVN_REPO_ROOT}/trunk"

# Check if tag already exists (skip check in dry-run — svn.exe output goes to
# a SourceForge server and may be slow; just show what would happen)
TAG_EXISTS=false
if ! $DRY_RUN; then
  if svn.exe list "$TAG_URL" >/dev/null 2>&1; then
    TAG_EXISTS=true
  fi
fi

if $TAG_EXISTS; then
  echo "Tag already exists: $TAG_URL (skipping)"
else
  echo "SVN tag to create: $TAG_URL"
  if $DRY_RUN; then
    echo "[DRY RUN] svn.exe copy $TRUNK_URL $TAG_URL -m \"Tag release ${VERSION}\""
  else
    read -r -p "Create tag now? [y/N]: " confirm
    if [[ "${confirm,,}" != "y" ]]; then
      echo "Aborted by user. Remaining steps (8, 9) were not run."
      exit 1
    fi
    svn.exe copy "$TRUNK_URL" "$TAG_URL" -m "Tag release ${VERSION}"
    echo "Tag created: $TAG_URL"
  fi
fi

# ══════════════════════════════════════════════════════════════════════════════
step "Step 8: Create next version changelog"
# ══════════════════════════════════════════════════════════════════════════════

NEXT_RELEASE=$((RELEASE + 1))
NEXT_VERSION="${MAJOR_VER}.${MINOR_VER}.${NEXT_RELEASE}"
NEXT_CHANGES="Documentation/CHANGES-${NEXT_VERSION}.md"

if [[ -f "$NEXT_CHANGES" ]]; then
  echo "Already exists: $NEXT_CHANGES (skipping)"
elif $DRY_RUN; then
  echo "[DRY RUN] Would create: $NEXT_CHANGES"
  echo "[DRY RUN] svn.exe add Documentation/CHANGES-${NEXT_VERSION}.md"
else
  cat > "$NEXT_CHANGES" <<__CHANGELOG__
# GExperts ${NEXT_VERSION} Changes

Changes since version ${VERSION} (released ${TODAY})

## New Features

## Bug Fixes

## Contributors

- Thomas Mueller (twm)
__CHANGELOG__
  echo "Created: $NEXT_CHANGES"
  svn.exe add "Documentation/CHANGES-${NEXT_VERSION}.md"
  echo "Added to SVN."
fi

# ══════════════════════════════════════════════════════════════════════════════
step "Step 9: Bump version number and commit"
# ══════════════════════════════════════════════════════════════════════════════

VERSION_INI="Projects/GExperts_version.ini"
CURRENT_RELEASE=$(grep -m1 '^Release=' "$VERSION_INI" | cut -d= -f2 | tr -d '\r')

if [[ "$CURRENT_RELEASE" != "$RELEASE" ]]; then
  echo "WARNING: $VERSION_INI has Release=${CURRENT_RELEASE}, expected ${RELEASE}."
  echo "Version bump may have already been applied. Skipping."
else
  if $DRY_RUN; then
    echo "[DRY RUN] Would update $VERSION_INI: Release=${RELEASE} → Release=${NEXT_RELEASE}"
    echo "[DRY RUN] svn.exe commit -m \"Release ${VERSION}: bump version to ${NEXT_VERSION}, add changelog for next release\""
  else
    # sed -i can fail on NTFS mounts in WSL; use temp-file workaround instead
    sed "s/^Release=${RELEASE}$/Release=${NEXT_RELEASE}/" "$VERSION_INI" \
      > /tmp/gx_version_bump_$$.ini
    cp /tmp/gx_version_bump_$$.ini "$VERSION_INI"
    rm /tmp/gx_version_bump_$$.ini
    echo "Updated $VERSION_INI: Release=${RELEASE} → Release=${NEXT_RELEASE}"

    svn.exe commit \
      -m "Release ${VERSION}: bump version to ${NEXT_VERSION}, add changelog for next release" \
      "Projects/GExperts_version.ini" \
      "Documentation/CHANGES-${NEXT_VERSION}.md"
    echo "Committed."
  fi
fi

# ══════════════════════════════════════════════════════════════════════════════
( printf '\033]0;GExperts Release — Complete\007' > /dev/tty ) 2>/dev/null || true
echo ""
echo "=== Release ${VERSION} complete! ==="
echo ""
echo "  Installers:    ${HTTPS_BASE_URL}/${SUBDIR}/"
echo "  Download page: ${HTTPS_HTML_URL}/${VERSION}.html"
echo "  SVN tag:       ${TAG_URL}"
echo "  Next version:  ${NEXT_VERSION}"
