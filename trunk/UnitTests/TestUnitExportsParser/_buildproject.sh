#!/bin/bash
# Builds the UnitExportsParser Unit Tests under WSL by calling the batch file.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

QUIET=0
for arg in "$@"; do
  if [ "$arg" = "--quiet" ]; then
    QUIET=1
  fi
done

if [ "$QUIET" = "1" ]; then
  LOGFILE="/tmp/build_TestUnitExportsParser.txt"
  cmd.exe /c "_BuildProject.cmd --nowait" > "$LOGFILE" 2>&1
  EXIT_CODE=$?
  if [ $EXIT_CODE -ne 0 ]; then
    echo "BUILD FAILED (exit code $EXIT_CODE). Last 30 lines:"
    tail -30 "$LOGFILE"
  fi
  exit $EXIT_CODE
else
  cmd.exe /c "_BuildProject.cmd --nowait"
fi
