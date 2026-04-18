#!/bin/bash
# Runs the Rename Identifier Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestRenameIdentifier.exe --console testresults.txt"
EXIT_CODE=$?
cat testresults.txt
exit $EXIT_CODE
