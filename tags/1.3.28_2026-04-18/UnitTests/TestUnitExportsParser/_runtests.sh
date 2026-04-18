#!/bin/bash
# Runs the UnitExportsParser Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestUnitExportsParser.exe --console testresults.txt"
EXIT_CODE=$?
cat testresults.txt
exit $EXIT_CODE
