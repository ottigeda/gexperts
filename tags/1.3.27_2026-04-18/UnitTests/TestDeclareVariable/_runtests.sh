#!/bin/bash
# Runs the Declare Variable Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestDeclareVariable.exe --console testresults.txt"
EXIT_CODE=$?
cat testresults.txt
exit $EXIT_CODE
