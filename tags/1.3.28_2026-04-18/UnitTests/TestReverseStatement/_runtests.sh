#!/bin/bash
# Runs the Reverse Statement Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestReverseStatement.exe --console testresults.txt"
cat testresults.txt
