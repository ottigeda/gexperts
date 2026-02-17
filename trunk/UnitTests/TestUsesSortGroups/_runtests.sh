#!/bin/bash
# Runs the Uses Sort Groups Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestUsesSortGroups.exe --console testresults.txt"
cat testresults.txt
