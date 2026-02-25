#!/bin/bash
# Runs the FileScanner Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestFileScanner.exe --console testresults.txt"
cat testresults.txt
