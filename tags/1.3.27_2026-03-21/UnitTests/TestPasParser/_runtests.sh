#!/bin/bash
# Runs the PasParser Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestPasParser.exe --console testresults.txt"
cat testresults.txt
