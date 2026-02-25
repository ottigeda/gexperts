#!/bin/bash
# Runs the PasLex Unit Tests under WSL in console mode.
# Results are written to testresults.txt.

cd "$(dirname "$0")"
cmd.exe /c "TestPasLex.exe --console testresults.txt"
cat testresults.txt
