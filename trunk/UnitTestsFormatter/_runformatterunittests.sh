#!/bin/bash
# Runs the Formatter Unit Tests under WSL.

cd "$(dirname "$0")"

# Delete stale output files from previous runs
rm -f Testcases/output/*/testfile_*.pas

./GX_FormatterUnitTests.exe --console testresults.txt
