#!/bin/bash
# Runs the Formatter Unit Tests under WSL.

cd "$(dirname "$0")"

# Delete stale output files from previous runs
rm -f Testcases/output/*/testfile_*.pas
rm -f Testcases/output/*/failure_*.pas
rm -f Testcases/SpecialCases/*/output/*.pas

./GX_FormatterUnitTests.exe --console testresults.txt
