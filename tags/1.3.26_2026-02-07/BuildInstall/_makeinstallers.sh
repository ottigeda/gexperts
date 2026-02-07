#!/bin/bash
# Builds all 32-bit GExperts installers under WSL

cd "$(dirname "$0")"
cmd.exe /c "MakeInstallers.cmd"
