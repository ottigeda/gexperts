#!/bin/bash
# Builds the 64-bit GExperts installer for RAD Studio 13 under WSL

cd "$(dirname "$0")"
cmd.exe /c "MakeD13_64Installer.cmd"
