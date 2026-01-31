#!/bin/bash
# Builds the GExperts 64-bit DLL for RAD Studio 13 under WSL

cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
