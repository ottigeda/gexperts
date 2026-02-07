#!/bin/bash
# Build DelphiLspProbe from WSL
cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
