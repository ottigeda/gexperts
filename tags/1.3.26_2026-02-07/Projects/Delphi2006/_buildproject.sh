#!/bin/bash
# Builds the GExperts DLL for Delphi 2006 under WSL

cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
