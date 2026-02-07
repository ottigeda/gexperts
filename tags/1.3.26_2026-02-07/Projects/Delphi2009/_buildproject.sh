#!/bin/bash
# Builds the GExperts DLL for Delphi 2009 under WSL

cd "$(dirname "$0")"
cmd.exe /c "__Build_Project.cmd --nowait"
