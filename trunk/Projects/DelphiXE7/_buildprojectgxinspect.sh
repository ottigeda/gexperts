#!/bin/bash
cd "$(dirname "$0")"
if [ "$1" = "--quiet" ]; then
  LOGFILE=../../tmp/build_$(basename "$(pwd)")_gxinspect.txt
  mkdir -p ../../tmp
  cmd.exe /c "__Build_Project_GxInspect.cmd --nowait" > "$LOGFILE" 2>&1
  RC=$?
  if [ $RC -ne 0 ]; then
    echo "BUILD FAILED — last 30 lines:"
    tail -30 "$LOGFILE"
  else
    WARNINGS=$(grep -i "warning" "$LOGFILE" | grep -iv "Warning(s)")
    if [ -n "$WARNINGS" ]; then
      echo "BUILD SUCCEEDED with warnings:"
      echo "$WARNINGS"
    fi
  fi
  exit $RC
else
  cmd.exe /c "__Build_Project_GxInspect.cmd --nowait"
fi
