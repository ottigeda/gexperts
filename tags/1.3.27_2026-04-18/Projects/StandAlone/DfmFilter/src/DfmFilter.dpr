program DfmFilter;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes;

const
  OffendingPrefixes: array[0..4] of string = (
    'Margins.Left =',
    'Margins.Top =',
    'Margins.Right =',
    'Margins.Bottom =',
    'AlignWithMargins ='
  );

var
  DoFix: Boolean = False;
  Quiet: Boolean = False;
  Paths: TStringList;
  TotalFiles: Integer = 0;
  TotalHits: Integer = 0;
  FilesWithHits: Integer = 0;

function IsOffendingLine(const _Line: string): Boolean;
var
  Trimmed: string;
  i: Integer;
begin
  Trimmed := Trim(_Line);
  for i := Low(OffendingPrefixes) to High(OffendingPrefixes) do begin
    if Copy(Trimmed, 1, Length(OffendingPrefixes[i])) = OffendingPrefixes[i] then begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function OffendingProperty(const _Line: string): string;
var
  Trimmed: string;
  p: Integer;
begin
  Trimmed := Trim(_Line);
  p := Pos(' =', Trimmed);
  if p > 0 then
    Result := Copy(Trimmed, 1, p - 1)
  else
    Result := Trimmed;
end;

procedure ProcessFile(const _FileName: string);
var
  Lines: TStringList;
  i: Integer;
  HitCount: Integer;
  FirstHitLine: Integer;
  OutLines: TStringList;
begin
  Inc(TotalFiles);
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(_FileName);
    HitCount := 0;
    FirstHitLine := -1;

    if DoFix then begin
      OutLines := TStringList.Create;
      try
        for i := 0 to Lines.Count - 1 do begin
          if IsOffendingLine(Lines[i]) then begin
            Inc(HitCount);
            if FirstHitLine < 0 then
              FirstHitLine := i + 1;
          end else begin
            OutLines.Add(Lines[i]);
          end;
        end;
        if HitCount > 0 then begin
          Inc(FilesWithHits);
          Inc(TotalHits, HitCount);
          WriteLn(_FileName, '(', FirstHitLine, '): removed ', HitCount,
            ' propert', Copy('yies', 1 + Ord(HitCount > 1), 1 + 2 * Ord(HitCount > 1)));
          OutLines.SaveToFile(_FileName);
        end;
      finally
        OutLines.Free;
      end;
    end else begin
      for i := 0 to Lines.Count - 1 do begin
        if IsOffendingLine(Lines[i]) then begin
          Inc(HitCount);
          if FirstHitLine < 0 then
            FirstHitLine := i + 1;
          if not Quiet then
            WriteLn(_FileName, '(', i + 1, '): ', OffendingProperty(Lines[i]));
        end;
      end;
      if HitCount > 0 then begin
        Inc(FilesWithHits);
        Inc(TotalHits, HitCount);
        if Quiet then
          WriteLn(_FileName, ': ', HitCount, ' hit(s)');
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure FindDfmFiles(const _Dir: string);
var
  SR: TSearchRec;
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(_Dir);
  if FindFirst(Path + '*', faAnyFile, SR) = 0 then begin
    try
      repeat
        if (SR.Name = '.') or (SR.Name = '..') then
          Continue;
        if (SR.Attr and faDirectory) <> 0 then
          FindDfmFiles(Path + SR.Name)
        else if SameText(ExtractFileExt(SR.Name), '.dfm') then
          ProcessFile(Path + SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end;
end;

procedure ProcessPath(const _Path: string);
begin
  if DirectoryExists(_Path) then
    FindDfmFiles(_Path)
  else if FileExists(_Path) then
    ProcessFile(_Path)
  else
    WriteLn('Warning: path not found: ', _Path);
end;

procedure ShowHelp;
begin
  WriteLn('DfmFilter - Detect/remove Delphi 2006+ properties from DFM files');
  WriteLn;
  WriteLn('Usage: DfmFilter [options] <path> [<path> ...]');
  WriteLn('  <path>           DFM file or directory (directories searched recursively)');
  WriteLn('  --fix            Remove offending properties in-place');
  WriteLn('  --quiet          Only show files with problems (no per-line detail)');
  WriteLn('  --help           Show this help');
  WriteLn;
  WriteLn('Properties detected:');
  WriteLn('  Margins.Left, Margins.Top, Margins.Right, Margins.Bottom, AlignWithMargins');
  WriteLn;
  WriteLn('Exit code: 0 = no problems found (or all fixed), 1 = problems found');
end;

procedure ParseArgs;
var
  i: Integer;
  Arg: string;
begin
  Paths := TStringList.Create;
  for i := 1 to ParamCount do begin
    Arg := ParamStr(i);
    if SameText(Arg, '--fix') then
      DoFix := True
    else if SameText(Arg, '--quiet') then
      Quiet := True
    else if SameText(Arg, '--help') or SameText(Arg, '-h') or SameText(Arg, '/?') then begin
      ShowHelp;
      Halt(0);
    end else
      Paths.Add(Arg);
  end;
end;

var
  i: Integer;
begin
  ParseArgs;
  try
    if Paths.Count = 0 then begin
      ShowHelp;
      Halt(0);
    end;

    for i := 0 to Paths.Count - 1 do
      ProcessPath(Paths[i]);

    if not Quiet then begin
      WriteLn;
      if DoFix then
        WriteLn('Fixed: ', TotalHits, ' properties in ', FilesWithHits, ' of ', TotalFiles, ' files')
      else
        WriteLn('Found: ', TotalHits, ' properties in ', FilesWithHits, ' of ', TotalFiles, ' files');
    end;

    if (TotalHits > 0) and not DoFix then
      Halt(1)
    else
      Halt(0);
  finally
    Paths.Free;
  end;
end.
