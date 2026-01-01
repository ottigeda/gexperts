unit u_dzDialogUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  Classes,
  Types,
  Controls,
  Contnrs,
  u_dzTranslator,
  u_dzTypes;

type
  IFileFilterBuilder = interface
    ///<summary>
    /// Adds a new filter, the first one added is the default.
    /// @param Description is the file type description e.g. 'Text file'. This descrition
    ///                    should be localized.
    /// @param Mask is the file mask, e.g. '*.txt', it should not be localized.
    /// @param AddMaskToDesc determines whether the mask is appended to the descripition as
    ///                      ' (*.bla)', defaults to true, e.g. 'Text file (*.txt)
    /// @returns the interface itself, so chaining is possible like this:
    ///          od.Filter := FileFilterHelper.AddFilter('bla', '*.bla').AddFilter('blub', '*.blu').Value;
    function Add(const _Description: string; const _Mask: string;
      _AddMaskToDesc: Boolean = True): IFileFilterBuilder;
    ///<summary>
    /// Adds an entry <descriptionfmt> (<maskfmt>)|<maskfmt>
    /// where both *fmts are passed through Format(*fmt, Values) </summary>
    function AddFmt(const _DescriptionFmt: string; const _MaskFmt: string;
      const _Values: array of const): IFileFilterBuilder;
    function AddAvi: IFileFilterBuilder;
    function AddBmp: IFileFilterBuilder;
    function AddCsv: IFileFilterBuilder;
    function AddDbf: IFileFilterBuilder;
    function AddEmf: IFileFilterBuilder;
    function AddExe: IFileFilterBuilder;
    function AddGif: IFileFilterBuilder;
    function AddHtml: IFileFilterBuilder;
    function AddIni: IFileFilterBuilder;
    function AddJpg: IFileFilterBuilder;
    function AddLog: IFileFilterBuilder;
    function AddMdb: IFileFilterBuilder;
    function AddOdt: IFileFilterBuilder;
    function AddOds: IFileFilterBuilder;
    function AddPdf: IFileFilterBuilder;
    function AddPicture: IFileFilterBuilder;
    function AddRtf: IFileFilterBuilder;
    function AddTiff: IFileFilterBuilder;
    function AddTxt: IFileFilterBuilder;
    function AddXml: IFileFilterBuilder;
    function AddXls: IFileFilterBuilder;
    function Value: string;
  end;

function FileFilterBuilder(_IncludeAllFiles: Boolean = True; const _AllSupported: string = ''): IFileFilterBuilder;

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean = True): Boolean; overload;

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean = True): Boolean; overload;

function TSaveDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;

implementation

uses
  Dialogs,
  SysUtils,
  u_dzVclUtils,
  u_dzStringUtils,
  u_dzStringArrayUtils;

function _(const _s: string): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := dzlibGetText(_s);
end;

function TOpenDialogXp_Init(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; _FileMustExist: Boolean): TOpenDialog;
begin
  Result := TOpenDialog.Create(_Owner);
  try
    Result.Name := '';
    Result.Title := _Title;
    Result.Filter := _Filter;
    Result.Options := [ofHideReadOnly, ofPathMustExist, ofEnableSizing];
    if _FileMustExist then
      Result.Options := Result.Options + [ofFileMustExist];

    Result.FileName := ExtractFileName(_fn);
    Result.InitialDir := ExtractFileDir(_fn);
    Result.DefaultExt := '*';
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TOpenDialogXP_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean): Boolean; overload;
var
  od: TOpenDialog;
begin
  od := TOpenDialogXp_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    Result := od.Execute({$IFDEF OPENDIALOG_EXCUTE_HAS_HANDLE}_Owner.Handle{$ENDIF});
    if not Result then
      Exit;
    _fn := od.FileName;
  finally
    FreeAndNil(od);
  end;
end;

function TOpenDialogXP_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean): Boolean; overload;
var
  od: TOpenDialog;
begin
  od := TOpenDialogXp_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    od.Options := od.Options + [ofAllowMultiSelect];
    Result := od.Execute({$IFDEF OPENDIALOG_EXCUTE_HAS_HANDLE}_Owner.Handle{$ENDIF});
    if not Result then
      Exit; //==>

    _Files := TStringArray_FromStrings(od.Files);
  finally
    FreeAndNil(od);
  end;
end;

{$IF Declared(TFileOpenDialog)}
function TFileOpenDialog_Init(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; _FileMustExist: Boolean = True): TFileOpenDialog;
var
  FilterParts: TStringArray;
  i: Integer;
  FileType: TFileTypeItem;
begin
  Result := TFileOpenDialog.Create(_Owner);
  try
    Result.Name := '';
    Result.Options := [fdoPathMustExist];
    if _FileMustExist then
      Result.Options := Result.Options + [fdoFileMustExist];
    Result.Title := _Title;
    FilterParts := SplitString(_Filter, '|');
    i := 0;
    while i < Length(FilterParts) - 1 do begin
      FileType := Result.FileTypes.Add;
      FileType.DisplayName := FilterParts[i];
      FileType.FileMask := FilterParts[i + 1];
      Inc(i, 2);
    end;
    Result.FileName := ExtractFileName(_fn);
    Result.DefaultFolder := ExtractFileDir(_fn);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TFileOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean = True): Boolean; overload;
var
  od: TFileOpenDialog;
begin
  od := TFileOpenDialog_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    Result := od.Execute;
    if not Result then
      Exit; //==>

    _fn := od.FileName;
  finally
    FreeAndNil(od);
  end;
end;

function TFileOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean = True): Boolean; overload;
var
  od: TFileOpenDialog;
begin
  od := TFileOpenDialog_Init(_Owner, _Title, _Filter, _fn, _FileMustExist);
  try
    od.Options := od.Options + [fdoAllowMultiSelect];
    Result := od.Execute;
    if not Result then
      Exit; //==>

    _Files := TStringArray_FromStrings(od.Files);
  finally
    FreeAndNil(od);
  end;
end;
{$IFEND}

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; _FileMustExist: Boolean): Boolean;
begin
  TCommonDialog_CenterWithBackgroundThread;
{$IF Declared(TFileOpenDialog)}
  if Win32MajorVersion >= 6 then begin
    // only available on Windows Vista and later
    Result := TFileOpenDialog_Execute(_Owner, _Title, _Filter, _fn, _FileMustExist);
    Exit; //==>
  end;
{$IFEND}
  Result := TOpenDialogXP_Execute(_Owner, _Title, _Filter, _fn, _FileMustExist);
end;

function TOpenDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  const _fn: string; out _Files: TStringArray; _FileMustExist: Boolean = True): Boolean; overload;
begin
  TCommonDialog_CenterWithBackgroundThread;
{$IF Declared(TFileOpenDialog)}
  if Win32MajorVersion >= 6 then begin
    // only available on Windows Vista and later
    Result := TFileOpenDialog_Execute(_Owner, _Title, _Filter, _fn, _Files, _FileMustExist);
    Exit; //==>
  end;
{$IFEND}
  Result := TOpenDialogXP_Execute(_Owner, _Title, _Filter, _fn, _Files, _FileMustExist);
end;

function TSaveDialogXP_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(_Owner);
  try
    sd.Name := '';
    sd.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
    sd.Title := _Title;
    sd.Filter := _Filter;
    sd.FileName := ExtractFileName(_fn);
    sd.InitialDir := ExtractFileDir(_fn);
    sd.DefaultExt := _DefaultExt;
    Result := sd.Execute({$IFDEF OPENDIALOG_EXCUTE_HAS_HANDLE}_Owner.Handle{$ENDIF});
    if not Result then
      Exit; //==>
    _fn := sd.FileName;
  finally
    FreeAndNil(sd);
  end;
end;

{$IF Declared(TFileSaveDialog)}
function TFileSaveDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;
var
  sd: TFileSaveDialog;
  FilterParts: TStringArray;
  i: Integer;
  FileType: TFileTypeItem;
begin
  sd := TFileSaveDialog.Create(_Owner);
  try
    sd.Name := '';
    sd.Options := [fdoOverWritePrompt, fdoNoReadOnlyReturn, fdoPathMustExist];
    sd.Title := _Title;
    sd.DefaultExtension := _DefaultExt;
    FilterParts := SplitString(_Filter, '|');
    i := 0;
    while i < Length(FilterParts) - 1 do begin
      FileType := sd.FileTypes.Add;
      FileType.DisplayName := FilterParts[i];
      FileType.FileMask := FilterParts[i + 1];
      Inc(i, 2);
    end;
    sd.FileName := ExtractFileName(_fn);
    sd.DefaultFolder := ExtractFileDir(_fn);
    Result := sd.Execute;
    if not Result then
      Exit; //==>
    _fn := sd.FileName;
  finally
    FreeAndNil(sd);
  end;
end;
{$IFEND}

function TSaveDialog_Execute(_Owner: TWinControl; const _Title: string; const _Filter: string;
  var _fn: string; const _DefaultExt: string = ''): Boolean;
begin
  TCommonDialog_CenterWithBackgroundThread;
{$IF Declared(TFileSaveDialog)}
  if Win32MajorVersion >= 6 then begin
    // only available on Windows Vista and later
    Result := TFileSaveDialog_Execute(_Owner, _Title, _Filter, _fn, _DefaultExt);
    Exit; //==>
  end;
{$IFEND}
  // fallback for older Delphi versions and Windows before Vista
  Result := TSaveDialogXP_Execute(_Owner, _Title, _Filter, _fn, _DefaultExt);
end;

type
  TFileFilter = class
  private
    FDescription: string;
    FMask: string;
  public
    constructor Create(const _Description, _Mask: string; _AddMaskToDesc: Boolean);
    property Description: string read FDescription;
    property Mask: string read FMask;
  end;

  TFileFilterBuilder = class(TInterfacedObject, IFileFilterBuilder)
  private
    FFilters: TObjectList;
    FIncludeAllFiles: Boolean;
    FAllSupported: string;
  private
    ///<summary> Adds a new filter, the first one added is the default.
    ///          @param Description is the file type description e.g. 'Text file'. This descrition
    ///                             should be localized.
    ///          @param Mask is the file mask, e.g. '*.txt', it should not be localized.
    ///          @param AddMaskToDesc determines whether the mask is appended to the descripition as
    ///                               ' (*.bla)', defaults to true, e.g. 'Text file (*.txt)
    ///          @returns the interface itself, so chaining is possible like this:
    ///                   od.Filter := FileFilterHelper.AddFilter('bla', '*.bla').AddFilter('blub', '*.blu').Value;
    function Add(const _Description: string; const _Mask: string;
      _AddMaskToDesc: Boolean = True): IFileFilterBuilder;
    ///<summary>
    /// Adds an entry <descriptionfmt> (<maskfmt>)|<maskfmt>
    /// where both *fmts are passed through Format(*fmt, Values) </summary>
    function AddFmt(const _DescriptionFmt: string; const _MaskFmt: string;
      const _Values: array of const): IFileFilterBuilder;
    function AddAvi: IFileFilterBuilder;
    function AddBmp: IFileFilterBuilder;
    function AddCsv: IFileFilterBuilder;
    function AddDbf: IFileFilterBuilder;
    function AddEmf: IFileFilterBuilder;
    function AddExe: IFileFilterBuilder;
    function AddGif: IFileFilterBuilder;
    function AddHtml: IFileFilterBuilder;
    function AddIni: IFileFilterBuilder;
    function AddJpg: IFileFilterBuilder;
    function AddLog: IFileFilterBuilder;
    function AddMdb: IFileFilterBuilder;
    function AddOdt: IFileFilterBuilder;
    function AddOds: IFileFilterBuilder;
    function AddPdf: IFileFilterBuilder;
    function AddPicture: IFileFilterBuilder;
    function AddRtf: IFileFilterBuilder;
    function AddTiff: IFileFilterBuilder;
    function AddTxt: IFileFilterBuilder;
    function AddXml: IFileFilterBuilder;
    function AddXls: IFileFilterBuilder;
    function Value: string;
  public
    constructor Create(_IncludeAllFiles: Boolean = True; const _AllSupported: string = '');
    destructor Destroy; override;
  end;

function FileFilterBuilder(_IncludeAllFiles: Boolean = True; const _AllSupported: string = ''): IFileFilterBuilder;
begin
  Result := TFileFilterBuilder.Create(_IncludeAllFiles, _AllSupported);
end;

{ TFileFilterBuilder }

constructor TFileFilterBuilder.Create(_IncludeAllFiles: Boolean; const _AllSupported: string);
begin
  inherited Create;
  FFilters := TObjectList.Create;
  FIncludeAllFiles := _IncludeAllFiles;
  FAllSupported := _AllSupported;
end;

destructor TFileFilterBuilder.Destroy;
begin
  FreeAndNil(FFilters);
  inherited;
end;

function TFileFilterBuilder.Add(const _Description, _Mask: string;
  _AddMaskToDesc: Boolean): IFileFilterBuilder;
begin
  FFilters.Add(TFileFilter.Create(_Description, _Mask, _AddMaskToDesc));
  Result := Self;
end;

function TFileFilterBuilder.AddFmt(const _DescriptionFmt, _MaskFmt: string;
  const _Values: array of const): IFileFilterBuilder;
begin
  Result := Add(Format(_DescriptionFmt, _Values), Format(_MaskFmt, _Values));
end;

function TFileFilterBuilder.AddAvi: IFileFilterBuilder;
begin
  Result := Add(_('AVI files'), '*.AVI');
end;

function TFileFilterBuilder.AddBmp: IFileFilterBuilder;
begin
  Result := Add(_('Bitmap Files'), '*.BMP');
end;

function TFileFilterBuilder.AddCsv: IFileFilterBuilder;
begin
  Result := Add(_('Comma-separated values'), '*.CSV');
end;

function TFileFilterBuilder.AddDbf: IFileFilterBuilder;
begin
  Result := Add(_('DBase tables'), '*.DBF');
end;

function TFileFilterBuilder.AddEmf: IFileFilterBuilder;
begin
  Result := Add(_('Windows Enhanced Metafile'), '*.EMF');
end;

function TFileFilterBuilder.AddExe: IFileFilterBuilder;
begin
  Result := Add(_('Executable Files'), '*.EXE');
end;

function TFileFilterBuilder.AddGif: IFileFilterBuilder;
begin
  Result := Add(_('GIF Image'), '*.GIF');
end;

function TFileFilterBuilder.AddHtml: IFileFilterBuilder;
begin
  Result := Add(_('Hypertext Markup Language'), '*.html');
end;

function TFileFilterBuilder.AddIni: IFileFilterBuilder;
begin
  Result := Add(_('INI files'), '*.INI');
end;

function TFileFilterBuilder.AddJpg: IFileFilterBuilder;
begin
  Result := Add(_('JPEG Files'), '*.jpg;*.jpeg');
end;

function TFileFilterBuilder.AddLog: IFileFilterBuilder;
begin
  Result := Add(_('Log files'), '*.LOG');
end;

function TFileFilterBuilder.AddMdb: IFileFilterBuilder;
begin
  Result := Add(_('Microsoft Access Databases'), '*.mdb');
end;

function TFileFilterBuilder.AddOds: IFileFilterBuilder;
begin
  Result := Add(_('Open Document Spreadsheet'), '*.ODS');
end;

function TFileFilterBuilder.AddOdt: IFileFilterBuilder;
begin
  Result := Add(_('Open Document Text'), '*.ODT');
end;

function TFileFilterBuilder.AddPdf: IFileFilterBuilder;
begin
  Result := Add(_('Portable Document Format'), '*.PDF');
end;

function TFileFilterBuilder.AddPicture: IFileFilterBuilder;
begin
  Result := Add(_('Picture files'), '*.bmp;*.jpg;*.jpeg').AddBmp.AddJpg;
end;

function TFileFilterBuilder.AddRtf: IFileFilterBuilder;
begin
  Result := Add(_('Rich Text Format'), '*.RTF');
end;

function TFileFilterBuilder.AddTiff: IFileFilterBuilder;
begin
  Result := Add(_('Tagged Image File Format'), '*.TIF;*.TIFF');
end;

function TFileFilterBuilder.AddTxt: IFileFilterBuilder;
begin
  Result := Add(_('Text files'), '*.TXT');
end;

function TFileFilterBuilder.AddXls: IFileFilterBuilder;
begin
  Result := Add(_('Microsoft Excel File Format'), '*.XLS');
end;

function TFileFilterBuilder.AddXml: IFileFilterBuilder;
begin
  Result := Add(_('Extensible Markup Language'), '*.XML');
end;

function TFileFilterBuilder.Value: string;

  procedure AddToResult(const _Description, _Mask: string); overload;
  begin
    if Result <> '' then
      Result := Result + '|';
    Result := Result + _Description + '|' + _Mask;
  end;

  procedure AddToResult(_Filter: TFileFilter); overload;
  begin
    AddToResult(_Filter.Description, _Filter.Mask);
  end;

var
  i: Integer;
  s: string;
begin
  Result := '';

  if FAllSupported <> '' then begin
    s := '';
    for i := 0 to FFilters.Count - 1 do begin
      if s <> '' then
        s := s + ';';
      s := s + TFileFilter(FFilters[i]).Mask;
    end;
    AddToResult(FAllSupported, s);
  end;

  for i := 0 to FFilters.Count - 1 do begin
    AddToResult(TFileFilter(FFilters[i]));
  end;

  if FIncludeAllFiles then
    AddToResult(_('all files') + ' (*.*)', '*.*');
end;

{ TFileFilter }

constructor TFileFilter.Create(const _Description, _Mask: string; _AddMaskToDesc: Boolean);
begin
  inherited Create;
  FDescription := _Description;
  FMask := _Mask;
  if _AddMaskToDesc then
    FDescription := FDescription + ' (' + _Mask + ')';
end;

// This would have been too easy: In theory setting this to true would
// show the "new" TFileOpen/SaveDialogs available in Windows Vista+ even when using TOpen/SaveDialog
// with a fallback to the old dialogs for Windows XP.
// Unfortunately this apparently only works if styling is enabled for the program.
initialization
//{$IF Declared(UseLatestCommonDialogs)}
//  UseLatestCommonDialogs := True;
//{$IFEND}
end.
