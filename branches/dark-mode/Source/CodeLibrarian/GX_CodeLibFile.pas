unit GX_CodeLibFile;

interface

uses
  SysUtils,
  Classes,
  GpStructuredStorage,
  GX_StringList;

//TODO 3 -cIssue -oAnyone: Handle invalid filenames with \/, or use Topic attribute as the display text?
//                         (See AssertValidFilename method calls.)

const
  CodeLibPathSep = '\';

type
  TGXStorageFile = class(TObject)
  private
    FStorage: IGpStructuredStorage;
    FFile: TStream;
    FFileName: TGXUnicodeString;
    function GetFileText: TGXUnicodeString;
    procedure SetFileText(const Value: TGXUnicodeString);
    procedure AssertFileIsOpen;
    procedure AssertValidFileName(FileName: TGXUnicodeString);
    function GetFileInfo: IGpStructuredFileInfo;
  public
    constructor Create(const FileName: TGXUnicodeString);

    procedure AssertExistingObjectName(const ObjectName: TGXUnicodeString);
    procedure AssertValidFolderName(FolderName: TGXUnicodeString);
    procedure CompactStorage;
    procedure CloseStorage;
    procedure OpenStorage(const FileName: WideString);
    procedure SaveStorage;

    procedure OpenFile(const FullPath: TGXUnicodeString); overload;
    procedure OpenFile(const FolderName, FileName: TGXUnicodeString); overload;
    procedure Delete(const ObjectName: TGXUnicodeString);
    procedure Move(const OldName, NewName: TGXUnicodeString);
    procedure CloseFile;
    procedure CreateFolder(const FolderName: TGXUnicodeString);
    function FolderExists(const FolderName: TGXUnicodeString): Boolean;
    function FileExists(const FileName: TGXUnicodeString): Boolean;
    function ObjectExists(const ObjectName: TGXUnicodeString): Boolean;

    procedure ListFiles(const FolderName: TGXUnicodeString; Files: TStrings);
    procedure ListFolders(const FolderName: TGXUnicodeString; Files: TStrings);

    property FileText: TGXUnicodeString read GetFileText write SetFileText;
    function AttributeAsString(const AttrName: TGXUnicodeString; const Default: TGXUnicodeString = ''): TGXUnicodeString;
    procedure SetAttribute(const AttrName: TGXUnicodeString; const Value: TGXUnicodeString); overload;
    procedure SetAttribute(const AttrName: TGXUnicodeString; const Value: Integer); overload;
    procedure SetObjectAttribute(const ObjectName, AttrName, AttrValue: TGXUnicodeString);
    function GetObjectAttribute(const ObjectName, AttrName: TGXUnicodeString): TGXUnicodeString; overload;
    function GetObjectAttribute(const FolderName, FileName, AttrName: TGXUnicodeString): TGXUnicodeString; overload;
    function MakeFileName(const FolderName, FileName: TGXUnicodeString): TGXUnicodeString;
    function AddSlash(const Foldername: TGXUnicodeString): TGXUnicodeString;
  end;

implementation

uses
  u_dzStringUtils;

{ TGXStorageFile }

function TGXStorageFile.AddSlash(const FolderName: TGXUnicodeString): TGXUnicodeString;

  function IsPathSep(const s: TGXUnicodeString; Index: Integer): Boolean;
  begin
    Result := (Index > 0) and (Index <= Length(s)) and (s[Index] = CodeLibPathSep)
      and (ByteType(s, Index) = mbSingleByte);
  end;

begin
  Result := FolderName;
  if not IsPathSep(Result, Length(Result)) then
    Result := Result + CodeLibPathSep;
end;

function TGXStorageFile.MakeFileName(const FolderName, FileName: TGXUnicodeString): TGXUnicodeString;
begin
  Result := FolderName;
  if Result = '' then
    Result := CodeLibPathSep
  else
    Result := AddSlash(Result);
  if FileName <> '' then
  begin
    if FileName[1] = CodeLibPathSep then
      Result := Result + Copy(FileName, 2)
    else
      Result := Result + FileName;
  end;
end;

procedure TGXStorageFile.AssertFileIsOpen;
begin
  if not Assigned(FFile) then
    raise Exception.Create('No file is currently open');
  if FFileName = '' then
    raise Exception.Create('The file name is blank');
end;

procedure TGXStorageFile.AssertValidFileName(FileName: TGXUnicodeString);
begin
  if FileName = '' then
    raise Exception.Create('The file name can not be blank');
  if FileName[1] = CodeLibPathSep then
    raise Exception.Create('The file name can not start with a slash: ' + FileName);
end;

procedure TGXStorageFile.AssertValidFolderName(FolderName: TGXUnicodeString);
begin
  if FolderName = '' then
    raise Exception.Create('FolderName can not be blank');
  if FolderName[1] <> CodeLibPathSep then
    raise Exception.Create('FolderName must start with a slash');
end;

function TGXStorageFile.AttributeAsString(const AttrName, Default: TGXUnicodeString): TGXUnicodeString;
var
  FileInfo: IGpStructuredFileInfo;
begin
  Result := Default;
  FileInfo := GetFileInfo;
  Result := FileInfo.GetAttribute(AttrName);
end;

procedure TGXStorageFile.CloseFile;
begin
  FreeAndNil(FFile);
end;

procedure TGXStorageFile.CloseStorage;
begin
  CloseFile;
  FStorage := nil;
end;

constructor TGXStorageFile.Create(const FileName: TGXUnicodeString);
begin
  inherited Create;
  OpenStorage(FileName);
end;

procedure TGXStorageFile.CreateFolder(const FolderName: TGXUnicodeString);
begin
  FStorage.CreateFolder(FolderName);
end;

procedure TGXStorageFile.Delete(const ObjectName: TGXUnicodeString);
begin
  Assert(Length(ObjectName) > 0);
  CloseFile;
  FStorage.Delete(ObjectName);
end;

function TGXStorageFile.FileExists(const FileName: TGXUnicodeString): Boolean;
begin
  Result := FStorage.FileExists(FileName);
end;

function TGXStorageFile.FolderExists(const FolderName: TGXUnicodeString): Boolean;
begin
  Result := FStorage.FolderExists(FolderName);
end;

function TGXStorageFile.GetFileInfo: IGpStructuredFileInfo;
begin
  AssertFileIsOpen;
  Result := FStorage.GetFileInfo(FFileName);
  if not Assigned(Result) then
    raise Exception.CreateFmt('The file %s does not exist to get the file info', [FFileName]);
end;

function TGXStorageFile.GetFileText: TGXUnicodeString;
var
  StringStream: TStringStream;
begin
  AssertFileIsOpen;
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(FFile, 0);
    Result := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TGXStorageFile.ListFiles(const FolderName: TGXUnicodeString; Files: TStrings);
begin
  AssertValidFolderName(FolderName);
  FStorage.FileNames(FolderName, Files);
end;

procedure TGXStorageFile.ListFolders(const FolderName: TGXUnicodeString; Files: TStrings);
begin
  AssertValidFolderName(FolderName);
  FStorage.FolderNames(FolderName, Files);
end;

procedure TGXStorageFile.Move(const OldName, NewName: TGXUnicodeString);
begin
  if OldName = NewName then
    Exit;
  FStorage.Move(OldName, NewName);
  if SameFileName(FFileName, OldName) then
    OpenFile(NewName);
end;

procedure TGXStorageFile.OpenFile(const FolderName, FileName: TGXUnicodeString);
begin
  AssertValidFolderName(FolderName);
  AssertValidFileName(FileName);
  OpenFile(MakeFileName(FolderName, FileName));
end;

procedure TGXStorageFile.OpenStorage(const FileName: WideString);
begin
  if not Assigned(FStorage) then begin
    FStorage := CreateStructuredStorage;
    if SysUtils.FileExists(FileName) then
      FStorage.Initialize(FileName, fmOpenReadWrite)
    else
      FStorage.Initialize(FileName, fmCreate or fmOpenReadWrite);
  end;
end;

procedure TGXStorageFile.OpenFile(const FullPath: TGXUnicodeString);
begin
  FreeAndNil(FFile);
  Assert(Assigned(FStorage));
  AssertValidFolderName(FullPath);
  Assert(Length(FullPath) > 0);
  FFile := FStorage.OpenFile(FullPath, fmCreate or fmOpenReadWrite);
  FFileName := FullPath;
end;

procedure TGXStorageFile.CompactStorage;
begin
  FreeAndNil(FFile);
  FStorage.Compact;
end;

procedure TGXStorageFile.SaveStorage;
begin
  //FStorage.Save;
  { TODO -oAnyone -ccheck : Is this commented out on purpose? Or did somebody forget to reenable this code? }
end;

procedure TGXStorageFile.SetAttribute(const AttrName: TGXUnicodeString; const Value: Integer);
begin
  SetAttribute(AttrName, IntToStr(Value));
end;

procedure TGXStorageFile.SetAttribute(const AttrName: TGXUnicodeString; const Value: TGXUnicodeString);
var
  FileInfo: IGpStructuredFileInfo;
begin
  AssertFileIsOpen;
  FileInfo := GetFileInfo;
  FileInfo.Attribute[AttrName] := Value;
end;

procedure TGXStorageFile.SetFileText(const Value: TGXUnicodeString);
var
  StringStream: TStringStream;
begin
  AssertFileIsOpen;
  StringStream := TStringStream.Create(Value);
  try
    FFile.Size := StringStream.Size;
    FFile.Position := 0;
    FFile.CopyFrom(StringStream, 0);
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TGXStorageFile.SetObjectAttribute(const ObjectName, AttrName, AttrValue: TGXUnicodeString);
var
  FileInfo: IGpStructuredFileInfo;
begin
  FileInfo := FStorage.FileInfo[ObjectName];
  Assert(Assigned(FileInfo));
  FileInfo.Attribute[AttrName] := AttrValue;
end;

function TGXStorageFile.GetObjectAttribute(const ObjectName, AttrName: TGXUnicodeString): TGXUnicodeString;
var
  FileInfo: IGpStructuredFileInfo;
begin
  AssertExistingObjectName(ObjectName);
  FileInfo := FStorage.FileInfo[ObjectName];
  Assert(Assigned(FileInfo));
  Result := FileInfo.Attribute[AttrName];
end;

function TGXStorageFile.GetObjectAttribute(const FolderName, FileName,
  AttrName: TGXUnicodeString): TGXUnicodeString;
begin
  Result := GetObjectAttribute(MakeFileName(FolderName, FileName), AttrName);
end;

procedure TGXStorageFile.AssertExistingObjectName(const ObjectName: TGXUnicodeString);
begin
  Assert(ObjectExists(ObjectName), ObjectName + ' does not exist');
end;

function TGXStorageFile.ObjectExists(const ObjectName: TGXUnicodeString): Boolean;
begin
  Result := FileExists(ObjectName) or FolderExists(ObjectName);
end;

end.

