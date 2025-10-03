///<summary>
/// DUnit Folder Iterator Extension by Uwe Raabe
/// https://www.uweraabe.de/Blog/2012/03/17/a-dunit-folder-iterator-extension/
/// Adapted to Delphi 2007 by twm
unit TestFrameworkExt;

interface

uses
  SysUtils,
  Classes,
  TestFramework;

type
  TFileTestCaseClass = class of TFileTestCase;
  TFileTestCase = class(TTestCase)
  private
    FTestFileName: string;
  public
    constructor Create(const AMethodName, ATestFileName: string); reintroduce; overload; virtual;
    function GetName: string; override;
    property TestFileName: string read FTestFileName;
  end;

  TFolderTestSuite = class(TTestSuite)
  private
    FBaseFolder: string;
    FFileMask: string;
    FRecursive: Boolean;
  protected
    procedure AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string); overload; virtual;
    procedure ProcessFile(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, FileName: string); virtual;
    procedure ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, Path, FileMask: string;
      Recursive: Boolean); overload; virtual;
    procedure ProcessBaseFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod: string); virtual;
  public
    constructor Create(TestClass: TFileTestCaseClass; const ABaseFolder, AFileMask: string; ARecursive: Boolean); overload;
    procedure AddTests(TestClass: TTestCaseClass); override;
    property BaseFolder: string read FBaseFolder;
    property FileMask: string read FFileMask;
    property Recursive: Boolean read FRecursive;
  end;

implementation

uses
  Types,
  u_dzFileUtils;

constructor TFolderTestSuite.Create(TestClass: TFileTestCaseClass; const ABaseFolder, AFileMask: string; ARecursive:
  Boolean);
begin
  FBaseFolder := ABaseFolder;
  FFileMask := AFileMask;
  FRecursive := ARecursive;
  inherited Create(TestClass);
end;

procedure TFolderTestSuite.AddMethodTests(TestClass: TTestCaseClass; const NameOfMethod: string);
var
  Suite: ITestSuite;
begin
  if TestClass.InheritsFrom(TFileTestCase) then begin
    Suite := TTestSuite.Create(NameOfMethod);
    AddSuite(Suite);
    ProcessBaseFolder(Suite, TFileTestCaseClass(TestClass), NameOfMethod);
  end else begin
    AddTest(TestClass.Create(NameOfMethod));
  end;
end;

procedure TFolderTestSuite.AddTests(TestClass: TTestCaseClass);
var
  MethodIter: Integer;
  NameOfMethod: string;
  MethodEnumerator: TMethodEnumerator;
begin
  { call on the method enumerator to get the names of the test
    cases in the testClass }
  MethodEnumerator := nil;
  try
    MethodEnumerator := TMethodEnumerator.Create(TestClass);
    { make sure we add each test case  to the list of tests }
    for MethodIter := 0 to MethodEnumerator.Methodcount - 1 do begin
      NameOfMethod := MethodEnumerator.NameOfMethod[MethodIter];
      AddMethodTests(TestClass, NameOfMethod);
    end;
  finally
    MethodEnumerator.free;
  end;
end;

procedure TFolderTestSuite.ProcessBaseFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod:
  string);
begin
  ProcessFolder(Suite, TestClass, NameOfMethod, BaseFolder, FileMask, Recursive);
end;

procedure TFolderTestSuite.ProcessFile(Suite: ITestSuite; TestClass: TFileTestCaseClass; const NameOfMethod, FileName:
  string);
begin
  Suite.AddTest(TestClass.Create(NameOfMethod, FileName));
end;

procedure TFolderTestSuite.ProcessFolder(Suite: ITestSuite; TestClass: TFileTestCaseClass;
  const NameOfMethod, Path, FileMask: string; Recursive: Boolean);
var
  list: TStringList;
  S: string;
  tmp: ITestSuite;
begin
  list := TStringList.Create;
  try
    TSimpleDirEnumerator.EnumFilesOnly(IncludeTrailingPathDelimiter(Path) + FileMask, list, True);
    for S in list do
      ProcessFile(Suite, TestClass, NameOfMethod, S);
    if Recursive then begin
      list.Clear;
      TSimpleDirEnumerator.EnumDirsOnly(IncludeTrailingPathDelimiter(Path) + '*', list, True);
      for S in list do begin
        tmp := TTestSuite.Create(ExtractFileName(S));
        Suite.AddSuite(tmp);
        ProcessFolder(tmp, TestClass, NameOfMethod, S, FileMask, True);
      end;
    end;
  finally
    FreeAndNil(list);
  end;
end;

constructor TFileTestCase.Create(const AMethodName, ATestFileName: string);
begin
  FTestFileName := ATestFileName;
  inherited Create(AMethodName);
end;

function TFileTestCase.GetName: string;
begin
  result := ChangeFileExt(ExtractFileName(TestFileName), '');
end;

end.
