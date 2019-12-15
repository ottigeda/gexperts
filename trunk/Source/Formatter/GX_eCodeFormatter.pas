// the code formatter expert as an editor expert
// Original Author:     Thomas Mueller (http://blog.dummzeuch.de)
unit GX_eCodeFormatter;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_EditorExpert,
  GX_CodeFormatterExpert,
  GX_ConfigurationInfo,
  GX_KbdShortCutBroker,
  GX_GenericUtils;

type
  TeCodeFormatterExpert = class(TEditorExpert)
  private
    FExpert: TCodeFormatterExpert;
  protected
    function GetBitmapFileName: string; override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
    function IsDefaultActive: Boolean; override;
    procedure AddToCapitalization(const _Identifier: TGXUnicodeString);
  end;

var
  gblCodeFormatter: TeCodeFormatterExpert;

implementation

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Menus,
  GX_IdeUtils,
  GX_GExperts,
  GX_About,
  GX_CodeFormatterConfigHandler,
  GX_CodeFormatterSettings;

{ TeCodeFormatterExpert }

procedure TeCodeFormatterExpert.AddToCapitalization(const _Identifier: TGXUnicodeString);
var
  Settings: TCodeFormatterSettings;
  Timestamp: TDateTime;
begin
  if FExpert.AddToCapitalization(_Identifier) then begin
    Settings := FExpert.Engine.Settings;

    Timestamp := Settings.CapFileTimestamp;
    if TCodeFormatterConfigHandler.WriteCaptialization(Settings.CapitalizationFile, Settings.CapNames,
      Timestamp) then begin
      Settings.CapFileTimestamp := Timestamp;
    end else begin
      // The file has been changed since it was last read
      // todo: Do something intelligent here
    end;
  end;
end;

procedure TeCodeFormatterExpert.Configure;
begin
  FExpert.Configure;
end;

constructor TeCodeFormatterExpert.Create;
begin
  inherited Create;

  FExpert := TCodeFormatterExpert.Create;
  gblCodeFormatter := Self;
end;

destructor TeCodeFormatterExpert.Destroy;
begin
  gblCodeFormatter := nil;
  FreeAndNil(FExpert);
  inherited;
end;

function TeCodeFormatterExpert.IsDefaultActive: Boolean;
begin
  Result := not RunningRSXEOrGreater and not RunningCPPBuilder;
end;

procedure TeCodeFormatterExpert.Execute(Sender: TObject);
begin
  if FExpert.Execute then
    IncCallCount;
end;

function TeCodeFormatterExpert.GetBitmapFileName: string;
begin
  Result := 'TCodeFormatterExpert';
end;

function TeCodeFormatterExpert.GetDefaultShortCut: TShortCut;
begin
  if RunningRS2010OrGreater then begin
    // Starting with Delphi 2010 it has a built in code formatter
    // it doesn't work very well in the early incarnations but
    // if the user wants to replace it, he should configure it himself.
    Result := Menus.ShortCut(Word('F'), [ssCtrl, ssAlt])
  end else begin
    // for older versions, hijack the Ctrl+D shortcut that in later versions is
    // used by the built in code formatter
    Result := Menus.ShortCut(Word('D'), [ssCtrl]);
  end;
end;

function TeCodeFormatterExpert.GetDisplayName: string;
begin
  Result := 'Code Formatter';
end;

function TeCodeFormatterExpert.GetHelpString: string;
resourcestring
  SFormatterHelp =
    'This expert is the source code formatter formerly known as ' +
    'DelForEx. To switch between different configuration sets, ' +
    'use the tools button.' + sLineBreak +
    'To force a configuration set for a particular unit, add' + sLineBreak +
    '    {GXFormatter.config=<configname>}' + sLineBreak +
    'as the first line to the unit.' + sLineBreak +
    'You can also use a GXFormatter.ini per directory with content ' +
    'like:' + sLineBreak +
    '    [FileSettings]' + sLineBreak +
    '    <mask>=<configname>' + sLineBreak +
    'The formatter will use the first match for the file name. ' +
    'An empty <configname> means that it should work ' +
    'as configured in this dialog.' + sLineBreak +
    'You can also export your own configuration as' + sLineBreak +
    '    FormatterSettings-<YourName>.ini' + sLineBreak +
    'and put it into the GExperts installation directory.' + sLineBreak +
    'After doing this <YourName> can be used as <configname> as ' +
    'described above.';
begin
  Result := SFormatterHelp;
end;

class function TeCodeFormatterExpert.GetName: string;
begin
  Result := 'CodeFormatter';
end;

function TeCodeFormatterExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TeCodeFormatterExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited;
  FExpert.InternalLoadSettings(_Settings);
end;

procedure TeCodeFormatterExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited;
  FExpert.InternalSaveSettings(_Settings);
end;

function FormatFile(_FileName: PWideChar): Boolean;
{$IFNDEF GX_BCB} export;
{$ENDIF GX_BCB}
var
  FormatterStandAlone: TeCodeFormatterExpert;
begin
  Result := False;
  InitSharedResources;
  try
    try
      FormatterStandAlone := TeCodeFormatterExpert.Create;
      try
        FormatterStandAlone.LoadSettings;
        Result := FormatterStandAlone.FExpert.FormatFile(_FileName);
        FormatterStandAlone.SaveSettings;
      finally
        FormatterStandAlone.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, E.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

{ This is a simple implementation. It originally TStrings.StrictDelimiter
  was used but that only exists in Delphi 2006 and up. }

procedure SplitStr(_InStr: string; _Delimiter: Char; _sl: TStrings);
var
  Start: Integer;
  i: Integer;
begin
  Assert(Assigned(_sl));

  _InStr := _InStr + ';';
  _sl.Clear;
  Start := 1;
  for i := 1 to Length(_InStr) do begin
    if _InStr[i] = _Delimiter then begin
      _sl.Add(Copy(_InStr, Start, i - Start));
      Start := i + 1;
    end;
  end;
end;

procedure FormatFiles(AFileNames: PWideChar);
{$IFNDEF GX_BCB} export;
{$ENDIF GX_BCB}
var
  FormatterStandAlone: TeCodeFormatterExpert;
  FileList: TStringList;
  i: Integer;
begin
  InitSharedResources;
  try
    try
      FormatterStandAlone := TeCodeFormatterExpert.Create;
      try
        FormatterStandAlone.LoadSettings;
        FileList := TStringList.Create;
        try
          SplitStr(AFileNames, ';', FileList);
          for i := 0 to Pred(FileList.Count) do
            FormatterStandAlone.FExpert.FormatFile(FileList[i]);
        finally
          FileList.Free;
        end;
        FormatterStandAlone.SaveSettings;
      finally
        FormatterStandAlone.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, E.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

procedure ConfigureFormatter();
{$IFNDEF GX_BCB} export;
{$ENDIF GX_BCB}
var
  FormatterStandAlone: TeCodeFormatterExpert;
begin
  InitSharedResources;
  try
    try
      FormatterStandAlone := TeCodeFormatterExpert.Create;
      try
        FormatterStandAlone.LoadSettings;
        FormatterStandAlone.Configure;
        FormatterStandAlone.SaveSettings;
      finally
        FormatterStandAlone.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, E.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

procedure AboutFormatter();
{$IFNDEF GX_BCB} export;
{$ENDIF GX_BCB}
var
  frm: TfmAbout;
begin
  InitSharedResources;
  try
    try
      frm := gblAboutFormClass.Create(nil);
      try
        frm.ShowModal;
      finally
        frm.Free;
      end;
    except
      on E: Exception do begin
        GxLogAndShowException(E, E.Message);
      end;
    end;
  finally
    FreeSharedResources;
  end;
end;

exports
  FormatFile,
  FormatFiles,
  ConfigureFormatter,
  AboutFormatter;

initialization
  RegisterEditorExpert(TeCodeFormatterExpert);
end.
