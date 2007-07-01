// the code formatter expert as an editor expert
// (Used if you define GX_FORMATTER_IS_EDITOR_EXPERT.)
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_eCodeFormatter;

{$I GX_CondDefine.inc}

interface

implementation

{$IFDEF GX_FORMATTER_IS_EDITOR_EXPERT}

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  SysUtils,
  Classes,
  Menus,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_CodeFormatterExpert;

type
  TeCodeFormatterExpert = class(TEditorExpert)
  private
    FExpert: TCodeFormatterExpert;
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function GetBitmapFileName: string; override;
    procedure GetHelpString(List: TStrings); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
  end;

{ TeCodeFormatterExpert }

procedure TeCodeFormatterExpert.Configure;
begin
  FExpert.Configure;
end;

constructor TeCodeFormatterExpert.Create;
begin
  inherited Create;

  FExpert := TCodeFormatterExpert.Create;

  ShortCut := Menus.ShortCut(Word('F'), [ssCtrl, ssAlt]);
end;

destructor TeCodeFormatterExpert.Destroy;
begin
  FreeAndNil(FExpert);
  inherited;
end;

procedure TeCodeFormatterExpert.Execute(Sender: TObject);
begin
  FExpert.Execute;
end;

function TeCodeFormatterExpert.GetBitmapFileName: string;
begin
  Result := '';
end;

function TeCodeFormatterExpert.GetDisplayName: string;
begin
  Result := 'Code Formatter';
end;

procedure TeCodeFormatterExpert.GetHelpString(List: TStrings);
resourcestring
  SFormatterHelp =
    '  This expert is the source code formatter formerly known as' +
    ' DelForEx. It can handle different configuration sets between which' +
    ' you can switch using the tools button.' + sLineBreak +
    '  To force a configuration set for a particular unit, add' + sLineBreak + sLineBreak +
    ' {GXFormatter.config=<name>}' + sLineBreak + sLineBreak +
    ' as the first line to the unit.';
begin
  List.Text := SFormatterHelp;
end;

class function TeCodeFormatterExpert.GetName: string;
begin
  Result := 'CodeFormatter';
end;

function TeCodeFormatterExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

procedure TeCodeFormatterExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FExpert.InternalLoadSettings(ConfigurationKey, Settings);
end;

procedure TeCodeFormatterExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited;
  FExpert.InternalSaveSettings(ConfigurationKey, Settings);
end;

initialization
  RegisterEditorExpert(TeCodeFormatterExpert);
{$ENDIF GX_FORMATTER_IS_EDITOR_EXPERT}

end.

