unit GX_eDate;

interface

{$I GX_CondDefine.inc}

uses
  SysUtils, Classes, Controls, Forms, StdCtrls,
  GX_EditorExpert, GX_ConfigurationInfo, GX_BaseForm;

type
  TfmDateFormat = class(TfmBaseForm)
    lblFormat: TLabel;
    cbFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
  end;

implementation

{$R *.dfm}

uses
  GX_OtaUtils, u_dzVclUtils;

type
  TDateTimeExpert = class(TEditorExpert)
  private
    FDateFormat: string;
  protected
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
    procedure InternalLoadSettings(_Settings: IExpertSettings); override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    property DateFormat: string read FDateFormat write FDateFormat;
  end;

{ TDateTimeExpert }

procedure TDateTimeExpert.Configure;
var
  frm: TfmDateFormat;
begin
  frm := TfmDateFormat.Create(nil);
  try
    frm.cbFormat.Text := DateFormat;
    if frm.ShowModal = mrOk then
      DateFormat := frm.cbFormat.Text;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TDateTimeExpert.Create;
begin
  inherited Create;
  FDateFormat := {$IFDEF GX_VER220_up}FormatSettings.{$ENDIF}ShortDateFormat;
end;

procedure TDateTimeExpert.Execute(Sender: TObject);
resourcestring
  SInvalidDateTimeFormat = 'Invalid date/time format';
var
  InsertString: string;
begin
  try
    InsertString := FormatDateTime(FDateFormat, Date + Time);
  except
    on E: EConvertError do
      InsertString := SInvalidDateTimeFormat;
  end;
  GxOtaInsertLineIntoEditor(InsertString);
  IncCallCount;
end;

function TDateTimeExpert.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scAlt + Ord('A');
end;

function TDateTimeExpert.GetDisplayName: string;
resourcestring
  SDateExpertName = 'Insert Date/Time';
begin
  Result := SDateExpertName;
end;

function TDateTimeExpert.GetHelpString: string;
resourcestring
  SDateExpertHelp =
    '  This expert inserts the current date/time at the cursor position in ' +
    'the code editor. The format of the date/time text is configurable ' +
    'using standard VCL date format specifiers.  See the FormatDateTime ' +
    'help topic in the VCL documentation for full details.';
begin
  Result := SDateExpertHelp;
end;

class function TDateTimeExpert.GetName: string;
begin
  Result := 'DateTime';
end;

procedure TDateTimeExpert.InternalLoadSettings(_Settings: IExpertSettings);
begin
  inherited InternalLoadSettings(_Settings);
  DateFormat := _Settings.ReadString('Format', FDateFormat); // Do not localize
end;

procedure TDateTimeExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);
  _Settings.WriteString('Format', DateFormat); // Do not localize
end;

initialization
  RegisterEditorExpert(TDateTimeExpert);
end.

