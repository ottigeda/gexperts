unit GX_ProofreaderExpert;

{$I GX_CondDefine.inc}

interface

uses
  GX_ProofreaderData, GX_ConfigurationInfo, GX_Experts, Classes, Types;

type
  TCodeProofreaderExpert = class(TGX_Expert)
  private
    FProofreaderData: TProofreaderData;
    procedure CopyDefaultsFromResource(const _FileName: string);
  protected
    procedure SetActive(New: Boolean); override;
    procedure InternalSaveSettings(_Settings: IExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
  end;

var
  CodeProofreaderExpert: TCodeProofreaderExpert = nil;

implementation

uses
  SysUtils, Dialogs, Controls,
  GX_ProofreaderConfig, GX_MessageBox;

type
  TCreateDefaultTablesMessage = class(TGxQuestionBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

{ TCreateDefaultTablesMessage }

function TCreateDefaultTablesMessage.GetMessage: string;
resourcestring
  SDefaultTablesCreated = 'Could not find the Code Proofreader data file:'
  + sLineBreak + '%s' + sLineBreak +
  'Would you like to create the default replacement rules and dictionary entries?';
begin
  Result := Format(SDefaultTablesCreated, [FData]);
end;

{ TCodeProofreaderExpert }

procedure TCodeProofreaderExpert.Execute(Sender: TObject);
begin
  Configure;
end;

procedure TCodeProofreaderExpert.Configure;
var
  Data: TProofreaderData;
  Dlg: TfmProofreaderConfig;
  FileName: string;
begin
  Data := FProofreaderData;
  if Data = nil then begin
    // the expert is not active, crate a local instance of the data just for configuring it
    Data := TProofreaderData.Create;
  end;
  try
    FileName := Data.GetXmlFileName;
    if (not FileExists(FileName)) and
      (ShowGxMessageBox(TCreateDefaultTablesMessage, FileName) = mrYes) then
    begin
      CopyDefaultsFromResource(FileName);
    end;
    Data.ReloadData;

    Dlg := TfmProofreaderConfig.Create(nil, Self, Data);
    try
      SetFormIcon(Dlg);
      Dlg.ShowModal;
    finally
      FreeAndNil(Dlg);
    end;
    SaveSettings;
  finally
    if not Assigned(FProofreaderData) then begin
      // We created a local instance just for configuring the expert, but since it is not active
      // we no longer need it.
      FreeAndNil(Data);
    end;
  end;
end;

procedure TCodeProofreaderExpert.CopyDefaultsFromResource(const _FileName: string);
var
  ResStream: TResourceStream;
  FileStream: TFileStream;
begin
  FileStream := nil;
  ResStream := TResourceStream.Create(hInstance, 'CodeProofreaderDefault', RT_RCDATA);
  try
    FileStream := TFileStream.Create(_FileName, fmOpenReadWrite or fmCreate);
    FileStream.CopyFrom(ResStream, ResStream.Size);
  finally
    FreeAndNil(FileStream);
    FreeAndNil(ResStream);
  end;
end;

constructor TCodeProofreaderExpert.Create;
begin
  inherited;

  FreeAndNil(CodeProofreaderExpert);
  CodeProofreaderExpert := Self;
end;

destructor TCodeProofreaderExpert.Destroy;
begin
  FreeAndNil(FProofreaderData);
  CodeProofreaderExpert := nil;

  inherited Destroy;
end;

function TCodeProofreaderExpert.GetActionCaption: string;
resourcestring
  SProofMenuCaption = 'Code &Proofreader...';
begin
  Result := SProofMenuCaption;
end;

class function TCodeProofreaderExpert.GetName: string;
begin
  Result := 'CodeProofreader';
end;

procedure TCodeProofreaderExpert.InternalSaveSettings(_Settings: IExpertSettings);
begin
  inherited InternalSaveSettings(_Settings);

  if Assigned(FProofreaderData) then
    FProofreaderData.SaveSettings(_Settings);
end;

procedure TCodeProofreaderExpert.SetActive(New: Boolean);
begin
  inherited SetActive(New);

  if Active then
  begin
    if not Assigned(FProofreaderData) then
      FProofreaderData := TProofreaderData.Create;

    FProofreaderData.ReloadData;
  end
  else
    FreeAndNil(FProofreaderData);
end;

initialization
  RegisterGX_Expert(TCodeProofreaderExpert);

end.

