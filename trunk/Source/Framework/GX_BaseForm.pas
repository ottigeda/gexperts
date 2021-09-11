unit GX_BaseForm;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  // All forms except docking forms must descend from this class.
  // Changes here must also be made to TfmIdeDockForm, since it must descend
  // from the IDE-internal TDockableForm class.
  TfmBaseForm = class(TForm)
  private
    class procedure ExecuteNonDpiAware(_Owner: TComponent); overload; virtual;
  public
    class procedure Execute(_Owner: TComponent); overload; virtual;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GX_GxUtils;

class procedure TfmBaseForm.Execute(_Owner: TComponent);
begin
  ExecuteNonDpiAware(_Owner);
end;

class procedure TfmBaseForm.ExecuteNonDpiAware(_Owner: TComponent);
var
  frm: TForm;
{$IFDEF IDE_IS_HIDPI_AWARE}
  previousDpiContext: DPI_AWARENESS_CONTEXT;
{$ENDIF}
begin
{$IFDEF IDE_IS_HIDPI_AWARE}
  // See
  // https://www.uweraabe.de/Blog/2021/08/28/delphi-vcl-applications-with-mixed-dpi/
  // why we do this, and
  // https://en.delphipraxis.net/topic/5516-the-state-of-gexperts-support-for-delphi-11/?do=findComment&comment=47733
  // for the modified trick.
  previousDpiContext := SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_UNAWARE_GDISCALED);
  try
{$ENDIF}
    frm := Self.Create(nil);
    try
      frm.ShowModal;
    finally
      frm.Free;
    end;
{$IFDEF IDE_IS_HIDPI_AWARE}
  finally
    SetThreadDpiAwarenessContext(previousDpiContext);
  end;
{$ENDIF}
end;

constructor TfmBaseForm.Create(AOwner: TComponent);
begin
  inherited;
  GxSetDefaultFont(Self);
end;

end.
