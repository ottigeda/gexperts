unit GX_NTAEditServiceNotifier;

{$I GX_CondDefine.inc}

interface

uses
  Classes, ToolsAPI, DockForm, GX_Logging;

{$IFDEF GX_VER160_up}

type
  ///<summary>
  /// This is a dummy implementation of INTAEditServicesNotifier that does nothing.
  /// Descendants override some methods to get the notifications they are interested in. </summary>
  TGxNTAEditServiceNotifier = class(TNotifierObject)
  private
    Logger: IGxLogger;
  protected // INTAEditServicesNotifier
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean); virtual;
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation); virtual;
    procedure WindowActivated(const EditWindow: INTAEditWindow); virtual;
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean); virtual;
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); virtual;
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView); virtual;
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm); virtual;
{$IFOPT D+}
  protected // IOTANotifier
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
{$ENDIF}
   public
     constructor Create;
  end;
{$ENDIF}

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Windows;


{$IFDEF GX_VER160_up}

{ TGxNTAEditServiceNotifier }

constructor TGxNTAEditServiceNotifier.Create;
begin
  inherited Create;
  Logger := CreateModuleLogger('TGxNTAEditServiceNotifier');
end;

{$IFOPT D+}
procedure TGxNTAEditServiceNotifier.AfterSave;
begin
  Logger.Info('AfterSave');
end;

procedure TGxNTAEditServiceNotifier.BeforeSave;
begin
  Logger.Info('BeforeSave');
end;

procedure TGxNTAEditServiceNotifier.Destroyed;
begin
  Logger.Info('Destroyed');
end;

procedure TGxNTAEditServiceNotifier.Modified;
begin
   Logger.Info('Modified');
end;
{$ENDIF}


procedure TGxNTAEditServiceNotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  Logger.InfoFmt('DockFormRefresh(%s, %s)', [EditWindow.Form.Caption, DockForm.Caption]);
end;

procedure TGxNTAEditServiceNotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  Logger.InfoFmt('DockFormUpdated(%s, %s)', [EditWindow.Form.Caption, DockForm.Caption]);
end;

procedure TGxNTAEditServiceNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  Logger.InfoFmt('DockFormVisibleChanged(%s, %s)', [EditWindow.Form.Caption, DockForm.Caption]);
end;

procedure TGxNTAEditServiceNotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  Logger.InfoFmt('EditorViewActivated(%s)', [EditWindow.Form.Caption]);
end;

procedure TGxNTAEditServiceNotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  Logger.InfoFmt('EditorViewModified(%s)', [EditWindow.Form.Caption]);
end;

procedure TGxNTAEditServiceNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  Logger.InfoFmt('WindowActivated(%s)', [EditWindow.Form.Caption]);
end;

procedure TGxNTAEditServiceNotifier.WindowCommand(const EditWindow: INTAEditWindow;
  Command, Param: Integer; var Handled: Boolean);
begin
  Logger.InfoFmt('WindowActivated(%s, %d, %d)', [EditWindow.Form.Caption, Command, Param]);
end;

procedure TGxNTAEditServiceNotifier.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
  Logger.InfoFmt('WindowNotification(%s, %d)', [EditWindow.Form.Caption, Ord(Operation)]);
end;

procedure TGxNTAEditServiceNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
  Logger.InfoFmt('WindowShow(%s)', [EditWindow.Form.Caption]);
end;
{$ENDIF}

end.
