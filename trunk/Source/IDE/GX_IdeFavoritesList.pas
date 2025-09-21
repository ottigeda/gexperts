unit GX_IdeFavoritesList;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GX_BaseForm,
  StdCtrls,
  ComCtrls,
  ExtCtrls;

type
  TOnEditFavoritesEntry = procedure(_Sender: TWinControl;
    var _Name, _Value: string; var _OK: Boolean) of object;

type
  Tf_GxIdeFavoritesList = class(TfmBaseForm)
    b_Close: TButton;
    lv_Favorites: TListView;
    b_Add: TButton;
    b_Edit: TButton;
    b_Delete: TButton;
    b_Help: TButton;
    procedure b_AddClick(Sender: TObject);
    procedure b_EditClick(Sender: TObject);
    procedure b_DeleteClick(Sender: TObject);
    procedure lv_FavoritesDblClick(Sender: TObject);
    procedure b_HelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FOnEdit: TOnEditFavoritesEntry;
    FOnCallHelpContext: TNotifyEvent;
    procedure SetData(const _Caption: string; _OnEdit: TOnEditFavoritesEntry; _Favorites: TStrings; _OnCallHelpContext:TNotifyEvent=nil);
    procedure GetData(_Favorites: TStrings);
    procedure EditCurrent;
    function doOnEdit(var _Name, _Value: string): Boolean;
  protected
    procedure CreateWnd;                              override; // show sizeable form like a dialog
    procedure CreateParams(var Params:TCreateParams); override; // form without caption icon
  public
    class procedure Execute(_Owner: TComponent; const _Caption: string;
      _OnEdit: TOnEditFavoritesEntry; _Favorites: TStringList;_OnCallHelpContext:TNotifyEvent=nil);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

uses
  u_dzVclUtils;

{$R *.dfm}

{ Tf_GxIdeFavoritesList }

class procedure Tf_GxIdeFavoritesList.Execute(_Owner: TComponent; const _Caption: string;
  _OnEdit: TOnEditFavoritesEntry; _Favorites: TStringList; _OnCallHelpContext:TNotifyEvent=nil);
var
  frm: Tf_GxIdeFavoritesList;
begin
  frm := Tf_GxIdeFavoritesList.Create(_Owner);
  try
    frm.SetData(_Caption, _OnEdit, _Favorites, _OnCallHelpContext);
    frm.ShowModal;
    frm.GetData(_Favorites);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_GxIdeFavoritesList.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);

  InitDpiScaler;
end;

procedure Tf_GxIdeFavoritesList.b_AddClick(Sender: TObject);
var
  li: TListItem;
  FavName: string;
  FavPath: string;
begin
  FavName := 'New Entry';
  FavPath := '';
  if not doOnEdit(FavName, FavPath) then
    Exit;
  li := lv_Favorites.Items.Add;
  li.Caption := FavName;
  li.SubItems.Add(FavPath);
end;

procedure Tf_GxIdeFavoritesList.b_DeleteClick(Sender: TObject);
var
  li: TListItem;
  Idx : Integer;
begin
  li := lv_Favorites.Selected;
  if not Assigned(li) then
    Exit;
  if MessageDlg('Delete this entry?', mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
    Exit;

  Idx := lv_Favorites.ItemIndex;
  lv_Favorites.DeleteSelected;
  if Idx<lv_Favorites.Items.Count then lv_Favorites.ItemIndex:=Idx
                                  else lv_Favorites.ItemIndex:=Idx-1
end;

procedure Tf_GxIdeFavoritesList.b_EditClick(Sender: TObject);
begin
  EditCurrent;
end;

function Tf_GxIdeFavoritesList.doOnEdit(var _Name, _Value: string): Boolean;
begin
  Result := False;
  if Assigned(FOnEdit) then
    FOnEdit(Self, _Name, _Value, Result);
end;

procedure Tf_GxIdeFavoritesList.EditCurrent;
var
  li: TListItem;
  FavName: string;
  FavPath: string;
begin
  li := lv_Favorites.Selected;
  if not Assigned(li) then
    Exit;
  FavName := li.Caption;
  FavPath := li.SubItems[0];
  if not doOnEdit(FavName, FavPath) then
    Exit;
  li.Caption := FavName;
  li.SubItems[0] := FavPath;
end;

procedure Tf_GxIdeFavoritesList.GetData(_Favorites: TStrings);
var
  i: Integer;
  FavName: string;
  FavValue: string;
  li: TListItem;
begin
   _Favorites.Clear;
  for i := 0 to lv_Favorites.Items.Count - 1 do begin
    li := lv_Favorites.Items[i];
    FavName := li.Caption;
    FavValue := li.SubItems[0];
    _Favorites.Values[FavName] := FavValue;
  end;
end;

procedure Tf_GxIdeFavoritesList.lv_FavoritesDblClick(Sender: TObject);
begin
  EditCurrent;
end;

procedure Tf_GxIdeFavoritesList.SetData(const _Caption: string; _OnEdit: TOnEditFavoritesEntry;
  _Favorites: TStrings; _OnCallHelpContext:TNotifyEvent=nil);
var
  i: Integer;
  FavName: string;
  FavValue: string;
  li: TListItem;
begin
  Caption := _Caption;
  FOnEdit := _OnEdit;
  FOnCallHelpContext := _OnCallHelpContext;
  for i := 0 to _Favorites.Count - 1 do begin
    FavName := _Favorites.Names[i];
    FavValue := _Favorites.Values[FavName];
    li := lv_Favorites.Items.Add;
    li.Caption := FavName;
    li.SubItems.Add(FavValue);
  end;
  lv_Favorites.ItemIndex:=0;

  b_Help.Visible := Assigned(_OnCallHelpContext);
  if not(b_Help.Visible) then b_Close.Top := b_Help.Top;
end;

procedure Tf_GxIdeFavoritesList.b_HelpClick(Sender: TObject);
begin
  inherited;
  FOnCallHelpContext(Self);
end;

procedure Tf_GxIdeFavoritesList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key=vk_F1 then b_HelpClick(nil);
end;

// show sizeable form like a dialog
// form without caption icon
procedure Tf_GxIdeFavoritesList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle:=Params.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure Tf_GxIdeFavoritesList.CreateWnd;
begin
  inherited;
  SendMessage(Handle,WM_SETICON,1,0);
end;

end.
