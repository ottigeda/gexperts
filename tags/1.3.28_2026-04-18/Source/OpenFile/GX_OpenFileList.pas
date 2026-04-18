unit GX_OpenFileList;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, ActnList;

type
  TfrOpenFileList = class(TFrame)
    pnlList: TPanel;
    ListView: TListView;
    pnlFooter: TPanel;
    btnAddToFavorites: TButton;
  public
    constructor Create(_Owner: TComponent); override;
    procedure Init(_AddToFavoritesAction: TBasicAction; _OnDblClick: TNotifyEvent);
  end;

implementation

{$R *.dfm}

{ TfrOpenFileList }

constructor TfrOpenFileList.Create(_Owner: TComponent);
begin
  inherited;
  ListView.DoubleBuffered := True;
end;

procedure TfrOpenFileList.Init(_AddToFavoritesAction: TBasicAction; _OnDblClick: TNotifyEvent);
begin
  btnAddToFavorites.Action := _AddToFavoritesAction;
  ListView.OnDblClick := _OnDblClick;
end;

end.
