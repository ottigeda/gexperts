
resourcestring

     sExpertName        = 'BookmarksExpert';   // internal name of expert for expert identification = BaseKeyName in registry
     sExpertFormName    = 'fmGxBookmarksForm'; // as defined in IDE
     sExpertMenuCaption = 'Editor Bookmarks';  // caption of a menu item entry, do we really need?

type TBookmarksExpert   = class(TGX_Expert)
                          private

                           {$IFDEF GX_VER170_up}
                           FNotifierIdx                     : Integer;
                           procedure EditorViewActivated    (_Sender:TObject;_EditView:IOTAEditView);
                           {$ENDIF}

                          protected

                           procedure SetActive              (New:Boolean);    override;

                          public

                           constructor Create;                                override;
                           destructor  Destroy;                               override;

                           function       GetActionCaption  : String;         override;
                           class function GetName           : String;         override;
                           function       HasConfigOptions  : Boolean;        override;
                           procedure      Execute           (Sender:TObject); override;

                          end;

var  fmBookmarks        : TfmGxBookmarksForm = nil;
     BookmarksExpert    : TBookmarksExpert   = nil;

constructor TBookmarksExpert.Create;
begin
 inherited Create;

 {$IFDEF GX_VER170_up}
 if Assigned(BorlandIDEServices) then begin
  FNotifierIdx:=(BorlandIDEServices as IOTAEditorServices).AddNotifier(
  TGxNTAEditServiceNotifierActivate.Create(EditorViewActivated));
  end;
 {$ENDIF}

 BookmarksExpert:=Self;
 IdeDockManager.RegisterDockableForm(TfmGxBookmarksForm,fmBookmarks,sExpertFormName);
end;

destructor TBookmarksExpert.Destroy;
begin
 BookmarksExpert:=nil;

 {$IFDEF GX_VER170_up}
 if FNotifierIdx<>0 then (BorlandIDEServices as IOTAEditorServices).RemoveNotifier(FNotifierIdx);
 {$ENDIF}

 FreeAndNil(fmBookmarks);

 inherited Destroy;
end;

procedure TBookmarksExpert.SetActive(New:Boolean);
begin
 if New<>Active then begin

  inherited SetActive(New);

  if New then       IdeDockManager.  RegisterDockableForm(TfmGxBookmarksForm,fmBookmarks,sExpertFormName)
         else begin IdeDockManager.UnRegisterDockableForm(                   fmBookmarks,sExpertFormName);
                    FreeAndNil(fmBookmarks);
                    end;
  end;
end;

procedure TBookmarksExpert.Execute(Sender:TObject);
begin
 if fmBookmarks=nil then begin fmBookmarks:=TfmGxBookmarksForm.Create(nil);
                               SetFormIcon(fmBookmarks);
                               end;
 fmBookmarks.ScanBookmarks;
 IdeDockManager.ShowForm(fmBookmarks);
 EnsureFormVisible(fmBookmarks);
 IncCallCount;
end;

{$IFDEF GX_VER170_up}
procedure TBookmarksExpert.EditorViewActivated(_Sender:TObject;_EditView:IOTAEditView);
begin
 if Assigned(fmBookmarks) then fmBookmarks.ScanBookmarks;
end;
{$ENDIF}

function TBookmarksExpert.GetActionCaption:String;
begin
 Result:=sExpertMenuCaption;
end;

class function TBookmarksExpert.GetName:String;
begin
 Result:=sExpertName;
end;

function TBookmarksExpert.HasConfigOptions:Boolean;
begin
 Result:=false;
end;
