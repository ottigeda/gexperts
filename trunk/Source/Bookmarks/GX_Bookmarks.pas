{ *************************************************************************************
  * Unit Name: GX_Bookmarks
  * Authors  : Thomas Mueller http://blog.dummzeuch.de
  *            Ralph Zornig, March 2025
  ************************************************************************************* }

unit GX_Bookmarks;

{$I GX_CondDefine.inc}

interface { --------------------------------------------------------------------------- }

uses SysUtils,Classes,Controls,Forms,Dialogs,StdCtrls,ComCtrls,ToolsAPI,ExtCtrls,Messages,
     ToolWin,Types,Graphics,StrUtils,Math,ImgList,ImageList,

     {$IFDEF GX_IDE_IS_HIDPI_AWARE}
     u_dzDpiScaleUtils,
     {$ENDIF}

     GX_Experts,
     GX_BaseForm,
     GX_IdeDock,
     GX_GxUtils,

     GX_BookmarksConst,
     GX_BookmarksFuncProcName,
     GX_BookmarksLists;

type tSortMode          = (smIdx,smName,smLine);
     TfmGxBookmarksForm = class(TfmIdeDockForm)

                           paToolbar                        : TPanel;
                            ToolBar                         : TToolBar;
                             tbAdd1                         : TToolButton;
                             tbAdd2                         : TToolButton;
                             tbEdit                         : TToolButton;
                             tbSep0                         : TToolButton;
                             tbSort                         : TToolButton;
                             tbSep1                         : TToolButton;
                             tbDelete                       : TToolButton;
                             tbDeleteAll                    : TToolButton;
                             tbSep2                         : TToolButton;
                             tbOptions                      : TToolButton;
                             tbHelp                         : TToolButton;

                           paListbox                        : TPanel;
                            paListboxGutterRight            : TPanel;   
                            paListboxGutterBottom           : TPanel;   
                            lbBookmarks                     : TListBox;
                            me                              : TMemo;    // debugging only

                           SBar                             : TStatusBar;

                           tiUpdateBookmarks                : TTimer;

                           ilEnabled                        : TImageList;
                           ilDisabled                       : TImageList;

                           procedure lbBookmarksDrawItem    (_Control:TWinControl;_Index:Integer;_Rect:TRect;_State:TOwnerDrawState);
                           procedure lbBookmarksMouseMove   (Sender:TObject;Shift:TShiftState;X,Y:Integer);
                           procedure lbBookmarksClick       (Sender:TObject);
                           procedure lbBookmarksDblClick    (Sender:TObject);
                           procedure lbBookmarksKeyDown     (Sender:TObject;var Key:Word;Shift:TShiftState);

                           procedure tbAdd1Click            (Sender:TObject);
                           procedure tbAdd2Click            (Sender:TObject);
                           procedure tbEditClick            (Sender:TObject);
                           procedure tbSortClick            (Sender:TObject);
                           procedure tbDeleteClick          (Sender:TObject);
                           procedure tbDeleteAllClick       (Sender:TObject);
                           procedure tbOptionsClick         (Sender:TObject);
                           procedure tbHelpMouseMove        (Sender:TObject;Shift:TShiftState;X,Y:Integer);
                           procedure tbHelpClick            (Sender:TObject);

                           procedure SBarResize             (Sender:TObject);

                           procedure FormResize             (Sender:TObject);
                           procedure FormMouseWheel         (Sender:TObject;Shift:TShiftState;WheelDelta:Integer;MousePos:TPoint;var Handled:Boolean);

                           procedure tiUpdateBookmarksTimer (Sender:TObject);

                          private

                           FBookmarks                       : TBookmarkList;
                           FSortMode                        : tSortMode;
                           FLastSortMode                    : tSortMode;
                           FEditorPathFName                 : String;
                           FUseOldEditForm                  : Boolean;
                           FEnlargedLineDisplay             : Boolean;
                           FLastEnlargedLineDisplay         : Boolean;
                           FRemoveLeadingBlanks             : Boolean;
                           FHideClassName                   : Boolean;
                           FHideLBFocus                     : Boolean;
                           FDenyDoubleBookmarks             : Boolean;
                           FListboxColorBackground          : TColor;
                           FListboxColorBMline              : TColor;
                           FBkColorFuncProcName             : TColor;
                           FFontColorFuncProcName           : TColor;
                           FLastHintIdx                     : Integer;

                           procedure WMAddBookmark          (var Msg:TMessage); message WM_AddBookmark;

                           procedure SaveSettings;
                           procedure LoadSettings;
                           procedure SetListboxItemHeight;
                           procedure SetButtonState;
                           function  GetEditView            (var _SourceEditor:IOTASourceEditor;var _EditView:IOTAEditView):Boolean;
                           procedure SetLBItemIndex         (LineNo:Integer);

                           procedure BookmarkSet            (const _ModuleName:String;_LineNo:Integer;_BmIdx:Integer=-1);
                           procedure BookmarkDel            (const _ModuleName:String;_BmIdx:Integer);
                           procedure Jump2Bookmark;
                           procedure ScanBookmarks;

                           procedure SBarShowModulInfo      (PathFName:String);
                           procedure SBarShowSortInfo;

                          protected

                           {$IFDEF GX_IDE_IS_HIDPI_AWARE}
                           procedure ArrangeControls;                            override;
                           {$ENDIF}

                          public

                           constructor Create               (_Owner:TComponent); override;
                           destructor  Destroy;                                  override;

                          end;

implementation { ---------------------------------------------------------------------- }

{$R *.dfm}

uses {$IFOPT D+} GX_DbugIntf, {$ENDIF}
     Windows,
     u_dzStringUtils,
     u_dzVclUtils,
     GX_GExperts,
     GX_ConfigurationInfo,
     GX_OtaUtils,
     GX_GenericUtils,
     GX_NTAEditServiceNotifier,

     GX_BookmarksEdit,
     GX_BookmarksOptions,
     GX_BookmarksSelect;

const
     sExpertFormName    = 'fmGxBookmarksForm'; // as defined in IDE (must not be a resource string)

resourcestring

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
  // do not translate (and do not make a resource string)
 Result:='BookmarksExpert';
end;

function TBookmarksExpert.HasConfigOptions:Boolean;
begin
 Result:=false;
end;

// ......................................................................................

{$IFDEF GX_IDE_IS_HIDPI_AWARE}
procedure TfmGxBookmarksForm.ArrangeControls; // virtual procedure of TfmIdeDockForm
begin
 inherited;
 SetListboxItemHeight;
end;
{$ENDIF}

// ............................................................................. Messages

procedure TfmGxBookmarksForm.WMAddBookmark(var Msg:TMessage);
var LineNo : Integer;
begin
 LineNo:=Msg.WParam;

 BookmarkSet(FEditorPathFName,LineNo,-1);

 ScanBookmarks;
 Jump2Bookmark;
 SetLBItemIndex(LineNo);

 Msg.Result:=FBookmarks.Count;
end;

// ................................................................................. Misc

procedure TfmGxBookmarksForm.SaveSettings;
var SubSettings : IExpertSettings;
begin
 SubSettings:=Settings.Subkey('Bookmarks'); // do not localize

 SubSettings.WriteBool   ('BookmarksUseOldEditForm',         FUseOldEditForm        );
 SubSettings.WriteBool   ('BookmarksEnlargedLineDisplay',    FEnlargedLineDisplay   );
 SubSettings.WriteBool   ('BookmarksRemoveLeadingBlanks',    FRemoveLeadingBlanks   );
 SubSettings.WriteBool   ('BookmarksHideClassName',          FHideClassName         );
 SubSettings.WriteBool   ('BookmarksHideLBFocus',            FHideLBFocus           );
 SubSettings.WriteBool   ('BookmarksDenyDoubleBookmarks',    FDenyDoubleBookmarks   );

 SubSettings.WriteInteger('BookmarksLBColorBackground',      FListboxColorBackground);
 SubSettings.WriteInteger('BookmarksLBColorBMline',          FListboxColorBMline    );
 SubSettings.WriteInteger('BookmarksLBColorBkFuncProcName',  FBkColorFuncProcName   );
 SubSettings.WriteInteger('BookmarksLBColorFontFuncProcName',FFontColorFuncProcName );
 SubSettings.WriteInteger('BookmarksLastSort',               ord(FSortMode)         );
end;

procedure TfmGxBookmarksForm.LoadSettings;
var SubSettings : IExpertSettings;
begin
 SubSettings:=Settings.Subkey('Bookmarks'); // do not localize

 FUseOldEditForm        :=          SubSettings.ReadBool   ('BookmarksUseOldEditForm',         DefUseOldEditForm          );
 FEnlargedLineDisplay   :=          SubSettings.ReadBool   ('BookmarksEnlargedLineDisplay',    DefEnlargedLineDisplay     );
 FRemoveLeadingBlanks   :=          SubSettings.ReadBool   ('BookmarksRemoveLeadingBlanks',    DefRemoveLeadingBlanks     );
 FHideClassName         :=          SubSettings.ReadBool   ('BookmarksHideClassName',          DefHideClassName           );
 FHideLBFocus           :=          SubSettings.ReadBool   ('BookmarksHideLBFocus',            DefHideLBFocus             );
 FDenyDoubleBookmarks   :=          SubSettings.ReadBool   ('BookmarksDenyDoubleBookmarks',    DefDenyDoubleBookmarks     );

 FListboxColorBackground:=          SubSettings.ReadInteger('BookmarksLBColorBackground',      DefLBColorBackground       );
 FListboxColorBMline    :=          SubSettings.ReadInteger('BookmarksLBColorBMline',          DefLBColorBMline           );
 FBkColorFuncProcName   :=          SubSettings.ReadInteger('BookmarksLBColorBkFuncProcName',  DefLBColorBkFuncProcName   );
 FFontColorFuncProcName :=          SubSettings.ReadInteger('BookmarksLBColorFontFuncProcName',DefLBColorFontFuncProcName );
 FSortMode              :=tSortMode(SubSettings.ReadInteger('BookmarksLastSort',               ord(smLine)               ));
end;

procedure TfmGxBookmarksForm.SetListboxItemHeight;
var N : Integer;
begin
 if FEnlargedLineDisplay then N:=5 else N:=3;
 lbBookmarks.ItemHeight:=succ(BookmarksHeightHeader+N*BookmarksHeightLine);
end;

procedure TfmGxBookmarksForm.SetButtonState;

 function CanSetBookmark:Boolean;
 var SourceEditor : IOTASourceEditor;
     EditView     : IOTAEditView;
     i            : Integer;
 begin
  Result:=true;
  if GetEditView(SourceEditor,EditView) then
   for i:=0 to lbBookmarks.Count-1 do
    if (TBookmark(lbBookmarks.Items.Objects[i]).Line=EditView.CursorPos.Line) and
       FDenyDoubleBookmarks                                                   then begin
     Result:=false;
     Break;
     end;
 end;

 function NoDFM:Boolean;
 begin
  Result:=not(AnsiEndsText('.dfm',FEditorPathFName));
 end;

begin
 tbAdd1.     Enabled:=(lbBookmarks.Count<maxBookmarks) and CanSetBookmark;
 tbAdd2.     Enabled:=(lbBookmarks.Count<maxBookmarks) and NoDFM;
 tbEdit.     Enabled:=(lbBookmarks.Count>0)            and (lbBookmarks.ItemIndex>=0);
 tbSort.     Enabled:=true;
 tbDelete.   Enabled:=(lbBookmarks.Count>0)            and (lbBookmarks.ItemIndex>=0);
 tbDeleteAll.Enabled:=(lbBookmarks.Count>1);
end;

procedure TfmGxBookmarksForm.SetLBItemIndex(LineNo:Integer);
var i : Integer;
begin
 for i:=0 to lbBookmarks.Count-1 do
  if TBookmark(lbBookmarks.Items.Objects[i]).Line=LineNo then begin
   lbBookmarks.ItemIndex:=i;
   lbBookmarks.TopIndex :=lbBookmarks.Count-(lbBookmarks.ClientHeight div lbBookmarks.ItemHeight);
   lbBookmarks.Refresh;
   Break;
   end;
end;

function TfmGxBookmarksForm.GetEditView(var _SourceEditor:IOTASourceEditor;var _EditView:IOTAEditView):Boolean;
begin
 Result:=false;
 if not Ota.TryGetCurrentSourceEditor(_SourceEditor) then exit;
 if _SourceEditor.EditViewCount=0                    then exit;

 _EditView:=_SourceEditor.GetEditView(0);
 Result   :=Assigned(_EditView);
end;

   function TryGetEditView(const _fn:String;out _EditView:IOTAEditView):Boolean; // warum das auch noch ???
   var SourceEditor : IOTASourceEditor;
   begin
    SourceEditor:=Ota.GetSourceEditor(_fn);
    Result      :=Assigned(SourceEditor) and (SourceEditor.EditViewCount>0);
    if Result then _EditView := SourceEditor.EditViews[0];
   end;

procedure TfmGxBookmarksForm.BookmarkSet(const _ModuleName:String;_LineNo:Integer;_BmIdx:Integer=-1);
var EditView  : IOTAEditView;
    BMEditPos : TOTAEditPos;
    i         : Integer;
begin
 if not(TryGetEditView(_ModuleName,EditView)) then exit;

 if _BmIdx<0 then begin // no bookmark index was given, find 1st free bookmark index

  for i:=0 to maxBookmarks-1 do
   if EditView.BookmarkPos[i].Line=0 then begin
    _BmIdx:=i;
    Break;
    end;

  if _BmIdx<0 then exit; // no free bookmark index found
  end;

 try
  BMEditPos.Line:=_LineNo;
  BMEditPos.Col :=1;
  EditView.SetCursorPos(BmEditPos);
  EditView.BookmarkRecord(_BmIdx);

 finally
  EditView.MoveViewToCursor;
  Ota.FocusCurrentIDEEditControl;
  EditView.Paint;
  end;
end;

procedure TfmGxBookmarksForm.BookmarkDel(const _ModuleName:String;_BmIdx:Integer);
var EditView      : IOTAEditView;
    SaveCursorPos : TOTAEditPos;
    BMEditPos     : TOTAEditPos;
begin
 if not(TryGetEditView(_ModuleName,EditView)) then exit;

 if EditView.BookmarkPos[_BmIdx].Line<>0 then begin

  SaveCursorPos:=EditView.GetCursorPos;
  try
   BMEditPos.Line:=EditView.BookmarkPos[_BmIdx].Line;
   BMEditPos.Col :=EditView.BookmarkPos[_BmIdx].CharIndex;
   EditView.SetCursorPos(BmEditPos);
   EditView.BookmarkToggle(_BmIdx);

  finally
   EditView.SetCursorPos(SaveCursorPos);
   EditView.Paint;
   end;
  end;
end;

procedure TfmGxBookmarksForm.Jump2Bookmark;
var BM           : TBookmark;
    Module       : IOTAModule;
    SourceEditor : IOTASourceEditor;
    EditView     : IOTAEditView;
begin
 if not(TListBox_GetSelectedObject(lbBookmarks,Pointer(BM))) then exit;

 if not(GxOtaMakeSourceVisible(BM.Module)) then raise Exception.CreateFmt('Could not open file %s',[BM.Module]);

 Module:=Ota.GetModule(BM.Module);
 if not(Assigned(Module)) then exit;

 SourceEditor:=Ota.GetSourceEditorFromModule(Module,BM.Module);
 if not(Assigned(SourceEditor)) then exit;

 SourceEditor.Show;

 if not(Ota.TryGetTopMostEditView(SourceEditor,EditView)) then exit;

 EditView.BookmarkGoto(BM.Number);
 EditView.MoveViewToCursor;
 lbBookmarks.SetFocus; //�3 -cInfo : MUST for updating IDE SBar, EditView.Paint has no effect
 Ota.FocusCurrentIDEEditControl;
 // EditView.Paint;
end;

procedure TfmGxBookmarksForm.ScanBookmarks;
var SourceEditor       : IOTASourceEditor;
    EditView           : IOTAEditView;
    i                  : Integer;
    BM                 : TBookmark;
    NewBookmarks       : TBookmarkList;
    FuncProcName       : String;

 procedure GetBookmarks(const _ModuleName:String;_EditView:IOTAEditView;_Bookmarks:TBookmarkList);
 var i     : Integer;
     BMPos : TOTACharPos;
 begin
  for i:=0 to maxBookmarks-1 do begin
   BMPos:=_EditView.BookmarkPos[i];
   if BMPos.Line<>0 then _Bookmarks.Add(i,BMPos.Line,BMPos.CharIndex,_ModuleName);
   end;
 end;

 function BookmarksChanged(_NewBookmarks:TBookmarkList):Boolean;
 var BM1 : TBookmark;
     BM2 : Pointer;
     i   : Integer;
 begin
  Result:=false;
  if not(assigned(FBookmarks)) then exit;

  if _NewBookmarks.Count<>FBookmarks.Count then Result:=true
                                           else

   for i:=0 to _NewBookmarks.Count-1 do begin
    BM1:=_NewBookmarks[i];
    BM2:=FBookmarks[i];
    if not BM1.IsSame(BM2) then begin Result:=true;
                                      exit;
                                      end;
    end;
 end;

 procedure AddOrInsertIdx(LB:TListbox;Idx:Integer;FuncProcName:String;BMptr:Pointer);
 begin
  LB.AddItem(FuncProcName,BMptr); // SortIdx is standard behavior
 end;

 procedure AddOrInsertName(LB:TListbox;Idx:Integer;FuncProcName:String;BMptr:Pointer);
 var i,p,k : Integer;
 begin                                 
  p:=-1;
  for i:=0 to LB.Count-1 do begin

   k:=AnsiCompareText(LB.Items[i],FuncProcName);
   if k>0 then begin p:=i;
                     Break;
                     end

          else if k=0 then
                if TBookmark(LB.Items.Objects[i]).Line>TBookmark(BMptr).Line then begin p:=i;
                                                                                        Break;
                                                                                        end
   end;

  if p>=0 then LB.Items.InsertObject(p,FuncProcName,BMptr)
          else LB.AddItem(FuncProcName,BMptr);
 end;

 procedure AddOrInsertLine(LB:TListbox;Idx:Integer;FuncProcName:String;BMptr:Pointer);
 var i,p : Integer;
 begin
  p:=-1;
  for i:=0 to LB.Count-1 do
   if TBookmark(LB.Items.Objects[i]).Line>TBookmark(BMptr).Line then begin p:=i;
                                                                           Break;
                                                                           end;
  if p>=0 then LB.Items.InsertObject(p,FuncProcName,BMptr)
          else LB.AddItem(FuncProcName,BMptr);
 end;

begin
 tiUpdateBookmarks.Enabled:=false;
 SetButtonState; // !!!

 try
  if not(GetEditView(SourceEditor,EditView)) then exit;
  NewBookmarks:=TBookmarkList.Create;

  try
   GetBookmarks(SourceEditor.Filename,EditView,NewBookmarks);
   SBarShowModulInfo(SourceEditor.Filename);
   FEditorPathFName:=SourceEditor.Filename;

   if BookmarksChanged(NewBookmarks)                   or         // refill if new bookmarks avail
      (           FSortMode<>FLastSortMode           ) or         // or sortmode changed
      (FEnlargedLineDisplay<>FLastEnlargedLineDisplay) then begin // or linecount2display changed

    FLastSortMode           :=FSortMode;
    FLastEnlargedLineDisplay:=FEnlargedLineDisplay;

    FreeAndNil(FBookmarks);
    FBookmarks:=NewBookmarks;
    NewBookmarks:=nil;

    lbBookmarks.Items.BeginUpdate;
    lbBookmarks.Items.Clear;

    lbBookmarks.Enabled:=(FBookmarks.Count>0); // no focus rectangle if lb is empty
    Caption:='Bookmarks'+' ('+IntToStr(FBookmarks.Count)+')';
    BuildLinkArray;

    for i:=0 to FBookmarks.Count-1 do begin

     BM:=FBookmarks[i];

     if FEnlargedLineDisplay then begin if BM.Line=1 then BM.Text:=CRLF+CRLF;
                                        if BM.Line=2 then BM.Text:=CRLF;

                                        BM.Text:=BM.Text+Ota.GetEditorLineAsString(EditView,BM.Line-2);
                                        BM.Text:=BM.Text+Ota.GetEditorLineAsString(EditView,BM.Line-1);
                                        BM.Text:=BM.Text+Ota.GetEditorLineAsString(EditView,BM.Line  );
                                        BM.Text:=BM.Text+Ota.GetEditorLineAsString(EditView,BM.Line+1);
                                        BM.Text:=BM.Text+Ota.GetEditorLineAsString(EditView,BM.Line+2);
                                        end

                             else begin if BM.Line=1 then BM.Text:=CRLF;

                                        BM.Text:=        Ota.GetEditorLineAsString(EditView,BM.Line-1);
                                        BM.Text:=BM.Text+Ota.GetEditorLineAsString(EditView,BM.Line  );
                                        BM.Text:=BM.Text+Ota.GetEditorLineAsString(EditView,BM.Line+1);
                                        end;

     FuncProcName:=GetFuncProcName(BM.Line);
     case FSortMode of
      smIdx  : AddOrInsertIdx (lbBookmarks,i,FuncProcName,BM);
      smName : AddOrInsertName(lbBookmarks,i,FuncProcName,BM);
      smLine : AddOrInsertLine(lbBookmarks,i,FuncProcName,BM);
      end;
     end;

    lbBookmarks.Items.EndUpdate;
    end;

  finally
   FreeAndNil(NewBookmarks);
   end;

 finally
  tiUpdateBookmarks.Enabled:=true;
  end;
end;

// ............................................................................. Controls

procedure TfmGxBookmarksForm.lbBookmarksDrawItem(_Control:TWinControl;_Index:Integer;_Rect:TRect;_State:TOwnerDrawState);
var ListBox      : TListBox absolute _Control;
    lbCanvas     : TCanvas;
    BM           : TBookmark;
    FuncProcName : String;

 procedure PaintHeader(_Rect:TRect);
 var TextTop,MaxL,p,
     GutterL,GutterR : Integer;
     BMNo            : String;
     LineNo          : String;
 begin
  TextTop:=_Rect.Top+FScaler.Calc(2);
  BMNo   :=Format('%.2d',   [BM.Number]);
  LineNo :=Format('Line %d',[BM.Line]  );

  lbCanvas.Font. Name :='Verdana';
  lbCanvas.Font. Size :=8;
  lbCanvas.Font. Color:=clBtnText;
  if _Index=Listbox.ItemIndex then lbCanvas.Brush.Color:=FBkColorFuncProcName
                              else lbCanvas.Brush.Color:=clBtnFace;
  lbCanvas.FillRect(_Rect);

  _Rect.Right :=_Rect.Right -FScaler.Calc(1);
  _Rect.Bottom:=_Rect.Bottom-FScaler.Calc(1);
  Frame3D(lbCanvas,_Rect,clBtnHighlight,clBtnShadow,FScaler.Calc(1));

  _Rect.Right :=_Rect.Right +FScaler.Calc(1);
  _Rect.Bottom:=_Rect.Bottom+FScaler.Calc(1);

  lbCanvas.Pen.Color:=cl3DDkShadow;
  lbCanvas.MoveTo(_Rect.Left -FScaler.Calc(1),_Rect.Bottom); // ---
  lbCanvas.LineTo(_Rect.Right+FScaler.Calc(1),_Rect.Bottom); //
  lbCanvas.MoveTo(_Rect.Right,_Rect.Top   -FScaler.Calc(1)); // |
  lbCanvas.LineTo(_Rect.Right,_Rect.Bottom+FScaler.Calc(1)); //

  GutterL:=FScaler.Calc(3);
  GutterR:=FScaler.Calc(6);

  lbCanvas.TextOut(_Rect.Left +GutterL,                           TextTop,BMNo);
  lbCanvas.TextOut(_Rect.Right-GutterR-lbCanvas.TextWidth(LineNo),TextTop,LineNo);

  lbCanvas.Font.Color:=FFontColorFuncProcName;
  lbCanvas.Font.Style:=lbCanvas.Font.Style+[fsBold];

  GutterL:=FScaler.Calc(24);
  MaxL   :=_Rect.Right-GutterR-lbCanvas.TextWidth(LineNo)-_Rect.Left-GutterL;

  if FHideClassName then begin
   p:=pos('.',FuncProcName);
   if p>0 then delete(FuncProcName,1,p-1); //�3 -cInfo : group local + class methods
   end;

  lbCanvas.TextOut(_Rect.Left+GutterL,TextTop,Shorten(lbCanvas,MaxL,FuncProcName,#133));

  lbCanvas.Font.Color:=clBtnText;
  lbCanvas.Font.Style:=lbCanvas.Font.Style-[fsBold];
 end;

 procedure PaintLines(_Rect:TRect);
 var LineTop,
     GutterL,i,MaxL : Integer;
     SL             : TStringList;
     BGNormal       : TColor;
     BGBookmark     : TColor;

  function SelectedAndFocused:Boolean;
  begin
   Result:=([odSelected,odFocused]*_State=[odSelected,odFocused]);
  end;

  procedure RemoveLeadingBlanks(var SL:TStringList);
  var i,Min,n : Integer;

   function CountBlanks(S:String):Integer;
   begin
    Result:=0;
    repeat
     inc(Result);
     until (Result>length(S)) or (S[Result]<>' ');
   end;

  begin
   Min:=maxInt;

   for i:=0 to SL.Count-1 do
    if SL[i]<>'' then begin
     n:=CountBlanks(SL[i]);
     if n<Min then Min:=n;
     end;

   for i:=0 to SL.Count-1 do
    if SL[i]<>'' then
     SL[i]:=copy(SL[i],Min,255)
  end;

 begin
  lbCanvas.Font.Name:='Courier New';
  lbCanvas.Font.Size:=8;

  if SelectedAndFocused then begin lbCanvas.Font.Color:=clHighLightText;
                                   BGNormal           :=clHighLight;
                                   BGBookmark         :=SetLightness(clHighLight,+30);
                                   end

                        else if (_Index=lbBookmarks.ItemIndex) and
                                             not(FHideLBFocus) then begin lbCanvas.Font.Color:=clBlack;
                                                                          BGNormal           :=$00D9D9D9;
                                                                          BGBookmark         :=$00CDCDCD;
                                                                          end

                                                               else begin lbCanvas.Font.Color:=clWindowText;
                                                                          BGNormal           :=lbBookmarks.Color;
                                                                          BGBookmark         :=FListboxColorBMline;
                                                                          end;
  LineTop:=_Rect.Top;
  GutterL:=FScaler.Calc(3);
  SL     :=TStringList.Create;

  try
   SL.Text:=BM.Text;
   if FRemoveLeadingBlanks then RemoveLeadingBlanks(SL);

   for i:=0 to SL.Count-1 do begin

    if (    FEnlargedLineDisplay  and (i=2)) or
       (not(FEnlargedLineDisplay) and (i=1)) then lbCanvas.Brush.Color:=BGBookmark
                                             else lbCanvas.Brush.Color:=BGNormal;

    lbCanvas.FillRect(Rect(_Rect.Left,        LineTop,_Rect.Right,LineTop+BookmarksHeightLine));
    MaxL:=_Rect.Right-_Rect.Left-GutterL;
    lbCanvas.TextOut (     _Rect.Left+GutterL,LineTop,Shorten(lbCanvas,MaxL,SL[i],#133));

    inc(LineTop,BookmarksHeightLine);
    end;

  finally
   SL.Free;
   end;

  if _Index<Listbox.Count-1 then begin lbCanvas.Pen.Color:=cl3DDkShadow;
                                       lbCanvas.MoveTo(_Rect.Left- FScaler.Calc(1),_Rect.Bottom-FScaler.Calc(1)); // ---
                                       lbCanvas.LineTo(_Rect.Right+FScaler.Calc(1),_Rect.Bottom-FScaler.Calc(1));
                                       end;
 end;

begin
 BM:=TBookmark(ListBox.Items.Objects[_Index]);

 if Assigned(BM) then begin

  lbCanvas    :=ListBox.Canvas;
  FuncProcName:=Trim(ListBox.Items[_Index]);

  PaintHeader(Rect(_Rect.Left,
                   _Rect.Top,
                   _Rect.Right,
                   _Rect.Top+FScaler.Calc(BookmarksHeightHeader)));

  PaintLines (Rect(_Rect.Left,
                   _Rect.Top+FScaler.Calc(BookmarksHeightHeader),
                   _Rect.Right,
                   _Rect.Bottom));
  end;

 if (odFocused in _State) then ListBox.Canvas.DrawFocusRect(_Rect);
end;

procedure TfmGxBookmarksForm.lbBookmarksMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
var Idx,H : Integer;
    R     : TRect;
begin
 if FHideClassName then begin

  Idx:=lbBookmarks.ItemAtPos(Point(x,y),true);
  H  :=BookmarksHeightHeader;
  R  :=lbBookmarks.ItemRect(Idx);

  if (Idx<0    ) or
     (y>R.Top+H) then Application.CancelHint
                 else if FLastHintIdx<>Idx then begin
                       Hint:=' '+Trim(lbBookmarks.Items[Idx])+' ';
                       Application.ActivateHint(Point(x,y));
                       FLastHintIdx:=Idx;
                       end;
  end;
end;

procedure TfmGxBookmarksForm.lbBookmarksClick(Sender:TObject);
begin
 lbBookmarks.Refresh;
 SetButtonState;
end;

procedure TfmGxBookmarksForm.lbBookmarksDblClick(Sender:TObject);
begin
 Jump2Bookmark;
end;

procedure TfmGxBookmarksForm.lbBookmarksKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if Shift=[]        then case Key of
                          vk_Space,
                          vk_Return : lbBookmarksDblClick(nil);
                          vk_Delete : tbDeleteClick(tbDelete);
                          vk_F1     : if not(FormIsDocked(Self)) then tbHelp.Click; // no help call via F1 in docked forms
                          end
                    else

 if Shift=[ssShift] then case Key of
                          vk_Delete : tbDeleteAllClick(tbDeleteAll);
                          end;
end;

// .............................................................................. Toolbar

procedure TfmGxBookmarksForm.tbAdd1Click(Sender:TObject);
var SourceEditor : IOTASourceEditor;
    EditView     : IOTAEditView;
    ModuleName   : String;
    LineNo       : Integer;
    BMIndex      : Integer;
begin
 if not(GetEditView(SourceEditor,EditView)) then exit;

 try
  ModuleName:=SourceEditor.Filename;
  LineNo    :=EditView.CursorPos.Line;
  BMindex   :=-1; // use next free bookmark

  if FUseOldEditForm then
   if not(TfoBookmarksEdit.Execute(Self,ModuleName,LineNo,BMIndex)) then exit;

  BookmarkSet(ModuleName,LineNo,BMIndex);

 finally
  ScanBookmarks;
  Jump2Bookmark;
  SetLBItemIndex(LineNo);
  SetButtonState;
  end;
end;

procedure TfmGxBookmarksForm.tbAdd2Click(Sender:TObject);
var SourceEditor  : IOTASourceEditor;
    EditView      : IOTAEditView;
    ModuleName    : String;
    LineNo        : Integer;
    Settings      : IExpertSettings; // for usage in TfoBookmarksSelect
begin
 if not(GetEditView(SourceEditor,EditView)) then exit;

 try
  ModuleName:=SourceEditor.Filename;
  Settings  :=TBookmarksExpert.GetSettings; // for usage in TfoBookmarksSelect and ..Options

  if not(TfoBookmarksSelect.Execute(Self,
                                    ModuleName,
                                    FBookmarks,
                                    LineNo)) then exit;

  BookmarkSet(ModuleName,LineNo,-1);

 finally
  ScanBookmarks;
  Jump2Bookmark;
  SetLBItemIndex(LineNo);
  SetButtonState;
  end;
end;

procedure TfmGxBookmarksForm.tbEditClick(Sender:TObject);
var BM     : TBookmark;
    Module : String;
    LineNo : Integer;
begin
 if not(FUseOldEditForm) or
    not(TListBox_GetSelectedObject(lbBookmarks,Pointer(BM))) then exit;

 try
  Module:=BM.Module;
  LineNo:=BM.Line;

  if not(TfoBookmarksEdit.Execute(Self,Module,LineNo)) then exit;

  if (Module=BM.Module) and
     (LineNo=BM.Line  ) then exit
                        else BookmarkSet(Module,LineNo,BM.Number);
 finally
  ScanBookmarks;
  Jump2Bookmark;
  SetLBItemIndex(LineNo);
  SetButtonState;
  end;
end;

procedure TfmGxBookmarksForm.tbSortClick(Sender:TObject);
begin
 if FSortMode=smIdx then                          FSortMode:=smName
                    else if FSortMode=smName then FSortMode:=smLine
                                             else FSortMode:=smIdx;
 SaveSettings;
 SBarShowSortInfo;
end;

procedure TfmGxBookmarksForm.tbDeleteClick(Sender:TObject);
var BM : TBookmark;
begin
 if TListBox_GetSelectedObject(lbBookmarks,Pointer(BM)) then begin

  BookmarkDel(BM.Module,BM.Number);

  SBarShowSortInfo;
  SetButtonState;
  end;
end;

procedure TfmGxBookmarksForm.tbDeleteAllClick(Sender:TObject);
var BM : TBookmark;
    i  : Integer;
begin
 tiUpdateBookmarks.Enabled:=false;

 if NewDialog(Self,mtConfirmation,'Confirm delete',
              #13+'Sure to delete all bookmarks?',
             NewDialogFont,
             ['Delete','Abort'])=1 then begin

  for i:=lbBookmarks.Count-1 downto 0 do begin
   BM:=TBookmark(lbBookmarks.Items.Objects[i]);
   BookmarkDel(BM.Module,BM.Number);
   end;

  ScanBookmarks; // and enable tiUpdateBookmarks

  SBarShowSortInfo;
  SetButtonState;
  end
                                   else tiUpdateBookmarks.Enabled:=true;
end;

procedure TfmGxBookmarksForm.tbOptionsClick(Sender:TObject);
begin
 tiUpdateBookmarks.Enabled:=false;
 
 if TfoBookmarksOptions.Execute(Self) then begin

  Application.CancelHint; // !!!
  LoadSettings;

  SetListboxItemHeight;
  tbEdit.     Visible:=FUseOldEditForm;
  lbBookmarks.Color  :=FListboxColorBackground;
  InvalidateRect(lbBookmarks.Handle,nil,true);

  ScanBookmarks; // and restart timer
  end;
end;

procedure TfmGxBookmarksForm.tbHelpMouseMove(Sender:TObject;Shift:TShiftState;X,Y:Integer);
begin
 inherited;
 if FormIsDocked(Self) then TToolbutton(Sender).Hint:='Help'
                       else TToolbutton(Sender).Hint:='Help (F1)'
end;

procedure TfmGxBookmarksForm.tbHelpClick(Sender:TObject);
begin
 GXContextHelp(Self,59);
end;

// ................................................................................. SBar

procedure TfmGxBookmarksForm.SBarResize(Sender:TObject);
begin
 SBar.Panels[0].Width:=SBar.ClientWidth-SBar.Panels[1].Width;
end;

procedure TfmGxBookmarksForm.SBarShowModulInfo(PathFName:String);
begin
 SBar.Panels[0].Text:=Shorten(SBar.Canvas,SBar.Panels[0].Width-20,ExtractFileName(PathFName),'...');
 SBar.Hint:=' '+PathFName+' ';
end;

procedure TfmGxBookmarksForm.SBarShowSortInfo;
begin
 if FSortMode=smName then                          SBar.Panels[1].Text:='Sort name'
                     else if FSortMode=smLine then SBar.Panels[1].Text:='Sort line'
                                              else SBar.Panels[1].Text:='Sort index'
end;

// ................................................................................. Form

procedure TfmGxBookmarksForm.FormResize(Sender:TObject);
begin
 inherited;
 lbBookmarks.Refresh;
end;

procedure TfmGxBookmarksForm.FormMouseWheel(Sender:TObject;Shift:TShiftState;WheelDelta:Integer;MousePos:TPoint;var Handled:Boolean);
var LB : TListbox;
begin
 Handled:=true;
 LB     :=lbBookmarks;

 if Shift=[ssShift] then if WheelDelta>0 then LB.Perform(WM_VScroll,SB_LINEUP,  0)
                                         else LB.Perform(WM_VScroll,SB_LINEDOWN,0)

                    else  if WheelDelta>0 then if LB.ItemIndex>0          then LB.ItemIndex:=LB.ItemIndex-1
                                                                          else
                                          else if LB.ItemIndex<LB.Count-1 then LB.ItemIndex:=LB.ItemIndex+1
                                                                          else;
 lbBookmarksClick(nil);
end;

constructor TfmGxBookmarksForm.Create(_Owner:TComponent);
begin
 inherited;

 InitDPIScaler;
 if Assigned(BookmarksExpert) then begin BookmarksExpert.SetFormIcon(Self);
                                         Settings:=TBookmarksExpert.GetSettings;
                                         end;
 
 // ensure visibility at 1st call before expert is integrated into IDE
 TControl_SetMinConstraints(Self);
 // ------------------------------------------------------------------

 LoadSettings;
 SetListboxItemHeight;

 paToolbar.            Color  :=Color;            // different colors are
 paListboxGutterRight. Color  :=Color;            // very comfortable
 paListboxGutterBottom.Color  :=Color;            // in designtime
 Toolbar.              Color  :=Color;            // ..
 tbEdit.               Visible:=FUseOldEditForm;
 lbBookmarks.          Color  :=FListboxColorBackground;

 FLastSortMode:=smLine;
 SBarShowSortInfo;
end;

destructor TfmGxBookmarksForm.Destroy;
begin
 SaveSettings;
 fmBookmarks:=nil;
 FreeAndNil(FBookmarks);

 inherited;
end;

// ................................................................................ Timer

procedure TfmGxBookmarksForm.tiUpdateBookmarksTimer(Sender:TObject);
begin
 ScanBookmarks;
end;

initialization // -----------------------------------------------------------------------

 RegisterGX_Expert(TBookmarksExpert);

end.
