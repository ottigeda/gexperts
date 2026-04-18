unit GX_BookmarksSelect;

interface { --------------------------------------------------------------------------- }

uses Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,Dialogs,StdCtrls,StrUtils,
     ComCtrls,ToolWin,Math,ExtCtrls,ImgList,

     u_dzStringUtils, // for PosEx in Delphi 6

     GX_BaseForm,
     GX_BookmarksLists;

type tSort              = (soLine,soType,soName,soNoData);
     tTypeID            = (idConstr,idDestr,idFunc,idProc,idNone);

     tSelectRec         = class
                           Line   : Integer;
                           TypeID : tTypeID;
                           Name   : String;
                           Locked : Boolean;
                           end;

     TfoBookmarksSelect = class(TfmBaseForm)

                           laModule                       : TLabel;
                           paNoMatch                      : TPanel;
                           ToolBar                        : TToolBar;
                            tbSort                        : TToolButton;

                           lbFuncsProcs                   : TListBox;

                           buHelp                         : TButton;
                           buSelect                       : TButton;
                           buCancel                       : TButton;

                           SBar                           : TStatusBar;

                           ilToolbar                      : TImageList;

                           procedure lbFuncsProcsDrawItem (Control:TWinControl;Index:Integer;_Rect:TRect;State:TOwnerDrawState);
                           procedure lbFuncsProcsClick    (Sender:TObject);
                           procedure lbFuncsProcsDblClick (Sender:TObject);
                           procedure cbKeepMaskClick      (Sender:TObject);

                           procedure tbSortClick          (Sender:TObject);
                           procedure buHelpClick          (Sender:TObject);

                           procedure SBarResize           (Sender:TObject);

                           procedure FormKeyDown          (Sender:TObject;var Key:Word;Shift:TShiftState);
                           procedure FormMouseWheel       (Sender:TObject;Shift:TShiftState;WheelDelta:Integer;MousePos:TPoint;var Handled:Boolean);
                           procedure FormCreate           (Sender:TObject);
                           procedure FormResize           (Sender:TObject);
                           procedure FormShow             (Sender:TObject);
                           procedure FormClose            (Sender:TObject;var Action:TCloseAction);

                          private

                           FParentForm                    : TForm;
                           FBM                            : TBookmarkList;
                           FDataList                      : TList;
                           FFltrList                      : TList;
                           FSort                          : tSort;
                           FModulePathFName               : String;
                           FFilter                        : String;
                           FDenyDoubleBookmarks           : Boolean;
                           FFilterLock                    : Boolean;

                           procedure LoadSettings;
                           procedure SaveSettings;

                           procedure SetData              (const Module:String;const BMlist:TBookmarklist);
                           procedure GetData              (  out LineNo:Integer);

                           procedure ScanModule;
                           procedure SortList             (SortCol:tSort;RefreshLB:Boolean=true);
                           procedure SetButtonState;
                           procedure ShowMaskResetInfo;
                           procedure SetFilter;
                           procedure ResetFilter;

                           procedure SBarShowIndexInfo;
                           procedure SBarShowFilterInfo;
                           procedure SBarShowSortInfo;

                          protected

                           procedure CreateWnd;                                       override;
                           procedure CreateParams         (var Params:TCreateParams); override;

                          public

                           class function Execute         (Owner:TWinControl;const Module:String;
                                                                             const BMlist:TBookmarklist;
                                                                               var LineNo:Integer):Boolean;

                           constructor    Create          (Owner:TComponent); override;
                           destructor     Destroy;                            override;

                          end;

implementation { ---------------------------------------------------------------------- }

{$R *.dfm}

uses  u_dzVclUtils,

      GX_ConfigurationInfo,
      GX_GenericUtils,
      GX_OtaUtils,
      GX_GxUtils,
      
      GX_BookmarksConst;

// ................................................................................. Misc

procedure TfoBookmarksSelect.LoadSettings;
var SubSettings : IExpertSettings;
    L,T,W,H     : Integer;
begin
 SubSettings:=Settings.Subkey('Bookmarks'); // do not localize

 FDenyDoubleBookmarks:=SubSettings.ReadBool('BookmarksDenyDoubleBookmarks',DefDenyDoubleBookmarks);

 SubSettings:=Settings.Subkey('Select');

 L:=SubSettings.ReadInteger('SelectLeft',  NA);
 T:=SubSettings.ReadInteger('SelectTop',   NA);
 W:=SubSettings.ReadInteger('SelectWidth', NA);
 H:=SubSettings.ReadInteger('SelectHeight',NA);

 if (L<>NA) and (T<>NA) and
    (W<>NA) and (H<>NA) then begin Left  :=L;
                                   Top   :=T;
                                   Width :=W;
                                   Height:=H;
                                   end

                        else begin Left  :=Mouse.CursorPos.X;
                                   Top   :=Mouse.CursorPos.Y;
                                   Width :=Constraints.MinWidth;
                                   Height:=Constraints.MinHeight;
                                   end;

 FSort  :=tSort(SubSettings.ReadInteger('SelectLastSort',ord(soLine)));
 FFilter:=      SubSettings.ReadString ('SelectLastMask',''          );
end;

procedure TfoBookmarksSelect.SaveSettings;
var SubSettings : IExpertSettings;
begin
 SubSettings:=Settings.Subkey('Select'); //  do not localize

 SubSettings.WriteInteger('SelectLeft',    Left      );
 SubSettings.WriteInteger('SelectTop',     Top       );
 SubSettings.WriteInteger('SelectWidth',   Width     );
 SubSettings.WriteInteger('SelectHeight',  Height    );
 SubSettings.WriteInteger('SelectLastSort',ord(FSort));
 SubSettings.WriteString ('SelectLastMask',FFilter   );
end;

procedure TfoBookmarksSelect.SetData(const Module:String;const BMlist:TBookmarklist);
begin
 laModule.Caption:=Shorten(laModule.Canvas,laModule.Width,ExtractFileName(Module),'...');
 laModule.Hint   :=' '+Module+' ';
 FModulePathFName:=Module;
 FBM             :=BMlist;
end;

procedure TfoBookmarksSelect.GetData(out LineNo:Integer);
begin
 LineNo:=tSelectRec(FFltrList[lbFuncsProcs.ItemIndex]).Line
end;

procedure TfoBookmarksSelect.ScanModule;
var Text,T,
    s1,s2,s3,s4  : String;
    SL           : TStringList;
    NewRec       : tSelectRec;
    Idx,p,
    FuncsCount,
    ProcsCount,
    ConstrCount,
    DestrCount   : Integer;
    ID           : tTypeID;
    SkipComment1 : Boolean;
    SkipComment2 : Boolean;

 function NotInTextA(LineText:String;p:Integer):Boolean;
 var p1,p2 : Integer;
 begin
  p1:=pos  ('''',LineText    );
  p2:=posex('''',LineText,p+1);
  Result:=not((p1>0) and (p1<p) and (p2>0));
 end;

 function NotInTextB(LineText:String;p:Integer):Boolean;
 var ch1,ch2 : Char;
 begin
  if p-1>=1                then ch1:=LineText[p-1] else ch1:='A';
  if p+5< length(LineText) then ch2:=LineText[p+5] else ch2:='A';
  Result:=not(((p=1)                   or CharInSet(ch1,Chars)) or
              ((p+5>=length(LineText)) or CharInSet(ch2,Chars)));
 end;

 function FollowingText(LineText:String;p:Integer):Boolean;
 begin
  Result:=false;

  if p<length(LineText) then
   while (p<length(LineText)) and (LineText[p]=' ') do inc(p);

  if p<length(LineText) then Result:=CharInSet(LineText[p],Chars);
 end;

 function TrimLine(Line:String):String;
 var p : Integer;
 begin
  Result:=Line;

  p:=pos('(',Result);                          // trim trailing params
  if p>0 then Result:=copy(Result,1,p-1);

  p:=pos(':',Result);                          // trim trailing func result
  if p>0 then Result:=copy(Result,1,p-1);

  p:=pos(';',Result);                          // trim trailing ;
  if p>0 then Result:=copy(Result,1,p-1);

  p:=pos('function',AnsiLowerCase(Result));    // trim leading function
  if p>0 then Result:=copy(Result,p+9,255);

  p:=pos('procedure',AnsiLowerCase(Result));   // trim leading procedure
  if p>0 then Result:=copy(Result,p+10,255);

  p:=pos('constructor',AnsiLowerCase(Result)); // trim leading function
  if p>0 then Result:=copy(Result,p+11,255);

  p:=pos('destructor',AnsiLowerCase(Result));  // trim leading function
  if p>0 then Result:=copy(Result,p+10,255);

  Result:=Trim(Result)                         // remove leading + trailing blanks
 end;

 function BMAlreadySet(Line:Integer):Boolean;
 var i : Integer;
 begin
  Result:=false;
  if FDenyDoubleBookmarks then
   for i:=0 to FBM.Count-1 do
    if FBM[i].Line=Line then begin Result:=true;
                                   Break;
                                   end;
 end;

begin
 lbFuncsProcs.Items.BeginUpdate;
 lbFuncsProcs.Clear;
 SBarShowIndexInfo;

 SL:=TStringList.Create;
 try
  if Ota.GetActiveEditorTextAsString(Text,false) then begin

   SL.Text     :=Text;
   FuncsCount  :=0;
   ProcsCount  :=0;
   ConstrCount :=0;
   DestrCount  :=0;
   Idx         :=0;
   SkipComment1:=false;
   SkipComment2:=false;

   while (Idx<SL.Count-1                               ) and // skip interface section
         not(AnsiContainsText(SL[Idx],'implementation')) do inc(Idx);
   inc(idx);

   while Idx<SL.Count-1 do begin

    p:=pos('//',SL[Idx]);
    if p>0 then SL[Idx]:=copy(SL[Idx],1,p-1); // trim trailing comment

    p:=pos('{',SL[Idx]);
    if (p>0                  ) and
       NotInTextA(SL[Idx],p  ) then begin T:=copy(SL[Idx],1,p-1);                 // keep leading
                                         p:=pos('}',SL[Idx]);
                                         if p>0 then T:=T+copy(SL[Idx],p+1,255)   // keep leading+trailing
                                                else SkipComment1:=true;
                                         SL[Idx]:=T;
                                         end;
    p:=pos('(*',SL[Idx]);
    if (p>0                  ) and
       NotInTextA(SL[Idx],p+1) then begin T:=copy(SL[Idx],1,p-1);                 // keep leading
                                          p:=pos('*)',SL[Idx]);
                                          if p>0 then T:=T+copy(SL[Idx],p+2,255)  // keep leading+trailing
                                                 else SkipComment2:=true;
                                          SL[Idx]:=T;
                                          end;

    p:=pos('class',AnsiLowerCase(SL[Idx]));
    if (p>0                ) and
       NotInTextB(SL[Idx],p) then begin if posex(';',SL[Idx],p+5)>0 then inc(Idx) // single line class
                                                                    else while (Idx<SL.Count-1) and
                                                                               not(AnsiContainsText(SL[Idx],'end')) do inc(Idx);
                                        end;
    ID:=idNone;

    p:=pos('function',AnsiLowerCase(SL[Idx]));
    if (p>0                     ) and
       FollowingText(SL[Idx],p+8) then begin ID:=idFunc;
                                             inc(FuncsCount);
                                             end;
    if ID=idNone then begin
     p:=pos('procedure',AnsiLowerCase(SL[Idx]));
     if (p>0 ) and
        FollowingText(SL[Idx],p+9) then begin ID:=idProc;
                                              inc(ProcsCount);
                                              end;
     end;

    if ID=idNone then begin
     p:=pos('constructor',AnsiLowerCase(SL[Idx]));
     if (p>0 ) and
        FollowingText(SL[Idx],p+11) then begin ID:=idConstr;
                                               inc(ConstrCount);
                                               end;
     end;

    if ID=idNone then begin
     p:=pos('destructor',AnsiLowerCase(SL[Idx]));
     if (p>0 ) and
        FollowingText(SL[Idx],p+10) then begin ID:=idDestr;
                                               inc(DestrCount);
                                               end;
     end;

    if ID<>idNone then begin
     NewRec:=tSelectRec.Create;
     NewRec.Line  :=Idx+1;
     NewRec.TypeID:=ID;
     NewRec.Name  :=TrimLine(SL[Idx]);
     NewRec.Locked:=BMAlreadySet(Idx+1);

     lbFuncsProcs.Items.AddObject('..',NewRec);

     FFltrList.Add(FDataList[FDataList.Add(NewRec)]);
     end;

    if SkipComment1 then
     while (Idx<SL.Count-1) and SkipComment1 do begin
      inc(Idx);
      p:=pos('}',SL[Idx]);
      if p>0 then begin SL[Idx]:=copy(SL[Idx],p+1,255);
                        SkipComment1:=false
                        end
      end
                    else if SkipComment2 then
                          while (Idx<SL.Count-1) and SkipComment2 do begin
                           inc(Idx);
                           p:=pos('*)',SL[Idx]);
                           if p>0 then begin SL[Idx]:=copy(SL[Idx],p+2,255);
                                             SkipComment2:=false
                                             end
                           end
                                         else inc(Idx);
    end;

   s1:='s'; if  FuncsCount=1 then s1:='';
   s2:='s'; if  ProcsCount=1 then s2:='';
   s3:='s'; if ConstrCount=1 then s3:='';
   s4:='s'; if  DestrCount=1 then s4:='';

   SBar.Hint:=Format(' %.3d  function%s ',   [ FuncsCount,s1])+#13+
              Format(' %.3d  procedure%s ',  [ ProcsCount,s2])+#13+
              Format(' %.3d  constructor%s ',[ConstrCount,s3])+#13+
              Format(' %.3d  destructor%s ', [ DestrCount,s4]);

   SBarShowIndexInfo;
   end;

 finally
  SL.Free;
  end;

 lbFuncsProcs.Items.EndUpdate;

 tbSort.  Enabled:=(lbFuncsProcs.Count>1);
 buSelect.Enabled:=(lbFuncsProcs.Count>0);

 if buSelect.Enabled then begin lbFuncsProcs.ItemIndex:=0;
                                if FSort<>soLine then SortList(FSort,true);
                                end

                     else begin lbFuncsProcs.Enabled:=false;
                                FSort:=soNoData;
                                end;
 SBarShowIndexInfo;
 SBarShowSortInfo;
end;

 function CompareLine(Item1,Item2:Pointer):Integer;
 begin
  Result:=CompareValue(tSelectRec(Item1).Line,tSelectRec(Item2).Line);
 end;

 function CompareType(Item1,Item2:Pointer):Integer;
 begin
  Result:=CompareValue(ord(tSelectRec(Item1).TypeID),ord(tSelectRec(Item2).TypeID));
  if Result=0 then
   Result:=AnsiCompareStr(tSelectRec(Item1).Name,tSelectRec(Item2).Name);
   if Result=0 then
    Result:=CompareValue(tSelectRec(Item1).Line,tSelectRec(Item2).Line);
 end;

 function CompareName(Item1,Item2:Pointer):Integer;
 begin
  Result:=AnsiCompareStr(tSelectRec(Item1).Name,tSelectRec(Item2).Name);
   if Result=0 then
    Result:=CompareValue(tSelectRec(Item1).Line,tSelectRec(Item2).Line);
 end;

procedure TfoBookmarksSelect.SortList(SortCol:tSort;RefreshLB:Boolean=true);
begin
 case FSort of
  soLine : FFltrList.Sort(CompareLine);
  soType : FFltrList.Sort(CompareType);
  soName : FFltrList.Sort(CompareName);
  end;

 if RefreshLB then begin      // 03.02.2025
  lbFuncsProcs.ItemIndex:=0;  // ..
  lbFuncsProcs.Refresh;       // ..
  end;                        // ..
end;

procedure TfoBookmarksSelect.SetButtonState;
begin
 buSelect.Enabled:=not(tSelectRec(FFltrList[lbFuncsProcs.ItemIndex]).Locked);
end;

procedure TfoBookmarksSelect.ShowMaskResetInfo;
var i : Integer;
begin
 paNoMatch.Visible:=true;

           for i:=1 to 10 do begin
            if paNoMatch.Color=$00FF99FF then paNoMatch.Color:=clBtnFace
                                         else paNoMatch.Color:=$00FF99FF;
            Wait(200);
            end;

 paNoMatch.Visible:=false;
end;

procedure TfoBookmarksSelect.SetFilter;
var Count,i : Integer;

 function GetFltrList:Integer;
 var i : Integer;
 begin
  FFltrList.Clear;

  for i:=0 to FDataList.Count-1 do
   if AnsiContainsText(tSelectRec(FDataList[i]).Name,FFilter) then
    FFltrList.Add(FDataList[i]);

  Result:=FFltrList.Count;
 end;

begin
 FFilterLock:=true;

 Count:=GetFltrList;
 if (          Count=0) and
    (length(FFilter)>0) then begin SBarShowFilterInfo;
                                   ShowMaskResetInfo;
                                   delete(FFilter,length(FFilter),1);
                                   Count:=GetFltrList;
                                   end;
 if Count=0 then ResetFilter
            else SortList(FSort,false);

 lbFuncsProcs.Items.BeginUpdate;
 lbFuncsProcs.Clear;

 for i:=0 to FFltrList.Count-1 do
  lbFuncsProcs.Items.AddObject('..',FFltrList[i]);

 lbFuncsProcs.ItemIndex:=0;
 lbFuncsProcs.Items.EndUpdate;

 SBarShowFilterInfo;
 SBarShowIndexInfo;

 FFilterLock:=false;
end;

procedure TfoBookmarksSelect.ResetFilter;
begin
 FFltrList.Assign(FDataList,laCopy);
end;

// ............................................................................. Controls

procedure TfoBookmarksSelect.lbFuncsProcsDrawItem(Control:TWinControl;Index:Integer;_Rect:TRect;State:TOwnerDrawState);
var ListBox : TListBox absolute Control;
    L,T,N   : String;
    R       : TRect;
begin
 if odSelected in State then Listbox.Canvas.Brush.Color:=clHighLight
                        else Listbox.Canvas.Brush.Color:=ListBox.Color;
 Listbox.Canvas.FillRect(_Rect);

 L:=Format('%.5d',[tSelectRec(FFltrList[Index]).Line]);
 N:=               tSelectRec(FFltrList[Index]).Name;

 if tSelectRec(FFltrList[Index]).TypeID=idFunc   then T:='func'   else
 if tSelectRec(FFltrList[Index]).TypeID=idProc   then T:='proc'   else
 if tSelectRec(FFltrList[Index]).TypeID=idConstr then T:='constr' else T:='destr';

 if tSelectRec(FFltrList[Index]).Locked then begin Listbox.Canvas.Font.Style:=[fsItalic];
                                                   Listbox.Canvas.Font.Color:=clSilver;
                                                   end

                                        else begin Listbox.Canvas.Font.Style:=[];
                                                   if odSelected in State then Listbox.Canvas.Font.Color:=clHighlightText
                                                                          else Listbox.Canvas.Font.Color:=TListBox(Control).Font.Color;
                                                   end;

 Listbox.Canvas.TextOut(_Rect.Left+FScaler.Calc(2),_Rect.Top,L);
 Listbox.Canvas.TextOut(_Rect.Left+FScaler.Calc(86),_Rect.Top,Shorten(Listbox.Canvas,_Rect.Right-_Rect.Left-FScaler.Calc(82),N,#133));

 R:=Rect(_Rect.Left+FScaler.Calc(30),_Rect.Top,_Rect.Left+FScaler.Calc(30)+FScaler.Calc(64),_Rect.Bottom);
 DrawText(Listbox.Canvas.Handle,pchar(T),-1,R,DT_SINGLELINE or DT_CENTER or DT_VCENTER);

 if (odFocused in State) then Listbox.Canvas.DrawFocusRect(_Rect);
end;

procedure TfoBookmarksSelect.lbFuncsProcsClick(Sender:TObject);
begin
 SetButtonState;
 SBarShowIndexInfo;
end;

procedure TfoBookmarksSelect.lbFuncsProcsDblClick(Sender:TObject);
var BMcount : Integer;
begin
 if buSelect.Enabled then
  if CtrlDown then begin
   BMcount:=SendMessage(FParentForm.Handle,WM_AddBookmark,tSelectRec(FFltrList[lbFuncsProcs.ItemIndex]).Line,0);
   tSelectRec(FFltrList[lbFuncsProcs.ItemIndex]).Locked:=true;
   lbFuncsProcs.Refresh;
   buSelect.Enabled:=(BMcount<maxBookmarks);
   if BMcount=maxBookmarks then NewDialog(Self,mtInformation,
                                          'Information','Maximum number of bookmarks has been reached',
                                          'MS Sans Serif',
                                          ['Ok']);

   end
              else buSelect.Click
end;

procedure TfoBookmarksSelect.cbKeepMaskClick(Sender:TObject);
begin
 if Self.Visible then
  if lbFuncsProcs.CanFocus then
   lbFuncsProcs.SetFocus;
end;

procedure TfoBookmarksSelect.buHelpClick(Sender:TObject);
begin
 inherited;
 GXContextHelp(Self,61);
end;

// .................................................................... Toolbar + Buttons

procedure TfoBookmarksSelect.tbSortClick(Sender:TObject);
begin
 if FSort=soLine then                      FSort:=soType
                 else if FSort=soType then FSort:=soName
                                      else FSort:=soLine;
 SBarShowSortInfo;
 SortList(FSort,true);
end;

// ................................................................................. SBar

procedure TfoBookmarksSelect.SBarResize(Sender:TObject);
begin
 SBar.Panels[0].Width:=SBar.Clientwidth-SBar.Panels[1].Width-SBar.Panels[2].Width;
end;

procedure TfoBookmarksSelect.SBarShowIndexInfo;
var LBIdx,LBcount : Integer;
begin
 LBidx  :=lbFuncsProcs.ItemIndex;
 LBcount:=lbFuncsProcs.Count;

 if LBcount=0 then                    SBar.Panels[0].Text:=' '+       'No items found'
              else if FFilter='' then SBar.Panels[0].Text:=' '+Format('Item %1.0n of %1.0n',              [succ(LBidx)/1,LBcount/1]                  )
                                 else SBar.Panels[0].Text:=' '+Format('Item %1.0n of %1.0n (%1.0n items)',[succ(LBidx)/1,LBcount/1,FDataList.Count/1])
end;

procedure TfoBookmarksSelect.SBarShowFilterInfo;
begin
 SBar.Panels[1].Text:='>'+FFilter+'<';
end;

procedure TfoBookmarksSelect.SBarShowSortInfo;
begin
 case FSort of
  soLine : SBar.Panels[2].Text:=' Sort line';
  soType : SBar.Panels[2].Text:=' Sort type';
  soName : SBar.Panels[2].Text:=' Sort name';
      else SBar.Panels[2].Text:=' No data';
  end;
end;

// ................................................................................. Form

class function TfoBookmarksSelect.Execute(Owner:TWinControl;const Module:String;
                                                            const BMlist:TBookmarklist;
                                                            var LineNo:Integer):Boolean;
var fo : TfoBookmarksSelect;
begin
 fo:=TfoBookmarksSelect.Create(Owner);
 try
  fo.Left       :=Mouse.CursorPos.X+30;
  fo.Top        :=Mouse.CursorPos.y+60;
  fo.FParentForm:=TForm(Owner);

  fo.SetData(Module,BMlist);
  Result:=(fo.ShowModal=mrOk);
  if Result then fo.GetData(LineNo);

 finally
  FreeAndNil(fo);
  end;
end;

procedure TfoBookmarksSelect.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if Key=vk_F1 then buHelpClick(nil)
              else
  if not(FFilterLock) then begin
   if Key=vk_Delete then FFilter:=''
                    else if Key=vk_Back then delete(FFilter,length(FFilter),1)
                                        else if (length(FFilter)<MaxFilterLen) and
                                                CharInSet(chr(Key),Chars)      then FFilter:=FFilter+chr(Key)
                                                                               else exit;
   Key:=0;
   SetFilter;
   end;
end;

procedure TfoBookmarksSelect.FormMouseWheel(Sender:TObject;Shift:TShiftState;WheelDelta:Integer;MousePos:TPoint;var Handled: Boolean);
var LB : TListbox;
begin
 Handled:=true;
 LB     :=lbFuncsProcs;

 if Shift=[ssShift] then if WheelDelta>0 then LB.Perform(WM_VScroll,SB_LINEUP,  0)
                                         else LB.Perform(WM_VScroll,SB_LINEDOWN,0)

                    else  if WheelDelta>0 then if LB.ItemIndex>0          then LB.ItemIndex:=LB.ItemIndex-1
                                                                          else
                                          else if LB.ItemIndex<LB.Count-1 then LB.ItemIndex:=LB.ItemIndex+1
                                                                          else;
 lbFuncsProcsClick(nil);
end;

procedure TfoBookmarksSelect.FormCreate(Sender:TObject);
begin
 BorderIcons:=[biSystemMenu];
 BorderStyle:=bsSizeable;
 AutoScroll :=false;
end;

procedure TfoBookmarksSelect.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 Params.ExStyle:=Params.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TfoBookmarksSelect.CreateWnd;
begin
 inherited;
 SendMessage(Handle,WM_SETICON,1,0);
end;

procedure TfoBookmarksSelect.FormResize(Sender:TObject);
begin
 inherited;
 lbFuncsProcs.Refresh;
 SBarResize(nil);
end;

procedure TfoBookmarksSelect.FormShow(Sender:TObject);
begin
 LoadSettings;
 EnsureFormVisible(Self);

 ScanModule;
 if FFilter<>'' then SetFilter;

 //SBar.Font.Name:='MS Sans Serif'; // ?!!
 //SBar.Font.Size:=8;               // ?!!

 SBarShowFilterInfo;
end;

procedure TfoBookmarksSelect.FormClose(Sender:TObject;var Action:TCloseAction);
begin
 SaveSettings;
end;

// ......................................................................................

constructor TfoBookmarksSelect.Create(Owner:TComponent);
begin
 inherited;
 InitDPIScaler;

 FDataList:=TList.Create;
 FFltrList:=TList.Create;

 Toolbar.Color:=Color;
end;

destructor TfoBookmarksSelect.Destroy;
var i : Integer;
begin
 if Assigned(FDataList) then
  for i:=0 to FDataList.Count-1 do tSelectRec(FDataList[i]).Free;

 FreeAndNil(FDataList);
 FreeAndNil(FFltrList);

 inherited;
end;

// ......................................................................................
end.
