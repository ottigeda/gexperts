unit GX_BookmarksEdit;

interface { --------------------------------------------------------------------------- }

uses Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,Dialogs,StdCtrls,ExtCtrls,

     GX_BaseForm;

type TfoBookmarksEdit = class(TfmBaseForm)

                         laModule                      : TLabel;
                         coModule                      : TComboBox;
                         laLine                        : TLabel;
                         edLine                        : TEdit;
                         laBookmarkIdx                 : TLabel;
                         coBookmarkIdx                 : TComboBox;
                         imF1                          : TImage;
                         imF1NA                        : TImage;
                         buOk                          : TButton;
                         buCancel                      : TButton;

                         procedure coModuleChange      (Sender:TObject);
                         procedure edLineChange        (Sender:TObject);
                         procedure coBookmarkIdxChange (Sender:TObject);
                         procedure imF1Click           (Sender:TObject);

                         procedure FormKeyDown         (Sender:TObject;var Key:Word;Shift:TShiftState);
                         procedure FormCreate          (Sender:TObject);
                         procedure FormShow            (Sender:TObject);
                         procedure FormClose           (Sender:TObject;var Action:TCloseAction);

                        private

                         procedure LoadSettings;
                         procedure SaveSettings;
                         procedure SetData             (const _Module:String;    _LineNo:Integer;    _BMIndex:Integer);
                         procedure GetData             (out   _Module:String;out _LineNo:Integer;out _BMIndex:Integer);
                         procedure CheckInput;

                        protected

                         procedure CreateWnd;                                      override;
                         procedure CreateParams        (var Params:TCreateParams); override;

                        public

                         class function Execute        (_Owner:TWinControl;  var _Module:String;var _LineNo:Integer;var _BmIndex:Integer):Boolean; overload;
                         class function Execute        (_Owner:TWinControl;const _Module:String;var _LineNo:Integer):Boolean;                      overload;

                         constructor    Create         (_Owner:TComponent); override;
                         destructor     Destroy;                            override;

                        end;

implementation { ---------------------------------------------------------------------- }

{$R *.dfm}

uses u_dzVclUtils,

     GX_ConfigurationInfo,
     GX_GenericUtils,
     GX_OtaUtils,

     GX_BookmarksConst;

// ................................................................................. Misc

procedure TfoBookmarksEdit.LoadSettings;
var SubSettings : IExpertSettings;
    L,T         : Integer;
begin
 SubSettings:=Settings.Subkey('Edit'); //  do not localize

 L:=SubSettings.ReadInteger('EditLeft',NA);
 T:=SubSettings.ReadInteger('EditTop', NA);

 if (L<>NA) and (T<>NA) then begin Left:=L;
                                   Top :=T;
                                   end;
end;

procedure TfoBookmarksEdit.SaveSettings;
var SubSettings : IExpertSettings;
begin
 SubSettings:=Settings.Subkey('Edit'); //  do not localize

 SubSettings.WriteInteger('EditLeft',Self.Left);
 SubSettings.WriteInteger('EditTop', Self.Top );
end;

procedure TfoBookmarksEdit.SetData(const _Module:String;_LineNo,_BMIndex:Integer);
begin
 TComboBox_Select(coModule,ExtractFileName(_Module));
 TComboBox_SelectByObject(coBookmarkIdx,_BmIndex);
 edLine.Text:=IntToStr(_LineNo);
end;

procedure TfoBookmarksEdit.GetData(out _Module:String;out _LineNo,_BmIndex:Integer);
var ui : TUnitInfo;
begin
 if not(TComboBox_GetSelectedObject(coModule,Pointer(ui))) then
  raise Exception.Create('You must select a module');

 _Module :=ui.Filename;
 _BmIndex:=TComboBox_GetSelectedObjectDef(coBookmarkIdx,-1);
 _LineNo :=StrToInt(edLine.Text);
end;

procedure TfoBookmarksEdit.CheckInput;
var Ok    : Boolean;
    Value : Integer;
    S     : String;
begin
 Ok:=TComboBox_GetSelected(coModule,S);
 Ok:=OK and TryStrToInt(edLine.Text,Value) and (Value>0); // coBookmarkIdx may be left empty
 buOk.Enabled:=Ok;
end;

// ............................................................................. Controls

procedure TfoBookmarksEdit.coModuleChange(Sender:TObject);
begin
 CheckInput;
end;

procedure TfoBookmarksEdit.edLineChange(Sender:TObject);
begin
 CheckInput;
end;

procedure TfoBookmarksEdit.coBookmarkIdxChange(Sender:TObject);
begin
 CheckInput;
end;

procedure TfoBookmarksEdit.imF1Click(Sender:TObject);
begin
 CallHtmlHelp(HLP_HelpFileName,HLP_BookmarksHelpEdit);
end;

// ................................................................................. Form

class function TfoBookmarksEdit.Execute(_Owner:TWinControl;var _Module:String;var _LineNo,_BMIndex:Integer):Boolean;
var fo : TfoBookmarksEdit;
begin
 fo:=TfoBookmarksEdit.Create(_Owner);
 try
  fo.Caption:='Add bookmark';
  fo.Left   :=Mouse.CursorPos.X+30;
  fo.Top    :=Mouse.CursorPos.y+60;

  fo.SetData(_Module,_LineNo,_BMIndex);
  Result:=(fo.ShowModal=mrok);
  if Result then fo.GetData(_Module,_LineNo,_BMIndex);

 finally
  FreeAndNil(fo);
  end;
end;

class function TfoBookmarksEdit.Execute(_Owner:TWinControl;const _Module:String;var _LineNo:Integer):Boolean;
var fo     : TfoBookmarksEdit;
    Dummy  : Integer;
    Module : String;
begin
 fo:=TfoBookmarksEdit.Create(_Owner);
 try
  fo.Caption:='Edit bookmark';
  fo.Left   :=Mouse.CursorPos.X+30;
  fo.Top    :=Mouse.CursorPos.y+60;

  fo.laBookmarkIdx.Visible:=false;
  fo.coBookmarkIdx.Visible:=false;

  fo.SetData(_Module,_LineNo,-1);
  Result:=(fo.ShowModal=mrok);
  if Result then fo.GetData(Module,_LineNo,Dummy);

 finally
  FreeAndNil(fo);
  end;
end;

// ......................................................................................

procedure TfoBookmarksEdit.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if Key=vk_F1 then imF1Click(nil);
end;

procedure TfoBookmarksEdit.FormCreate(Sender: TObject);
begin
 BorderIcons:=[biSystemMenu];
 BorderStyle:=bsSizeable;
 AutoScroll :=false;
end;

procedure TfoBookmarksEdit.CreateParams(var Params:TCreateParams);
begin
 inherited CreateParams(Params);
 Params.ExStyle:=Params.ExStyle or WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
end;

procedure TfoBookmarksEdit.CreateWnd;
begin
 inherited;
 SendMessage(Handle,WM_SETICON,1,0);
end;

procedure TfoBookmarksEdit.FormShow(Sender:TObject);
begin
 LoadSettings;
 Width :=Constraints.MinWidth;
 Height:=Constraints.MinHeight;
 EnsureFormVisible(Self);
 if not(FileExists(ConfigInfo.ConfigPath+HLP_HelpFileName)) then begin
  imF1.  Visible:=false;
  imF1NA.Left   :=imF1.Left;
  imF1NA.Top    :=imF1.Top;
  imF1NA.Visible:=true;
  end;
end;

procedure TfoBookmarksEdit.FormClose(Sender:TObject;var Action:TCloseAction);
begin
 SaveSettings
end;

constructor TfoBookmarksEdit.Create(_Owner:TComponent);
var i             : Integer;
    List          : TList;
    UI            : TUnitInfo;
    CurrentSource : String;
begin
 inherited;
 InitDPIScaler;

 List:=TList.Create;
 try
  GxOtaFillUnitInfoListForProject(List,GxOtaGetCurrentProject);
  for i:=0 to List.Count-1 do begin
   UI:=TObject(List[i]) as TUnitInfo;
   coModule.Items.AddObject(ExtractFileName(UI.Filename),UI);
   end;

 CurrentSource:=ExtractFileName(GxOtaGetCurrentSourceFile); // consider file not in project list
 if coModule.Items.IndexOf(CurrentSource)<0 then begin
  UI:=TUnitInfo.Create;
  UI.FormName  :=''; // not refused
  UI.SourceName:=''; // ..
  UI.FileName  :=GxOtaGetCurrentSourceFile;
  coModule.Items.InsertObject(0,CurrentSource,UI);
  end;

 finally
  FreeAndNil(List);
  end;

 // -1 would raise an exception in Delphi 6 when retrieving it again, but -2 is fine too
 TComboBox_AddIntObject(coBookmarkIdx,'Next',-2);              // 1st item = NEXT
 for i:=0 to 19 do begin                                       // add 20 numbers >> max Bookmarks = 20
  TComboBox_AddIntObject(coBookmarkIdx,IntToStr(i),i);
  end;
 coBookmarkIdx.ItemIndex:=0;
 
 InitDPIScaler;
end;

destructor TfoBookmarksEdit.Destroy;
begin
 TComboBox_ClearWithObjects(coModule);
 inherited;
end;

// ......................................................................................
end.
