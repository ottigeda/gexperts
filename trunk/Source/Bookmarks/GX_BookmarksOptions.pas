unit GX_BookmarksOptions;

{$I GX_CondDefine.inc}

interface { --------------------------------------------------------------------------- }

uses Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,Dialogs,StdCtrls,ExtCtrls,

     GX_BaseForm;

type TfoBookmarksOptions = class(TfmBaseForm)

                            cbUseOldEditForm                     : TCheckBox;
                            cbEnlargedLineDisplay                : TCheckBox;
                            cbRemoveLeadingBlanks                : TCheckBox;
                            cbHideClassName                      : TCheckBox;
                            cbHideLBFocus                        : TCheckBox;
                            cbDenyDoubleBookmarks                : TCheckBox;

                            paColorBase                          : TPanel;
                             laListboxColors1                    : TLabel;
                             paLBcolorBkg                        : TPanel;
                             paLBcolorBMline                     : TPanel;
                             laListboxColors2                    : TLabel;
                             paHeaderBkColor                     : TPanel;
                              laHeaderFontColor                  : TLabel;
                            imF1                                 : TImage;
                            buSave                               : TButton;
                            buCancel                             : TButton;

                            ColorDialog                          : TColorDialog;

                            procedure paLBcolorBkDblClick        (Sender:TObject);
                            procedure paLBcolorBkMouseDown       (Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
                            procedure paLBcolorBMlineDblClick    (Sender:TObject);
                            procedure paLBcolorBMlineMouseDown   (Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
                            procedure paHeaderXXXDblClick        (Sender:TObject);
                            procedure paHeaderXXXColorMouseDown  (Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
                            procedure imF1Click                  (Sender:TObject);

                            procedure ColorDialogShow            (Sender:TObject);

                            procedure FormKeyDown                (Sender:TObject;var Key:Word;Shift:TShiftState);
                            procedure FormShow                   (Sender:TObject);

                           private

                            FUserColors                          : TStringList;

                            procedure LoadSettings;
                            procedure SaveSettings;
                            procedure LoadPositionAndUserColors;
                            procedure SavePositionAndUserColors;
                            procedure LoadCustomColors           (ColorDialog:TColorDialog);
                            procedure SaveCustomColors           (ColorDialog:TColorDialog);

                           public

                            class function Execute               (Owner:TWinControl):Boolean;
                            constructor    Create                (Owner:TComponent); override;
                            destructor     Destroy; override;

                           end;

implementation { ---------------------------------------------------------------------- }

{$R *.dfm}

uses GX_ConfigurationInfo,
     GX_GenericUtils,
     GX_GxUtils,

     GX_BookmarksConst;

const idColorDialogBk       = 1;
      idColorDialogSel      = 2;
      idColorDialogFnt      = 3;
      idColorDialogHeaderBk = 4;

// ................................................................................. Misc

procedure TfoBookmarksOptions.LoadSettings;
var SubSettings : IExpertSettings;
begin
 FUserColors.Clear;

 SubSettings:=Settings.Subkey('Bookmarks'); //  do not localize

 cbUseOldEditForm.      Checked:=SubSettings.ReadBool   ('BookmarksUseOldEditForm',         DefUseOldEditForm          );
 cbEnlargedLineDisplay. Checked:=SubSettings.ReadBool   ('BookmarksEnlargedLineDisplay',    DefEnlargedLineDisplay     );
 cbRemoveLeadingBlanks. Checked:=SubSettings.ReadBool   ('BookmarksRemoveLeadingBlanks',    DefRemoveLeadingBlanks     );
 cbHideClassName.       Checked:=SubSettings.ReadBool   ('BookmarksHideClassName',          DefHideClassName           );
 cbHideLBFocus.         Checked:=SubSettings.ReadBool   ('BookmarksHideLBFocus',            DefHideLBFocus             );
 cbDenyDoubleBookmarks. Checked:=SubSettings.ReadBool   ('BookmarksDenyDoubleBookmarks',    DefDenyDoubleBookmarks     );

 paLBcolorBkg.          Color  :=SubSettings.ReadInteger('BookmarksLBColorBackground',      DefLBColorBackground       );
 paLBcolorBMline.       Color  :=SubSettings.ReadInteger('BookmarksLBColorBMline',          DefLBColorBMline           );
 paHeaderBkColor.       Color  :=SubSettings.ReadInteger('BookmarksLBColorBkFuncProcName',  DefLBColorBkFuncProcName   );
 laHeaderFontColor.Font.Color  :=SubSettings.ReadInteger('BookmarksLBColorFontFuncProcName',DefLBColorFontFuncProcName );
end;

procedure TfoBookmarksOptions.SaveSettings;
var SubSettings : IExpertSettings;
begin
 SubSettings:=Settings.Subkey('Bookmarks'); //  do not localize

 SubSettings.WriteBool   ('BookmarksUseOldEditForm',         cbUseOldEditForm.     Checked);
 SubSettings.WriteBool   ('BookmarksEnlargedLineDisplay',    cbEnlargedLineDisplay.Checked);
 SubSettings.WriteBool   ('BookmarksRemoveLeadingBlanks',    cbRemoveLeadingBlanks.Checked);
 SubSettings.WriteBool   ('BookmarksHideClassName',          cbHideClassName.      Checked);
 SubSettings.WriteBool   ('BookmarksHideLBFocus',            cbHideLBFocus.        Checked);
 SubSettings.WriteBool   ('BookmarksDenyDoubleBookmarks',    cbDenyDoubleBookmarks.Checked);

 SubSettings.WriteInteger('BookmarksLBColorBackground',      paLBcolorBkg.          Color );
 SubSettings.WriteInteger('BookmarksLBColorBMline',          paLBcolorBMline.       Color );
 SubSettings.WriteInteger('BookmarksLBColorBkFuncProcName',  paHeaderBkColor.       Color );
 SubSettings.WriteInteger('BookmarksLBColorFontFuncProcName',laHeaderFontColor.Font.Color );
end;

procedure TfoBookmarksOptions.LoadPositionAndUserColors;
var SubSettings : IExpertSettings;
    L,T         : Integer;
begin
 SubSettings:=Settings.Subkey('Options'); //  do not localize

 L:=SubSettings.ReadInteger('OptionsLeft',NA         );
 T:=SubSettings.ReadInteger('OptionsTop', NA         );
    Subsettings.ReadStrings('UserColors', FUserColors);

 if (L<>NA) and (T<>NA) then begin Left:=L;
                                   Top :=T;
                                   end;
end;

procedure TfoBookmarksOptions.SavePositionAndUserColors;
var SubSettings : IExpertSettings;
begin
 SubSettings:=Settings.Subkey('Options'); //  do not localize

 SubSettings.WriteInteger('OptionsLeft',Self.Left  );
 SubSettings.WriteInteger('OptionsTop', Self.Top   );
 SubSettings.WriteStrings('UserColors', FUserColors);
end;

procedure TfoBookmarksOptions.ColorDialogShow(Sender:TObject);
var CD  : TColorDialog absolute Sender;
    X,Y : Integer;
begin
 X:=Mouse.CursorPos.X+20;
 Y:=Mouse.CursorPos.Y-80;

 if CD.Tag=idColorDialogBk  then ChangeColorDialogText(CD,X,Y,'Select background color of listbox'      ) else
 if CD.Tag=idColorDialogSel then ChangeColorDialogText(CD,X,Y,'Select background color of bookmark line') else
 if CD.Tag=idColorDialogFnt then ChangeColorDialogText(CD,X,Y,'Select font color of header text'        ) else
                                 ChangeColorDialogText(CD,X,Y,'Select background color of header'       )
end;

procedure TfoBookmarksOptions.LoadCustomColors(ColorDialog:TColorDialog);
var i : Integer;
begin
 ColorDialog.CustomColors.Clear;
 for i:=0 to FUserColors.Count-1 do ColorDialog.CustomColors.Add(FUserColors[i]);
end;

procedure TfoBookmarksOptions.SaveCustomColors(ColorDialog:TColorDialog);
var i : Integer;
begin
 FUserColors.Clear;
 for i:=0 to ColorDialog.CustomColors.Count-1 do FUserColors.Add(ColorDialog.CustomColors[i]);
end;

// ............................................................................. Controls

procedure TfoBookmarksOptions.paLBcolorBkDblClick(Sender:TObject);
begin
 ColorDialog.Color:=paLBcolorBkg.Color;
 ColorDialog.Tag  :=idColorDialogBk;

 LoadCustomColors(ColorDialog);
 if ColorDialog.Execute then paLBcolorBkg.Color:=ColorDialog.Color;
 SaveCustomColors(ColorDialog);
end;

procedure TfoBookmarksOptions.paLBcolorBkMouseDown(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if Button=mbRight then paLBcolorBkg.Color:=DefLBColorBackground;
end;

procedure TfoBookmarksOptions.paLBcolorBMlineDblClick(Sender:TObject);
begin
 ColorDialog.Color:=paLBcolorBMline.Color;
 ColorDialog.Tag  :=idColorDialogSel;

 LoadCustomColors(ColorDialog);
 if ColorDialog.Execute then paLBcolorBMline.Color:=ColorDialog.Color;
 SaveCustomColors(ColorDialog);
end;

procedure TfoBookmarksOptions.paLBcolorBMlineMouseDown(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if Button=mbRight then paLBcolorBMline.Color:=DefLBColorBMline;
end;

procedure TfoBookmarksOptions.paHeaderXXXDblClick(Sender:TObject);
begin
 if Sender=laHeaderFontColor then begin ColorDialog.Tag  :=idColorDialogFnt;
                                        ColorDialog.Color:=laHeaderFontColor.Font.Color;
                                        end

                             else begin ColorDialog.Tag  :=idColorDialogHeaderBk;
                                        ColorDialog.Color:=paHeaderBkColor.Color;
                                        end;
 LoadCustomColors(ColorDialog);
 if ColorDialog.Execute then
  if ColorDialog.Tag=idColorDialogFnt then laHeaderFontColor. Font.Color:=ColorDialog.Color
                                      else paHeaderBkColor.Color:=ColorDialog.Color;
 SaveCustomColors(ColorDialog);
end;

procedure TfoBookmarksOptions.paHeaderXXXColorMouseDown(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 if Button=mbRight then
  if Sender=laHeaderFontColor then laHeaderFontColor.Font.Color:=DefLBColorFontFuncProcName
                              else paHeaderBkColor.       Color:=DefLBColorBkFuncProcName;
end;

procedure TfoBookmarksOptions.imF1Click(Sender:TObject);
begin
 GXContextHelp(Self,62);
end;

// ................................................................................. Form

class function TfoBookmarksOptions.Execute(Owner:TWinControl):Boolean;
var fo : TfoBookmarksOptions;
begin
 fo:=TfoBookmarksOptions.Create(Owner);
 try
  fo.paLBcolorBkg.     Hint:='Select color with DblClick'+#13+'Restore default color with RightClick';
  fo.paLBcolorBMline.  Hint:=fo.paLBcolorBkg.Hint;
  fo.paHeaderBkColor.  Hint:=fo.paLBcolorBkg.Hint;
  fo.laHeaderFontColor.Hint:=fo.paLBcolorBkg.Hint;

  Result:=(fo.ShowModal=mrok);
  if Result then fo.SaveSettings;
  fo.SavePositionAndUserColors;

 finally
  FreeAndNil(fo);
  end;
end;

procedure TfoBookmarksOptions.FormKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if Key=vk_F1 then imF1Click(nil);
end;

procedure TfoBookmarksOptions.FormShow(Sender:TObject);
begin
 LoadSettings;
 LoadPositionAndUserColors;
 EnsureFormVisible(Self);
end;

// ......................................................................................

constructor TfoBookmarksOptions.Create(Owner:TComponent);
begin
 inherited;
 InitDPIScaler;

{$IFDEF GX_DELPHIXE3_UP}
 // §3 -cInfo : PanelColors > pay attention to this in the future!
 // Alternatively we could use the same TPanel interceptor class as the Grep Results Options dialog
 paLBcolorBkg.   StyleElements:=[];
 paLBcolorBMline.StyleElements:=[];
 paHeaderBkColor.StyleElements:=[];
{$ENDIF}

 paColorBase.Color:=Color;
 FUserColors:=TStringList.Create;
end;

destructor TfoBookmarksOptions.Destroy;
begin
 FUserColors.Free;
 inherited;
end;


// ......................................................................................
end.
