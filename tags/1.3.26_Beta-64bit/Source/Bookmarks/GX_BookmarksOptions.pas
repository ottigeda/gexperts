unit GX_BookmarksOptions;

{$I GX_CondDefine.inc}

interface { --------------------------------------------------------------------------- }

uses Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,Dialogs,StdCtrls,ExtCtrls,

     GX_BaseForm;


type TPanel              = class(ExtCtrls.TPanel)      // Ferenc Kiffer, Hungary <kifferferenc@yahoo.com>
                           private                     // Simple painting without theeming, so we can set the background color
                            procedure CustomPaint;     // if Tag = 1 call CustomPaint, otherwise call inherited Paint
                           protected                   // see unit GX_GrepResultsOptions
                            procedure Paint; override;
                           end;

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

                            buHelp                               : TButton;
                            buSave                               : TButton;
                            buCancel                             : TButton;

                            ColorDialog                          : TColorDialog;

                            procedure paLBcolorBkDblClick        (Sender:TObject);
                            procedure paLBcolorBkMouseDown       (Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
                            procedure paLBcolorBMlineDblClick    (Sender:TObject);
                            procedure paLBcolorBMlineMouseDown   (Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
                            procedure paHeaderXXXDblClick        (Sender:TObject);
                            procedure paHeaderXXXColorMouseDown  (Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
                            procedure buHelpClick                (Sender:TObject);

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

// ............................................................. TPanel interceptor class

procedure TPanel.CustomPaint;
const Alignments : array[TAlignment] of LongWord = (DT_LEFT, DT_RIGHT, DT_CENTER);
var   Rect       : TRect;
      FontHeight : Integer;
      Flags      : LongWord;

 procedure DrawBevel(_Bevel:TPanelBevel;_Rect:TRect);
 var TopColor    : TColor;
     BottomColor : TColor;
  begin
   TopColor:=clBtnHighlight;
   if _Bevel=bvLowered then TopColor:=clBtnShadow;

   BottomColor:=clBtnShadow;
   if _Bevel=bvLowered then BottomColor:=clBtnHighlight;

   Frame3D(Canvas,_Rect,TopColor,BottomColor,BevelWidth);
  end;

begin
 Rect:=GetClientRect;
 if BevelOuter<>bvNone then DrawBevel(BevelOuter,Rect);
 Frame3D(Canvas, Rect, Color, Color, BorderWidth);
 if BevelInner<>bvNone then DrawBevel(BevelInner,Rect);

 Canvas.Brush.Color:=Color;
 Canvas.FillRect(Rect);

 FontHeight:=Canvas.TextHeight('W');
 {$IFDEF GX_IDE_IS_HIDPI_AWARE}
   FontHeight:=MulDiv(FontHeight, GetParentForm(Self).CurrentPPI,GetDesignDpi);
 {$ENDIF}

 Rect.Top   :=((Rect.Bottom+Rect.Top)-FontHeight) div 2;
 Rect.Bottom:=Rect.Top+FontHeight;

 Canvas.Brush.Style:=bsClear;
 Canvas.Font       :=Self.Font;

 Flags:=DT_EXPANDTABS or DT_VCENTER or Alignments[Self.Alignment];
 Flags:=DrawTextBiDiModeFlags(Flags);

 DrawText(Canvas.Handle,PChar(Self.Caption),-1,Rect,Flags);
end;

procedure TPanel.Paint;
begin
 if Tag=1 then CustomPaint
          else inherited
end;

// .................................................................. TfoBookmarksOptions
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

procedure TfoBookmarksOptions.buHelpClick(Sender:TObject);
begin
 inherited;
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
 if Key=vk_F1 then buHelpClick(nil);
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

 // §3 -cInfo : PanelColors > pay attention to this in the future!

 (* Already in Delphi 2010, coloring the panels no longer works.
    Since Delphi 2010 does not yet support "StyleElements"
    the TPanel interceptor class is used according to the template of GX_GrepResultsOptions

    {$IFDEF GX_DELPHIXE3_UP}
        paLBcolorBkg.   StyleElements:=[];
        paLBcolorBMline.StyleElements:=[];
        paHeaderBkColor.StyleElements:=[];
    {$ENDIF}
   *)

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
