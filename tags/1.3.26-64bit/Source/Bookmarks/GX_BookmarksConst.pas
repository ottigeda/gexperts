unit GX_BookmarksConst;

interface { --------------------------------------------------------------------------- }

uses  Windows,Graphics,Forms,Types,Dialogs,Controls,Messages,StdCtrls,Math,

      GX_ConfigurationInfo;

const WM_AddBookmark             = WM_User+300;
      WM_Dock                    = WM_User+301;

      BookmarksHeightHeader      =   19;
      BookmarksHeightLine        =   16;

      Chars                      = ['0'..'9','A'..'Z','a'..'z'];

      DefEnlargedLineDisplay     = true;
      DefHideClassName           = false;
      DefHideLBFocus             = true;
      DefDenyDoubleBookmarks     = false;
      DefLBColorBackground       = $00FBEBE7;
      DefLBColorBMline           = $00F3CFC3;
      DefLBColorBkFuncProcName   = $00FFDFFF;
      DefLBColorFontFuncProcName = $003A903A;
      DefRemoveLeadingBlanks     = true;
      DefUseOldEditForm          = false;

      maxBookmarks               =   20; // 0..19
      maxFilterLen               =    8; // 1.. 8

      NA                         = maxInt;
      NewDialogFont              ='Segoe UI';

var   Settings                   : IExpertSettings;

function  CtrlDown               : Boolean;
function  Shorten                (Canvas:TCanvas;MaxL:Integer;Text,Trailer:String):String;
function  SetLightness           (Color:TColor;Percentage:ShortInt):TColor;
function  NewDialog              (foParent:TForm;aType:TMsgDlgType;const aCaption,aMessage,aFontName:String;buText:Array of String):Integer;
procedure Wait                   (mSek:Cardinal);
procedure ChangeColorDialogText  (Dialog:TColorDialog;X,Y:Integer;CaptionText:String);
function  FormIsDocked           (FO:TCustomForm):Boolean;

implementation { ---------------------------------------------------------------------- }

function CtrlDown:Boolean;
var State : TKeyboardState;
begin
 GetKeyboardState(State);
 Result:=((State[vk_Control] and 128)<>0);
end;

function Shorten(Canvas:TCanvas;MaxL:Integer;Text,Trailer:String):String;
var   l       : Integer;
begin
 Result:=Text;
 if Canvas.TextWidth(Text)>MaxL then begin
  for l:=length(Text) downto 1 do
   if Canvas.TextWidth(copy(Text,1,l)+Trailer)<MaxL then Break;
  Result:=copy(Text,1,l)+Trailer;
  end;
end;

function SetLightness(Color:TColor;Percentage:ShortInt):TColor;
var R,G,B : Byte;                  // -100..+100 (dunkler..heller)
begin                              // himitsu DP 25.11.2003
 Color:=ColorToRGB(Color);

 R:=(Color and $000000FF);
 G:=(Color and $0000FF00) shr  8;
 B:=(Color and $00FF0000) shr 16;

 if (Percentage<    0) and
    (Percentage>=-100) then begin R:=Trunc(R+(R*Percentage/100));
                                  G:=Trunc(G+(G*Percentage/100));
                                  B:=Trunc(B+(B*Percentage/100));
                                  end

                 else if (Percentage<=100) and
                         (Percentage>   0) then begin R:=Ceil(R+((255-R)*Percentage/100));
                                                      G:=Ceil(G+((255-G)*Percentage/100));
                                                      B:=Ceil(B+((255-B)*Percentage/100));
                                                      end;
 Result:=RGB(R,G,B);
end;

function NewDialog(foParent:TForm;aType:TMsgDlgType;const aCaption,aMessage,aFontName:String;buText:Array of String):Integer;
var aMsgDlg   : TForm;
    i,buIdx   : Integer;                // Code from JON in stackoverflow Sep 13, 2024
    dlgButton : TButton;                //  slightly modified by Ralph Zornig Jan 2025
    Buttons   : TMsgDlgButtons;      
begin
 Buttons:=[];
 for i:=0 to High(buText) do
  if i<=ord(High(TMsgDlgBtn)) then
   Buttons:=Buttons+[TMsgDlgBtn(i)];

 aMsgDlg:=CreateMessageDialog(aMessage,aType,Buttons);
 if aFontName<>'' then aMsgDlg.Font.Name:=aFontName;
 // aMsgDlg.Font.Size:=9; //§3 -cInfo : Setting aMsgDlg.Font.Size:=9 has no effect?!

 try
  aMsgDlg.Caption:=aCaption;

  if foParent<>nil then begin
   aMsgDlg.Left:=foParent.Left+(foParent.Width  div 2)-(aMsgDlg.Width  div 2);
   aMsgDlg.Top :=foParent.Top +(foParent.Height div 2)-(aMsgDlg.Height div 2);
   end;

  buIdx:=0;
  for i:=0 to aMsgDlg.ComponentCount-1 do
   if aMsgDlg.Components[i] is TButton then begin
    dlgButton            :=aMsgDlg.Components[i] as TButton;
    dlgButton.Caption    :=buText[buIdx];
    dlgButton.ModalResult:=buIdx+1;
    inc(buIdx);
    end;

  Result:=aMsgDlg.ShowModal;

 finally
  aMsgDlg.free;
  end;
end;

procedure Wait(mSek:Cardinal);
var Start : Cardinal;
begin
 Start:=Windows.GetTickCount;
 repeat
  Application.ProcessMessages;
  until Int64(Windows.GetTickCount)-Int64(Start)>=mSek
end;

procedure ChangeColorDialogText(Dialog:TColorDialog;X,Y:Integer;CaptionText:String);
const COLOR_ADD          = 712;
      COLOR_MIX          = 719;  // Delphi-PRAXiS / GeorgPeifer 16.Dezember 2004
      COLOR_HUEACCEL     = 723;  // Label "Benutzerdefinierte Farben" im Colordialog andern?
      COLOR_SATACCEL     = 724;  //
      COLOR_LUMACCEL     = 725;  // MUST be called in OnShow() of ColorDialog
      COLOR_REDACCEL     = 726;
      COLOR_GREENACCEL   = 727;
      COLOR_BLUEACCEL    = 728;
      COLOR_SOLID_LEFT   = 730;
      COLOR_SOLID_RIGHT  = 731;
      COLOR_BASIC_COLORS = 65535;

var   DialogHWND,
      ChildHWnd          : HWND;
      IsFullOpen         : Boolean;
begin
 SetWindowPos(Dialog.Handle,HWND_TOP,X,Y,0,0,SWP_NOSIZE);
 if CaptionText='' then CaptionText:='Select color';

 IsFullOpen:=(cdFullOpen in TColorDialog(Dialog).Options);
 DialogHWnd:=Dialog.Handle;

 ChildHWnd:=GetWindow(DialogHWnd,GW_CHILD);       // handle of 1st control
 if IsWindow(ChildHWnd) then begin
  ChildHWnd:=GetWindow(ChildHWnd,GW_HWNDNEXT);    // handle of 2nd control
  if IsWindow(ChildHWnd) then begin
   ChildHWnd:=GetWindow(ChildHWnd,GW_HWNDNEXT);   // handle of 3rd control = "Benutzerd.."
   if IsWindow(ChildHWnd) then
    SendMessage(ChildHWnd,WM_SETTEXT,0,Longint(PChar('Custom colors'))); // change text of "Benutzerd.."
   end;
  end;

 SetWindowText(           DialogHWnd,                    pChar(CaptionText           ));
 SetWindowText(GetDlgItem(DialogHWnd,idOk),              pChar('OK'                  ));
 SetWindowText(GetDlgItem(DialogHWnd,idCancel),          pChar('Cancel'              ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_BASIC_COLORS),pChar('Basic colors'        ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_ADD),         pChar('Add to custom colors'));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_HUEACCEL),    pChar('Hue'                 ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_SATACCEL),    pChar('Sat'                 ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_LUMACCEL),    pChar('Lum'                 ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_REDACCEL),    pChar('Red'                 ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_GREENACCEL),  pChar('Green'               ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_BLUEACCEL),   pChar('Blue'                ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_SOLID_LEFT),  pChar('Color |'             ));
 SetWindowText(GetDlgItem(DialogHWnd,COLOR_SOLID_RIGHT), pChar(' Solid'              ));

 if IsFullOpen then    ShowWindow(GetDlgItem(DialogHWnd,COLOR_MIX),SW_HIDE) // don't show deactivated button
               else SetWindowText(GetDlgItem(DialogHWnd,COLOR_MIX),pChar('Define custom colors >>'));
end;

function FormIsDocked(FO:TCustomForm):Boolean;
begin
 Result:=(FO<>nil) and (FO.Parent<>nil);
end;

// ......................................................................................
end.
