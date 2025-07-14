unit rzC_Splitter;

// ======================================================================================
//
// Name        : rzC_Splitter.pas
// Author      : Ralph Zornig
// Created     : unknown
// Last Change : 2025-07-14
//
// Free for personal and commercial use.
// No rights reserved.
//
// See rzC_Splitter.rtf for some explanations (german only)
// Use in GExperts as interposer class only
//
// ======================================================================================

interface { --------------------------------------------------------------------------- }

uses Windows,Messages,Classes,ExtCtrls,Graphics,Controls;

type TrzSplitter = class(TSplitter)       
                   private                
                 
                    FMaxSize                     : NaturalNumber;
                    FCenter                      : NaturalNumber;
                    FCenterThreshold             : Integer;
                    FPosition                    : NaturalNumber;
                    FDblCount                    : Integer;
                    FDblStart                    : DWord;
                    FDrawSymbol                  : Boolean;
                    FOnDblClick                  : TNotifyEvent;
                                                 
                    function  FindCtrl2Size      : TControl;
                    procedure SetMaxSize         (const Value:NaturalNumber);
                    procedure SetPosition        (const Value:NaturalNumber);
                    procedure SetCenterThreshold (const Value:Integer);
                    procedure SetHintText;                    

                   protected

                    procedure SetParent      (AParent:TWinControl);                               override;
                    procedure Paint;                                                              override;
                    procedure MouseDown      (Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
                    procedure MouseMove      (Shift:TShiftState;X,Y:Integer);                     override;
                    procedure MouseUp        (Button:TMouseButton;Shift:TShiftState;X,Y:Integer); override;
                    
                   public

                    constructor Create       (AOwner:TComponent);                                 override;

                   published

                    property MaxSize         : NaturalNumber read FMaxSize         write SetMaxSize;
                    property Position        : NaturalNumber read FPosition        write SetPosition;
                    property Center          : NaturalNumber read FCenter;
                    property CenterThreshold : Integer       read FCenterThreshold write SetCenterThreshold;
                    property DrawSymbol      : Boolean       read FDrawSymbol      write FDrawSymbol;
                    property ShowHint;
                    property OnDblClick      : TNotifyEvent  read FOnDblClick      write FOnDblClick;

                   end;

// procedure Register;

implementation { ---------------------------------------------------------------------- }

function TrzSplitter.FindCtrl2Size:TControl;
var P : TPoint;
    i : Integer;
    R : TRect;
begin
 Result:=nil;
 P:=Point(Left,Top);
 case Align of
  alLeft : dec(P.X);
  alTop  : dec(P.Y);
      else exit;
  end;

 for i:=0 to Parent.ControlCount-1 do begin
  Result:=Parent.Controls[i];
  if Result.Visible   and
     Result.Enabled   and
     not(Result=Self) then begin R:=Result.BoundsRect;
                                 if (R.Right-R.Left)=0 then
                                  if Align in [alTop,alLeft] then dec(R.Left)
                                                             else inc(R.Right);
                                 if (R.Bottom-R.Top)=0 then
                                  if Align in [alTop,alLeft] then dec(R.Top)
                                                             else inc(R.Bottom);

                                 if PtInRect(R,P) then exit;
                                 end;
  end;

 Result:=nil;
end;

procedure TrzSplitter.SetHintText;
begin
 if FMaxSize<=MinSize then                                             Hint:='Splitter movement not allowed,'+#13+
                                                                             'form must be enlarged ...'
                      else if (FMaxSize-MinSize<FCenterThreshold) or
                              (FCenterThreshold=0               ) or
                              (FCenterThreshold=maxInt          ) then Hint:='Toggle MinSize / MaxSize with DblClick'
                                                                  else Hint:='Toggle MinSize / Center / MaxSize with DblClick';
end;

procedure TrzSplitter.SetMaxSize(const Value:NaturalNumber);
begin
 FMaxSize:=Value;
 if FMaxSize<MinSize then FMaxSize:=MinSize;
 FCenter:=MinSize+(FMaxSize-MinSize) div 2;

 if FPosition<MinSize then                            SetPosition( MinSize)
                      else if FPosition>FMaxSize then SetPosition(FMaxSize);
 SetHintText;                     
end;

procedure TrzSplitter.SetPosition(const Value:NaturalNumber);
begin
 if FindCtrl2Size<>nil then
  if Value<>FPosition then begin

   if Value<MinSize then                        FPosition:= MinSize
                    else if Value>FMaxSize then FPosition:=FMaxSize
                                           else FPosition:=Value;

   if Align=alLeft then FindCtrl2Size.Width :=FPosition  // set width  of control left of splitter
                   else FindCtrl2Size.Height:=FPosition; // set height of control top  of splitter

   if Assigned(OnMoved) then OnMoved(Self);
   end;
end;

procedure TrzSplitter.SetCenterThreshold (const Value:Integer);
begin
 if Value<>FCenterThreshold then begin FCenterThreshold:=Value;
                                       SetHintText;
                                       end;
end;

procedure TrzSplitter.SetParent(AParent:TWinControl);
var i : Integer;
begin
 inherited;
 if AParent<>nil then
  for i:=0 to AParent.ControlCount-1 do
   if AParent.Controls[i] is TPanel then
    if TPanel(AParent.Controls[i]).Align=alTop then begin Align:=alTop;
                                                          Height:=10;
                                                          Break;
                                                          end
end;

procedure TrzSplitter.Paint;
var T,L,D : Integer;
begin
 inherited;
 if FDrawSymbol then
  if Align=alLeft then begin if Width>=8 then
                              with Canvas do begin          // prefer widths of 8, 10, ..
                               T:=((Height div 3) div 3)*3; // avoid fragmented dots (1 dot = 3 pixel)
                               D:=(Width-2) div 2;
                               if odd(Width) then inc(D);
                               Brush.Color:=Color;
                               Brush.Style:=bsSolid;
                               FillRect(Rect(0,0,Width,Height));
                               Pen.Style:=psDot;
                               Pen.Color:=clWhite;MoveTo(D,  T);LineTo(D,  T*2);
                               Pen.Color:=clGray; MoveTo(D+1,T);LineTo(D+1,T*2);
                               end
                             end

                  else begin if Height>=8 then
                              with Canvas do begin          // prefer heights of 8, 10, ..
                               L:=((Width div 3) div 3)*3;  // avoid fragmented dots (1 dot = 3 pixel)
                               D:=(Height-2) div 2;
                               if odd(Height) then inc(D);
                               Brush.Color:=Color;
                               Brush.Style:=bsSolid;
                               FillRect(Rect(0,0,Width,Height));
                               Pen.Style:=psDot;
                               Pen.Color:=clWhite;MoveTo(L,D  );LineTo(L*2,D  );
                               Pen.Color:=clGray; MoveTo(L,D+1);LineTo(L*2,D+1);
                               end;
                             end;
end;

procedure TrzSplitter.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var P        : Integer;
    MP       : TPoint;
    ClipR    : TRect;
    FControl : TControl;

 function ThresholdOK:Boolean;
 begin                                         // deactivate center jump
  Result:=not(FCenterThreshold=0) and          // with FCenterThreshold=0
         ((FMaxSize-MinSize)>FCenterThreshold) //   or FCenterThreshold=Number>(Max-Min)
 end;

 procedure CenterCursor;
 begin
  with Mouse do
   if Align=alLeft then CursorPos:=ClientToScreen(Point(Width div 2,     Top+Height div 2))
                   else CursorPos:=ClientToScreen(Point(Left+Width div 2,Height div 2    ))
 end;

begin
 AutoSnap:=false; // no AutoSnap !!!

 if Button=mbLeft then begin

  inc(FDblCount);                                           // we neither can use WinProc
  if (FDblCount=2)                                and       // nor DblClick-Event ?!
     (GetTickCount-FDblStart<=GetDoubleClickTime) then begin

   if Assigned(OnDblClick) then OnDblClick(Self) // app default OR user processed
                           else begin
    P:=FPosition;                                              // sm([P,MinSize,Center,MaxSize]);
    if (P=MinSize) then
     if ThresholdOK then                                         SetPosition(FCenter)
                    else                                         SetPosition(FMaxSize)
                   else if (P=FMaxSize) or
                           (P<FCenter)  then                     SetPosition(MinSize)
                                        else if (P=FCenter) or
                                                (P>FCenter) then SetPosition(FMaxSize);
    CenterCursor;
    end;
   exit;
   end
                                                  else begin
   FDblCount:=1;
   FDblStart:=GetTickCount;
   end;

  FControl:=FindCtrl2Size;
  if Assigned(FControl) then begin
   if Align=alLeft then begin // VerSplitter

    CenterCursor;
    MP:=Self.ScreenToClient(Mouse.CursorPos);

    ClipR.TopLeft    :=ClientToScreen(Point(     MinSize-Left+Width div 2, Top+Height div 2));
    ClipR.BottomRight:=ClientToScreen(Point(succ(MaxSize-Left+Width div 2),Top+Height div 2));
    ClipCursor(@ClipR);
    end
                   else begin // HorSplitter

    CenterCursor;
    MP:=Self.ScreenToClient(Mouse.CursorPos);

    ClipR.TopLeft    :=ClientToScreen(Point(Left+Width div 2,     MinSize-Top+Height div 2 ));
    ClipR.BottomRight:=ClientToScreen(Point(Left+Width div 2,succ(MaxSize-Top+Height div 2)));
    ClipCursor(@ClipR);
    end;

   inherited MouseDown(Button,Shift,MP.X,MP.Y);
   end;
  end
                  else inherited MouseDown(Button,Shift,X,Y);
end;

procedure TrzSplitter.MouseMove(Shift:TShiftState;X,Y:Integer);
begin
 inherited;
 if Align=alLeft then Position:=Left else Position:=Top;
end;

procedure TrzSplitter.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
 inherited;
 ClipCursor(nil);
 if Align=alLeft then FPosition:=Left else FPosition:=Top;
 if Assigned(OnMoved) then OnMoved(Self);
end;

constructor TrzSplitter.Create(AOwner:TComponent);
begin
 inherited;
  MinSize        :=100;
 FMaxSize        :=200;
 FCenter         :=MinSize+(FMaxSize-MinSize) div 2;
 FCenterThreshold:=100;
 FDrawSymbol     :=true;
 Align           :=alLeft;
 Width           :=10;     
 ShowHint        :=true;
end;

// ............................................................................. Register

// procedure Register;
// begin
//  RegisterComponents('rzLib',[TrzSplitter]);
// end;

// ......................................................................................
end.
