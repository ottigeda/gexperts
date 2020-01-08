unit u_dzSpeedBitBtn;

{$INCLUDE 'dzlib.inc'}

interface

uses
  Windows,
  Classes,
  SysUtils,
  Types,
  Buttons,
  Graphics;

type
  ///<summary>
  /// A helper component that turns a TBitBtn into a button that works similar to a TSpeedButton
  /// but can receive the focus. It allows to either set a caption or a Glyph, but will ignore
  /// the Glyph if Caption is <> ''.
  /// Clicking the button will first set its Tag property to 0 (up) or down (1) and then call the
  /// original OnClick method.
  /// To use it create it with TdzSpeedBitBtn.Create(BitBtn) where BitBtn is an already existing
  /// TBitBtn component. TdzSpeedBitBtn will be automatically destroyed when the associated BitBtn
  /// is destroyed, so don't free it yourself.
  /// Hotkeys do not work, neither to Actions.
  /// Note that this has so far only been tested with Windows 8.1. I have no idea what it looks
  /// like on Windows XP, Vista, 7 or 10. </summary>
  TdzSpeedBitBtn = class(TComponent)
  private
    FCaption: string;
    FBtn: TBitBtn;
    FOrigBmp: TBitmap;
    FOrigOnClick: TNotifyEvent;
    FUpBmp: TBitmap;
    FDownBmp: TBitmap;
    FData: Pointer;
    procedure doOnClick(_Sender: TObject);
    procedure HandleOnClick(_Sender: TObject);
    function GetDown: Boolean;
    procedure SetDown(const Value: Boolean);
    procedure UpdateGlyph;
  public
    constructor Create(_btn: TComponent); override;
    destructor Destroy; override;
    property Down: Boolean read GetDown write SetDown;
    property BitBtn: TBitBtn read FBtn;
    property Data: Pointer read FData write FData;
  end;

type
  ///<summary>
  /// Emulates a TRadioGroup, but with BitBtns
  /// Add any number of BitBtns to this object (internally it will create TdzSpeedBitBtns for these)
  /// Note: The BitBtn's OnClick event will be used internally. </summary>
  TdzSpeedBitBtnGroup = class
  private
    FOnClick: TNotifyEvent;
    FList: TList;
    FAllowAllUp: Boolean;
    procedure HandleClick(_Sender: TObject);
    procedure doOnClick;
    function TryGetSelectedSb(out _Idx: Integer; out _sb: TdzSpeedBitBtn): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(_btn: TBitBtn; _Data: Pointer = nil): TdzSpeedBitBtn;
    ///<sumamry>
    /// Sets the given button's down state to False, if allowed
    /// @param Idx is the index of the button to change
    /// @returns True, if the button could be set to Down=False, which is only possible if
    ///                * AllowAllUp is true or
    ///                * There are only two buttons, in which case the other button was set to
    ///                  Down = True
    ///          False, otherwise </summary>
    function SetUp(_Idx: Integer): Boolean; overload;
    procedure SetDown(_Idx: Integer); overload;
    procedure SetDown(_btn: TBitBtn); overload;
    ///<summary>
    /// Note: This only works, if all Data values are different. Otherwise
    ///       all buttons matching Data will be set to down. </summary>
    procedure SetDown(_Data: Pointer); overload;
    function isDown(_Idx: Integer): Boolean; overload;
    function isDown(_btn: TBitBtn): Boolean; overload;
    ///<summary>
    /// Note: This only works, if all Data values are different. Otherwise
    ///       all buttons matching Data will be set to down. </summary>
    function isDown(_Data: Pointer): Boolean; overload;
    function TryGetSelected(out _Idx: Integer): Boolean; overload;
    function TryGetSelected(out _btn: TBitBtn): Boolean; overload;
    function TryGetSelected(out _Data: Pointer): Boolean; overload;
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

uses
  u_dzGraphicsUtils;

{ TdzSpeedBitBtn }

constructor TdzSpeedBitBtn.Create(_btn: TComponent);

  procedure PrepareBmp(_w, _h: Integer; _Color: TColor; _Edge: UINT; out _bmp: TBitmap);
  var
    cnv: TCanvas;

    procedure HandleBmpOnly;
    var
      x: Integer;
      y: Integer;
    begin
      x := FBtn.Margin;
      y := (_h - FOrigBmp.Height) div 2;
      if x = -1 then begin
        // center image the button
        x := (_w - FOrigBmp.Width) div 2;
      end else begin
        // left align image
      end;
      cnv.Draw(x, y, FOrigBmp);
    end;

    procedure HandleTextOnlySingleLine;
    var
      TextSize: TSize;
      x: Integer;
      y: Integer;
    begin
      TextSize := cnv.TextExtent(FCaption);
      x := FBtn.Margin;
      y := (_h - TextSize.cy) div 2;
      if x = -1 then begin
        // center
        x := (_w - TextSize.cx) div 2;
      end else begin
        // left align
      end;
      cnv.TextOut(x, y, FCaption);
    end;

    procedure HandleTextOnlyMultiLine;
    var
      qrc: TRect;
      TextWidth: Integer;
      TextHeight: Integer;
    begin
      if FBtn.Margin = -1 then begin
        // center
        qrc := Rect(0, 0, _w - 1, _h - 2);
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCalcRect, dtfCenter, dtfWordBreak]);
        TextWidth := qrc.Right - qrc.Left;
        TextHeight := qrc.Bottom - qrc.Top;
        qrc.Left := (_w - TextWidth) div 2;
        qrc.Top := (_h - TextHeight) div 2;
        qrc.Right := qrc.Left + TextWidth;
        qrc.Bottom := qrc.Top + TextHeight;
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCenter, dtfWordBreak]);
      end else begin
        // left align the centered text
        // Yes, that doesn't make much sense, but TBitBtn works that way.
        // Actually it's even worse: TBitBtn draws the text centered on the possible button width
        // and then moves it to the right which clips the text if it is too wide.
        // We don't make that mistake here but still center the text and then move it.
        qrc := Rect(0, 0, _w - 1 - FBtn.Margin, _h - 2);
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCalcRect, dtfCenter, dtfWordBreak]);
        TextWidth := qrc.Right - qrc.Left;
        TextHeight := qrc.Bottom - qrc.Top;
        qrc.Left := FBtn.Margin;
        qrc.Top := (_h - TextHeight) div 2;
        qrc.Right := qrc.Left + TextWidth;
        qrc.Bottom := qrc.Top + TextHeight;
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCenter, dtfWordBreak]);
      end;
    end;

    procedure HandleTextOnly;
    begin
  {$IFDEF HAS_BITBTN_WORDWRAP}
      if FBtn.WordWrap then begin
        HandleTextOnlyMultiLine;
      end else
  {$ENDIF}begin
        HandleTextOnlySingleLine;
      end;
    end;

    procedure HandleBmpAndSingleLineText;
    var
      TextSize: TSize;
      RequiredWidth: Integer;
      x: Integer;
    begin
      TextSize := cnv.TextExtent(FCaption);
      if FBtn.Margin = -1 then begin
        // center image and text on the button
        RequiredWidth := FOrigBmp.Width + FBtn.Spacing + TextSize.cx;
        x := (_w - RequiredWidth) div 2;
        cnv.Draw(x, (_h - FOrigBmp.Width) div 2, FOrigBmp);
        cnv.TextOut(x + FBtn.Margin + FBtn.Spacing + FOrigBmp.Width, (_h - TextSize.cy) div 2, FCaption);
      end else begin
        // left align image and text
        cnv.Draw(FBtn.Margin, (_h - FOrigBmp.Height) div 2, FOrigBmp);
        cnv.TextOut(FBtn.Margin + FBtn.Spacing + FOrigBmp.Width, (_h - TextSize.cy) div 2, FCaption);
      end;
    end;

    procedure HandleBmpAndMultilineText;
    var
      qrc: TRect;
      TextWidth: Integer;
      TextHeight: Integer;
      RequiredWidth: Integer;
      x: Integer;
    begin
      if FBtn.Margin = -1 then begin
        // center image and text on the button

        qrc := Rect(0, 0, _w - FOrigBmp.Width - 1 - FBtn.Spacing, _h - 2);
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCalcRect, dtfCenter, dtfWordBreak]);
        TextWidth := qrc.Right - qrc.Left;
        TextHeight := qrc.Bottom - qrc.Top;
        RequiredWidth := FOrigBmp.Width + FBtn.Spacing + TextWidth;
        x := (_w - RequiredWidth) div 2;
        cnv.Draw(x, (_h - FOrigBmp.Height) div 2, FOrigBmp);

        qrc.Left := x + FOrigBmp.Width + FBtn.Spacing;
        qrc.Top := (_h - TextHeight) div 2;
        qrc.Right := qrc.Left + TextWidth;
        qrc.Bottom := qrc.Top + TextHeight;
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCenter, dtfWordBreak]);
      end else begin
        // left align image and text

        qrc := Rect(0, 0, _w - FBtn.Margin - FOrigBmp.Width - 1 - FBtn.Spacing, _h - 2);
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCalcRect, dtfCenter, dtfWordBreak]);
        TextWidth := qrc.Right - qrc.Left;
        TextHeight := qrc.Bottom - qrc.Top;

        cnv.Draw(FBtn.Margin, (_h - FOrigBmp.Width) div 2, FOrigBmp);

        qrc.Left := FBtn.Margin + FOrigBmp.Width + FBtn.Spacing;
        qrc.Top := (_h - TextWidth) div 2;
        qrc.Right := qrc.Left + TextWidth;
        qrc.Bottom := qrc.Top + TextHeight;
        TCanvas_DrawText(cnv, FCaption, qrc, [dtfCenter, dtfWordBreak]);
      end;
    end;

    procedure HandleBmpAndText;
    begin
      // This is complicated. For now we will only support buttons with
      // Layout=blGlyphLeft
  {$IFDEF HAS_BITBTN_WORDWRAP}
      if FBtn.WordWrap then begin
        HandleBmpAndMultilineText;
      end else
  {$ENDIF}begin
        HandleBmpAndSingleLineText;
      end;
    end;

  var
    qrc: TRect;
  begin
    _bmp := TBitmap.Create;
    _bmp.Width := _w;
    _bmp.Height := _h;
    _bmp.TransparentColor := clFuchsia;

    cnv := _bmp.Canvas;

    cnv.Brush.Color := _Color;
    cnv.Brush.Style := bsSolid;
    cnv.FillRect(Rect(0, 0, _w, _h));
    cnv.Font := FBtn.Font;

    qrc := Rect(0, 0, _w - 1, _h - 2);
    DrawEdge(cnv.Handle, qrc, _Edge, BF_RECT);

    if FCaption <> '' then begin
      if (FOrigBmp.Width <> 0) and (FOrigBmp.Height <> 0) then begin
        HandleBmpAndText;
      end else begin
        // text only
        HandleTextOnly;
      end;
    end else begin
      HandleBmpOnly;
    end;
  end;

var
  w: Integer;
  h: Integer;
  ColBack1: TColor;
  ColBack2: TColor;
begin
  inherited Create(_btn);
  FBtn := _btn as TBitBtn;
  FOrigOnClick := FBtn.OnClick;
  FCaption := FBtn.Caption;

  FOrigBmp := TBitmap.Create;
  FOrigBmp.Assign(FBtn.Glyph);
  FOrigBmp.Transparent := True;

  FBtn.Caption := '';

  w := FBtn.Width - 1;
  h := FBtn.Height - 1;

  ColBack1 := rgb(240, 240, 240); // clBtnFace;
  ColBack2 := rgb(245, 245, 245); // a bit lighter than clBtnFace;

  PrepareBmp(w, h, ColBack1, EDGE_RAISED, FUpBmp);
  PrepareBmp(w, h, ColBack2, EDGE_SUNKEN, FDownBmp);

  FBtn.OnClick := HandleOnClick;

  FBtn.Margin := -1;
  FBtn.Spacing := 0;

  UpdateGlyph;
end;

destructor TdzSpeedBitBtn.Destroy;
begin
  // If we get here, either the constructor failed (which automatically calls the destructor)
  // or FBtn was already destroyed, so we must not access it at all.
  FUpBmp.Free;
  FDownBmp.Free;
  FOrigBmp.Free;
  inherited;
end;

procedure TdzSpeedBitBtn.doOnClick(_Sender: TObject);
begin
  if Assigned(FOrigOnClick) then
    FOrigOnClick(_Sender);
end;

procedure TdzSpeedBitBtn.HandleOnClick(_Sender: TObject);
begin
  Down := not Down;
  doOnClick(_Sender);
end;

function TdzSpeedBitBtn.GetDown: Boolean;
begin
  Result := (FBtn.Tag <> 0);
end;

procedure TdzSpeedBitBtn.SetDown(const Value: Boolean);
begin
  if Value then
    FBtn.Tag := 1
  else
    FBtn.Tag := 0;
  UpdateGlyph;
end;

procedure TdzSpeedBitBtn.UpdateGlyph;
begin
  if FBtn.Tag <> 0 then
    FBtn.Glyph := FDownBmp
  else
    FBtn.Glyph := FUpBmp;
end;

{ TdzSpeedBitBtnGroup }

constructor TdzSpeedBitBtnGroup.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TdzSpeedBitBtnGroup.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TdzSpeedBitBtnGroup.Add(_btn: TBitBtn; _Data: Pointer = nil): TdzSpeedBitBtn;
begin
  _btn.OnClick := Self.HandleClick;
  Result := TdzSpeedBitBtn.Create(_btn);
  Result.Data := _Data;
  FList.Add(Result);
end;

procedure TdzSpeedBitBtnGroup.doOnClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TdzSpeedBitBtnGroup.HandleClick(_Sender: TObject);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.BitBtn = _Sender then begin
      if FAllowAllUp then begin
        // the button has already been marked as down, so the logic here is inverted
        if sb.Down then
          SetDown(i)
        else
          SetUp(i);
      end else
        SetDown(i);
      break; //==>
    end;
  end;
  doOnClick;
end;

function TdzSpeedBitBtnGroup.isDown(_Idx: Integer): Boolean;
var
  sb: TdzSpeedBitBtn;
begin
  sb := TdzSpeedBitBtn(FList[_Idx]);
  Result := sb.Down;
end;

function TdzSpeedBitBtnGroup.isDown(_btn: TBitBtn): Boolean;
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.BitBtn = _btn then begin
      Result := sb.Down;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TdzSpeedBitBtnGroup.isDown(_Data: Pointer): Boolean;
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.Data = _Data then begin
      Result := sb.Down;
      Exit; //==>
    end;
  end;
  Result := False;
end;

procedure TdzSpeedBitBtnGroup.SetDown(_Idx: Integer);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if i = _Idx then
      sb.Down := True
    else
      sb.Down := False;
  end;
end;

procedure TdzSpeedBitBtnGroup.SetDown(_btn: TBitBtn);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.BitBtn = _btn then
      sb.Down := True
    else
      sb.Down := False;
  end;
end;

procedure TdzSpeedBitBtnGroup.SetDown(_Data: Pointer);
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  for i := 0 to FList.Count - 1 do begin
    sb := TdzSpeedBitBtn(FList[i]);
    if sb.Data = _Data then
      sb.Down := True
    else
      sb.Down := False;
  end;
end;

function TdzSpeedBitBtnGroup.SetUp(_Idx: Integer): Boolean;
var
  i: Integer;
  sb: TdzSpeedBitBtn;
begin
  if FAllowAllUp then begin
    for i := 0 to FList.Count - 1 do begin
      sb := TdzSpeedBitBtn(FList[i]);
      if i = _Idx then
        sb.Down := False;
    end;
    Result := True;
  end else begin
    if FList.Count = 2 then begin
      for i := 0 to FList.Count - 1 do begin
        sb := TdzSpeedBitBtn(FList[i]);
        if i = _Idx then
          sb.Down := False
        else
          sb.Down := True;
      end;
      Result := True;
    end else
      Result := False;
  end;
end;

function TdzSpeedBitBtnGroup.TryGetSelectedSb(out _Idx: Integer; out _sb: TdzSpeedBitBtn): Boolean;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do begin
    _sb := TdzSpeedBitBtn(FList[i]);
    if _sb.Down then begin
      Result := True;
      _Idx := i;
      Exit; //==>
    end;
  end;
  Result := False;
end;

function TdzSpeedBitBtnGroup.TryGetSelected(out _btn: TBitBtn): Boolean;
var
  Idx: Integer;
  sb: TdzSpeedBitBtn;
begin
  Result := TryGetSelectedSb(Idx, sb);
  if Result then
    _btn := sb.BitBtn;
end;

function TdzSpeedBitBtnGroup.TryGetSelected(out _Data: Pointer): Boolean;
var
  Idx: Integer;
  sb: TdzSpeedBitBtn;
begin
  Result := TryGetSelectedSb(Idx, sb);
  if Result then
    _Data := sb.Data;
end;

function TdzSpeedBitBtnGroup.TryGetSelected(out _Idx: Integer): Boolean;
var
  sb: TdzSpeedBitBtn;
begin
  Result := TryGetSelectedSb(_Idx, sb);
end;

end.
