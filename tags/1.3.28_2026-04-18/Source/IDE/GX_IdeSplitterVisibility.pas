unit GX_IdeSplitterVisibility;

interface

uses
  SysUtils,
  Classes,
  Types,
  GX_TimedCallback;

type
  TGxIdeSplitterVisibilityEnhancer = class
  private
    ///<summary>
    /// @returns True, if the editor Windows was found </summary>
    class function TrySetPaintHandlers(_Enabled: Boolean): Boolean;
    ///<summary>
    /// @NOTE: we do not create an instance of this class, so self is not initialized
    /// inside this method. </summary>
    procedure TimedCallback(_Sender: TObject);
  public
    class function GetEnabled: Boolean; // static;
    class procedure SetEnabled(const _Value: Boolean); //static;
  end;

implementation

uses
  ExtCtrls,
  Forms,
  GX_SplitterPainter;

{ TGxIdeSplitterVisibilityEnhancer }

var
  gblEnabled: Boolean = False;

class function TGxIdeSplitterVisibilityEnhancer.TrySetPaintHandlers(_Enabled: Boolean): Boolean;

  procedure HandleSplitter(_Splitter: TComponent);
  var
    cmp: TComponent;
  begin
    if Assigned(_Splitter) and (_Splitter is TSplitter) then begin
      cmp := _Splitter.FindComponent(_Splitter.Name + GXPainterSuffix);
      if not _Enabled and Assigned(cmp) then begin
        cmp.free;
      end else if _Enabled and not Assigned(cmp) then begin
        TSplitterPainter.Create(_Splitter);
      end;
    end;
  end;

  procedure HandleName(_cmp: TComponent; const _Name: string);
  var
    cmp: TComponent;
  begin
    cmp := _cmp.FindComponent(_Name);
    HandleSplitter(cmp);
  end;

var
  i: Integer;
  frm: TForm;
  cmp: TComponent;
begin
  Result := False;
  for i := 0 to Screen.FormCount - 1 do begin
    frm := Screen.Forms[i];
    if frm.ClassNameIs('TEditWindow') then begin
      Result := True;
      HandleName(frm, 'DockLeftSplitter');
      HandleName(frm, 'DockRightSplitter');
      HandleName(frm, 'DockBottomSplitter');
      HandleName(frm, 'DockBottomMiddleSplitter');
      HandleName(frm, 'EditorDockTopSplitter');
      cmp := frm.FindComponent('FileHistoryFrame');
      if Assigned(cmp) and cmp.ClassNameIs('TFileHistoryFrame') then begin
        HandleName(cmp, 'Splitter1');
        HandleName(cmp, 'Splitter2');
        HandleName(cmp, 'Splitter3');
        HandleName(cmp, 'Splitter4');
        HandleName(cmp, 'Splitter5');
      end;
    end else if frm.ClassNameIs('TProjectManagerForm') then begin
      HandleName(frm, 'Splitter1');
    end else if frm.ClassNameIs('TPropertyInspector') then begin
      HandleName(frm, 'DescriptSplitter');
      HandleName(frm, 'CommandSplitter');
    end;
  end;
end;

class function TGxIdeSplitterVisibilityEnhancer.GetEnabled: Boolean;
begin
  Result := gblEnabled;
end;

procedure TGxIdeSplitterVisibilityEnhancer.TimedCallback(_Sender: TObject);
begin
  // there is no intance for this call, so don't access Self here!
  if not TGxIdeSplitterVisibilityEnhancer.TrySetPaintHandlers(gblEnabled) then
    TTimedCallback.Create(TimedCallback, 500, True);
end;

class procedure TGxIdeSplitterVisibilityEnhancer.SetEnabled(const _Value: Boolean);
var
  dummy: TGxIdeSplitterVisibilityEnhancer;
begin
  if _Value and not gblEnabled then begin
    gblEnabled := True;
    if not TrySetPaintHandlers(_Value) then begin
      dummy := nil;
      TTimedCallback.Create(dummy.TimedCallback, 500, True);
    end;
  end else if not _Value and gblEnabled then begin
    TrySetPaintHandlers(_Value);
    gblEnabled := False;
  end;
end;

end.

