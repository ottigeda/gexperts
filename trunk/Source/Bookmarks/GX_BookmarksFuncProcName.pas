unit GX_BookmarksFuncProcName;

interface { --------------------------------------------------------------------------- }

uses SysUtils,Classes,StrUtils,

     ToolsAPI,
     GX_OtaUtils,

     u_dzStringUtils; // for PosEx in Delphi 6

procedure BuildLinkArray;
function  GetFuncProcName (LineNo:Integer):String; // LineNo 1..N

implementation { ---------------------------------------------------------------------- }

const NA                   ='Error';
      idNA                 = High(Word);

type  _Mem_Space           = Array[0..maxInt-1]of Byte; // from the good old times of
      _L_List              = ^_L_List_Node;             // "Chip special for Turbo Pascal"
      _L_List_Node         = record
                              Size  : Integer;
                              Info  : ^_Mem_Space;
                              Next  : _L_List end;
      tLiFo                = _L_List;


var   FSL                  : TStringList;
      FLiFo                : tLiFo;
      FLinkArray           : Array of Word;
      LineNoInterface,
      LineNoImplementation : Integer;

// ......................................................................................

procedure InitLiFo(var L:TLiFo);
begin
 L:=nil;
end;

procedure FreeLiFo(var L:TLiFo);
var p : TLiFo;
begin
 while L<>nil do begin
  p:=L;
  with p^ do begin
   FreeMem(Info,Size);
   L:=Next;
   end;
  FreeMem(p,SizeOf(p)); 
  end;
end;

function LiFoCount(L:TLiFo):Integer;
var p : TLiFo;
begin
 Result:=0;
 while L<>nil do begin
  p:=L;
  L:=p^.Next;
  inc(Result);
  end;
end;

procedure Push(var L:TLiFo;var Value;Len:Integer);
var p : TLiFo;
begin
 new(p);
 with p^ do begin
  Size:=Len;
  Next:=L;
  GetMem(Info,Size);
  Move(Value,Info^,Size);
  end;
 L:=p;
end;

procedure Pop(var L:TLiFo;var Value;Len:Integer);
var p : TLiFo;
begin
 p:=L;
 with p^ do
  if Size<>Len then raise EInOutError.Create('rzM_HeapLIFO, Pop-Error: Size<>Len')
               else begin Move(Info^,Value,Size);
                          FreeMem(Info,Size);
                          L:=Next;
                          FreeMem(p,SizeOf(p));
                          end;
end;

// ......................................................................................

function ParseLine(const LineText:String):String;
var L,p : Integer;

 function TrimText(Text:String;L:Integer):String;
 var p : Integer;
 begin
  Result:=Text;
  p:=pos(':',Result); if p>0 then delete(Result,p,L);
  p:=pos('(',Result); if p>0 then delete(Result,p,L);
  p:=pos(';',Result); if p>0 then delete(Result,p,L);
 end;

begin
 Result:=NA;
 L     :=length(LineText);

 if L>0 then begin
  p:=pos('function'#32,AnsiLowerCase(LineText));
  if p>0 then Result:=TrimText(copy(LineText,p+8,L),L)

         else begin p:=pos('procedure'#32,AnsiLowerCase(LineText));
                    if p>0 then Result:=TrimText(copy(LineText,p+9,L),L)

                           else begin p:=pos('constructor'#32,AnsiLowerCase(LineText));
                                      if p>0 then Result:=TrimText(copy(LineText,p+11,L),L)

                                             else begin p:=pos('destructor'#32,AnsiLowerCase(LineText));
                                                        if p>0 then Result:=TrimText(copy(LineText,p+10,L),L)
                                                        end;
                                      end;
                    end;
  end;
end;

procedure BuildLinkArray;
type  tBlockState = (bsTop,bsFuncProc,bsBot,bsNONE);
var   State       : tBlockState;
      LinesCount,
      StartIdx,
      EndIdx,i    : Integer;

 procedure GetActiveEditorLines(SL:TStringList);
 var EditReader : IOTAEditReader;
     EditorText : String;
 begin
  SL.Clear;
  EditReader:=GxOtaGetEditReaderForSourceEditor();
  if GxOtaGetActiveEditorTextAsString(EditorText,false) then SL.Text:=EditorText;
 end;

 procedure GetHeaderPositions(LinesCount:Integer);
 var LineNo   : Integer;
     LineText : String;
 begin
  LineNoInterface     :=0;
  LineNoImplementation:=0;
  LineNo              :=0;
  repeat
   inc(LineNo);
   LineText:=AnsiLowerCase(FSL[LineNo-1]);
   if pos('interface',     LineText)>0 then LineNoInterface     :=LineNo;
   if pos('implementation',LineText)>0 then LineNoImplementation:=LineNo;
   until (LineNo=LinesCount                              ) or
         ((LineNoInterface>0) and (LineNoImplementation>0));
 end;

 procedure GetBlock(StartIdx:Integer;var EndIdx:Integer;var State:tBlockState);
 var nBegin,nEnd,
     Idx,PopIdx,
     i,p          : Integer;
     LineText,
     Line2Check   : String;

  function CheckReservedWord(Line:String;RW:String):Integer;
  const Divider = [' ',';'];
  var   p,L     : Integer;

   function Literal(Line,RW:String;p,L:Integer):Boolean;
   var p1,p2 : Integer;
   begin
    Result:=false;
    p1:=pos(#39,Line);
    if p1<p-1 then begin p2:=posex(#39,Line,p1+1);
                         if p2>p+1+L then Result:=true
                         end;
   end;

  begin
   Result:=0;
   p     :=1;
   L     :=length(RW);
   repeat
    p:=posex(RW,Line,p);
    if (p>0) then begin if CharInSet(Line[p-1],Divider) and
                           CharInSet(Line[p+L],Divider) and
                           not(Literal(Line,RW,p,L))    then inc(Result);
                        inc(p,L);
                        end;
    until p=0;
  end;

  function BlankLine(Idx:Integer):Boolean;
  var TempLine : String;
      p1,p2    : Integer;
  begin
   TempLine:=FSL[Idx];
   p1:=pos('(*',TempLine);
   p2:=pos('*)',TempLine);

   if (p1>0) and (p2=0) then delete(TempLine,p1,255);
   if (p1=0) and (p2>0) then delete(TempLine,1,p2+2-p1);
   if (p1>0) and (p2>0) then delete(TempLine,p1,p2+2-p1);

   Result:=(Trim(TempLine)='');
  end;

 begin
  if StartIdx>=FSL.Count then exit; // twm

  LineText:=NA;
  Idx     :=StartIdx-1;
  nBegin  :=0;
  nEnd    :=0;

  repeat
   inc(Idx); // don't get text of 1st line !!!

   if Idx>StartIdx then LineText:=ParseLine(AnsiLowerCase(FSL[Idx]));

   Line2Check:=FSL[Idx];
   p:=pos('//',Line2Check);
   if p>0 then delete(Line2Check,p,255);

   Line2Check:=' '+AnsiLowerCase(Line2Check)+' ';
   Line2Check:=StringReplace(Line2Check,#9,#32,[rfReplaceAll]);

   inc(nBegin,CheckReservedWord(Line2Check,'begin'  ));
   inc(nBegin,CheckReservedWord(Line2Check,'case'   ));
   inc(nBegin,CheckReservedWord(Line2Check,'finally'));
   inc(nBegin,CheckReservedWord(Line2Check,'except' ));
   inc(nEnd,  CheckReservedWord(Line2Check,'end'    ));

   until (Idx>=LinesCount-2             )  or // eof
         (LineText<>NA                  )  or // next func proc found
         ((nBegin>0) and (nBegin-nEnd=0));    // end of func proc

  if LineText<>NA then                                          EndIdx:=Idx-1
                  else if ((nBegin>0) and (nBegin-nEnd=0)) then EndIdx:=Idx+1
                                                           else EndIdx:=Idx;

  if (nBegin>0) and (nEnd>0) then                                      State:=bsFuncProc
                             else if ParseLine(FSL[StartIdx])<>NA then State:=bsTop
                                                                  else State:=bsNONE;
                    if AnsiLowerCase(Trim(FSL[StartIdx]))='begin' then State:=bsBot;

  if State=bsTop then Push(FLifo,StartIdx,SizeOf(StartIdx));

  if (State=bsFuncProc) or
     (State=bsTop     ) then begin for i:=StartIdx to EndIdx-1 do FLinkArray[i]:=StartIdx;
                                   if not(BlankLine(EndIdx)) then FLinkArray[EndIdx]:=StartIdx;
                                   end;

  if State=bsBot        then begin if LiFoCount(FLiFo)>0 then Pop(FLifo,PopIdx,SizeOf(PopIdx))
                                                         else PopIdx:=idNA; 
                                   for i:=StartIdx to EndIdx-1 do FLinkArray[i]:=PopIdx;
                                   if not(BlankLine(EndIdx)) then FLinkArray[EndIdx]:=StartIdx;
                                   end;
 end;

begin                                      // processing ComCtrls (~23.000 Zeilen) takes about 62 mSec
 if FSL=nil then FSL:=TStringlist.Create;  // not fast, but fast enough ?!

 GetActiveEditorLines(FSL);
 LinesCount:=FSL.Count;
 SetLength(FLinkArray,LinesCount);

 if LinesCount>0 then begin

  if LiFoCount(FLiFo)>0 then FreeLiFo(FLiFo);
  InitLifo(FLiFo);

  SetLength(FLinkArray,LinesCount);
  for i:=Low(FLinkArray) to High(FLinkArray) do FLinkArray[i]:=0;

  GetHeaderPositions(LinesCount);

  //§2 -cError : results wrong names for INC-files
  //             INC-Files results LineNoImplementation = 0
  //                           and LinesCount           > 0

  StartIdx:=LineNoImplementation; // = 1st line after "implementation"
  repeat
   GetBlock(StartIdx,EndIdx,State);
   StartIdx:=EndIdx+1;
   until StartIdx>=LinesCount-1;
  end;
end;

function GetFuncProcName(LineNo:Integer):String;
var FSLidx : Integer;
begin
 if High(FLinkArray)<0 then
  raise Exception.Create('Error in >GetFuncProcName()<'+#13+'   LinkArray not initialized' +#13);

 FSLidx:=pred(LineNo);
 if FLinkArray[FSLidx]>0 then
 
  if FLinkArray[FSLidx]=idNA then Result:='No PopValue available' 
                             else Result:=Trim(ParseLine(FSL[FLinkArray[FSLidx]]))
 
                         else begin if LineNo<LineNoInterface      then Result:='before InterfaceSection' else
                                    if LineNo=LineNoInterface      then Result:='InterfaceHeader'         else
                                    if LineNo<LineNoImplementation then Result:='InterfaceSection'        else
                                    if LineNo=LineNoImplementation then Result:='ImplementationHeader'    else
                                    if LineNo>LineNoImplementation then Result:='ImplementationSection';
                                    end;
end;

// ......................................................................................
end.
