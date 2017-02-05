{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeClass_CodeToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2002-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JeClass_CodeToHtml;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JeExpert_Types;

type
  TJeCodeToHtml = class(TObject)
  private
    FColor: TJeCodeColor;
    function AttribToCss(Value: TJeFontAttributes): string;
    function Color16ToHtml(Value: TColor): string;
  protected
  public
    function CodeToHtml(Code: string): string;
    property ColorOptions: TJeCodeColor read FColor write FColor;
  end;

implementation

{*********************************************************************}

function TJeCodeToHtml.AttribToCss(Value: TJeFontAttributes): string;
begin
  result := 'color: ' + Color16ToHtml(Value.Color) + ';';
  if Value.BGColor <> -1 then
    result := result + ' BACKGROUND-COLOR: ' + Color16ToHtml(Value.BGColor) + ';';
  if (fsUnderline in Value.Style) then
    result := result + 'TEXT-DECORATION: underline;';
  if (fsBold in Value.Style) then
    result := result + 'FONT-WEIGHT: bold;';
  if (fsItalic in Value.Style) then
    result := result + 'FONT-STYLE: italic;';
end;
{**************************************************}

function TJeCodeToHtml.Color16ToHtml(Value: TColor): string;
begin
  case Value of
    0: result := 'Black';
    1: result := 'Maroon';
    2: result := 'Green';
    3: result := 'Olive';
    4: result := 'Navy';
    5: result := 'Purple';
    6: result := 'Teal';
    7: result := 'Silver';
    8: result := 'Gray';
    9: result := 'Red';
    10: result := 'Lime';
    11: result := 'Yellow';
    12: result := 'Blue';
    13: result := 'Fuchsia';
    14: result := 'Aqua';
    15: result := 'White';
  else
    result := 'white';
  end;
end;
{*********************************************************************}

function TJeCodeToHtml.CodeToHtml(Code: string): string;
const
  MaxWords = 104;
  ReservedWords: array[0..MaxWords - 1] of string = (
    'AND', 'ARRAY', 'AS', 'ASM', 'BEGIN', 'CASE', 'CLASS', 'CONST', 'CONSTRUCTOR', 'DESTRUCTOR', 'DISPINTERFACE', 'DIV',
      'DO',
    'DOWNTO', 'ELSE', 'END', 'EXCEPT', 'EXPORTS', 'FILE', 'FINALIZATION', 'FINALLY', 'FOR', 'FUNCTION', 'GOTO', 'IF',
      'IMPLEMENTATION',
    'IN', 'INHERITED', 'INITIALIZATION', 'INLINE', 'INTERFACE', 'IS', 'LABEL', 'LIBRARY', 'MOD', 'NIL', 'NOT', 'OBJECT',
      'OF',
    'OR', 'OUT', 'PACKED', 'PROCEDURE', 'PROGRAM', 'PROPERTY', 'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING', 'SET',
      'SHL', 'SHR',
    'STRING', 'THEN', 'THREADVAR', 'TO', 'TRY', 'TYPE', 'UNIT', 'UNTIL', 'USES', 'VAR', 'WHILE', 'WITH', 'XOR',
    'ABSOLUTE', 'ABSTRACT', 'ASSEMBLER', 'AUTOMATED', 'CDECL', 'CONTAINS', 'DEFAULT', 'DISPID',
    'DYNAMIC', 'EXPORT', 'EXTERNAL', 'FAR', 'FORWARD', 'IMPLEMENTS', 'INDEX', 'MESSAGE',
    'NAME', 'NEAR', 'NODEFAULT', 'OVERLOAD', 'OVERRIDE', 'PACKAGE', 'PASCAL', 'PRIVATE',
    'PROTECTED', 'PUBLIC', 'PUBLISHED', 'READ', 'READONLY', 'REGISTER', 'REINTRODUCE', 'REQUIRES',
    'RESIDENT', 'SAFECALL', 'STDCALL', 'STORED', 'VIRTUAL', 'WRITE', 'WRITEONLY');
type
  SpanStyle = (ssPlain, ssComment, ssReserved, ssIdentifier, ssSymbol, ssString,
    ssNumber, ssAssembler);
var
  i: integer;
//  StrToHtml: TJvStrToHtml;
  st: string;
  Txt: string;
  CurrentStyle: SpanStyle;
//  ProgressDlg: TJvProgressDlg;

  procedure SetStyle(Style: SpanStyle);
  begin
    if (CurrentStyle = Style) or (CurrentStyle = ssAssembler) then
      exit;
    if CurrentStyle <> ssPlain then
      Txt := Txt + '</SPAN>';
    case Style of
      ssComment: Txt := Txt + '<SPAN CLASS=Cmt>';
      ssReserved: Txt := Txt + '<SPAN CLASS=Res>';
      ssIdentifier: Txt := Txt + '<SPAN CLASS=Ident>';
      ssSymbol: Txt := Txt + '<SPAN CLASS=Symb>';
      ssString: Txt := Txt + '<SPAN CLASS=Str>';
      ssNumber: Txt := Txt + '<SPAN CLASS=Nmb>';
      ssAssembler: Txt := Txt + '<SPAN CLASS=Asm>';
    end;
    CurrentStyle := Style;
  end;

  procedure WriteWord(var st: string);
  var
    stu: string;
    j: integer;
  begin
    if st = '' then
      exit;
    stu := UpperCase(st);
    if (CurrentStyle = ssAssembler) and (stu = 'END') then
      CurrentStyle := ssPlain;

    j := 0;
    while (j < MaxWords) and (stu <> ReservedWords[j]) do
      inc(j);
    if j <> MaxWords then
      SetStyle(ssReserved)
    else if (StrToIntDef(st, -1) <> -1) or (StrToIntDef(st, 0) <> 0) then
      SetStyle(ssNumber)
    else
      SetStyle(ssIdentifier);

    Txt := Txt + st;

    st := '';
    if stu = 'ASM' then
      SetStyle(ssAssembler);

// todo: enable
//    ProgressDlg.Value := i;
    Application.ProcessMessages;
  end;

  procedure WriteSymbol(st: string);
  begin
    SetStyle(ssSymbol);
// todo: enable
//    Txt := Txt + StrToHtml.TextToHtml(st);
  end;

begin
// todo: enable
//  ProgressDlg := TJvProgressDlg.Create(nil);
//  ProgressDlg.Maximum := Length(Code);
//  ProgressDlg.Text := 'Converting...';
//  ProgressDlg.Value := 0;
//  ProgressDlg.AutoTimeLeft := true;
//  ProgressDlg.Show;

  with TStringList.Create do
  begin
    Add('<HTML>');
    Add('<HEAD>');
    Add(' <TITLE> JExperts - Code To Html Expert</TITLE>');
    Add(' <STYLE>');
    Add('  BODY{' + AttribToCss(FColor.Plain) + '}');
    Add('  .Cmt{' + AttribToCss(FColor.Comment) + '}');
    Add('  .Res{' + AttribToCss(FColor.Reserved) + '}');
    Add('  .Ident{' + AttribToCss(FColor.Identifier) + '}');
    Add('  .Symb{' + AttribToCss(FColor.Symbol) + '}');
    Add('  .Str{' + AttribToCss(FColor.String_) + '}');
    Add('  .Nmb{' + AttribToCss(FColor.Number) + '}');
    Add('  .Asm{' + AttribToCss(FColor.Assembler_) + '}');
    Add(' </STYLE>');
    Add('</HEAD>');
    Add('<BODY>');
    Add('<PRE>');
    Txt := Text;
    Free;
  end;

// todo: enable
//  StrToHtml := TJvStrToHtml.Create(nil);
  st := '';
  i := 1;
  CurrentStyle := ssPlain;
  while (i < Length(Code)) do
  begin
    case Code[i] of
      #10: WriteWord(st);
      #13:
        begin
          WriteWord(st);
          SetStyle(ssPlain);
          Txt := Txt + #13#10;
        end;
      '{':
        begin
          WriteWord(st);
          SetStyle(ssComment);
          Txt := Txt + '{';
          repeat
            inc(i);
// todo: enable
//            Txt := Txt + StrToHtml.CharToHtml(Code[i]);
          until (i >= Length(Code)) or (Code[i] = '}');
        end;
      '/': if Code[i + 1] = '/' then
        begin
          WriteWord(st);
          SetStyle(ssComment);
          while (i < Length(Code)) and not (Code[i] in [#10, #13]) do
          begin
// todo: enable
//            Txt := Txt + StrToHtml.CharToHtml(Code[i]);
            inc(i);
          end;
          dec(i);
        end
        else
          WriteSymbol(Code[i]);
      '-', '*', '+', ';', ')', '.', ',', '[', ']', '^', ':', '=', '<', '>':
        begin
          WriteWord(st);
          WriteSymbol(Code[i]);
        end;
      '''':
        begin
          WriteWord(st);
          SetStyle(ssString);
          Txt := Txt + '''';
          repeat
            inc(i);
// todo: enable
//            Txt := Txt + StrToHtml.CharToHtml(Code[i]);
          until (i >= Length(Code)) or (Code[i] = '''');
        end;
      '#':
        begin
          WriteWord(st);
          SetStyle(ssString);
          Txt := Txt + '#';
          inc(i);
          while (Code[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) do
          begin
            Txt := Txt + Code[i];
            inc(i);
          end;
        end;
      '(':
        begin
          WriteWord(st);
          if Code[i + 1] = '*' then
          begin
            while st <> '*)' do
            begin
              case Code[i] of
                '*': st := '*';
                ')': st := st + ')';
              else
                st := '';
              end;
              Txt := Txt + Code[i];
              inc(i);
            end;
          end
          else
            WriteSymbol(Code[i]);
        end;
      ' ':
        begin
          WriteWord(st);
          Txt := Txt + Code[i];
        end;
    else
      st := st + Code[i];
    end;
    inc(i);
  end;
// todo: enable
//  StrToHtml.Free;

  with TStringList.Create do
  begin
    Text := Txt;
    Add('</PRE>');
    Add('</BODY>');
    Add('</HTML>');
    result := Text;
    Free;
  end;
// todo: enable
//  ProgressDlg.Free;
end;
{*********************************************************************}

end.
