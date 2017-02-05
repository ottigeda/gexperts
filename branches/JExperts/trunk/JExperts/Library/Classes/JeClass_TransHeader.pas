{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeClass_TransHeader.PAS, released on 2001-02-28.

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

unit JeClass_TransHeader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, Activex, JvProgressDialog;

type
  TOnInfo = procedure(Sender: TObject; Text: string) of object;
  TJeHeaderTranslator = class
  private
    input: file of char;
    buffer: string;
    status: integer;
    output: TFileStream;
    gtype, galready, impl, impo, imple: TstringList;
    unitname: string;
    FOnInfo: TOnInfo;
    FOnInfoClear: TNotifyEvent;
    JvProgressDlg1: TJvProgressDialog;
    FOnStatus: TOnInfo;
    FPrepro: boolean;
    FStatusTag: integer;
    procedure StartTranslation;
    procedure PreProcessor;
    procedure WriteOut;
    procedure StartComment;
    procedure StartLineComment;
    procedure CheckStatus(Value: integer);
    procedure WriteOutS(Value: string);
    procedure TypeDef(recu: integer);
    function PascalType(Value: string): string;
    function ReadWord: string;
    procedure ReadRawCode(ts: TstringList);
    function GetPreProcessor: string;
    function GetTypeDef(recu: integer): TstringList;
    function CheckArray(var Value: string): string;
    function GetLineComment: string;
    function GetStartComment: string;
    procedure ReadClass;
    procedure Fonction;
    function Parameters: string;
    procedure CheckPointer(var variable: string; var letype: string);
    function Lisstring: string;
    procedure CheckDefault(var nom, letype: string);
    function retdef(Value: string): string;
    procedure AddGuid;
    function ReadUntilCh(Value: char): string;
    procedure AddInterface;
    procedure AddConst;
    procedure CorrectCRLF(FileName: string);
    procedure StatWork;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Translatefile(Src, Dest: string);
    property OnInfo: TOnInfo read FOnInfo write FOnInfo;
    property OnInfoClear: TNotifyEvent read FOnInfoClear write FOnInfoClear;
    property OnStatus: TOnInfo read FOnStatus write FOnStatus;
    property Prepro: boolean read FPrepro write FPrepro;
  end;

const
  S_None = 0;
  S_Type = 1;
  S_Var = 2;
  S_Const = 3;

implementation

{*****************************************************}

function ReplaceSub(sub, new, st: string): string;
var
  i, j, k: integer;
begin
  i := pos(sub, st);
  if (i <> 0) then
  begin
    k := 0;
    for j := i downto 1 do
      if st[j] = '''' then
        inc(k);
    if (k mod 2 = 0) then //simply verify that we aren't in a string !!!
      result := Copy(st, 1, i - 1) + new + Copy(st, length(sub) + i, length(st));
  end
  else
    result := st;
end;
{**************************************************************************}

function RemoveSpaces(Value: string): string;
var
  i: integer;
begin
  i := 1;
  while (i <= length(Value)) and (Value[i] in [' ', #9]) do
    inc(i);
  if Value <> '' then
    Value := Copy(Value, i, length(Value));
  i := length(Value);
  while (i > 0) and (Value[i] in [' ', #9]) do
    dec(i);
  if Value <> '' then
    Value := Copy(Value, 1, i);
  result := Value;
end;
{************************************************************}

procedure TJeHeaderTranslator.Translatefile(Src, Dest: string);
var
  st: string;
  i: integer;
  tmp: TstringStream;
begin
  if Assigned(FonInfoClear) then
    FOnInfoClear(self);
  if Assigned(FOnInfo) then
    FOnInfo(self, 'Translation project started.');
  if Assigned(FOnInfo) then
    FOnInfo(self, 'Source : ' + src);
  if Assigned(FOnInfo) then
    FOnInfo(self, 'Destination : ' + dest);
  if Assigned(FOnStatus) then
    FOnStatus(self, '');

  assignfile(input, src);
  output := TFileStream.Create(dest, fmCreate);
  reset(input);

  impl := TstringList.Create;
  impo := TstringList.Create;
  imple := TstringList.Create;
  gtype := TstringList.Create;
  galready := TstringList.Create;

  st := extractFileName(dest);
  i := length(st);
  while (i > 0) and (st[i] <> '.') do
    dec(i);
  st := Copy(st, 1, i - 1);
  unitname := st;

  //Show Progression
  self.JvProgressDlg1.InitValues(0, FileSize(input), 1, 0, 'progress','Phase 1 : Translating');
  self.JvProgressDlg1.Show;

  //start the translation
  StartTranslation;

  //close files/dialogs
  self.JvProgressDlg1.Hide;
  CloseFile(Input);

  tmp := TstringStream.Create('');
  buffer := 'Unit ' + st + ';' + #10#13#10#13 + 'interface' + #10#13#10#13 + 'Uses ' + #10#13 +
    '     Windows, Messages, SysUtils, Classes, Graphics, Controls, ' + #10#13 + '     Forms, Dialogs; ' + #10#13#10#13;
  tmp.WriteString(buffer);
  if gtype.count > 0 then
  begin
    buffer := 'Type' + #10#13;
    tmp.WriteString(buffer);
    for i := 0 to gtype.count - 1 do
    begin
      buffer := gtype[i] + #10#13;
      tmp.WriteString(buffer);
    end;
    buffer := #10#13;
    tmp.WriteString(buffer);
  end;
  output.position := 0;
  tmp.CopyFrom(output, output.Size);
  output.position := 0;
  tmp.Position := 0;
  output.CopyFrom(tmp, tmp.size);
  tmp.free;

  output.position := output.size;
  buffer := #10#13 + 'implementation' + #10#13 + #10#13;
  WriteOut;

  if impo.count > 0 then
  begin
    impo.Insert(0, '    ' + UnitName + 'DLL=''' + unitname + '.dll''; //The dll name may not be correct !');
    impo.Insert(0, '');
    impo.Insert(0, 'Const');
    impo.insert(0, '{               Imported functions start here                   }');
  end;
  for i := 0 to impo.count - 1 do
  begin
    buffer := impo[i] + #10#13;
    WriteOut;
  end;

  if impl.count > 0 then
  begin
    impl.insert(0, '{                   Macros start here                          }');
    if impo.count > 0 then
      impl.insert(0, '');
  end;
  for i := 0 to impl.count - 1 do
  begin
    buffer := impl[i] + #10#13;
    WriteOut;
  end;

  if imple.count > 0 then
  begin
    imple.insert(0, '{                   class functions start here                          }');
    if (impo.count > 0) or (impl.count > 0) then
      imple.insert(0, '');
  end;
  for i := 0 to imple.count - 1 do
  begin
    buffer := imple[i] + #10#13;
    WriteOut;
  end;

  buffer := #10#13#10#13 + 'end.';
  WriteOut;

  impl.free;
  impo.free;
  imple.free;
  gtype.free;
  galready.free;
  output.free;

  CorrectCRLF(Dest);
  if Assigned(FOnInfo) then
    FOnInfo(self, 'Translation project Ended.');
  if Assigned(FOnStatus) then
    FOnStatus(self, '');
end;
{************************************************************}

procedure TJeHeaderTranslator.WriteOut;
var
  i: integer;
  tmp: array[0..2000] of char;
begin
  //Write the buffers in the output file
  for i := 1 to length(buffer) do
    tmp[i - 1] := buffer[i];
  output.write(tmp, length(buffer));
end;
{************************************************************}

procedure TJeHeaderTranslator.WriteOutS(Value: string);
var
  i: integer;
  ch: char;
begin
  //Write a string in the output file
  for i := 1 to length(Value) do
  begin
    ch := Value[i];
    output.Write(ch, 1);
  end;
end;
{************************************************************}

procedure TJeHeaderTranslator.StartTranslation;
var
  ch: char;
begin
  {When we are in this procedure, we could find
        -a pre processor declaration
        -an imported function
        -a callback function
        -a class declaration
        -some comments
        -global variables
        -typedefs
  }

  buffer := '';
  while not (eof(input)) do
  begin
    self.JvProgressDlg1.Position := FilePos(input);
    StatWork;
    if eof(input) then
      break
    else
      read(input, ch);
    case ch of
      '#':
        begin
          buffer := '';
          PreProcessor;
        end;
      '/':
        begin
          //start of a comment
          if eof(input) then
            break
          else
            read(input, ch);
          case ch of
            '*': StartComment; //this is a big comment
            '/': StartLineComment; //this is a single line comment
          end;
        end;
      ' ', #9, #10, #13, '(':
        begin
          if UpperCase(buffer) = 'TYPEDEF' then
            TypeDef(5)
          else if UpperCase(buffer) = 'CLASS' then
          begin
            ReadClass;
            buffer := '';
          end
          else if (UpperCase(buffer) = 'ENUM') or (UpperCase(buffer) = 'UNION') or (UpperCase(buffer) = 'STRUCT') then
          begin
            seek(input, filepos(input) - (length(buffer) + 1));
            TypeDef(5);
          end
          else if UpperCase(buffer) = 'DEFINE_GUID' then
          begin
            AddGuid;
            buffer := '';
          end
          else if UpperCase(buffer) = 'DECLARE_INTERFACE_' then
          begin
            AddInterface;
            buffer := '';
          end
          else if UpperCase(buffer) = 'STATIC' then
          begin
            AddConst;
            buffer := '';
          end
          else if buffer <> '' then
          begin
            Fonction;
            buffer := '';
          end
          else if ch = #10 then
          begin
            output.write(ch, 1);
            ch := #13;
            output.write(ch, 1);
          end;
        end;
    else
      buffer := buffer + ch;
    end;
  end;
end;
{************************************************************}

function TJeHeaderTranslator.GetPreProcessor: string;
const
  P_NONE = 0;
  P_DEFINE = 1;
  P_IFDEF = 2;
  P_if = 3;
  P_ENDif = 4;
  P_else = 5;
  P_IFNDEF = 6;
var
  ch: char;
  lineToRead: integer;
  buf, buf2: string;
  typeof, i, j: integer;
  sts: array[0..20] of string;
  st, st2, st3, comment: string;
begin
  //read a preprocessor line (start with #)
  {When we are in this procedure, we could find
        - any pre processor command (ifdef,define,ifndef,...
        - include files
        - constant declarations
  }
  //Write the begining of a pre processor command
  Application.ProcessMessages;
  for i := 0 to 20 do
    sts[i] := '';

  typeof := 0;
  comment := '';

  lineToRead := 1;
  while LineToRead > 0 do
  begin
    repeat
      if eof(input) then
        break
      else
        read(input, ch);
      case ch of
        '(':
          begin
            if buf <> '' then
              sts[0] := buf;
            buf := '';
            ch := ' ';
            buf2 := '(';
            while ch <> ')' do
            begin
              if eof(input) then
                break
              else
                read(input, ch);
              case ch of
                '\': if eof(input) then
                    break
                  else
                    read(input, ch);
              else
                buf2 := buf2 + ch;
              end;
            end;
            i := 0;
            while (sts[i] <> '') and (i < 21) do
              inc(i);
            if i = 21 then
              sts[20] := sts[20] + buf2
            else
              sts[i] := buf2;
          end;
        '/':
          begin
            //start of a comment
            if eof(input) then
              break
            else
              read(input, ch);
            case ch of
              '*': Comment := GetStartComment; //this is a big comment
              '/': Comment := GetLineComment; //this is a single line comment
            end;
            ch := #10;
          end;
        '\':
          begin
            inc(LineToRead); //this tell me i have to continue on the next line
          end;
        ' ', #9, #10, #13:
          begin
            //analyse what is in the buffer
            buf2 := UpperCase(buf);
            if buf2 <> '' then
            begin
              if (typeof <> P_NONE) then
              begin
                i := 0;
                while (sts[i] <> '') and (i < 21) do
                  inc(i);
                if i = 21 then
                  sts[20] := sts[20] + buf
                else
                  sts[i] := buf;
              end
              else
              begin
                if buf2 = 'ENDIF' then
                begin
                  typeof := P_ENDIF;
                  buffer := '{$ENDIF}';
                end
                else if buf2 = 'IF' then
                begin
                  typeof := P_IF;
                  buffer := '{$if ';
                end
                else if buf2 = 'DEFINE' then
                begin
                  typeof := P_DEFINE;
                end
                else if buf2 = 'ELSE' then
                begin
                  typeof := P_ELSE;
                  buffer := '{$ELSE}';
                end
                else if buf2 = 'IFNDEF' then
                begin
                  typeof := P_IFNDEF;
                  buffer := '{$IFNDEF ';
                end
              end;
            end;
            buf := '';
          end;
        '"': buf := buf + Lisstring;
      else
        buf := buf + ch;
      end;
    until (ch = #10) or (ch = #13);
    dec(LineToRead);
    if (LinetoRead > 0) then
      if eof(input) then
        break
      else
        read(input, ch);
  end;

  case typeof of
    P_DEFINE:
      begin
        //Check if it isn't a macro

        st := '';
        for i := 0 to 20 do
          st := st + sts[i];

        j := 0;
        i := 0;
        while (i < length(st)) do
        begin
          if st[i] = '(' then
          begin
            while (i < length(st)) and (st[i] <> ')') do
              inc(i);
            inc(j);
          end
          else
            inc(i);
        end;

        if j > 1 then
        begin
          //This is a macro
          sts[1] := ReplaceSub('(', '', sts[1]);
          sts[1] := ReplaceSub(')', '', sts[1]);
          while (pos(',', sts[1]) <> 0) do
            sts[1] := ReplaceSub(',', ':integer' + #0, sts[1]);
          sts[1] := sts[1] + ':integer';

          for j := 1 to length(sts[1]) do
            if sts[1] = #0 then
              sts[1] := ',';

          impl.add('');
          impl.add('{****************  ' + sts[0] + '  ****************}');
          buffer := 'function ' + sts[0] + ' (' + sts[1] + '):integer;' + #10#13;
          WriteOut;

          st := '';
          for i := 2 to 20 do
            st := st + sts[i];

          while pos('<<', st) <> 0 do
            st := ReplaceSub('<<', ' shl ', st);
          while pos('>>', st) <> 0 do
            st := ReplaceSub('>>', ' shr ', st);
          while pos('|', st) <> 0 do
            st := ReplaceSub('|', ' or ', st);
          while pos('&', st) <> 0 do
            st := ReplaceSub('&', ' and ', st);
          while pos('%', st) <> 0 do
            st := ReplaceSub('%', ' mod ', st);
          while pos('0x', st) <> 0 do
            st := ReplaceSub('0x', '$', st);
          while pos('0X', st) <> 0 do
            st := ReplaceSub('0X', '$', st);
          i := 0;
          for j := 1 to length(st) do
            if st[j] = ':' then
              inc(i);
          for j := 1 to i - 1 do
            st := ReplaceSub(':', #10#13 + '     else' + #10#13 + '        ', st);
          st := ReplaceSub(':', #10#13 + '     else' + #10#13 + '        result/=', st);
          st := ReplaceSub('result/=', 'result:=', st);

          st[1] := ' ';
          st[length(st)] := ' ';
          st := RemoveSpaces(st);
          while pos('?', st) <> 0 do
          begin
            i := pos('?', st);
            st2 := Copy(st, i + 1, length(st));
            st := Copy(st, 1, i - 1);
            i := length(st);
            j := 1;
            while (i > 1) and (j > 0) do
            begin
              if st[i - 1] = ')' then
                inc(j)
              else if st[i - 1] = '(' then
                dec(j);
              dec(i);
            end;
            if i > 0 then
            begin
              st3 := Copy(st, 1, i - 1);
              st := Copy(st, i, length(st));
              if (pos('>', st) = 0) and (pos('<', st) = 0) and (pos('=', st) = 0) then
                st := st + '<>0';
              st2 := st3 + '     if ' + st + ' then ' + #10#13 + '        result:=' + st2;
              st := st2;
            end
            else
              st := '';
          end;

          //add it in the implementation
          for i := 1 to length(buffer) do
            if buffer[i] in [#10, #13] then
              buffer[i] := ' ';
          impl.add(buffer);
          impl.add('begin');
          if pos('result:=', st) = 0 then
            impl.add('     result:=' + st + ';')
          else
            impl.add(st + ';');
          impl.add('end;');

          buffer := '';
        end
        else
        begin
          if sts[1] = '' then
            buffer := '{$DEFINE ' + sts[0] + '}' //This is just a classic define
          else
          begin
            //define a constant
            CheckStatus(S_CONST);

            sts[1] := ReplaceSub('0x', '$', sts[1]);
            sts[1] := ReplaceSub('0X', '$', sts[1]);
            for i := 0 to length(sts[1]) do
              if sts[1][i] = '"' then
                sts[1][i] := '''';

            if pos('(', sts[1]) = 0 then
            begin
              if pos('L', sts[1]) = length(sts[1]) then
              begin
                sts[1] := ReplaceSub('L', '', sts[1]);
                sts[1] := 'longint(' + sts[1] + ')';
              end
              else if pos('U', sts[1]) = length(sts[1]) then
              begin
                sts[1] := ReplaceSub('U', '', sts[1]);
                sts[1] := 'integer(' + sts[1] + ')';
              end;
            end;
            buffer := '    ' + sts[0];
            i := length(buffer);
            while (i < 40) do
            begin
              inc(i);
              buffer := buffer + ' ';
            end;
            buffer := buffer + ' = ' + sts[1] + ';';
          end;
        end;
      end;
    P_IFNDEF: buffer := buffer + sts[0] + '}';
    P_if:
      begin
        i := 0;
        buf := '';
        while (sts[i] <> '') do
        begin
          buf := buf + sts[i];
          inc(i);
        end;
        buf := ReplaceSub('0x', '$', buf);
        buf := ReplaceSub('0X', '$', buf);
        buf := ReplaceSub('&&', 'and', buf);
        buf := ReplaceSub('||', 'or', buf);
        buf := ReplaceSub('^', 'xor', buf);
        buf := ReplaceSub('!', 'not ', buf);
        buffer := buffer + buf + '}';
      end;
  end;
  if prepro then
    if Assigned(FOnInfo) then
      FOnInfo(self, 'Added preprocessor command');

  if (typeof <> P_DEFINE) and (prepro = false) then
    buffer := '';
  result := buffer + ' ' + comment;
  buffer := '';
end;
{************************************************************}

procedure TJeHeaderTranslator.PreProcessor;
begin
  Buffer := GetPreProcessor;
  if pos('{$', buffer) <> 0 then
    Buffer := #10#13 + Buffer;
  WriteOut;
  Buffer := '';
end;
{************************************************************}

function TJeHeaderTranslator.GetStartComment: string;
var
  ch: char;
  incomment: boolean;
begin
  result := '(*';
  incomment := true;
  while incomment do
  begin
    if eof(input) then
      break
    else
      read(input, ch);
    if ch = '*' then
    begin
      if eof(input) then
        break
      else
        read(input, ch);
      if ch = '/' then
      begin
        result := result + '*)';
        incomment := false;
      end
      else
        result := result + '*' + ch;
    end
    else
      result := result + ch;
  end;
end;
{************************************************************}

procedure TJeHeaderTranslator.StartComment;
begin
  //Read comments until */ has been found
  buffer := GetStartComment;
  writeout;
  buffer := '';
end;
{************************************************************}

function TJeHeaderTranslator.GetLineComment: string;
var
  ch: char;
  incomment: boolean;
begin
  result := '//';
  incomment := true;
  while incomment do
  begin
    if eof(input) then
      break
    else
      read(input, ch);
    incomment := (ch <> #10) and (ch <> #13);
    result := result + ch;
  end;
end;
{************************************************************}

procedure TJeHeaderTranslator.StartLineComment;
begin
  //Read comments until end of line has been found
  buffer := GetLineComment;
  WriteOut;
  buffer := '';
end;
{************************************************************}

procedure TJeHeaderTranslator.CheckStatus(Value: integer);
begin
  if Value <> Status then
  begin
    Status := Value;
    case Value of
      S_Type: WriteOutS(#10#13 + 'Type' + #10#13);
      S_Var: WriteOutS(#10#13 + 'Var' + #10#13);
      S_Const: WriteOutS(#10#13 + 'Const' + #10#13);
    end;
  end;
end;
{************************************************************}

function TJeHeaderTranslator.ReadWord: string;
var
  ch: char;
begin
  //Read a word (until a space is found)
  if eof(input) then
    exit
  else
    read(input, ch);
  result := '';
  while ((result = '') or (ch <> ' ')) and (ch <> ';') and (ch <> #10) and (ch <> #13) do
  begin
    if ch <> ' ' then
      result := result + ch;
    if eof(input) then
      break
    else
      read(input, ch);
  end;
end;
{************************************************************}

function TJeHeaderTranslator.GetTypeDef(recu: integer): TstringList;
var
  rec, st, typ, nom, onom: string;
  ch: char;
  ok: boolean;
  i, j: integer;
  comment: string;
begin
  result := TstringList.Create;

  rec := '';
  for i := 1 to recu do
    rec := rec + ' ';

  checkstatus(s_type);
  buffer := '';

  buffer := ReadWord;

  buffer := UpperCase(buffer);
  if buffer = 'ENUM' then
  begin
    //this is an enumeration
    onom := '';
    if eof(input) then
      exit
    else
      read(input, ch);
    while (ch <> '{') do
    begin
      if ch = '/' then
      begin
        if eof(input) then
          break
        else
          read(input, ch);
        case ch of
          '*': StartComment; //this is a big comment
          '/': StartLineComment; //this is a single line comment
        end;
      end
      else if not (ch in [#10, #13, #9]) then
        onom := onom + ch;
      if eof(input) then
        break
      else
        read(input, ch);
    end;
    onom := RemoveSpaces(onom);

    comment := '';
    with result do
    begin
      add(' = (');
      buffer := '';
      if eof(input) then
        exit
      else
        read(input, ch);
      //read the content of an enumeration
      while (ch <> '}') do
      begin
        case ch of
          '#': add(GetPreProcessor); //include pre processor command
          ',': if buffer <> '' then
            begin
              st := UpperCase(buffer);
              if (st = 'STRUCT') or (st = 'UNION') or (st = 'ENUM') then
              begin
                Seek(input, FilePos(input) - (length(st) + 3));
                with GetTypeDef(recu + 5) do
                begin
                  while count > 0 do
                  begin
                    result.add(strings[0]);
                    delete(0);
                  end;
                end;
                buffer := '';
                st := '';
              end
              else
              begin
                buffer := ReplaceSub('0x', '$', buffer);
                add(rec + '     ' + buffer + ',' + comment);
                buffer := '';
                comment := '';
              end;
            end
            else if comment <> '' then
            begin
              add(rec + '     ' + comment);
              comment := '';
            end;
          '/':
            begin
              //start of a comment
              if eof(input) then
                break
              else
                read(input, ch);
              case ch of
                '*': comment := GetStartComment; //this is a big comment
                '/': comment := GetLineComment; //this is a single line comment
              end;
            end;
        else
          if (ch <> #10) and (ch <> #13) then
            buffer := buffer + ch;
        end;
        if eof(input) then
          break
        else
          read(input, ch);
      end;
      strings[count - 1] := replaceSub(',', '', strings[count - 1]);
      if comment <> '' then
        add(rec + '     ' + comment);
      add(rec + ');');

      //read the name of the record
      buffer := '';
      ok := false;
      repeat
        if eof(input) then
          break
        else
          read(input, ch);
        case ch of
          ';', ' ', ',', #9: if (buffer <> '') then
            begin
              if not (ok) then
              begin
                nom := buffer;
                strings[0] := rec + buffer + strings[0];
                ok := true;
              end
              else
              begin
                if UpperCase(buffer) <> 'FAR' then
                begin
                  if pos('*', buffer) <> 0 then
                  begin
                    buffer := ReplaceSub('*', '', buffer);
                    add(rec + buffer + ' = ^' + nom + ';');
                  end
                  else
                    add(rec + buffer + ' = ' + nom + ';');
                end;
              end;
              buffer := '';
            end;
        else
          buffer := buffer + ch;
        end;
      until ch = ';';
      if not (ok) then
      begin
        if onom = '' then
          strings[0] := rec + 'NoName' + strings[0]
        else
          strings[0] := rec + onom + strings[0]
      end;

      buffer := '';
    end;
    if Assigned(FOnInfo) then
      FOnInfo(self, 'Adding enumeration : ' + onom);
  end
  else if buffer = 'STRUCT' then
  begin
    //this is a structure
    if eof(input) then
      exit
    else
      read(input, ch);
    onom := '';
    while (ch <> '{') do
    begin
      if ch = '/' then
      begin
        if eof(input) then
          break
        else
          read(input, ch);
        case ch of
          '*': StartComment; //this is a big comment
          '/': StartLineComment; //this is a single line comment
        end;
      end
      else if ch = ';' then
      begin
        WriteOutS('//Ignored struct named : ' + onom);
        buffer := '';
        exit;
      end
      else if not (ch in [#10, #13, #9]) then
        onom := onom + ch;
      if eof(input) then
        break
      else
        read(input, ch);
    end;
    onom := removespaces(onom);

    with result do
    begin
      if recu = 5 then
        add(' = record')
      else
        add(' : record');
      buffer := '';
      typ := '';
      if eof(input) then
        exit
      else
        read(input, ch);
      //read the content of the record
      while (ch <> '}') do
      begin
        case ch of
          '#': add(GetPreProcessor); //include pre processor command
          ' ', #9: if buffer <> '' then
            begin
              st := UpperCase(buffer);
              if (st = 'STRUCT') or (st = 'UNION') or (st = 'ENUM') then
              begin
                Seek(input, FilePos(input) - (length(st) + 3));
                with GetTypeDef(recu + 5) do
                begin
                  while count > 0 do
                  begin
                    result.add(strings[0]);
                    delete(0);
                  end;
                end;
                buffer := '';
                st := '';
              end
              else
              begin
                if typ = '' then //added the test
                begin
                  typ := PascalType(buffer);
                  buffer := '';
                end;
              end;
            end;
          ';':
            begin
              st := CheckArray(buffer) + typ;
              CheckPointer(buffer, st);
              typ := rec + '     ' + buffer + ' : ' + st + ';' + comment;
              for i := 1 to length(typ) do
                if typ[i] in [#10, #13] then
                  typ[i] := ' ';
              add(typ);
              buffer := '';
              comment := '';
              typ := '';
            end;
          '/':
            begin
              //start of a comment
              if eof(input) then
                break
              else
                read(input, ch);
              case ch of
                '*': comment := GetStartComment; //this is a big comment
                '/': comment := GetLineComment; //this is a single line comment
              end;
            end;
        else
          if (ch <> #10) and (ch <> #13) then
            buffer := buffer + ch
          else if comment <> '' then
          begin
            add(rec + '     ' + comment);
            comment := '';
          end;
        end;
        if eof(input) then
          break
        else
          read(input, ch);
      end;
      if comment <> '' then
        add(rec + '     ' + comment);
      add(rec + 'end;');

      //read the name of the record
      buffer := '';
      ok := false;
      repeat
        if eof(input) then
          break
        else
          read(input, ch);
        case ch of
          ';', ' ', ',', #9: if (buffer <> '') then
            begin
              if not (ok) then
              begin
                nom := buffer;
                strings[0] := rec + buffer + strings[0];
                ok := true;
              end
              else
              begin
                if UpperCase(buffer) <> 'FAR' then
                begin
                  if pos('*', buffer) <> 0 then
                  begin
                    buffer := ReplaceSub('*', '', buffer);
                    add(rec + buffer + ' = ^' + nom + ';');
                  end
                  else
                    add(rec + buffer + ' = ' + nom + ';');
                end;
              end;
              buffer := '';
            end;
        else
          buffer := buffer + ch;
        end;
      until ch = ';';
      if not (ok) then
      begin
        if onom = '' then
          strings[0] := rec + 'NoName' + strings[0]
        else
          strings[0] := rec + onom + strings[0];
      end;

      buffer := '';
    end;
    if Assigned(FOnInfo) then
      FOnInfo(self, 'Adding structure : ' + onom);
  end
  else if buffer = 'UNION' then
  begin
    //this is an union

    if eof(input) then
      exit
    else
      read(input, ch);
    onom := '';
    while (ch <> '{') do
    begin
      if ch = '/' then
      begin
        if eof(input) then
          break
        else
          read(input, ch);
        case ch of
          '*': StartComment; //this is a big comment
          '/': StartLineComment; //this is a single line comment
        end;
      end
      else if not (ch in [#10, #13, #9]) then
        onom := onom + ch;
      if eof(input) then
        break
      else
        read(input, ch);
    end;
    onom := RemoveSpaces(onom);

    j := 0;
    with result do
    begin
      if recu = 5 then
        add(' = record')
      else
        add(' : record');
      add(rec + '     case integer of');
      buffer := '';
      if eof(input) then
        exit
      else
        read(input, ch);
      //read the content of the record
      while (ch <> '}') do
      begin
        case ch of
          '#': add(GetPreProcessor); //include pre processor command
          ' ', #9: if buffer <> '' then
            begin
              st := UpperCase(buffer);
              if (st = 'STRUCT') or (st = 'UNION') or (st = 'ENUM') then
              begin
                Seek(input, FilePos(input) - (length(st) + 3));
                with GetTypeDef(recu + 5) do
                begin
                  if count > 0 then
                  begin
                    strings[0] := rec + '     ' + IntToStr(j) + ' : (' + RemoveSpaces(strings[0]);
                    strings[count - 1] := strings[count - 1] + ');';
                    inc(j);
                  end;
                  while count > 0 do
                  begin
                    result.add(strings[0]);
                    delete(0);
                  end;
                end;
                buffer := '';
                st := '';
              end
              else
              begin
                if typ = '' then //added the test
                begin
                  typ := PascalType(buffer);
                  buffer := '';
                end;
              end;
            end;
          ';':
            begin
              st := CheckArray(buffer) + typ;
              CheckPointer(buffer, st);
              typ := rec + '     ' + IntToStr(j) + ': (' + buffer + ' : ' + st + ');' + comment;
              for i := 1 to length(typ) do
                if typ[i] in [#10, #13] then
                  typ[i] := ' ';
              add(typ);

              inc(j);
              buffer := '';
              comment := '';
              typ := '';
            end;
          '/':
            begin
              //start of a comment
              if eof(input) then
                break
              else
                read(input, ch);
              case ch of
                '*': comment := GetStartComment; //this is a big comment
                '/': comment := GetLineComment; //this is a single line comment
              end;
            end;
        else
          if (ch <> #10) and (ch <> #13) then
            buffer := buffer + ch
          else if comment <> '' then
          begin
            add(rec + '     ' + comment);
            comment := '';
          end;
        end;
        if eof(input) then
          break
        else
          read(input, ch);
      end;
      if comment <> '' then
        add(rec + '     ' + comment);
      add(rec + 'end;');

      //read the name of the record
      buffer := '';
      ok := false;
      repeat
        if eof(input) then
          break
        else
          read(input, ch);
        case ch of
          ';', ' ', ',', #9: if (buffer <> '') then
            begin
              if not (ok) then
              begin
                nom := buffer;
                strings[0] := rec + buffer + strings[0];
                ok := true;
              end
              else
              begin
                if UpperCase(buffer) <> 'FAR' then
                begin
                  if pos('*', buffer) <> 0 then
                  begin
                    buffer := ReplaceSub('*', '', buffer);
                    add(rec + buffer + ' = ^' + nom + ';');
                  end
                  else
                    add(rec + buffer + ' = ' + nom + ';');
                end;
              end;
              buffer := '';
            end;
        else
          buffer := buffer + ch;
        end;
      until ch = ';';
      if not (ok) then
      begin
        if onom = '' then
          strings[0] := rec + 'NoName' + strings[0]
        else
          strings[0] := rec + onom + strings[0];
      end;

      buffer := '';
    end;
    if Assigned(FOnInfo) then
      FOnInfo(self, 'Adding union : ' + onom);
  end
  else
  begin
    //this is a classic typedef
    if Assigned(FOnInfo) then
      FOnInfo(self, 'Adding other type');
    st := PascalType(buffer);
    repeat
      if eof(input) then
        break
      else
        read(input, ch);
      case ch of
        ',', ';':
          begin
            if buffer <> '' then
            begin
              CheckStatus(S_Type);
              if pos('*', buffer) <> 0 then
              begin
                buffer := RemoveSpaces(ReplaceSub('*', '', buffer));
                buffer := '    ' + buffer + ' = ^' + st;
              end
              else
                buffer := '    ' + RemoveSpaces(buffer) + ' = ' + st;
              buffer := buffer + ';' + #10#13;
              WriteOut;
              buffer := '';
            end;
          end;
        #10, #13: if ch = #10 then
            buffer := buffer + #10#13;
      else
        buffer := buffer + ch;
      end;
    until ch = ';';
  end;
end;
{************************************************************}

procedure TJeHeaderTranslator.TypeDef(recu: integer);
begin
  with GetTypeDef(recu) do
    while count > 0 do
    begin
      buffer := strings[0];
      if (pos(#10, strings[0]) = 0) and (pos(#13, strings[0]) = 0) then
        buffer := buffer + #10#13;
      WriteOut;
      delete(0);
    end;
  buffer := '';
end;
{************************************************************}

function TJeHeaderTranslator.PascalType(Value: string): string;
var
  st: string;
begin
  //Convert a c type to a pascal type (and read more if needed)
  buffer := '';
  st := UpperCase(Value);
  if st = 'UNSIGNED' then
  begin
    st := UpperCase(ReadWord);
    if st = 'INT' then
      result := 'cardinal'
    else if st = 'CHAR' then
      result := 'char'
    else if st = 'LONG' then
      result := 'cardinal'
    else if st = 'SHORT' then
      result := 'smallint';
  end
  else if st = 'LONG' then
  begin
    st := ReadWord;
    if UpperCase(st) = 'DOUBLE' then
      result := 'extended'
    else
    begin
      result := 'longint';
      seek(input, filepos(input) - (length(st) + 1));
    end;
  end
  else if st = 'STRUCT' then
    result := PascalType(ReadWord)
  else if st = 'INT' then
    result := 'integer'
  else if st = 'HANDLE' then
    result := 'THandle'
  else if st = 'FLOAT' then
    result := 'real'
  else if st = 'SHORT' then
    result := 'smallint'
  else if st = 'BOOL' then
    result := 'boolean'
  else if st = 'USHORT' then
    result := 'smallint'
  else if st = 'ULONG' then
    result := 'cardinal'
  else if st = 'VOID' then
    result := 'pointer'
  else if st = 'WCHAR' then
    result := 'WideChar'
  else if st = 'PWCHAR' then
    result := 'PWideChar'
  else if st = 'LPSTR' then
    result := 'PAnsiChar'
  else if st = 'LPCSTR' then
    result := 'PAnsiChar'
  else if st = 'LPCTSTR' then
    result := 'PAnsiChar'
  else if st = 'LPTSTR' then
    result := 'PAnsiChar'
  else if st = 'LPWSTR' then
    result := 'PWideChar'
  else if st = 'LPCWSTR' then
    result := 'PWideChar'
  else if st = 'DWORD' then
    result := 'LongWord'
  else if st = 'LPDWORD' then
    result := 'PDWORD'
  else if st = 'UCHAR' then
    result := 'Byte'
  else if st = 'LCID' then
    result := 'DWORD'
  else if st = 'LANGID' then
    result := 'Word'
  else if st = 'PSID' then
    result := 'pointer'
  else if st = 'RECT' then
    result := 'TRect'
  else if st = 'Cstring' then
    result := 'string'
  else if st = 'COBJECT' then
    result := 'TObject'
  else
    result := Value;
end;
{************************************************************}

function TJeHeaderTranslator.CheckArray(var Value: string): string;
var
  st, valsav: string;
begin
  if pos('[', Value) <> 0 then
  begin
    result := 'array [';

    st := Copy(Value, pos('[', Value) + 1, length(Value));
    st := Copy(st, 1, pos(']', st) - 1);

    if (StrToIntDef(st, -1) <> -1) then
      result := result + '0..' + IntToStr(StrToIntDef(st, -1) - 1)
    else
      result := result + '0..' + st + '-1';

    valsav := Copy(Value, 1, pos('[', Value) - 1);
    Value := Copy(Value, pos(']', Value) + 1, length(Value));

    while pos('[', Value) <> 0 do
    begin
      st := Copy(Value, pos('[', Value) + 1, length(Value));
      st := Copy(st, 1, pos(']', st) - 1);
      Value := Copy(Value, pos(']', Value) + 1, length(Value));

      st := RemoveSpaces(st);

      if (StrToIntDef(st, -1) <> -1) then
        result := result + ',0..' + IntToStr(StrToIntDef(st, -1) - 1)
      else
        result := result + ',0..' + RemoveSpaces(st) + '-1';
    end;
    result := result + '] of ';
    Value := valsav;
  end
  else
    result := '';
end;
{************************************************************}

procedure TJeHeaderTranslator.Fonction;
var
  parm, st: string;
  ch: char;
  ret: string;
  i: integer;
begin
  //Fonction importée
  st := UpperCase(buffer);

  if st = 'EXTERN' then
  begin
    //extern c begin ...
    ch := ' ';
    while not (eof(input)) and (ch <> '{') do
      if eof(input) then
        break
      else
        read(input, ch);
    buffer := '//Extern c starts here' + #10#13;
    WriteOut;
  end
  else if st = '}' then
  begin
    //extern c end ... normally :)
    buffer := '//Extern C ends here' + #10#13;
    WriteOut;
  end
  else
  begin
    //it must be an imported function
    ret := PascalType(buffer);

    ch := ' ';
    st := buffer;
    if eof(input) then
      exit
    else
      read(input, ch);
    while ch <> '(' do
    begin
      if not (ch in [#10, #13]) then
        st := st + ch
      else
        st := st + ' ';
      if eof(input) then
        break
      else if eof(input) then
        break
      else
        read(input, ch);
    end;

    //st may contain the name of the function ... (ex  coucougne name) must only take name
    st := RemoveSpaces(st);
    i := length(st);
    while (i > 0) and (st[i] <> ' ') do
      dec(i);
    if i > 0 then
      st := Copy(st, i + 1, length(st));

    //st contains the name
    parm := Parameters;
    CheckPointer(st, ret);
    if parm <> '' then
      parm := '(' + parm + ')';
    if ret = '' then
    begin
      if Assigned(FOnInfo) then
        FOnInfo(self, 'Adding procedure : ' + st);
      buffer := 'procedure ' + st + ' ' + Parm + ':' + ret + ';';
      impo.Add('procedure ' + st + ';external ' + unitname + 'DLL name ''' + st + ''';');
    end
    else
    begin
      if Assigned(FOnInfo) then
        FOnInfo(self, 'Adding function : ' + st);
      buffer := 'function ' + st + ' ' + Parm + ':' + ret + ';';
      impo.Add('function ' + st + ';external ' + unitname + 'DLL name ''' + st + ''';');
    end;
    WriteOut;

    //Read until ';' has been found
    ch := ' ';
    while (ch <> ';') and (not (eof(input))) do
    begin
      //check if comment ...
      if ch = '/' then
      begin
        if eof(input) then
          break
        else
          read(input, ch);
        case ch of
          '*': StartComment; //this is a big comment
          '/': StartLineComment; //this is a single line comment
        end;
      end
      else if eof(input) then
        break
      else
        read(input, ch);
    end;
  end;
  buffer := '';
  CheckStatus(S_None);
end;
{************************************************************}

function TJeHeaderTranslator.Parameters: string;
var
  ch: char;
  st: string;
begin
  //read parameters (from a function)
  ch := ' ';
  result := '';
  while ch <> ')' do
  begin
    if eof(input) then
      break
    else
      read(input, ch);
    while (ch in [' ', #10, #13, #9]) do
    begin
      result := result + ch;
      if eof(input) then
      begin
        buffer := '';
        exit;
      end
      else
        read(input, ch);
    end;
    buffer := '';
    if ch = '/' then
    begin
      if eof(input) then
        break
      else
        read(input, ch);
      case ch of
        '*': StartComment; //this is a big comment
        '/': StartLineComment; //this is a single line comment
      end;
    end
    else if ch = '#' then
    begin
      result := result + GetPreProcessor;
    end
    else if ch <> ')' then
    begin
      st := ch;
      st := PascalType(st + readword);
      ch := ' ';
      while (ch in [' ', #10, #13, #9]) do
        if eof(input) then
          break
        else
          read(input, ch);
      while (ch <> ',') and (ch <> ')') and (ch <> #10) and (ch <> #13) do
      begin
        buffer := buffer + ch;
        if eof(input) then
          break
        else
          read(input, ch);
      end;

      CheckPointer(buffer, st);
      result := result + buffer + ' : ' + st;
      if ch = ',' then
        result := result + ';'
      else if ch <> ')' then
        result := result + ch;

      buffer := '';
    end;
  end;
  buffer := '';
end;
{************************************************************}

procedure TJeHeaderTranslator.CheckPointer(var variable: string; var letype: string);
begin
  if pos('*', variable) <> 0 then
  begin
    variable := ReplaceSub('*', '', variable);
    if galready.IndexOf(UpperCase('P' + letype)) = -1 then
    begin
      gtype.Add('    P' + letype + ' = ^' + letype + ';');
      galready.Add(UpperCase('P' + letype));
    end;
    letype := 'P' + letype;
  end;
end;
{************************************************************}

function TJeHeaderTranslator.Lisstring: string;
var
  ch: char;
  i: integer;
  st: string;
  instring: boolean;
begin
  st := '';
  if eof(input) then
    exit
  else
    read(input, ch);
  while ch <> '"' do
  begin
    st := st + ch;
    if eof(input) then
      break
    else
      read(input, ch);
  end;
  i := 1;
  instring := false;
  result := '';
  while (i < length(st)) do
  begin
    if st[i] = '\' then
    begin
      if st[i + 1] in ['a'..'z', '0'..'9', 'A'..'Z'] then
      begin
        if instring then
          result := result + '''+#'
        else
          result := result + '#';
        instring := false;
        inc(i);
        case st[i] of
          'x', 'X':
            begin
              result := result + '$' + st[i + 1];
              if (st[i + 2] in ['0'..'9', 'a'..'f', 'A'..'F']) then
              begin
                result := result + st[i + 2];
                i := i + 2;
              end
              else
                i := i + 1;
            end;
          '0'..'9':
            begin
              result := result + st[i];
              if st[i + 1] in ['0'..'9'] then
              begin
                result := result + st[i + 1];
                if st[i + 2] in ['0'..'9'] then
                begin
                  result := result + st[i + 2];
                  i := i + 2;
                end
                else
                  i := i + 1;
              end;
            end;
        else
          begin
            // \n, ... \f...
            case upcase(st[i]) of
              'N': result := result + '13';
              'T': result := result + '9';
              'F': result := result + '';
              'A': result := result + '';
              '"': result := result + '"';
            end;
          end;
        end;
      end
      else
      begin
        result := result + '''+' + #10#13 + '''';
        inc(i);
        while (st[i] in [#10, #13, #9, ' ']) do
          inc(i);
        dec(i);
      end;
    end
    else
    begin
      if instring then
      begin
        if st[i] = '''' then
          result := result + ''''
        else
          result := result + st[i]
      end
      else
      begin
        if result <> '' then
        begin
          if st[i] = '''' then
            result := result + '+''' + ''''
          else
            result := result + '+''' + st[i];
        end
        else
        begin
          if st[i] = '''' then
            result := result + '''' + ''''
          else
            result := result + '''' + st[i];
        end;
        instring := true;
      end;
    end;
    inc(i);
  end;
  if instring then
    result := result + '''';
end;
{************************************************************}

procedure TJeHeaderTranslator.ReadClass;
var
  notfinished: boolean;
  ch: char;
  fin, st, nom, st2, st3: string;
  i, j, k: integer;
  tmp, tmp2, params, res, toanal: string;
  read1: boolean;
  ts: TstringList;
begin
  ch := ' ';
  if eof(input) then
    exit
  else
    read(input, ch);
  while (ch <> '{') do
  begin
    if not (ch in [#10, #13, #9]) then
      st := st + ch;
    if eof(input) then
      break
    else
      read(input, ch);
  end;
  st := RemoveSpaces(st);
  CheckStatus(S_TYPE);
  if pos(':', st) <> 0 then
  begin
    i := pos(':', st);
    st2 := RemoveSpaces(Copy(st, 1, i - 1));
    while pos(' ', st2) <> 0 do
      st2 := Copy(st2, pos(' ', st2) + 1, length(st2));
    nom := st2;

    st2 := RemoveSpaces(Copy(st, i + 1, length(st)));
    while pos(' ', st2) <> 0 do
      st2 := Copy(st2, pos(' ', st2) + 1, length(st2));
    st2 := PascalType(st2);
    buffer := '   ' + nom + ' = class(' + st2 + ')' + #10#13;
    WriteOut;
  end
  else
  begin
    st2 := RemoveSpaces(st);
    while pos(' ', st2) <> 0 do
      st2 := Copy(st2, pos(' ', st2) + 1, length(st2));
    nom := st2;
    buffer := '   ' + nom + ' = Class' + #10#13;
    WriteOut;
  end;

  notfinished := false;

  buffer := '';
  toanal := '';
  res := '';
  while (notfinished = false) do
  begin
    if eof(input) then
      break;
    if eof(input) then
      break
    else
      read(input, ch);
    case ch of
      ' ', #10, #13, ';':
        begin
          st3 := UpperCase(buffer);
          if st3 = 'VIRTUAL' then
            fin := 'virtual;'
          else if st3 = 'FRIEND' then
          begin
            //nothing to do
          end
          else
          begin
            buffer := ReplaceSub(#9, '', buffer);
            if res = '' then
              res := RemoveSpaces(PascalType(buffer))
            else
              toanal := toanal + ' ' + RemoveSpaces(buffer);
          end;
          buffer := '';
          if ch = #10 then
            WriteOutS(#10#13)
          else if ch = ';' then
          begin
            WriteOutS('       ' + RemoveSpaces(toanal + ' : ' + res) + ';' + fin);
            toanal := '';
            res := '';
            fin := '';
          end;
        end;
      ':':
        begin
          st3 := UpperCase(buffer);
          if st3 = 'PROTECTED' then
            WriteOutS('   protected')
          else if st3 = 'PUBLIC' then
            WriteOutS('   public')
          else if st3 = 'PUBLISHED' then
            WriteOutS('   published');
          buffer := '';
        end;
      '/':
        begin
          try
            read(input, ch);
          except
            notfinished := true;
            ch := ' ';
          end;
          case ch of
            '*': StartComment; //this is a big comment
            '/': StartLineComment; //this is a single line comment
          end;
        end;
      '}':
        begin
          notfinished := true;
        end;
      '(':
        begin
          //this is a function to analyse

          //adjust name if necessary
          Toanal := RemoveSpaces(Toanal);
          if ToAnal = '' then
            toanal := RemoveSpaces(buffer);

          //read parameters
          params := '';
          tmp := '';
          tmp2 := '';
          read1 := true;
          try
            read(input, ch);
          except
            notfinished := true;
            ch := ')';
          end;
          while (ch <> ')') do
          begin
            case ch of
              ',':
                begin
                  CheckDefault(tmp2, tmp);
                  params := params + tmp2 + ':' + tmp + ';';
                  tmp := '';
                  tmp2 := '';
                  read1 := true;
                end;
              ' ':
                begin
                  if tmp <> '' then
                  begin
                    tmp := PascalType(tmp);
                    read1 := false;
                  end;
                end;
              #10, #13, #9: tmp2 := tmp2 + ch;
            else
              if read1 then
                tmp := tmp + ch
              else
                tmp2 := tmp2 + ch;
            end;
            if eof(input) then
              break
            else
              read(input, ch);
          end;
          CheckDefault(tmp2, tmp);
          if tmp2 <> '' then
            params := params + tmp2 + ':' + tmp;
          if params <> '' then
            params := '(' + params + ')';

          //read until the end of the declaration
          if eof(input) then
            break
          else
            read(input, ch);
          while (ch <> ';') do
          begin
            case ch of
              '{':
                begin
                  //this is raw cpp code
                  if UpperCase(toanal) = UpperCase(nom) then
                    imple.add('Constructor ' + nom + '.Create' + retdef(params) + ';')
                  else if UpperCase(toanal) = '~' + UpperCase(nom) then
                    imple.add('Destructor ' + nom + '.Destroy' + retdef(params) + ';')
                  else if res = '' then
                    imple.add('procedure ' + nom + '.' + toanal + retdef(params) + ';')
                  else
                    imple.add('function ' + nom + '.' + toanal + retdef(params) + ':' + res + ';');
                  imple.add('begin');

                  ts := TstringList.Create;
                  ReadRawCode(ts);
                  for k := 0 to ts.Count - 1 do
                    imple.add(ts[k]);
                  ts.free;
                  imple.add('end;');
                  imple.add('');

                  ch := ';';
                end;
              ':':
                begin
                  //this is variable initialisation
                  if UpperCase(toanal) = UpperCase(nom) then
                    imple.add('Constructor ' + nom + '.Create' + retdef(params) + ';')
                  else if UpperCase(toanal) = '~' + UpperCase(nom) then
                    imple.add('Destructor ' + nom + '.Destroy' + retdef(params) + ';')
                  else if res = '' then
                    imple.add('procedure ' + nom + '.' + toanal + retdef(params) + ';')
                  else
                    imple.add('function ' + nom + '.' + toanal + retdef(params) + ':' + res + ';');
                  imple.add('begin');

                  //read all initialisations
                  tmp := '';
                  tmp2 := '';
                  if eof(input) then
                    break
                  else
                    read(input, ch);
                  while ch <> '{' do
                  begin
                    case ch of
                      '(':
                        begin
                          tmp2 := '';
                          if eof(input) then
                            break
                          else
                            read(input, ch);
                          while ch <> ')' do
                          begin
                            tmp2 := tmp2 + ch;
                            if eof(input) then
                              break
                            else
                              read(input, ch);
                          end;
                          for j := 1 to length(tmp2) do
                            if tmp2[j] = '"' then
                              tmp2[j] := '''';
                          imple.add('    ' + tmp + ':=' + tmp2 + ';');
                          tmp := '';
                        end;
                    else if not (ch in [',', #10, #13, #9, ' ']) then
                      tmp := tmp + ch;
                    end;
                    if eof(input) then
                      break
                    else
                      read(input, ch);
                  end;
                  while ch <> '}' do
                    if eof(input) then
                      break
                    else
                      read(input, ch);

                  imple.add('end;');
                  imple.add('');
                  ch := ';';
                  //                                            if eof(input) then break else read(input,ch);
                end;
            else
              read(input, ch);
            end;
          end;

          //add it to the pascal file
          if UpperCase(toanal) = UpperCase(nom) then
          begin
            //this is a Constructor
            WriteOutS('      Constructor Create' + params + ';override;' + fin);
          end
          else if UpperCase(toanal) = '~' + UpperCase(nom) then
          begin
            //this is a Destructor
            toanal := ReplaceSub('~', '', toanal);
            WriteOutS('      Destructor Destroy' + params + ';override;' + fin);
          end
          else
          begin
            //this is a classic procedure/function
            if res = '' then
              WriteOutS('      procedure ' + toanal + params + ';' + fin)
            else
              WriteOutS('      function ' + toanal + params + ':' + res + ';' + fin);
          end;
          toanal := '';
          res := '';
          fin := '';
          buffer := '';
        end;
      '{':
        begin
          //this is c code for the function
          while (ch <> '}') do
            if eof(input) then
              break
            else
              read(input, ch);
        end;
    else
      buffer := buffer + ch;
    end;
  end;
  if Assigned(FOnInfo) then
    FOnInfo(self, 'Adding class : ' + nom);
  if not (eof(input)) then
    read(input, ch);
  WriteOutS('   end;');
  buffer := '';
end;
{************************************************************}

procedure TJeHeaderTranslator.CheckDefault(var nom: string; var letype: string);
begin
  if pos('=', nom) <> 0 then
  begin
    letype := letype + Copy(nom, pos('=', nom), length(nom));
    nom := Copy(nom, 1, pos('=', nom) - 1);
    while pos('"', letype) <> 0 do
      letype[pos('"', letype)] := '''';
  end;
  if pos('&', nom) <> 0 then
  begin
    nom := 'var ' + nom;
    nom := ReplaceSub('&', '', nom);
  end;
end;
{************************************************************}

function TJeHeaderTranslator.retdef(Value: string): string;
var
  i: integer;
begin
  i := 1;
  result := '';
  while (i <= length(Value)) do
  begin
    if Value[i] = '=' then
    begin
      while (i < length(Value)) and (Value[i] <> ';') do
        inc(i);
    end
    else
    begin
      result := result + Value[i];
      inc(i);
    end;
  end;
end;
{************************************************************}

function TJeHeaderTranslator.ReadUntilCh(Value: char): string;
var
  ch: char;
begin
  if eof(input) then
    exit
  else
    read(input, ch);
  result := '';
  while ch <> Value do
  begin
    result := result + ch;
    if eof(input) then
      break
    else
      read(input, ch);
  end;
  result := RemoveSpaces(result);
end;
{************************************************************}

procedure TJeHeaderTranslator.AddGuid;
var
  n, nom, val: string;
  i: integer;
begin
  if Assigned(FOnInfo) then
    FOnInfo(self, 'Adding interface (ignored)');
  CheckStatus(S_CONST);

  n := ReplaceSub('IID_', '', UpperCase(ReadUntilCh(',')));

  //read the Value of the guid

  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := IntToHex(i, 8) + '-';

  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 4) + '-';

  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 4) + '-';

  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2);
  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2) + '-';

  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2);
  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2);
  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2);
  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2);
  nom := ReadUntilCh(',');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2);
  nom := ReadUntilCh(')');
  i := StrToInt(ReplaceSub('OX', '$', UpperCase(nom)));
  val := val + IntToHex(i, 2);

  while length(n) < 25 do
    n := n + ' ';
  buffer := '   GUID_' + n + ' = ''' + val + ''';';
  WriteOut;
  buffer := '';

  ReadUntilCh(';');
end;
{************************************************************}

procedure TJeHeaderTranslator.AddInterface;
begin
  WriteOutS('(* Ignored interface ... '#10#13);
  WriteOutS(ReadUntilCh('}'));
  ReadUntilCh(';');
  WriteOutS('*)');
end;
{************************************************************}

procedure TJeHeaderTranslator.AddConst;
var
  st, st2: string;
  i, j, k: integer;
begin
  //Jump to a Const section
  CheckStatus(S_Const);
  st := PascalType(ReadWord); //The type of the const

  buffer := buffer + ReadUntilCh('=');

  if pos('[', buffer) <> 0 then
  begin
    //Array
    buffer := Copy(buffer, 1, pos('[', buffer) - 1);
    st2 := ReadUntilCh(';') + ';';
    j := 1;
    k := 0;
    for i := 0 to length(st2) do
    begin
      if st2[i] = '{' then
      begin
        st2[i] := '(';
        inc(k);
      end
      else if st2[i] = '}' then
      begin
        st2[i] := ')';
        dec(k);
      end
      else if (st2[i] = ',') and (k = 1) then
        inc(j);
    end;
    st := buffer + ' : array [0..' + IntToStr(j - 1) + '] of ' + st + ' = ' + st2;
  end
  else
  begin
    //Non array
    st := buffer + ' : ' + st + ' = ' + ReadUntilCh(';') + ';';

    for i := 0 to length(st) do
      if st[i] = '{' then
        st[i] := '('
      else if st[i] = '}' then
        st[i] := ')';
  end;

  WriteOutS(st);
  buffer := '';
end;
{************************************************************}

procedure TJeHeaderTranslator.ReadRawCode(ts: TstringList);
var
  st, st2: string;
  ch: char;
  i: integer;
begin
  st := '   ';
  if eof(input) then
    exit
  else
    read(input, ch);
  while ch <> '}' do
  begin
    st := st + ch;
    if eof(input) then
      break
    else
      read(input, ch);
  end;
  //correct the code
  i := 1;
  while i <= length(st) do
  begin
    if st[i] = '=' then
    begin
      if st[i + 1] = '=' then
      begin
        st2 := st2 + '=';
        inc(i);
      end
      else
        st2 := st2 + ':=';
    end
    else
      st2 := st2 + st[i];
    inc(i);
  end;
  st2 := ReplaceSub('return', 'result :=', st2);
  ts.Add(st2);
end;
{************************************************************}

procedure TJeHeaderTranslator.CorrectCRLF(FileName: string);
var
  src: TFileStream;
  dest: TMemoryStream;
  buf: array[0..1024] of byte;
  buf2: array[0..2048] of byte;
  i, j, k: integer;
begin
  src := TFileStream.Create(FileName, fmOpenRead);
  src.Position := 0;
  dest := TMemoryStream.Create;

  self.JvProgressDlg1.Max := src.Size;
  self.JvProgressDlg1.Text := 'Phase 2 : Checking CRLF integrity...';
  self.JvProgressDlg1.Position := 0;
  self.JvProgressDlg1.Show;

  while src.position < src.Size do
  begin
    i := src.Read(buf, 1024);
    k := 0;
    for j := 0 to i - 1 do
    begin
      if buf[j] = 13 then
      begin
        buf2[k] := 10;
        inc(k);
        buf2[k] := 13;
        inc(k);
      end
      else if buf[j] <> 10 then
      begin
        buf2[k] := buf[j];
        inc(k);
      end;
    end;
    dest.Write(buf2, k);
    self.JvProgressDlg1.Position := self.JvProgressDlg1.Position + i;
    Application.ProcessMessages;
  end;
  src.free;

  src := TFileStream.Create(FileName, fmCreate);
  dest.Position := 0;
  src.CopyFrom(dest, dest.size);
  src.free;
  dest.free;

  self.JvProgressDlg1.Hide;
end;
{************************************************************}

constructor TJeHeaderTranslator.Create(AOwner: TComponent);
begin
  JvProgressDlg1 := TJvProgressDialog.Create(nil);
  FPrepro := false;
  FStatusTag := 0;
end;
{************************************************************}

destructor TJeHeaderTranslator.Destroy;
begin
  JvProgressDlg1.free;
  inherited;
end;
{************************************************************}

procedure TJeHeaderTranslator.StatWork;
begin
  if Assigned(FonStatus) then
  begin
    case (FStatusTag div 10) of
      0: FOnStatus(self, 'Working -');
      1: FOnStatus(self, 'Working \');
      2: FOnStatus(self, 'Working |');
      3: FOnStatus(self, 'Working /');
    end;
    FStatusTag := (FStatusTag + 1) mod 40;
  end;
end;
{************************************************************}
end.
