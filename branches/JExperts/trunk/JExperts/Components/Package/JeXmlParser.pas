{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeXmlParser.PAS, released on 2001-02-28.

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

unit JeXmlParser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TJeXmlElement = class(TObject)
  private
    FElements:TStringList;
    FText: string;
    FName: string;
    function GetElement(Index: Integer): TJeXmlElement;
    function GetCount: integer;
    function GetElementValue(const Name: string): string;
    function GetElementNamed(const Name: string): TJeXmlElement;
  protected
  public
    constructor Create;
    destructor Destroy;override;

    procedure Delete(Index: Integer);
    function Add(TagName: string;Value: string=''):TJeXmlElement;overload;
    function Add(TagName: string;Value: Boolean):TJeXmlElement;overload;
    function Add(TagName: string;Value: Integer):TJeXmlElement;overload;
    function Add(TagName: string;Value: real):TJeXmlElement;overload;
    procedure Add(Element: TJeXmlElement);overload;
    function GetElementList:string;
    function IndexOf(Name: string): Integer;
    procedure Clear;
    procedure RenameElement(OldValue, NewValue: string);

    property Elements[Index: Integer]: TJeXmlElement read GetElement;default;
    property ElementNamed[const Name:string]: TJeXmlElement read GetElementNamed;
    property Values[const Name:string]: string read GetElementValue;
    property Text:string read FText write FText;
    property TagName:string read FName write FName;
    property Count:integer read GetCount;
  end;

  TJeXmlParser = class(TComponent)
  private
    FRoot: TJeXmlElement;
    FFileName: TFileName;
    procedure SetFileName(const Value: TFileName);
  protected
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;

    procedure LoadFromFile(FileName:TFileName);
    procedure SaveToFile(FileName:TFileName);
    procedure LoadFromStream(Stream:TStream);
    procedure SaveToStream(Stream:TStream);

    property Root:TJeXmlElement read FRoot;
  published
    property FileName:TFileName read FFileName write SetFileName;
  end;

implementation

{***********************************************************}
function TJeXmlElement.Add(TagName: string;Value: Boolean):TJeXmlElement;
begin
  result := Add(TagName,IntToStr(Integer(Value)));
end;
{***********************************************************}
function TJeXmlElement.Add(TagName, Value: string): TJeXmlElement;
begin
  result := TJeXmlElement.Create;
  result.TagName := TagName;
  result.Text := Value;
  FElements.AddObject(TagName,TObject(result));
end;
{***********************************************************}
procedure TJeXmlElement.Add(Element: TJeXmlElement);
begin
  FElements.AddObject(Element.TagName,TObject(Element));
end;
{***********************************************************}
function TJeXmlElement.Add(TagName: string; Value: Integer): TJeXmlElement;
begin
  result := Add(TagName,IntToStr(Value));
end;
{***********************************************************}
function TJeXmlElement.Add(TagName: string; Value: real): TJeXmlElement;
begin
  result := Add(TagName,FloatToStr(Value));
end;
{***********************************************************}
procedure TJeXmlElement.Clear;
var
 i:integer;
begin
  for i:=0 to FElements.Count-1 do
    TJeXmlElement(FElements.Objects[i]).Free;
  FElements.Clear;
end;
{***********************************************************}
constructor TJeXmlElement.Create;
begin
  FElements := TStringList.Create;
end;
{***********************************************************}
procedure TJeXmlElement.Delete(Index: Integer);
begin
  FElements.Delete(Index);
end;
{***********************************************************}
destructor TJeXmlElement.Destroy;
begin
  Clear;
  FElements.Free;
  inherited;
end;
{***********************************************************}
function TJeXmlElement.GetCount: integer;
begin
  result := FElements.Count;
end;
{***********************************************************}
function TJeXmlElement.GetElement(Index: Integer): TJeXmlElement;
begin
  result := TJeXmlElement(FElements.Objects[Index]);
end;
{***********************************************************}
function TJeXmlElement.GetElementList: string;
begin
  result := FElements.Text;
end;
{***********************************************************}
function TJeXmlElement.GetElementNamed(const Name: string): TJeXmlElement;
begin
  result := GetElement(FElements.IndexOf(Name));
end;
{***********************************************************}
function TJeXmlElement.GetElementValue(const Name: string): string;
begin
  result := GetElement(FElements.IndexOf(Name)).Text;
end;
{***********************************************************}
function TJeXmlElement.IndexOf(Name: string): Integer;
begin
  result := FElements.IndexOf(Name);
end;
{***********************************************************}
procedure TJeXmlElement.RenameElement(OldValue, NewValue: string);
begin
  FElements[FElements.IndexOf(OldValue)] := NewValue;
  GetElement(FElements.IndexOf(NewValue)).TagName := NewValue;
end;
{***********************************************************}


{***********************************************************}
constructor TJeXmlParser.Create(AOwner: TComponent);
begin
  inherited;
  FRoot := TJeXmlElement.Create;
end;
{***********************************************************}
destructor TJeXmlParser.Destroy;
begin
  FRoot.Free;
  inherited;
end;
{***********************************************************}
procedure TJeXmlParser.LoadFromFile(FileName: TFileName);
var
  Stream:TFileStream;
begin
  Stream := TFileStream.Create(Filename,fmOpenRead);
  LoadFromStream(Stream);
  Stream.Free;
end;
{***********************************************************}
procedure TJeXmlParser.LoadFromStream(Stream: TStream);
var
 index:integer;
 Content:string;

 function ReadStr:string;
 begin
   result:='';
   while (index<Length(Content)) and (Content[Index] in [' ',#10,#13]) do inc(index);
   while (index<Length(Content)) and (not((result<>'')and(Content[Index]='<')))
         and (Content[index]<>'>') do
   begin
     result:=result+Content[index];
     inc(index);
   end;
   if (index<Length(Content)) and (Content[index]='>') then
   begin
     result:=result+'>';
     inc(index);
   end;
   result:=Trim(Result);
 end;

 procedure Decode(Parent:TJeXmlElement);
 var
  st,st2:string;
  OldIndex:integer;
  Elem:TJeXmlElement;

   function EndTag(Tag:string):string;
   begin
     result:='</'+Copy(Tag,2,Length(Tag));
   end;

   function ExtractName(st:string):string;
   begin
     result:=trim(copy(st,2,Length(st)-2));
   end;

 begin
   st:=ReadStr;
   if st='' then
     exit;

   if Parent=nil then
   begin
     Elem:=FRoot;
     if Elem.TagName<>'' then
       Raise Exception.Create('Invalid XMF File : More than one root element !');
   end
   else
     Elem:=Parent.Add(ExtractName(st));
   Elem.TagName:=ExtractName(st);
   st2:=UpperCase(EndTag(st));

   OldIndex:=Index;
   st:=ReadStr;
   while (UpperCase(st)<>st2) and (Index<Length(Content)) do
   begin
     if st[1]='<' then
     begin
       Index:=OldIndex;
       Decode(Elem);
     end
     else
     begin
       st:=StringReplace(st,'&lt;','<',[rfReplaceAll,rfIgnoreCase]);
       st:=StringReplace(st,'&gt;','>',[rfReplaceAll,rfIgnoreCase]);
       Elem.Text:=st;
     end;
     OldIndex:=Index;
     st:=ReadStr;
   end;
   if UpperCase(st)<>st2 then
     raise Exception.Create('Invalid XML File : Tag '+st+' not terminated !');
 end;

begin
  FRoot.Free;
  FRoot:=TJeXmlElement.Create;
  with TStringList.Create do
  begin
    LoadFromStream(Stream);
    Content:=Text;
    Free;
  end;

  index:=1;
  while (index<Length(Content)) do
    Decode(nil);
end;
{***********************************************************}
procedure TJeXmlParser.SaveToFile(FileName: TFileName);
var
 Stream:TFileStream;
begin
  Stream:=TFileStream.Create(FileName,fmCreate);
  SaveToStream(Stream);
  Stream.Free;
end;
{***********************************************************}
procedure TJeXmlParser.SaveToStream(Stream: TStream);
var
 ts:TStringList;

   procedure WriteElement(Element:TJeXmlElement;Level:string);
   var
    i:integer;
    st,st2:string;
   begin
     st2:=StringReplace(Element.Text,'<','&lt;',[rfReplaceAll]);
     st2:=StringReplace(st2,'>','&gt;',[rfReplaceAll]);

     st:=Level+'<'+Element.TagName+'>'+st2;
     if Element.Count=0 then
     begin
       st:=st+'</'+Element.TagName+'>';
       ts.Add(st);
     end
     else
     begin
       ts.Add(st);
       for i:=0 to Element.Count-1 do
         WriteElement(Element[i],Level+' ');
       ts.Add(Level+'</'+Element.TagName+'>');
     end;
   end;

begin
  ts:=TStringList.Create;
  with ts do
  begin
    WriteElement(Root,'');
    SaveToStream(Stream);
    Free;
  end;
end;
{***********************************************************}
procedure TJeXmlParser.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;
{***********************************************************}


end.
