// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterTokenList;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes,
  GX_StringList,
  GX_CodeFormatterTokens;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TPascalToken;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  {: List for storing TPascalToken items }
  TPascalTokenList = class(_DZ_OBJECT_LIST_TEMPLATE_)
  public
    procedure WriteTo(const _fn: string);
  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}

{ TPascalTokenList }

procedure TPascalTokenList.WriteTo(const _fn: string);
var
  i: Integer;
  T: System.TextFile;
begin
  AssignFile(T, _fn);
  Rewrite(T);
  for i := 0 to Count - 1 do begin
    WriteLn(T, {Format('%.3d ', [i]) + }Items[i].GetForDebug);
  end;
  CloseFile(T);
end;

end.

