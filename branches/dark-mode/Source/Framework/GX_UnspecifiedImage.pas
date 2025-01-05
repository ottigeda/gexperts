unit GX_UnspecifiedImage;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
  Classes;

procedure RegisterUnspecifiedBitmapWithIde;
function GxUnspecifiedImageIndex: Integer;

implementation

uses
  Graphics,
  ToolsAPI,
  GX_OtaUtils,
  GX_GxUtils;

var
  gblGxUnspecifiedImageIndex: Integer = -1;

procedure RegisterUnspecifiedBitmapWithIde;
const
  UnspecifiedName = 'Unspecified';
var
  Bitmap: TBitmap;
  NTAServices: INTAServices;
begin
  if IsStandAlone then
    Exit;

  if GxUnspecifiedImageIndex <> -1 then
    Exit;

  Bitmap := nil;
  try
    GxLoadBitmapForExpert(UnspecifiedName, Bitmap);
    if Bitmap = nil then
      Exit;

    NTAServices := BorlandIDEServices as INTAServices;
    Assert(Assigned(NTAServices));
    gblGxUnspecifiedImageIndex := NTAServices.AddMasked(Bitmap, Bitmap.TransparentColor, UnspecifiedName);
  finally
    FreeAndNil(Bitmap);
  end;
end;

function GxUnspecifiedImageIndex: Integer;
begin
  Result := gblGxUnspecifiedImageIndex;
end;

end.

