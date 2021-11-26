unit GX_SharedImages;

{$I GX_CondDefine.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  ImgList,
  ImageList;

{$IFDEF IDE_IS_HIDPI_AWARE}
type
  PScaledImagesRec = ^TScaledImagesRec;
  TScaledImagesRec = record
    FDpi: Integer;
    FImages: TImageList;
    FDisabledImages: TImageList;
  end;
{$ENDIF}

type
  TdmSharedImages = class(TDataModule)
    Images: TImageList;
    DisabledImages: TImageList;
  private
{$IFDEF IDE_IS_HIDPI_AWARE}
    FScaledImages: array of TScaledImagesRec;
    function GetScaledRec(_DPI: Integer): PScaledImagesRec;
    function ResizeImagesforHighDPI(const ImgList: TImageList; _DPI: Integer): TImageList;
{$ENDIF}
  public
    function GetScaledImages(_DPI: Integer): TImageList;
    function GetScaledDisabledImageas(_DPI: Integer): TImageList;
  end;

const
  ImageIndexNew = 10;
  ImageIndexExpand = 12;
  ImageIndexContract = 13;
  ImageIndexInfo = 16;
  ImageIndexTrash = 30;
  ImageIndexFunction = 29;
  ImageIndexGear = 28;
  ImageIndexClosedFolder = 21;
  ImageIndexOpenFolder = 22;
  ImageIndexDocument = 23;
  ImageIndexArrow = 43;
  ImageIndexCheck = 46;
  ImageIndexBlank = 47;
  ImageIndexVisibility = 53;
  ImageIndexMemberType = 48;
  ImageIndexWindow = 57;
  ImageIndexWindows = 58;
  ImageIndexUnit = 59;
  ImageIndexToDoPriority = 71;

  ImageIndexVirtual = 82;
  ImageIndexDynamic = 83;
  ImageIndexOverride = 84;
  ImageIndexMessage = 85;

  ImageIndexInterface = 86;

function GetSharedImageList: TImageList;

implementation

{$R *.dfm}

uses
  Graphics,
  CommCtrl,
  u_dzMiscUtils,
  GX_GExperts;

function GetSharedImageList: TImageList;
begin
  Result := GExpertsInst.GetSharedImages;
end;

{ TdmSharedImages }

function TdmSharedImages.GetScaledDisabledImageas(_DPI: Integer): TImageList;
begin
{$IFDEF IDE_IS_HIDPI_AWARE}
  Result := GetScaledRec(_DPI).FDisabledImages;
{$ELSE}
  Result := DisabledImages;
{$ENDIF}
end;

function TdmSharedImages.GetScaledImages(_DPI: Integer): TImageList;
begin
{$IFDEF IDE_IS_HIDPI_AWARE}
  Result := GetScaledRec(_DPI).FImages;
{$ELSE}
  Result := Images;
{$ENDIF}
end;

{$IFDEF IDE_IS_HIDPI_AWARE}

function TdmSharedImages.GetScaledRec(_DPI: Integer): PScaledImagesRec;
var
  i: Integer;
begin
  for i := Low(FScaledImages) to High(FScaledImages) do begin
    if _DPI = FScaledImages[i].FDpi then begin
      Result := @(FScaledImages[i]);
      Exit; //==>
    end;
  end;
  i := Length(FScaledImages);
  SetLength(FScaledImages, i + 1);
  Result := @(FScaledImages[i]);
  Result.FDpi := _DPI;
  if _DPI = 96 then begin
    Result.FImages := Images;
    Result.FDisabledImages := DisabledImages;
  end else begin
    Result.FImages := ResizeImagesforHighDPI(Images, _DPI);
    Result.FDisabledImages := ResizeImagesforHighDPI(DisabledImages, _DPI);
  end;
end;

procedure ClearBmp(_cnv: TCanvas); inline;
begin
  _cnv.FillRect(_cnv.ClipRect);
end;

// taken from
// http://zarko-gajic.iz.hr/resizing-delphis-timagelist-bitmaps-to-fit-high-dpi-scaling-size-for-menus-toolbars-trees-etc/
// but heavily modified for readability and performance
function TdmSharedImages.ResizeImagesforHighDPI(const ImgList: TImageList; _DPI: Integer): TImageList;
var
  i: Integer;
  OrigBmp, OrigMask: TBitmap;
  ScaledBmp, ScaledMask: TBitmap;
  OrigWidth: Integer;
  OrigHeight: Integer;
  ScaledWidth: Integer;
  ScaledHeight: Integer;
begin
  Result := TImageList.Create(Self);

  //set size to match DPI size (like 250% of 16px = 40px)
  OrigWidth := ImgList.Width;
  OrigHeight := ImgList.Height;
  ScaledWidth := MulDiv(OrigWidth, _DPI, 96);
  ScaledHeight := MulDiv(OrigHeight, _DPI, 96);

  Result.SetSize(ScaledWidth, ScaledHeight);

  InitializeNil(OrigMask, OrigBmp, ScaledBmp, ScaledMask);
  try
    OrigBmp := TBitmap.Create;
    OrigMask := TBitmap.Create;
    OrigBmp.SetSize(OrigWidth, OrigHeight);
    OrigMask.SetSize(OrigWidth, OrigHeight);

    ScaledBmp := TBitmap.Create;
    ScaledMask := TBitmap.Create;
    ScaledBmp.SetSize(ScaledWidth, ScaledHeight);
    ScaledMask.SetSize(ScaledWidth, ScaledHeight);

    // add images stretched
    for i := 0 to ImgList.Count - 1 do begin
      ClearBmp(ScaledBmp.Canvas);
      ClearBmp(ScaledMask.Canvas);
      ClearBmp(OrigBmp.Canvas);
      ClearBmp(OrigMask.Canvas);

      ImageList_DrawEx(ImgList.Handle, i, OrigBmp.Canvas.Handle, 0, 0, OrigBmp.Width, OrigBmp.Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
      ImageList_DrawEx(ImgList.Handle, i, OrigMask.Canvas.Handle, 0, 0, OrigMask.Width, OrigMask.Height, CLR_NONE, CLR_NONE, ILD_MASK);

      ScaledBmp.Canvas.StretchDraw(Rect(0, 0, ScaledBmp.Width, ScaledBmp.Width), OrigBmp);
      ScaledMask.Canvas.StretchDraw(Rect(0, 0, ScaledMask.Width, ScaledMask.Width), OrigMask);
      Result.Add(ScaledBmp, ScaledMask);
    end;
  finally
    FreeAndNil(OrigMask, OrigBmp, ScaledBmp, ScaledMask);
  end;
end;
{$ENDIF}

end.
