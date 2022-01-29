unit u_dzDpiScaleUtilsDummy;

interface

uses
  SysUtils,
  Classes,
  Types,
  Controls,
  Forms;

type
  PScaledImagesRec = ^TScaledImagesRec;
  TScaledImagesRec = record
    FDpi: Integer;
    FImages: TImageList;
  end;

type
  TImageListScaler = class(TComponent)
  private
    FOriginal: TImageList;
  public
    constructor Create(_Owner: TComponent; _Original: TImageList); reintroduce;
    function GetScaledList(_DPI: Integer): TImageList;
  end;

type
  TFormDpiScaler = class
  public
    constructor Create(_Form: TForm);
    function Calc(_Value: Integer): Integer;
    procedure ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
  end;

implementation

{ TFormDpiScaler }

constructor TFormDpiScaler.Create(_Form: TForm);
begin
  inherited Create;
end;

procedure TFormDpiScaler.ApplyDpi(_NewDpi: Integer; _NewBounds: PRect);
begin
  // do nothing
end;

function TFormDpiScaler.Calc(_Value: Integer): Integer;
begin
  Result := _Value;
end;

{ TImageListScaler }

constructor TImageListScaler.Create(_Owner: TComponent; _Original: TImageList);
begin
  inherited Create(_Owner);
  FOriginal := _Original;
end;

function TImageListScaler.GetScaledList(_DPI: Integer): TImageList;
begin
  Result := FOriginal;
end;

end.

