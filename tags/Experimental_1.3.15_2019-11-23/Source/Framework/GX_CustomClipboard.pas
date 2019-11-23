unit GX_CustomClipboard;

interface

uses
  Windows;

type
  UInt32 = Cardinal;
  TByteDynArray = array of Byte;

function RegisterClipboardFormatW(lpszFormat: PWideChar): UINT; stdcall;
external user32 Name 'RegisterClipboardFormatW';

type
  /// NOTE: this class is meant as the ancestor implementing actual clipboard formats. </summary>
  TGXCustomClipboard = class
  protected
    FCustomFormat: UInt32;
    ///<summary>
    /// Writes the custom format to the clipboard
    /// @param Data is a dynamic array of byte with the data
    /// NOTE: We always prefix the acutal content with a UInt32 containing its size. </summary>
    procedure doWriteToClipboard(const _Data: TByteDynArray);
    ///<summary>
    /// Tries to read the custom format to the clipboard
    /// @param Data is dynamic array of byte with the data
    /// @returns True if the data could be read from the clipboard, false if not
    /// NOTE: We always expect the acutal content prefixed with a UInt32 containing its size. </summary>
    function doTryReadFromClipboard(out _Data: TByteDynArray): Boolean;
  public
    class function RegisterClipboardFormat(const _Name: WideString): UInt32;
    constructor Create(const _Name: WideString);
    destructor Destroy; override;
    function ClipboardHasFormat: Boolean;
  end;

implementation

uses
  Clipbrd,
  GX_dzMiscUtils;

{ TGXCustomClipboard }

class function TGXCustomClipboard.RegisterClipboardFormat(const _Name: WideString): UInt32;
begin
  Result := Windows.RegisterClipboardFormatW(PWideChar(_Name));
end;

constructor TGXCustomClipboard.Create(const _Name: WideString);
var
  Err: UInt32;
begin
  inherited Create;
  FCustomFormat := RegisterClipboardFormat(_Name);
  if FCustomFormat = 0 then begin
    Err := GetLastError;
    RaiseLastOsErrorEx(Err, 'Error %1:s (%0:d)');
  end;
end;

destructor TGXCustomClipboard.Destroy;
begin
  // nothing to do, there is no UnregisterClipboardFormat API function
  inherited;
end;

function TGXCustomClipboard.ClipboardHasFormat: Boolean;
begin
  Result := Clipboard.HasFormat(FCustomFormat);
end;

procedure TGXCustomClipboard.doWriteToClipboard(const _Data: TByteDynArray);
var
  Size: UInt32;
  MemHandle: HGLOBAL;
  MemPtr: PByte;
begin
  Size := Length(_Data);
  // Get a moveable memmory handle.
  // (according to the Windows API documentation of
  // SetClipboardData it must be moveable.)
  MemHandle := GlobalAlloc(GMEM_MOVEABLE, Size + SizeOf(UInt32));
  // To be able to write to such a handle, we must convert it to a pointer.
  MemPtr := GlobalLock(MemHandle);
  try
    Move(Size, MemPtr^, SizeOf(Size));
    Inc(MemPtr, SizeOf(Size));
    Move(_Data[0], PByte(MemPtr)^, Size);
  finally
    GlobalUnlock(MemHandle);
  end;
  // Now we call SetAsHandle to write it to the clipboard.
  Clipboard.SetAsHandle(FCustomFormat, MemHandle);
  // Since the memory is now stored in the clipboard,
  // we can not free it. So don't call GlobalFree here!
end;

function TGXCustomClipboard.doTryReadFromClipboard(out _Data: TByteDynArray): Boolean;
var
  MemHandle: THandle;
  MemPtr: PByte;
  Size: UInt32;
begin
  Result := ClipboardHasFormat;
  if not Result then
    Exit; //==>

  Clipboard.Open;
  try
    // Get the content.
    MemHandle := Clipboard.GetAsHandle(FCustomFormat);
    // Convert the memory handle to a pointer so we can access it.
    MemPtr := GlobalLock(MemHandle);
    Move(MemPtr^, Size, SizeOf(Size));
    Inc(MemPtr, SizeOf(Size));
    SetLength(_Data, Size);
    Move(MemPtr^, _Data[0], Size);
    GlobalUnlock(MemHandle);
  finally
    Clipboard.Close;
  end;
end;

end.
