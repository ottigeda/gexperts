{$INCLUDE 'dzlibjedi.inc'}

{$IFDEF DELPHI7_UP}
// TBitBtn.WordWrap was introduced in Delphi 7
{$DEFINE HAS_BITBTN_WORDWRAP}
// So was the $WARN USE_BEFORE_DEF define
{$DEFINE SUPPORTS_WARN_USE_BEFORE_DEF}
{$ENDIF}

{$IFDEF DELPHI2005_UP}
// Delphi 6 and 7 understood deprecated, but not for types
{$DEFINE SUPPORTS_DEPRECATED_TYPES}
{$ENDIF}

{$IFDEF DELPHI2006_UP}
// TBitmap.SetSize was it introduced with Delphi 2006
{$DEFINE SUPPPORTS_BITMAP_SETSIZE}
{$ENDIF}

{$IFDEF DELPHI7_UP}
{$IFNDEF DELPHI_2009_UP}
// Between Delphi 7 and 2007 Native(U)Int is wrongly declared as a 64 bit integer even though
// the compiler only supports 32 bits.
{$DEFINE NATIVE_INT_IS_WRONG}
{$ENDIF}
{$ENDIF}

{$IFDEF DELPHIX_TOKYO_UP}
{$DEFINE FILECTRL_DIRECTORYEXISTS_IS_DEPRECATED}
{$ENDIF}

// The following cond. defines address errors in various Delphi versions regarding the declaration
// of the FHandle field of THandleStream and the corresponding Create constructor parameter:

{$IFDEF DELPHI2009_UP}
// THandleStream.FHandle is declared as THandle (before that it's an Integer)
{$DEFINE THANDLESTREAM_HANDLE_IS_THANDLE}
{$ENDIF}

{$IFDEF DELPHIXE2_UP}
// AHandle is declared as THandle (otherwise it's an Integer)
{$DEFINE THANDLESTREAM_CREATE_HANDLE_IS_THANDLE}
{$ENDIF}

{$IFDEF DELPHIX_TOKYO_UP}
// this function probably already exist in earlier versions, I just run across a compile error for 10.2 Tokyo
{$DEFINE HAS_INTTOHEX_FUNCTION}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

