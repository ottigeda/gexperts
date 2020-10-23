{$INCLUDE 'dzlibjedi.inc'}

{$IFDEF RTL210_UP}
{$DEFINE HAS_JSON_SUPPORT}
{$ENDIF}

{$IFDEF RTL270_UP}
{$DEFINE HAS_UNIT_SYSTEM_JSON}
{$ENDIF}

{$IFDEF DELPHIXE7_UP}
// For now I assume the BDE is not installed for Delphi XE7 and later, but that
// largely depends on what the user selected for installation.
{$DEFINE BDE_IS_DEPRECATED}
{$DEFINE HAS_UNIT_NETENCODING}
{$ENDIF}

{$IFDEF DELPHI7_UP}
// TBitBtn.WordWrap was introduced in Delphi 7
{$DEFINE HAS_BITBTN_WORDWRAP}
// So was the $WARN USE_BEFORE_DEF define
{$DEFINE SUPPORTS_WARN_USE_BEFORE_DEF}
{$ENDIF}

{$IFDEF DELPHI2005_UP}
// Delphi 6 and 7 understood deprecated, but not for types
{$DEFINE SUPPORTS_DEPRECATED_TYPES}
{$DEFINE HAS_UNIT_WIDESTRINGS}
{$DEFINE OPENDIALOG_EXCUTE_HAS_HANDLE}
{$ENDIF}

{$IFDEF DELPHI2006_UP}
// TBitmap.SetSize was introduced with Delphi 2006
{$DEFINE SUPPPORTS_BITMAP_SETSIZE}
{$ENDIF}

{$IFDEF DELPHI2006_UP}
{$IFNDEF DELPHI2009_UP}
// Delphi 2006 introduced an overloaded version of TDataSet.GetFields which takes a TWideStringList
// parameter and deprecated the version that took TStrings. Delphi 2009 removed the deprecated
// warning again because TStrings is Unicode.
{$DEFINE DATASET_GETFIELDS_IS_WIDESTRINGLIST}
{$ENDIF}
{$ENDIF}

{$IFDEF DELPHI7_UP}
{$IFNDEF DELPHI2009_UP}
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
{$DEFINE HAS_TOBJECT_TOSTRING}
// TMonitor.PixelsPerInch probably didn't exist in Delphi 2009, it does exist in Delphi 10.2 adjust as necessary 
{$DEFINE HAS_TMONITOR_PIXELSPERINCH}
{$ENDIF}

{$IFDEF DELPHIXE_UP}
{$DEFINE HAS_UNIT_GENERICS_COLLECTION}
{$ENDIF}

{$IFDEF DELPHIXE2_UP}
// AHandle is declared as THandle (otherwise it's an Integer)
{$DEFINE THANDLESTREAM_CREATE_HANDLE_IS_THANDLE}
{$DEFINE MAXLISTSIZE_IS_DEPRECATED}
{$ENDIF}

{$IFDEF DELPHIXE6_UP}
{$DEFINE JSONOBJECT_HAS_COUNT}
{$ENDIF}

{$IFDEF DELPHIX_TOKYO_UP}
// this function probably already existed in earlier versions, I just run across a compile error for 10.2 Tokyo
{$DEFINE HAS_INTTOHEX_FUNCTION}
{$DEFINE HAS_INTTOHEX_FUNCTION_64}
{$ENDIF}

{$IFOPT T+}
{$DEFINE TYPEDADDRESS_IS_ON}
{$ENDIF}

// we currently support only Windows (and a very small subset of Win64), so we turn platform
// warnings off

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

