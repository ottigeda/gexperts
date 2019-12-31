{$INCLUDE 'dzlibjedi.inc'}

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

