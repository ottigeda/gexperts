{ GX_CondDefine.inc }

// Third-Party Product Defines
// --------------------------------------------------------------------------

// Define SYNEDIT below if you want to compile with the unicode SynEdit
// syntax highlighting text editor component.  It is required to use the
// Source Export tool and can optionally be used in several other places
// to show syntax highlighted code.  Note that an appropriately modified
// version of this component is included with the GExperts source.
// Delphi 2007 and earlier unconditionally require parts of this library to
// support TUnicodeStringList.  Home Page: http://mh-nexus.de/en/unisynedit.php

// Define GX_DEBUGLOG below to enable exception and SendDebug logging
// to <ModuleName>.debuglog.  This requires JCL 1.102 or later from
// http://sourceforge.net/projects/jcl and a detailed map file
// (see the linker options in the IDE).  Use sparingly, since this
// option will slow down the IDE.

{$IFNDEF GX_AutomatedBuild}
  // Turn things on we want compiled into GExperts
  {$DEFINE SYNEDIT}
  {.$DEFINE GX_DEBUGLOG}
{$ENDIF GX_AutomatedBuild}


// Miscellaneous GExperts Defines (do not change the items below!)
// ---------------------------------------------------------------

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IFDEF BCB}
    {$DEFINE GX_BCB}
  {$ELSE}
    {$DEFINE GX_Delphi}
  {$ENDIF}

  {$IF CompilerVersion >= 14}
    {$DEFINE GX_VER140_up} // Delphi 6
    {$IF CompilerVersion >= 15}
      {$DEFINE GX_VER150_up}  // Delphi 7
      {$IF CompilerVersion >= 16}
        {$DEFINE GX_VER160_up}  // Delphi 8
        {$IF CompilerVersion >= 17}
          {$DEFINE GX_VER170_up}  // Delphi 9/2005
          {$DEFINE SupportsInline}
          {$IF CompilerVersion >= 18}
            {$DEFINE GX_VER180_up}  // BDS 2006
            {$IFDEF VER185}
              {$DEFINE GX_VER185_up}  // Delphi 2007
            {$ENDIF}
            {$IF CompilerVersion >= 19}
              {$DEFINE GX_VER190_up}  // Delphi .NET 2007
              {$IF CompilerVersion >= 20}
                {$DEFINE GX_VER200_up}  // RAD Studio 2009
                {$IF CompilerVersion >= 21}
                  {$DEFINE GX_VER210_up}  // RAD Studio 2010
                  {$IF CompilerVersion >= 22}
                    {$DEFINE GX_VER220_up}  // RAD Studio XE 1
                    {$IF CompilerVersion >= 23}
                      {$DEFINE GX_VER230_up}  // RAD Studio XE 2
                      {$IF CompilerVersion >= 24}
                        {$DEFINE GX_VER240_up}  // RAD Studio XE 3
                      {$IFEND}
                    {$IFEND}
                  {$IFEND}
                {$IFEND}
              {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ELSE not CONDITIONALEXPRESSIONS}
  Sorry, but this version of GExperts does not support the IDE
  you are using. Please visit the GExperts web site at
  http://www.gexperts.org/ to obtain more information about
  support for your IDE.
{$ENDIF}

{$IFDEF LINUX}
Kylix is no longer supported.  You can try compiling with the GExperts source from early 2009.
{$ENDIF}

// Turn on IDE docking support
{$DEFINE EnableIdeDockingSupport}

// All IDEs support this feature to some degree (though Delphi 8+ do not support multiline editor tabs)
{$DEFINE GX_EditorEnhancements}

// Define GX_ENHANCED_EDITOR if SynEdit is available
{$IFDEF SYNEDIT}
  {$DEFINE GX_ENHANCED_EDITOR}
  {$DEFINE UniSynEdit}
{$ENDIF SYNEDIT}

{$IFOPT B+}
  GExperts will not work well with "Complete Boolean Evaluation" on.
  Please turn it off in the Project Options dialog.
{$ENDIF}

{$IFOPT C-}
  GExperts does not work well with assertions turned off.
  Please turn them on in the Project Options dialog.
{$ENDIF}

