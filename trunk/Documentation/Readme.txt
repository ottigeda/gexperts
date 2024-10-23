GExperts 1.3.25
Open Source Programming Tools for Delphi and C++Builder

Source code, the FAQ, and the latest news are available at:
  https://gexperts.dummzeuch.de/
  https://sourceforge.net/projects/gexperts/
Additional (older) information at
  http://www.gexperts.org/

Please send all bug reports and suggestions using the feedback wizard
available in the GExperts about box.  

For other GExperts related queries use the GExperts subforum in Delphi Praxis at
https://en.delphipraxis.net/forum/31-gexperts


INTRODUCTION
----------------------
GExperts is a set of tools built to increase the productivity of
Delphi and C++Builder programmers by adding several features to the
IDE.  GExperts is developed as Open Source software and is contributed
as freeware to the development community.  Users are encouraged to
download the source code and submit bug fixes and patches as well as
new features for inclusion in the GExperts distribution.


THE EXPERTS
----------------------
GExperts contains numerous experts.  See the online help
(GExperts.chm) for information on the usage of the experts.

The source code formatter expert is based on DelForExp by Egbert van Nes
who contributed it to GExperts.
http://web.archive.org/web/20121115141650/http://www.aew.wur.nl/UK/Delforexp

KNOWN LIMITATIONS/BUGS
-----------------------------------------------
- The following features are not supported under Delphi 8 - RAD Studio 10.2:
  Inner classes, class helpers, anonymous methods, and generics in the
  Class Browser, and some of the old editor tab enhancements, such as
  multiline editor tabs.  Note that the IDE converts some high ANSI and MBCS
  characters to UTF-8 UNICODE when loaded in the code editor, so you may
  experience problems with those files (especially in Delphi 2007 and earlier).
- The compiler replacement option of the Code Proofreader is only partially
  working under Delphi 7 - RAD Studio 10.  It does not correct when an identifier is
  terminated by pressing a symbol key such as a period or open parenthesis,
  but works fine when the symbol is terminated by a space (IDE limitation).
- Due to either native Open Tools API limitations or bugs, the following
  items can not be supported:
  Delphi 8 Only:
   - Rename Components (IDE bug setting the Name property)
   - Jumping to a form search match from Grep Results when the source is open
     (IDE bug)
  Delphi 2005 Only:
   - The Project Option Sets environment options (IDE bug)
   - Set Tab Order: Selecting components in the desired tab order (IDE bug)
  Delphi 8 - 2007:
   - Any interaction at all with the WinForms/.NET form designer/components
     (no WinForms support for IOTAFormEditor/IOTAComponent)
   - Replace Components for VCL.NET (No direct access to components)
   - Components to Code for VCL.NET (No direct access to components)
  Delphi 8 - RAD Studio 10:
   - It is no longer possible for addins to override some built-in IDE
     shortcuts.  You may need to configure your GExperts shortcuts (Prev/Next
     Identifier, Procedure List, etc.) to not conflict with your selected
     keymapping.  Also, the Macro Library can not always intercept the
     Shift+Ctrl+R keystroke to automatically grab keyboard macros, so you may
     need to create macros using the recording functions in the toolbar.
  Delphi 2009 - 2010 Update 1:
   - Record and playback in the Marco Library does not work.  This is an IDE
     bug handling editor macro streams (QC 78289).
  Delphi XE3 - 10.3 Update 3:
   - Replace Components fails for components that contain sub-components, e.g.
     TTable with static fields. This is an IDE bug (RSP25645).
	 Until this IDE bug has been fixed, this functionality has been disabled.


INSTALLATION
----------------------
GExperts is distributed as a self-installing executable that should
automatically install itself into the IDE.  If you are installing under
Vista as a non-administrator, you may need to manually register the GExperts
DLL with the IDE after installation.  This is most easily done using the
Expert Manager tool in the GExperts start menu group.  If you prefer, you
can also manually register the DLL with the IDE using the Windows registry
editor (RegEdit.exe).  Create a key similar to the following (the
version number appearing before "\Experts\" is IDE dependent):
HKEY_CURRENT_USER\Software\Embarcadero\BDS\23.0\Experts\ (RAD Studio 12)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\22.0\Experts\ (RAD Studio 11)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\21.0\Experts\ (RAD Studio 10.4)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\20.0\Experts\ (RAD Studio 10.3)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\19.0\Experts\ (RAD Studio 10.2)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\18.0\Experts\ (RAD Studio 10.1)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\17.0\Experts\ (RAD Studio 10)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\16.0\Experts\ (RAD Studio XE8)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\12.0\Experts\ (RAD Studio XE5)
HKEY_CURRENT_USER\Software\Embarcadero\BDS\9.0\Experts\  (RAD Studio XE2)
HKEY_CURRENT_USER\Software\CodeGear\BDS\6.0\Experts\     (RAD Studio 2009)
HKEY_CURRENT_USER\Software\Borland\BDS\5.0\Experts\      (RAD Studio 2007)
HKEY_CURRENT_USER\Software\Borland\Delphi\7.0\Experts\   (Delphi 7)
HKEY_CURRENT_USER\Software\Borland\Delphi\6.0\Experts\   (Delphi 6)

Then add a new string value that points to your GExperts DLL (X is the IDE
version such as 6, 7, 8, 2005, 2006, 2007, 2009, 2010, XE1, XE8, 10, etc.):
GExperts=C:\Program Files\GExperts\GExpertsRSX.dll
GExperts=C:\Program Files\GExperts\GExpertsDelphiX.dll
GExperts=C:\Program Files\GExperts\GExpertsBDSX.dll or
GExperts=C:\Program Files\GExperts\GExpertsDX.dll or

Before installing GExperts, it is strongly recommended that you install
the latest updates and patches for your IDE available from:
  http://www.embarcadero.com/


UNINSTALLATION
----------------------
Uninstallation of GExperts can be done from the Control Panel's
Add/Remove Programs or Programs [and Features] tool.


CHANGE LOG
----------------------
Sorry, I have not maintained the change log since I took over from
Eric Berry. There were many. In particular of course support for the
latest released versions of RAD Studio / Delphi.
-- Thomas Mueller
You can see the changes from 2006 to 2015 in the file
changelog(2006-2015).txt


FUTURE ENHANCEMENTS?
----------------------
- [Unassigned] Persistent editor bookmarks
- [Unassigned] Integrate file auto-save code
- [Unassigned] Add compile date to version information and sync
  FileVersion with ProductVersion
- [Unassigned] Add GX_MessageBox warnings to the Pascal only experts
  when used under C++Builder
- [Unassigned] Replace SortGrid with a TListView and make the various
  TListViews sortable
- [Unassigned] Allow adding some common editor and object inspector
  actions to the editor toolbar?
- [Unassigned] Add support for hiding the toolbar on all dockable experts
- [Unassigned] Add word wrapped printing to the To Do List, etc.


MISCELLANEOUS NOTES
----------------------
- If a DFM is opened as text from the Grep Results (for example), you
  may experience an IDE bug where it forgets the value of the Text DFM
  flag on the form and converts it to a binary format.
- The Code Proofreader can only perform corrections when the editor caret
  is directly after the word to correct following a change.  As a result,
  pressing enter will not trigger corrections.
- Changing the configured GExperts top-level menu item location requires
  an IDE restart before the new setting is active.
- The editor toolbar does not support separators when aligned left/right.
- If a DFM is opened as text, a subsequent Grep Search can not scan the
  DFM's associated source files.  Delphi does not provide access to the
  source file buffer.  Under C++Builder you might also see duplicated
  matches for the DFM file when it is being viewed as text.
- Perfect Layout was designed for Delphi 3, and as such will probably
  never support docking or undocking of windows in later version IDEs.
  Please use the desktop saving feature of your IDE instead.
- Nothing in GExperts is aware of or takes into account conditional
  defines, so code that is defined out will still be parsed by GExperts.
- Source Export only supports background colors when exporting to HTML.


GEXPERTS LICENSE
----------------------

GExperts is copyright 1996-2015 by GExperts, Inc, Erik Berry, and several
other authors who have submitted their code for inclusion. This license
agreement only covers code written by GExperts, Inc. and Erik Berry. You
should contact the other authors concerning their respective copyrights
and conditions.

The rules governing the use of GExperts and the GExperts source code are
derived from the official Open Source Definition, available at
http://opensource.org/osd

The conditions and limitations are as follows:

Usage of GExperts binary distributions is permitted for all developers.
You may not use the GExperts source code to develop proprietary or
commercial products including plugins or libraries for those products.
You may use the GExperts source code in an Open Source project, under
the terms listed below.

You may not use the GExperts source code to create and distribute custom
versions of GExperts under the "GExperts" name.  If you do modify and
distribute custom versions of GExperts, the binary distribution must be
named differently and clearly marked so users can tell they are not using
the official GExperts distribution.  A visible and unmodified version of
this license must appear in any modified distribution of GExperts.

Custom distributions of GExperts must include all of the custom changes
as a patch file that can be applied to the original source code.  This
restriction is in place to protect the integrity of the original author's
source code.  No support for modified versions of GExperts will be
provided by the original authors or on the GExperts mailing lists.

All works derived from GExperts must be distributed under a license
compatible with this license and the official Open Source Definition,
which can be obtained from http://www.opensource.org/.

Please note that GExperts, Inc. and the other contributing authors hereby
state that this package is provided "as is" and without any express or
implied warranties, including, but not without limitation, the implied
warranties of merchantability and fitness for a particular purpose. In
other words, we accept no liability for any damage that may result from
using GExperts or programs that use the GExperts source code.

EOF
