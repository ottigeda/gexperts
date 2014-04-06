** Installing GExperts Experimental **

1. Download the official GExperts installer from http://www.gexperts.org
2. Run it. It will create a directory under %ProgramFiles%, copy all
   necessary files there and register the GExperts DLL with the Delphi IDE.
3. Test the installation by starting Delphi and check that GExperts is
   available in the IDE's menu.
4. Extract this ZIP file somewhere.
5. Copy all files from the extracted directory to the GExperts directory.
   (Do *NOT* copy the subdirectories!)
6. Copy the appropriate GExperts DLL from one of the subdirectories
   EditorExpert or RegularExpert to the GExperts directory.
7. Start Delphi and check that the new GExperts DLL has been loaded by
   opening the GExperts About dialog.

** Installing without an official installer **
   
With two Delphi Releases per year and Erik Berry being busy otherwise,
new GExperts releases have been lagging behind. So in case there is no
official GExperts installer yet, these are the steps to install the
experimental version by hand:

1. Create a suitable GExperts directory. It doesn't have to be a
   subdirectory of %ProgramFiles% but may be anywhere, even on a network
   share if you are sure this share will always be available.
2. Extract all files from the ZIP somewhere
3. Copy all files from the extracted directory to the GExperts directory.
   (Do *NOT* copy the subdirectories!)
4. Copy the appropriate GExperts DLL from one of the subdirectories
   EditorExpert or RegularExpert to the GExperts directory.
5. Copy the files from the subdirectory FromRegularRelease to the
   GExperts directory.
6. Copy the appropriate cmd from the install subdirectory. To the GExperts
   directory.
7. Make sure that the Delphi IDE is not running
8. Run the cmd file. It will register the GExperts dll with the Delphi IDE.
9. Start Delphi and check that the new GExperts DLL has been loaded by
   opening the GExperts About dialog.

In theory it is possible to install GExperts for all Delphi versions into
the same GExperts directory. But be warned: This has not been tested
thoroughly.
