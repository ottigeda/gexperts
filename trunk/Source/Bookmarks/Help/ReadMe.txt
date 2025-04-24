===========================================================
Help files for the bookmarks expert
===========================================================

General
-----------------------------------------------------------
The individual experts typically use only a single help
page, which you can access in the main window of the 
expert. Unfortunately, the help currently only can be
created and assigned centrally by the GExperts-Team.

Since the Bookmarks Expert doesn't have any help so far,
I've come up with a special solution that allows several
help pages for each window of the bookmarks expert
without disrupting access to the default help pages
of other experts.


Provide help files
-----------------------------------------------------------
If you want to use my special help, 
you must follow these steps:

* Create the HelpFile BookmarksHelp.CHM with the
  HelpCompiler HHC.EXE (free download from MS available)

  e.g. xx\hhc.exe BookmarksHelp.hhp
  
* Copy/move then file BookmarksHelp.CHM to the GExperts
  UserDataDirectory (given in ConfigInfo.ConfigPath)
  
  e.g. c:\users\xxx\appdata\roaming\gexperts\delphiXXX
  
* By the way: You are free to modify the text of all
              htm files, but NEVER rename these files!
  
That's all :-)


Ralph Zornig
April 2025


