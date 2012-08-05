Building your own version of GExperts:

** PLEASE DO NOT DISTRIBUTE YOUR OWN VERSIONS WITHOUT CHANGING THE VERSION
INFORMATION FIRST! (see below) **

Once you have checked out all the source files, building your own version of
GExperts is quite simple:

1. switch to the projects folder
2. switch to the editorexperts or regularexperts folder, whichever you prefer
3. switch to the delphi version you are using
4. call __build_project.cmd

If I haven't made a mistake, this script should then compile a GExpertsXxx.dll
to the folder regularexpert or editorexpert respectively.
(Of course you need Delphi to be installed for this to work.)

To build all possible GExpert versions, you can just call __build.cmd in the
root of the source tree.
(Of course you need all necessary Delphi versions to be installed for this.)

To change the version information prior to building edit the files
* GExperts_version.ini in the projects directory
* GExperts_ExpertType_version.ini in the projects\editorexpert or
  projects\regularexpert respectively
These files are used by all projects so you only need to edit them.
