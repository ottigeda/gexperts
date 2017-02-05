{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_Strings.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2002-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JeExpert_Strings;

interface

resourcestring
  //-----------------------------------------------------------------////////////////////////////////
  //Misc strings
  //-----------------------------------------------------------------////////////////////////////////

  RC_CRLF = #13#10;
  RC_CRLFStr = '#13#10';

  RC_MenuName = 'JExperts';

  RC_SebEmail = 'sbuysse@buypin.com';
  RC_BrunoEmail = 'bruno@buypin.com';
  RC_EricEmail = 'eric@buypin.com';
  RC_SubjectEmail = 'JEDI Experts';
  RC_ThanksUsing = 'Thank you for using our software !';

  //-----------------------------------------------------------------////////////////////////////////
  //Stat messages
  //-----------------------------------------------------------------////////////////////////////////

  RC_StatEmail = 'Sending Email';
  RC_StatSurfing = 'Surfing to ';
  RC_StatAddedGuid = 'GUID Generated';
  RC_StatNoCompoSelected = 'Unabled to process operation, you haven''t selected any component !';
  RC_StatParentAdded = 'Panel added as parent of the selected component(s)';
  RC_StatSaveOptions = 'Saving and applying new options.';
  RC_StatSaveAllFiles = 'Saving all your files ...';
  RC_StatSaveFile = 'Saving current file ...';
  RC_StatSearchingFiles = 'Searching for files ...';
  RC_StatCompiling = 'Compiling ...';

  //-----------------------------------------------------------------////////////////////////////////
  //Error messages
  //-----------------------------------------------------------------////////////////////////////////

  //Base form
  RCE_MustBeCode = 'This expert can only be executed if you are editing a unit !.';
  RCE_LayoutExists = 'The name you entered already exists in the list!';

  //Case of errors
  RCE_NoVariable = 'You must specify a variable name!';
  RCE_NoCaseItems = 'There isn''t any item in your case ! Please add some !';
  RCE_InvalidCase = 'Inavlid Case file';

  //FindFirst errors
  RCE_NoProcName = 'You must specify a procedure name !';
  RCE_NoOptions = 'You must complete all the options !';

  //Enumerations errors
  RCE_NoEnumName = 'You must enter an enumeration name !';
  RCE_NoSetName = 'You must enter a set name !';
  RCE_NoSetItems = 'There isn''t any item in your enumeration ! Please add some !';

  //Union errors
  RCE_NoUnionName = 'You must specify a name for the union !';
  RCE_NoUnionItems = 'There isn''t any item in your enumeration ! Please add some !';
  RCE_InvalidUnion = 'Invalid Union file !';

  //Record errors
  RCE_NoRecordName = 'You must specify a name for the record !';
  RCE_NoRecordItems = 'There isn''t any item in your record ! Please add some !';
  RCE_InvalidRecord = 'Invalid Record file!';

  //Form to html
  RCE_NoFileName = 'You must specify an output filename for the Html!';

  //Message dialogs
  RCE_AtLeastOneBtn = 'You must check at least one button to check the result !';

  //CRLF corrector
  RCE_AtLeastOneFile = 'You must choose at least one file to correct !';

  //Batch Compiler
  RCE_NoDirectory = 'You must choose the directory to start with !';

  //Directory Cleaner
  RCE_AtLeastOneExt = 'You must select at least one extension to delete !';

  //Add Code To Library
  RCE_MustSelectArea = 'You must select the area in which to save the code !';
  RCE_CodeNoName = 'You must enter a non empty name for the code';
  RCE_InvalidFileName = 'The name you gave is invalid, please enter another one';
  RCE_NoCodeInMemo = 'You must enter at least something in the code area!';

  //Header
  RCE_NoFile = 'You must specify a file to translate!';

  //-----------------------------------------------------------------////////////////////////////////
  //Hint messages
  //-----------------------------------------------------------------////////////////////////////////

  RCH_RightClickType = 'Right click on the item to change his type';
  RCH_RightClickCode = 'Right click on the item to change the generated code';
  RCH_DoubleClickAdd = 'Double click on an item to add it to the form.';
  RCH_NoSelection = 'There is no code selected in your unit, you''ll have to enter it now !';
  RCH_ThanksEmail = 'Thanks for your support reporting bugs!';

implementation

end.
