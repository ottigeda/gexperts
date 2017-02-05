{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeExpert_CompoEditors.PAS, released on 2001-02-28.

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


unit JeExpert_CompoEditors;


interface

procedure RegisterComponentEditors;

implementation

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms,
  Dialogs,
  Registry,
  ToolsAPI,
  Menus,
  ComCtrls,
  Buttons,
  DesignEditors,
  DesignIntf,
  ExtCtrls,
  checklst,
  JeEditor_OpenDialog,
  JeEditor_FindDialog,
  JeEditor_ColorDialog,
  JeEditor_Panel,
  JeEditor_PrintDialog,
  JeEditor_PrintSetupDialog,
  JeEditor_FontDialog,
  JeEditor_RadioGroup,
  JeEditor_GroupBox,
  JeEditor_CheckBox,
  JeEditor_RadioButton,
  JeEditor_ListBox,
  JeEditor_Memo,
  JeEditor_CheckListBox,
  JeEditor_RichEdit,
  JeEditor_SpeedButton,
  JeEditor_BitBtn,
  JeEditor_Edit;

{*************************************************}
procedure RegisterComponentEditors;
begin
  RegisterComponentEditor(TOpenDialog,         TJeOpenDialogEditor);
  RegisterComponentEditor(TFindDialog,         TJeFindDialogEditor);
  RegisterComponentEditor(TColorDialog,        TJeColorDialogEditor);
  RegisterComponentEditor(TPrintDialog,        TJePrintDialogEditor);
  RegisterComponentEditor(TPrinterSetupDialog, TJePrinterSetupDialogEditor);
  RegisterComponentEditor(TFontDialog,         TJeFontDialogEditor);
  RegisterComponentEditor(TPanel,              TJePanelEditor);
  RegisterComponentEditor(TRadioGroup,         TJeRadioGroupBoxEditor);
  RegisterComponentEditor(TGroupBox,           TJeGroupBoxEditor);
  RegisterComponentEditor(TCheckBox,           TJeCheckBoxEditor);
  RegisterComponentEditor(TRadioButton,        TJeRadioButtonEditor);
  RegisterComponentEditor(TListBox,            TJeListBoxEditor);
  RegisterComponentEditor(TMemo,               TJeMemoEditor);
  RegisterComponentEditor(TCheckListBox,       TJeCheckListBoxEditor);
  RegisterComponentEditor(TRichEdit,           TJeRichEditEditor);
  RegisterComponentEditor(TSpeedButton,        TJeSpeedButtonEditor);
  RegisterComponentEditor(TBitBtn,             TJeBitBtnEditor);
  RegisterComponentEditor(TEdit,               TJeEditEditor);
end;
{*************************************************}
end.

