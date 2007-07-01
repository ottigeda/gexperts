library GExpertsD6_Formatter_EditorExpert;
{
GExperts License Agreement

GExperts is copyright 1996-2006 by GExperts, Inc, Erik Berry, and several
other authors who have submitted their code for inclusion. This license
agreement only covers code written by GExperts, Inc. and Erik Berry. You
should contact the other authors concerning their respective copyrights
and conditions.

The rules governing the use of GExperts and the GExperts source code are
derived from the official Open Source Definition, available at
http://www.opensource.org/. The conditions and limitations are as follows:

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

If you have license questions, please email Erik Berry at eberry@gexperts.org.
}

{$R 'Icons\GXIcons.res' 'Icons\GXIcons.rc'}

uses
  GX_LibrarySource in 'Src\GX_LibrarySource.pas',
  GX_Experts in 'Src\GX_Experts.pas',
  GX_GExperts in 'Src\GX_GExperts.pas',
  GX_ProcedureList in 'Src\GX_ProcedureList.pas' {fmProcedureList},
  GX_ProcedureListOptions in 'Src\GX_ProcedureListOptions.pas' {fmProcedureListOptions},
  GX_About in 'Src\GX_About.pas' {fmAbout},
  GX_ExpertManager in 'Src\GX_ExpertManager.pas' {fmExpertManager},
  GX_GrepResults in 'Src\Grep\GX_GrepResults.pas' {fmGrepResults},
  GX_Search in 'Src\Grep\GX_Search.pas',
  GX_GrepSearch in 'Src\Grep\GX_GrepSearch.pas' {fmGrepSearch},
  GX_GrepOptions in 'Src\Grep\GX_GrepOptions.pas' {fmGrepOptions},
  GX_MessageDialog in 'Src\GX_MessageDialog.pas' {fmMessageDialog},
  GX_MessageOptions in 'Src\GX_MessageOptions.pas' {fmMessageOptions},
  GX_EditReader in 'Src\GX_EditReader.pas',
  GX_Backup in 'Src\GX_Backup.pas' {fmBackup},
  GX_BackupOptions in 'Src\GX_BackupOptions.pas' {fmBackupOptions},
  GX_BackupConfig in 'Src\GX_BackupConfig.pas' {fmBackupConfig},
  GX_TabOrder in 'Src\GX_TabOrder.pas' {fmTabOrder},
  GX_CleanDirectories in 'Src\GX_CleanDirectories.pas' {fmCleanDirectories},
  GX_ClipboardHistory in 'Src\GX_ClipboardHistory.pas' {fmClipboardHistory},
  GX_ClipboardOptions in 'Src\GX_ClipboardOptions.pas' {fmClipboardOptions},
  GX_FavFiles in 'Src\FavFiles\GX_FavFiles.pas' {fmFavFiles},
  GX_FavOptions in 'Src\FavFiles\GX_FavOptions.pas' {fmFavOptions},
  GX_FavUtil in 'Src\FavFiles\GX_FavUtil.pas',
  GX_FavNewFolder in 'Src\FavFiles\GX_FavNewFolder.pas' {fmFavNewFolder},
  GX_FavFileProp in 'Src\FavFiles\GX_FavFileProp.pas' {fmFavFileProp},
  GX_ClassBrowser in 'Src\ClassBrowser\GX_ClassBrowser.pas' {fmClassBrowser},
  GX_ClassOptions in 'Src\ClassBrowser\GX_ClassOptions.pas' {fmClassOptions},
  GX_ClassReport in 'Src\ClassBrowser\GX_ClassReport.pas' {fmClassReport},
  GX_ClassProp in 'Src\ClassBrowser\GX_ClassProp.pas' {fmClassProp},
  GX_ClassIdentify in 'Src\ClassBrowser\GX_ClassIdentify.pas' {fmClassIdentify},
  GX_ClassParsing in 'Src\ClassBrowser\GX_ClassParsing.pas' {fmClassParsing},
  GX_SourceExport in 'Src\GX_SourceExport.pas' {fmSourceExport},
  GX_CodeLib in 'Src\CodeLib\GX_CodeLib.pas' {fmCodeLib},
  GX_CodeSrch in 'Src\CodeLib\GX_CodeSrch.pas' {fmCodeSearch},
  GX_CodeOpt in 'Src\CodeLib\GX_CodeOpt.pas' {fmCodeOptions},
  GX_Progress in 'Src\GX_Progress.pas' {fmProgress},
  GX_AsciiChart in 'Src\GX_AsciiChart.pas' {fmAsciiChart},
  GX_PeInformation in 'Src\GX_PeInformation.pas' {fmPeInformation},
  GX_PeInfo in 'Src\GX_PeInfo.pas',
  GX_ComponentGrid in 'Src\GX_ComponentGrid.pas' {fmComponentGrid},
  GX_IdeShortCuts in 'Src\GX_IdeShortCuts.pas' {fmIdeShortCuts},
  GX_ProjDepend in 'Src\GX_ProjDepend.pas' {fmProjDepend},
  GX_ProjDependProp in 'Src\GX_ProjDependProp.pas' {fmProjDependProp},
  GX_PerfectLayout in 'Src\GX_PerfectLayout.pas' {fmPerfectLayout},
  GX_ToDo in 'Src\ToDo\GX_ToDo.pas' {fmToDo},
  GX_ToDoOptions in 'Src\ToDo\GX_ToDoOptions.pas' {fmToDoOptions},
  GX_EditorExpert in 'Src\EditorExperts\GX_EditorExpert.pas',
  GX_eComment in 'Src\EditorExperts\GX_eComment.pas' {fmCommentConfig},
  GX_eDate in 'Src\EditorExperts\GX_eDate.pas' {fmDateFormat},
  GX_eFindDelimiter in 'Src\EditorExperts\GX_eFindDelimiter.pas',
  GX_EditorShortcut in 'Src\GX_EditorShortcut.pas' {fmEditorShortcut},
  GX_MenusForEditorExpert in 'Src\EditorExperts\GX_MenusForEditorExpert.pas',
  GX_ProofreaderExpert in 'Src\Proofreader\GX_ProofreaderExpert.pas',
  GX_ProofreaderConfig in 'Src\Proofreader\GX_ProofreaderConfig.pas' {fmProofreaderConfig},
  GX_ProofreaderCorrection in 'Src\Proofreader\GX_ProofreaderCorrection.pas',
  GX_ProofreaderUtils in 'Src\Proofreader\GX_ProofreaderUtils.pas',
  GX_ProofreaderKeyboard in 'Src\Proofreader\GX_ProofreaderKeyboard.pas',
  GX_ProofreaderDefaults in 'Src\Proofreader\GX_ProofreaderDefaults.pas',
  GX_IdeEnhance in 'Src\GX_IdeEnhance.pas',
  GX_Toolbar in 'Src\EditorToolbar\GX_Toolbar.pas',
  GX_IdeDock in 'Src\IdeIntegration\Native\GX_IdeDock.pas' {fmIdeDockForm},
  GX_ConfigurationInfo in 'Src\GX_ConfigurationInfo.pas',
  GX_Configure in 'Src\GX_Configure.pas' {fmConfiguration},
  GX_MultiLinePalette in 'Src\GX_MultiLinePalette.pas',
  GX_EditorEnhancements in 'Src\GX_EditorEnhancements.pas',
  GX_ProjOptionSets in 'Src\ProjOptionSets\GX_ProjOptionSets.pas' {fmProjOptionSets},
  GX_ProjOptMap in 'Src\ProjOptionSets\GX_ProjOptMap.pas',
  GX_MultilineHost in 'Src\GX_MultilineHost.pas',
  GX_VerDepConst in 'Src\GX_VerDepConst.pas',
  GX_CompsToCode in 'Src\GX_CompsToCode.pas' {fmCompsToCode},
  GX_MessageBox in 'Src\GX_MessageBox.pas' {fmGxMessageBox},
  GX_OtaUtils in 'Src\Utils\GX_OtaUtils.pas',
  GX_GenericUtils in 'Src\Utils\GX_GenericUtils.pas',
  GX_SynMemoUtils in 'Src\GX_SynMemoUtils.pas',
  GX_EditorExpertManager in 'Src\EditorExperts\GX_EditorExpertManager.pas',
  GX_KbdShortCutBroker in 'Src\GX_KbdShortCutBroker.pas',
  GX_GenericClasses in 'Src\Utils\GX_GenericClasses.pas',
  GX_IdeUtils in 'Src\Utils\GX_IdeUtils.pas',
  GX_ToolBarDropDown in 'Src\EditorToolbar\GX_ToolBarDropDown.pas',
  GX_ActionBroker in 'Src\GX_ActionBroker.pas',
  GX_EditorFormServices in 'Src\GX_EditorFormServices.pas',
  GX_KibitzComp in 'Src\GX_KibitzComp.pas',
  GX_MenuActions in 'Src\GX_MenuActions.pas',
  GX_ClassHacks in 'Src\GX_ClassHacks.pas',
  GX_EditorChangeServices in 'Src\GX_EditorChangeServices.pas',
  GX_ToolbarConfig in 'Src\EditorToolbar\GX_ToolbarConfig.pas' {fmToolbarConfig},
  GX_IconMessageBox in 'Src\GX_IconMessageBox.pas',
  GX_GxUtils in 'Src\Utils\GX_GxUtils.pas',
  GX_FavFolderProp in 'Src\FavFiles\GX_FavFolderProp.pas' {fmFavFolderProperties},
  GX_GetIdeVersion in 'Src\GX_GetIdeVersion.pas',
  GX_DbugIntf in 'Src\GX_DbugIntf.pas',
  GX_ClassMgr in 'Src\GX_ClassMgr.pas',
  GX_Consts in 'Src\GX_Consts.pas',
  GX_Actions in 'Src\GX_Actions.pas',
  GX_GrepExpert in 'Src\Grep\GX_GrepExpert.pas',
  GX_GrepBackend in 'Src\Grep\GX_GrepBackend.pas',
  GX_GrepPrinting in 'Src\Grep\GX_GrepPrinting.pas',
  GX_UsesManager in 'Src\GX_UsesManager.pas',
  GX_ePrevNextIdentifier in 'Src\EditorExperts\GX_ePrevNextIdentifier.pas',
  GX_SourceExportOptions in 'Src\GX_SourceExportOptions.pas' {fmSourceExportOptions},
  GX_FeedbackWizard in 'Src\GX_FeedbackWizard.pas' {fmFeedbackWizard},
  GX_MacroTemplatesExpert in 'Src\MacroTemplates\GX_MacroTemplatesExpert.pas',
  GX_MacroTemplateEdit in 'Src\MacroTemplates\GX_MacroTemplateEdit.pas' {fmMacroTemplateEdit},
  GX_MacroFile in 'Src\MacroTemplates\GX_MacroFile.pas',
  GX_MacroTemplates in 'Src\MacroTemplates\GX_MacroTemplates.pas' {fmMacroTemplates},
  GX_MacroSelect in 'Src\MacroTemplates\GX_MacroSelect.pas' {fmMacroSelect},
  GX_MacroExpandNotifier in 'Src\MacroTemplates\GX_MacroExpandNotifier.pas',
  GX_XmlUtils in 'Src\Utils\GX_XmlUtils.pas',
  GX_SharedImages in 'Src\GX_SharedImages.pas' {dmSharedImages: TDataModule},
  GX_eSelectionEditorExpert in 'Src\EditorExperts\GX_eSelectionEditorExpert.pas',
  GX_eReverseStatement in 'Src\EditorExperts\GX_eReverseStatement.pas',
  GX_CompRename in 'Src\GX_CompRename.pas' {fmCompRename},
  GX_CompRenameConfig in 'Src\GX_CompRenameConfig.pas' {fmCompRenameConfig},
  GX_CompRenameAdvanced in 'Src\GX_CompRenameAdvanced.pas' {fmCompRenameAdvanced},
  GX_DesignerMenu in 'Src\GX_DesignerMenu.pas',
  GX_CopyComponentNames in 'Src\GX_CopyComponentNames.pas',
  GX_GrepResultsOptions in 'Src\Grep\GX_GrepResultsOptions.pas' {fmGrepResultsOptions},
  GX_Replace in 'Src\Grep\GX_Replace.pas',
  GX_GrepReplace in 'Src\Grep\GX_GrepReplace.pas' {fmGrepReplace},
  GX_ProjDependFilter in 'Src\GX_ProjDependFilter.pas' {fmProjDependFilter},
  GX_eChangeCase in 'Src\EditorExperts\GX_eChangeCase.pas' {fmChangeCase},
  GX_eSelectIdentifier in 'Src\EditorExperts\GX_eSelectIdentifier.pas',
  GX_UnitPositions in 'Src\GX_UnitPositions.pas',
  GX_eUsesManager in 'Src\EditorExperts\GX_eUsesManager.pas' {fmUsesManager},
  GX_Zipper in 'Src\GX_Zipper.pas',
  GX_ProofreaderData in 'Src\Proofreader\GX_ProofreaderData.pas',
  GX_ProofreaderAutoCorrectEntry in 'Src\Proofreader\GX_ProofreaderAutoCorrectEntry.pas' {fmProofreaderAutoCorrectEntry},
  GX_MacroParser in 'Src\GX_MacroParser.pas',
  GX_ReplaceCompUtils in 'Src\ReplaceComponents\GX_ReplaceCompUtils.pas',
  GX_ReplaceComp in 'Src\ReplaceComponents\GX_ReplaceComp.pas' {fmReplaceComp},
  GX_ReplaceCompData in 'Src\ReplaceComponents\GX_ReplaceCompData.pas',
  GX_ReplaceCompLog in 'Src\ReplaceComponents\GX_ReplaceCompLog.pas' {fmReplaceCompLog},
  GX_ReplaceCompMapDets in 'Src\ReplaceComponents\GX_ReplaceCompMapDets.pas' {fmReplaceCompMapDets},
  GX_ReplaceCompMapGrpList in 'Src\ReplaceComponents\GX_ReplaceCompMapGrpList.pas' {fmReplaceCompMapGrpList},
  GX_ReplaceCompMapList in 'Src\ReplaceComponents\GX_ReplaceCompMapList.pas' {fmReplaceCompMapList},
  GX_FindComponentRef in 'Src\GX_FindComponentRef.pas',
  GX_SetComponentProps in 'Src\SetComponentProps\GX_SetComponentProps.pas',
  GX_SetComponentPropsStatus in 'Src\SetComponentProps\GX_SetComponentPropsStatus.pas' {fmSetComponentPropsStatus},
  GX_SetComponentPropsConfig in 'Src\SetComponentProps\GX_SetComponentPropsConfig.pas' {fmSetComponentPropsConfig},
  GX_OpenFile in 'Src\OpenFile\GX_OpenFile.pas' {fmOpenFile},
  GX_OpenFileConfig in 'Src\OpenFile\GX_OpenFileConfig.pas' {fmOpenFileConfig},
  GX_eAlign in 'Src\EditorExperts\GX_eAlign.pas' {fmAlign},
  GX_eAlignOptions in 'Src\EditorExperts\GX_eAlignOptions.pas' {fmAlignOptions},
  GX_FileScanner in 'Src\GX_FileScanner.pas',
  GX_MacroLibrary in 'Src\GX_MacroLibrary.pas' {fmMacroLibrary},
  GX_Formatter in 'Src\Formatter\GX_Formatter.pas',
  GX_CodeFormatterBreakpoints in 'Src\Formatter\GX_CodeFormatterBreakpoints.pas',
  GX_CodeFormatterConfig in 'Src\Formatter\GX_CodeFormatterConfig.pas' {fmCodeFormatterConfig},
  GX_CodeFormatterConfigHandler in 'Src\Formatter\GX_CodeFormatterConfigHandler.pas',
  GX_CodeFormatterDefaultSettings in 'Src\Formatter\GX_CodeFormatterDefaultSettings.pas',
  GX_CodeFormatterDone in 'Src\Formatter\GX_CodeFormatterDone.pas' {fmCodeFormatterDone},
  GX_CodeFormatterEditCapitalization in 'Src\Formatter\GX_CodeFormatterEditCapitalization.pas' {fmCodeFormatterEditCapitalization},
  GX_CodeFormatterExpert in 'Src\Formatter\GX_CodeFormatterExpert.pas',
  GX_CodeFormatterGXConfigWrapper in 'Src\Formatter\GX_CodeFormatterGXConfigWrapper.pas',
  GX_eCodeFormatter in 'Src\Formatter\GX_eCodeFormatter.pas',
  GX_CodeFormatterBookmarks in 'Src\Formatter\GX_CodeFormatterBookmarks.pas',
  GX_CollectionLikeLists in 'Src\Formatter\common\GX_CollectionLikeLists.pas',
  GX_CodeFormatterTypes in 'Src\Formatter\common\GX_CodeFormatterTypes.pas',
  GX_CodeFormatterTokens in 'Src\Formatter\engine\GX_CodeFormatterTokens.pas',
  GX_CodeFormatterFormatter in 'Src\Formatter\engine\GX_CodeFormatterFormatter.pas',
  GX_CodeFormatterParser in 'Src\Formatter\engine\GX_CodeFormatterParser.pas',
  GX_CodeFormatterSettings in 'Src\Formatter\engine\GX_CodeFormatterSettings.pas',
  GX_CodeFormatterStack in 'Src\Formatter\engine\GX_CodeFormatterStack.pas',
  GX_CodeFormatterEngine in 'Src\Formatter\engine\GX_CodeFormatterEngine.pas';

{$E dll}

{$R *.res}

end.

