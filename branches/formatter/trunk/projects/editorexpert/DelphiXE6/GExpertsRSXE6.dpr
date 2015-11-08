library GExpertsRSXE6;

{$R 'GXIcons.res' '..\..\..\gx\images\GXIcons.rc'}
{$R *_version.res}
{$E dll}

uses
  GX_About in '..\..\..\gx\source\Framework\GX_About.pas' {fmAbout},
  GX_ActionBroker in '..\..\..\gx\source\Framework\GX_ActionBroker.pas',
  GX_Actions in '..\..\..\gx\source\Framework\GX_Actions.pas',
  GX_AsciiChart in '..\..\..\gx\source\Experts\GX_AsciiChart.pas' {fmAsciiChart},
  GX_Backup in '..\..\..\gx\source\BackupProject\GX_Backup.pas' {fmBackup},
  GX_BackupConfig in '..\..\..\gx\source\BackupProject\GX_BackupConfig.pas' {fmBackupConfig},
  GX_BackupOptions in '..\..\..\gx\source\BackupProject\GX_BackupOptions.pas' {fmBackupOptions},
  GX_BaseForm in '..\..\..\gx\source\Framework\GX_BaseForm.pas' {fmBaseForm},
  GX_ClassBrowser in '..\..\..\gx\source\ClassBrowser\GX_ClassBrowser.pas' {fmClassBrowser},
  GX_ClassHacks in '..\..\..\gx\source\Framework\GX_ClassHacks.pas',
  GX_ClassIdentify in '..\..\..\gx\source\ClassBrowser\GX_ClassIdentify.pas' {fmClassIdentify},
  GX_ClassMgr in '..\..\..\gx\source\Framework\GX_ClassMgr.pas',
  GX_ClassOptions in '..\..\..\gx\source\ClassBrowser\GX_ClassOptions.pas' {fmClassOptions},
  GX_ClassParsing in '..\..\..\gx\source\ClassBrowser\GX_ClassParsing.pas' {fmClassParsing},
  GX_ClassProp in '..\..\..\gx\source\ClassBrowser\GX_ClassProp.pas' {fmClassProp},
  GX_ClassReport in '..\..\..\gx\source\ClassBrowser\GX_ClassReport.pas' {fmClassReport},
  GX_CleanDirectories in '..\..\..\gx\source\Experts\GX_CleanDirectories.pas' {fmCleanDirectories},
  GX_ClipboardHistory in '..\..\..\gx\source\ClipboardHistory\GX_ClipboardHistory.pas' {fmClipboardHistory},
  GX_ClipboardOptions in '..\..\..\gx\source\ClipboardHistory\GX_ClipboardOptions.pas' {fmClipboardOptions},
  GX_CodeLib in '..\..\..\gx\source\CodeLibrarian\GX_CodeLib.pas' {fmCodeLib},
  GX_CodeOpt in '..\..\..\gx\source\CodeLibrarian\GX_CodeOpt.pas' {fmCodeOptions},
  GX_CodeSrch in '..\..\..\gx\source\CodeLibrarian\GX_CodeSrch.pas' {fmCodeSearch},
  GX_ComponentGrid in '..\..\..\gx\source\Experts\GX_ComponentGrid.pas' {fmComponentGrid},
  GX_CompRename in '..\..\..\gx\source\RenameComponents\GX_CompRename.pas' {fmCompRename},
  GX_CompRenameAdvanced in '..\..\..\gx\source\RenameComponents\GX_CompRenameAdvanced.pas' {fmCompRenameAdvanced},
  GX_CompRenameConfig in '..\..\..\gx\source\RenameComponents\GX_CompRenameConfig.pas' {fmCompRenameConfig},
  GX_CompsToCode in '..\..\..\gx\source\Experts\GX_CompsToCode.pas' {fmCompsToCode},
  GX_ConfigurationInfo in '..\..\..\gx\source\Framework\GX_ConfigurationInfo.pas',
  GX_Configure in '..\..\..\gx\source\Framework\GX_Configure.pas' {fmConfiguration},
  GX_Consts in '..\..\..\gx\source\Framework\GX_Consts.pas',
  GX_CopyComponentNames in '..\..\..\gx\source\Experts\GX_CopyComponentNames.pas',
  GX_DbugIntf in '..\..\..\gx\source\Framework\GX_DbugIntf.pas',
  GX_DesignerMenu in '..\..\..\gx\source\Framework\GX_DesignerMenu.pas',
  GX_eAlign in '..\..\..\gx\source\Editor\GX_eAlign.pas' {fmAlign},
  GX_eAlignOptions in '..\..\..\gx\source\Editor\GX_eAlignOptions.pas' {fmAlignOptions},
  GX_eChangeCase in '..\..\..\gx\source\Editor\GX_eChangeCase.pas' {fmChangeCase},
  GX_eComment in '..\..\..\gx\source\Editor\GX_eComment.pas' {fmCommentConfig},
  GX_eDate in '..\..\..\gx\source\Editor\GX_eDate.pas' {fmDateFormat},
  GX_EditorChangeServices in '..\..\..\gx\source\Framework\GX_EditorChangeServices.pas',
  GX_EditorEnhancements in '..\..\..\gx\source\Framework\GX_EditorEnhancements.pas',
  GX_EditorExpert in '..\..\..\gx\source\Editor\GX_EditorExpert.pas',
  GX_EditorExpertManager in '..\..\..\gx\source\Editor\GX_EditorExpertManager.pas',
  GX_EditorFormServices in '..\..\..\gx\source\Framework\GX_EditorFormServices.pas',
  GX_EditorShortcut in '..\..\..\gx\source\Framework\GX_EditorShortcut.pas' {fmEditorShortcut},
  GX_EditReader in '..\..\..\gx\source\Framework\GX_EditReader.pas',
  GX_eFindDelimiter in '..\..\..\gx\source\Editor\GX_eFindDelimiter.pas',
  GX_ePrevNextIdentifier in '..\..\..\gx\source\Editor\GX_ePrevNextIdentifier.pas',
  GX_eReverseStatement in '..\..\..\gx\source\Editor\GX_eReverseStatement.pas',
  GX_eSelectIdentifier in '..\..\..\gx\source\Editor\GX_eSelectIdentifier.pas',
  GX_eSelectionEditorExpert in '..\..\..\gx\source\Editor\GX_eSelectionEditorExpert.pas',
  GX_eUsesManager in '..\..\..\gx\source\Editor\GX_eUsesManager.pas' {fmUsesManager},
  GX_ExpertManager in '..\..\..\gx\source\Experts\GX_ExpertManager.pas' {fmExpertManager},
  GX_Experts in '..\..\..\gx\source\Framework\GX_Experts.pas',
  GX_FavFileProp in '..\..\..\gx\source\FavoriteFiles\GX_FavFileProp.pas' {fmFavFileProp},
  GX_FavFiles in '..\..\..\gx\source\FavoriteFiles\GX_FavFiles.pas' {fmFavFiles},
  GX_FavFolderProp in '..\..\..\gx\source\FavoriteFiles\GX_FavFolderProp.pas',
  GX_FavNewFolder in '..\..\..\gx\source\FavoriteFiles\GX_FavNewFolder.pas' {fmFavNewFolder},
  GX_FavOptions in '..\..\..\gx\source\FavoriteFiles\GX_FavOptions.pas' {fmFavOptions},
  GX_FavUtil in '..\..\..\gx\source\FavoriteFiles\GX_FavUtil.pas',
  GX_FeedbackWizard in '..\..\..\gx\source\Framework\GX_FeedbackWizard.pas' {fmFeedbackWizard},
  GX_FileScanner in '..\..\..\gx\source\Framework\GX_FileScanner.pas',
  GX_FindComponentRef in '..\..\..\gx\source\Experts\GX_FindComponentRef.pas',
  GX_GenericClasses in '..\..\..\gx\source\Utils\GX_GenericClasses.pas',
  GX_GenericUtils in '..\..\..\gx\source\Utils\GX_GenericUtils.pas',
  GX_GetIdeVersion in '..\..\..\gx\source\Framework\GX_GetIdeVersion.pas',
  GX_GExperts in '..\..\..\gx\source\Framework\GX_GExperts.pas',
  GX_GrepBackend in '..\..\..\gx\source\Grep\GX_GrepBackend.pas',
  GX_GrepExpert in '..\..\..\gx\source\Grep\GX_GrepExpert.pas',
  GX_GrepOptions in '..\..\..\gx\source\Grep\GX_GrepOptions.pas' {fmGrepOptions},
  GX_GrepPrinting in '..\..\..\gx\source\Grep\GX_GrepPrinting.pas',
  GX_GrepRegExSearch in '..\..\..\gx\source\Grep\GX_GrepRegExSearch.pas',
  GX_GrepReplace in '..\..\..\gx\source\Grep\GX_GrepReplace.pas' {fmGrepReplace},
  GX_GrepResults in '..\..\..\gx\source\Grep\GX_GrepResults.pas' {fmGrepResults},
  GX_GrepResultsOptions in '..\..\..\gx\source\Grep\GX_GrepResultsOptions.pas' {fmGrepResultsOptions},
  GX_GrepSearch in '..\..\..\gx\source\Grep\GX_GrepSearch.pas' {fmGrepSearch},
  GX_GxUtils in '..\..\..\gx\source\Utils\GX_GxUtils.pas',
  GX_HideNonVisualComps in '..\..\..\gx\source\Experts\GX_HideNonVisualComps.pas',
  GX_IconMessageBox in '..\..\..\gx\source\Framework\GX_IconMessageBox.pas',
  GX_IdeDock in '..\..\..\gx\source\IDEDocking\GX_IdeDock.pas' {fmIdeDockForm},
  GX_IdeEnhance in '..\..\..\gx\source\IDE\GX_IdeEnhance.pas',
  GX_IdeFormEnhancer in '..\..\..\gx\source\IDE\GX_IdeFormEnhancer.pas',
  GX_IdeShortCuts in '..\..\..\gx\source\Experts\GX_IdeShortCuts.pas' {fmIdeShortCuts},
  GX_IdeUtils in '..\..\..\gx\source\Utils\GX_IdeUtils.pas',
  GX_KbdShortCutBroker in '..\..\..\gx\source\Framework\GX_KbdShortCutBroker.pas',
  GX_KibitzComp in '..\..\..\gx\source\Framework\GX_KibitzComp.pas',
  GX_LibrarySource in '..\..\..\gx\source\Framework\GX_LibrarySource.pas',
  GX_MacroExpandNotifier in '..\..\..\gx\source\MacroTemplates\GX_MacroExpandNotifier.pas',
  GX_MacroFile in '..\..\..\gx\source\MacroTemplates\GX_MacroFile.pas',
  GX_MacroLibrary in '..\..\..\gx\source\MacroLibrary\GX_MacroLibrary.pas' {fmMacroLibrary},
  GX_MacroLibraryNamePrompt in '..\..\..\gx\source\MacroLibrary\GX_MacroLibraryNamePrompt.pas' {fmMacroLibraryNamePrompt},
  GX_MacroLibraryConfig in '..\..\..\gx\source\MacroLibrary\GX_MacroLibraryConfig.pas' {fmGxMacroLibraryConfig},
  GX_MacroParser in '..\..\..\gx\source\Framework\GX_MacroParser.pas',
  GX_MacroSelect in '..\..\..\gx\source\MacroTemplates\GX_MacroSelect.pas' {fmMacroSelect},
  GX_MacroTemplateEdit in '..\..\..\gx\source\MacroTemplates\GX_MacroTemplateEdit.pas' {fmMacroTemplateEdit},
  GX_MacroTemplates in '..\..\..\gx\source\MacroTemplates\GX_MacroTemplates.pas' {fmMacroTemplates},
  GX_MacroTemplatesExpert in '..\..\..\gx\source\MacroTemplates\GX_MacroTemplatesExpert.pas',
  GX_MenuActions in '..\..\..\gx\source\Framework\GX_MenuActions.pas',
  GX_MenusForEditorExpert in '..\..\..\gx\source\Editor\GX_MenusForEditorExpert.pas',
  GX_MessageBox in '..\..\..\gx\source\Framework\GX_MessageBox.pas' {fmGxMessageBox},
  GX_MessageDialog in '..\..\..\gx\source\MessageDialog\GX_MessageDialog.pas' {fmMessageDialog},
  GX_MessageOptions in '..\..\..\gx\source\MessageDialog\GX_MessageOptions.pas' {fmMessageOptions},
  GX_MultilineHost in '..\..\..\gx\source\IDE\GX_MultilineHost.pas',
  GX_MultiLinePalette in '..\..\..\gx\source\IDE\GX_MultiLinePalette.pas',
  GX_OpenFile in '..\..\..\gx\source\OpenFile\GX_OpenFile.pas' {fmOpenFile},
  GX_OpenFileConfig in '..\..\..\gx\source\OpenFile\GX_OpenFileConfig.pas' {fmOpenFileConfig},
  GX_OtaUtils in '..\..\..\gx\source\Utils\GX_OtaUtils.pas',
  GX_PeInfo in '..\..\..\gx\source\Experts\GX_PeInfo.pas',
  GX_PeInformation in '..\..\..\gx\source\Experts\GX_PeInformation.pas' {fmPeInformation},
  GX_PerfectLayout in '..\..\..\gx\source\Experts\GX_PerfectLayout.pas' {fmPerfectLayout},
  GX_ProcedureList in '..\..\..\gx\source\ProcedureList\GX_ProcedureList.pas' {fmProcedureList},
  GX_ProcedureListOptions in '..\..\..\gx\source\ProcedureList\GX_ProcedureListOptions.pas' {fmProcedureListOptions},
  GX_Progress in '..\..\..\gx\source\Framework\GX_Progress.pas' {fmProgress},
  GX_ProjDepend in '..\..\..\gx\source\ProjectDependencies\GX_ProjDepend.pas' {fmProjDepend},
  GX_ProjDependFilter in '..\..\..\gx\source\ProjectDependencies\GX_ProjDependFilter.pas' {fmProjDependFilter},
  GX_ProjDependProp in '..\..\..\gx\source\ProjectDependencies\GX_ProjDependProp.pas' {fmProjDependProp},
  GX_ProjOptionSets in '..\..\..\gx\source\ProjectOptionSets\GX_ProjOptionSets.pas' {fmProjOptionSets},
  GX_ProjOptMap in '..\..\..\gx\source\ProjectOptionSets\GX_ProjOptMap.pas',
  GX_ProofreaderAutoCorrectEntry in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderAutoCorrectEntry.pas' {fmProofreaderAutoCorrectEntry},
  GX_ProofreaderConfig in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderConfig.pas' {fmProofreaderConfig},
  GX_ProofreaderCorrection in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderCorrection.pas',
  GX_ProofreaderData in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderData.pas',
  GX_ProofreaderDefaults in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderDefaults.pas',
  GX_ProofreaderExpert in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderExpert.pas',
  GX_ProofreaderKeyboard in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderKeyboard.pas',
  GX_ProofreaderUtils in '..\..\..\gx\source\CodeProofreader\GX_ProofreaderUtils.pas',
  GX_Replace in '..\..\..\gx\source\Grep\GX_Replace.pas',
  GX_ReplaceComp in '..\..\..\gx\source\ReplaceComponents\GX_ReplaceComp.pas' {fmReplaceComp},
  GX_ReplaceCompData in '..\..\..\gx\source\ReplaceComponents\GX_ReplaceCompData.pas',
  GX_ReplaceCompLog in '..\..\..\gx\source\ReplaceComponents\GX_ReplaceCompLog.pas' {fmReplaceCompLog},
  GX_ReplaceCompMapDets in '..\..\..\gx\source\ReplaceComponents\GX_ReplaceCompMapDets.pas' {fmReplaceCompMapDets},
  GX_ReplaceCompMapGrpList in '..\..\..\gx\source\ReplaceComponents\GX_ReplaceCompMapGrpList.pas' {fmReplaceCompMapGrpList},
  GX_ReplaceCompMapList in '..\..\..\gx\source\ReplaceComponents\GX_ReplaceCompMapList.pas' {fmReplaceCompMapList},
  GX_ReplaceCompUtils in '..\..\..\gx\source\ReplaceComponents\GX_ReplaceCompUtils.pas',
  GX_SetComponentProps in '..\..\..\gx\source\SetComponentProperties\GX_SetComponentProps.pas',
  GX_SetComponentPropsConfig in '..\..\..\gx\source\SetComponentProperties\GX_SetComponentPropsConfig.pas' {fmSetComponentPropsConfig},
  GX_SetComponentPropsStatus in '..\..\..\gx\source\SetComponentProperties\GX_SetComponentPropsStatus.pas' {fmSetComponentPropsStatus},
  GX_SetFocusControl in '..\..\..\gx\source\Experts\GX_SetFocusControl.pas',
  GX_SharedImages in '..\..\..\gx\source\Framework\GX_SharedImages.pas' {dmSharedImages: TDataModule},
  GX_SourceExport in '..\..\..\gx\source\SourceExport\GX_SourceExport.pas' {fmSourceExport},
  GX_SourceExportOptions in '..\..\..\gx\source\SourceExport\GX_SourceExportOptions.pas' {fmSourceExportOptions},
  GX_SynMemoUtils in '..\..\..\gx\source\Framework\GX_SynMemoUtils.pas',
  GX_TabOrder in '..\..\..\gx\source\Experts\GX_TabOrder.pas' {fmTabOrder},
  GX_ToDo in '..\..\..\gx\source\ToDoList\GX_ToDo.pas' {fmToDo},
  GX_ToDoOptions in '..\..\..\gx\source\ToDoList\GX_ToDoOptions.pas' {fmToDoOptions},
  GX_Toolbar in '..\..\..\gx\source\EditorToolbar\GX_Toolbar.pas',
  GX_ToolbarConfig in '..\..\..\gx\source\EditorToolbar\GX_ToolbarConfig.pas' {fmToolbarConfig},
  GX_ToolBarDropDown in '..\..\..\gx\source\EditorToolbar\GX_ToolBarDropDown.pas',
  GX_UnitPositions in '..\..\..\gx\source\Framework\GX_UnitPositions.pas',
  GX_UsesManager in '..\..\..\gx\source\Framework\GX_UsesManager.pas',
  GX_VerDepConst in '..\..\..\gx\source\Framework\GX_VerDepConst.pas',
  GX_XmlUtils in '..\..\..\gx\source\Utils\GX_XmlUtils.pas',
  GX_Zipper in '..\..\..\gx\source\BackupProject\GX_Zipper.pas',
  GX_Formatter in '..\..\..\source\GX_Formatter.pas',
  GX_CodeFormatterBreakpoints in '..\..\..\source\GX_CodeFormatterBreakpoints.pas',
  GX_CodeFormatterConfig in '..\..\..\source\GX_CodeFormatterConfig.pas' {fmCodeFormatterConfig},
  GX_CodeFormatterConfigHandler in '..\..\..\source\GX_CodeFormatterConfigHandler.pas',
  GX_CodeFormatterDefaultSettings in '..\..\..\source\GX_CodeFormatterDefaultSettings.pas',
  GX_CodeFormatterDone in '..\..\..\source\GX_CodeFormatterDone.pas' {fmCodeFormatterDone},
  GX_CodeFormatterEditCapitalization in '..\..\..\source\GX_CodeFormatterEditCapitalization.pas' {fmCodeFormatterEditCapitalization},
  GX_CodeFormatterExpert in '..\..\..\source\GX_CodeFormatterExpert.pas',
  GX_CodeFormatterGXConfigWrapper in '..\..\..\source\GX_CodeFormatterGXConfigWrapper.pas',
  GX_eCodeFormatter in '..\..\..\source\GX_eCodeFormatter.pas',
  GX_CodeFormatterBookmarks in '..\..\..\source\GX_CodeFormatterBookmarks.pas',
  GX_CodeFormatterTokenList in '..\..\..\source\engine\GX_CodeFormatterTokenList.pas',
  GX_CodeFormatterTypes in '..\..\..\source\engine\GX_CodeFormatterTypes.pas',
  GX_CodeFormatterTokens in '..\..\..\source\engine\GX_CodeFormatterTokens.pas',
  GX_CodeFormatterFormatter in '..\..\..\source\engine\GX_CodeFormatterFormatter.pas',
  GX_CodeFormatterParser in '..\..\..\source\engine\GX_CodeFormatterParser.pas',
  GX_CodeFormatterSettings in '..\..\..\source\engine\GX_CodeFormatterSettings.pas',
  GX_CodeFormatterStack in '..\..\..\source\engine\GX_CodeFormatterStack.pas',
  GX_CodeFormatterEngine in '..\..\..\source\engine\GX_CodeFormatterEngine.pas',
  GX_AboutExperimental in '..\..\..\source\GX_AboutExperimental.pas' {fmAboutExperimental},
  GX_CodeFormatterRegisterExpert in '..\GX_CodeFormatterRegisterExpert.pas',
  GX_CodeFormatterUnicode in '..\..\..\source\engine\GX_CodeFormatterUnicode.pas',
  GX_HideNavbar in '..\..\..\gx\source\Experts\GX_HideNavbar.pas',
  GX_IdeSearchPathEnhancer in '..\..\..\gx\source\IDE\GX_IdeSearchPathEnhancer.pas',
  GX_dzVclUtils in '..\..\..\gx\source\Utils\GX_dzVclUtils.pas',
  GX_IdeProjectOptionsEnhancer in '..\..\..\gx\source\IDE\GX_IdeProjectOptionsEnhancer.pas',
  GX_PasteAs in '..\..\..\gx\source\Framework\GX_PasteAs.pas',
  GX_ePasteAs in '..\..\..\gx\source\Editor\GX_ePasteAs.pas' {fmPasteAsConfig},
  GX_IdeToolPropertiesEnhancer in '..\..\..\gx\source\IDE\GX_IdeToolPropertiesEnhancer.pas';

begin
end.

