library GExpertsRSXE3;

{$R 'GXIcons.res' '..\Images\GXIcons.rc'}
{$R *.res}
{$E dll}

uses
  GX_About in 'Framework\GX_About.pas' {fmAbout},
  GX_ActionBroker in 'Framework\GX_ActionBroker.pas',
  GX_Actions in 'Framework\GX_Actions.pas',
  GX_AsciiChart in 'Experts\GX_AsciiChart.pas' {fmAsciiChart},
  GX_Backup in 'BackupProject\GX_Backup.pas' {fmBackup},
  GX_BackupConfig in 'BackupProject\GX_BackupConfig.pas' {fmBackupConfig},
  GX_BackupOptions in 'BackupProject\GX_BackupOptions.pas' {fmBackupOptions},
  GX_BaseForm in 'Framework\GX_BaseForm.pas' {fmBaseForm},
  GX_ClassBrowser in 'ClassBrowser\GX_ClassBrowser.pas' {fmClassBrowser},
  GX_ClassHacks in 'Framework\GX_ClassHacks.pas',
  GX_ClassIdentify in 'ClassBrowser\GX_ClassIdentify.pas' {fmClassIdentify},
  GX_ClassMgr in 'Framework\GX_ClassMgr.pas',
  GX_ClassOptions in 'ClassBrowser\GX_ClassOptions.pas' {fmClassOptions},
  GX_ClassParsing in 'ClassBrowser\GX_ClassParsing.pas' {fmClassParsing},
  GX_ClassProp in 'ClassBrowser\GX_ClassProp.pas' {fmClassProp},
  GX_ClassReport in 'ClassBrowser\GX_ClassReport.pas' {fmClassReport},
  GX_CleanDirectories in 'Experts\GX_CleanDirectories.pas' {fmCleanDirectories},
  GX_ClipboardHistory in 'ClipboardHistory\GX_ClipboardHistory.pas' {fmClipboardHistory},
  GX_ClipboardOptions in 'ClipboardHistory\GX_ClipboardOptions.pas' {fmClipboardOptions},
  GX_CodeLib in 'CodeLibrarian\GX_CodeLib.pas' {fmCodeLib},
  GX_CodeOpt in 'CodeLibrarian\GX_CodeOpt.pas' {fmCodeOptions},
  GX_CodeSrch in 'CodeLibrarian\GX_CodeSrch.pas' {fmCodeSearch},
  GX_ComponentGrid in 'Experts\GX_ComponentGrid.pas' {fmComponentGrid},
  GX_CompRename in 'RenameComponents\GX_CompRename.pas' {fmCompRename},
  GX_CompRenameAdvanced in 'RenameComponents\GX_CompRenameAdvanced.pas' {fmCompRenameAdvanced},
  GX_CompRenameConfig in 'RenameComponents\GX_CompRenameConfig.pas' {fmCompRenameConfig},
  GX_CompsToCode in 'Experts\GX_CompsToCode.pas' {fmCompsToCode},
  GX_ConfigurationInfo in 'Framework\GX_ConfigurationInfo.pas',
  GX_Configure in 'Framework\GX_Configure.pas' {fmConfiguration},
  GX_Consts in 'Framework\GX_Consts.pas',
  GX_CopyComponentNames in 'Experts\GX_CopyComponentNames.pas',
  GX_DbugIntf in 'Framework\GX_DbugIntf.pas',
  GX_DesignerMenu in 'Framework\GX_DesignerMenu.pas',
  GX_eAlign in 'Editor\GX_eAlign.pas' {fmAlign},
  GX_eAlignOptions in 'Editor\GX_eAlignOptions.pas' {fmAlignOptions},
  GX_eChangeCase in 'Editor\GX_eChangeCase.pas' {fmChangeCase},
  GX_eComment in 'Editor\GX_eComment.pas' {fmCommentConfig},
  GX_eDate in 'Editor\GX_eDate.pas' {fmDateFormat},
  GX_EditorChangeServices in 'Framework\GX_EditorChangeServices.pas',
  GX_EditorEnhancements in 'Framework\GX_EditorEnhancements.pas',
  GX_EditorExpert in 'Editor\GX_EditorExpert.pas',
  GX_EditorExpertManager in 'Editor\GX_EditorExpertManager.pas',
  GX_EditorFormServices in 'Framework\GX_EditorFormServices.pas',
  GX_EditorShortcut in 'Framework\GX_EditorShortcut.pas' {fmEditorShortcut},
  GX_EditReader in 'Framework\GX_EditReader.pas',
  GX_eFindDelimiter in 'Editor\GX_eFindDelimiter.pas',
  GX_ePrevNextIdentifier in 'Editor\GX_ePrevNextIdentifier.pas',
  GX_eReverseStatement in 'Editor\GX_eReverseStatement.pas',
  GX_eSelectIdentifier in 'Editor\GX_eSelectIdentifier.pas',
  GX_eSelectionEditorExpert in 'Editor\GX_eSelectionEditorExpert.pas',
  GX_eUsesManager in 'Editor\GX_eUsesManager.pas' {fmUsesManager},
  GX_ExpertManager in 'Experts\GX_ExpertManager.pas' {fmExpertManager},
  GX_Experts in 'Framework\GX_Experts.pas',
  GX_FavFileProp in 'FavoriteFiles\GX_FavFileProp.pas' {fmFavFileProp},
  GX_FavFiles in 'FavoriteFiles\GX_FavFiles.pas' {fmFavFiles},
  GX_FavFolderProp in 'FavoriteFiles\GX_FavFolderProp.pas',
  GX_FavNewFolder in 'FavoriteFiles\GX_FavNewFolder.pas' {fmFavNewFolder},
  GX_FavOptions in 'FavoriteFiles\GX_FavOptions.pas' {fmFavOptions},
  GX_FavUtil in 'FavoriteFiles\GX_FavUtil.pas',
  GX_FeedbackWizard in 'Framework\GX_FeedbackWizard.pas' {fmFeedbackWizard},
  GX_FileScanner in 'Framework\GX_FileScanner.pas',
  GX_FindComponentRef in 'Experts\GX_FindComponentRef.pas',
  GX_GenericClasses in 'Utils\GX_GenericClasses.pas',
  GX_GenericUtils in 'Utils\GX_GenericUtils.pas',
  GX_GetIdeVersion in 'Framework\GX_GetIdeVersion.pas',
  GX_GExperts in 'Framework\GX_GExperts.pas',
  GX_GrepBackend in 'Grep\GX_GrepBackend.pas',
  GX_GrepExpert in 'Grep\GX_GrepExpert.pas',
  GX_GrepOptions in 'Grep\GX_GrepOptions.pas' {fmGrepOptions},
  GX_GrepPrinting in 'Grep\GX_GrepPrinting.pas',
  GX_GrepRegExSearch in 'Grep\GX_GrepRegExSearch.pas',
  GX_GrepReplace in 'Grep\GX_GrepReplace.pas' {fmGrepReplace},
  GX_GrepResults in 'Grep\GX_GrepResults.pas' {fmGrepResults},
  GX_GrepResultsOptions in 'Grep\GX_GrepResultsOptions.pas' {fmGrepResultsOptions},
  GX_GrepSearch in 'Grep\GX_GrepSearch.pas' {fmGrepSearch},
  GX_GxUtils in 'Utils\GX_GxUtils.pas',
  GX_HideNonVisualComps in 'Experts\GX_HideNonVisualComps.pas',
  GX_IconMessageBox in 'Framework\GX_IconMessageBox.pas',
  GX_IdeDock in 'IDEDocking\GX_IdeDock.pas' {fmIdeDockForm},
  GX_IdeEnhance in 'IDE\GX_IdeEnhance.pas',
  GX_IdeFormEnhancer in 'IDE\GX_IdeFormEnhancer.pas',
  GX_IdeShortCuts in 'Experts\GX_IdeShortCuts.pas' {fmIdeShortCuts},
  GX_IdeUtils in 'Utils\GX_IdeUtils.pas',
  GX_KbdShortCutBroker in 'Framework\GX_KbdShortCutBroker.pas',
  GX_KibitzComp in 'Framework\GX_KibitzComp.pas',
  GX_LibrarySource in 'Framework\GX_LibrarySource.pas',
  GX_MacroExpandNotifier in 'MacroTemplates\GX_MacroExpandNotifier.pas',
  GX_MacroFile in 'MacroTemplates\GX_MacroFile.pas',
  GX_MacroLibrary in 'MacroLibrary\GX_MacroLibrary.pas' {fmMacroLibrary},
  GX_MacroLibraryNamePrompt in 'MacroLibrary\GX_MacroLibraryNamePrompt.pas' {fmMacroLibraryNamePrompt},
  GX_MacroLibraryConfig in 'MacroLibrary\GX_MacroLibraryConfig.pas' {fmGxMacroLibraryConfig},
  GX_MacroParser in 'Framework\GX_MacroParser.pas',
  GX_MacroSelect in 'MacroTemplates\GX_MacroSelect.pas' {fmMacroSelect},
  GX_MacroTemplateEdit in 'MacroTemplates\GX_MacroTemplateEdit.pas' {fmMacroTemplateEdit},
  GX_MacroTemplates in 'MacroTemplates\GX_MacroTemplates.pas' {fmMacroTemplates},
  GX_MacroTemplatesExpert in 'MacroTemplates\GX_MacroTemplatesExpert.pas',
  GX_MenuActions in 'Framework\GX_MenuActions.pas',
  GX_MenusForEditorExpert in 'Editor\GX_MenusForEditorExpert.pas',
  GX_MessageBox in 'Framework\GX_MessageBox.pas' {fmGxMessageBox},
  GX_MessageDialog in 'MessageDialog\GX_MessageDialog.pas' {fmMessageDialog},
  GX_MessageOptions in 'MessageDialog\GX_MessageOptions.pas' {fmMessageOptions},
  GX_MultilineHost in 'IDE\GX_MultilineHost.pas',
  GX_MultiLinePalette in 'IDE\GX_MultiLinePalette.pas',
  GX_OpenFile in 'OpenFile\GX_OpenFile.pas' {fmOpenFile},
  GX_OpenFileConfig in 'OpenFile\GX_OpenFileConfig.pas' {fmOpenFileConfig},
  GX_OtaUtils in 'Utils\GX_OtaUtils.pas',
  GX_PeInfo in 'Experts\GX_PeInfo.pas',
  GX_PeInformation in 'Experts\GX_PeInformation.pas' {fmPeInformation},
  GX_PerfectLayout in 'Experts\GX_PerfectLayout.pas' {fmPerfectLayout},
  GX_ProcedureList in 'ProcedureList\GX_ProcedureList.pas' {fmProcedureList},
  GX_ProcedureListOptions in 'ProcedureList\GX_ProcedureListOptions.pas' {fmProcedureListOptions},
  GX_Progress in 'Framework\GX_Progress.pas' {fmProgress},
  GX_ProjDepend in 'ProjectDependencies\GX_ProjDepend.pas' {fmProjDepend},
  GX_ProjDependFilter in 'ProjectDependencies\GX_ProjDependFilter.pas' {fmProjDependFilter},
  GX_ProjDependProp in 'ProjectDependencies\GX_ProjDependProp.pas' {fmProjDependProp},
  GX_ProjOptionSets in 'ProjectOptionSets\GX_ProjOptionSets.pas' {fmProjOptionSets},
  GX_ProjOptMap in 'ProjectOptionSets\GX_ProjOptMap.pas',
  GX_ProofreaderAutoCorrectEntry in 'CodeProofreader\GX_ProofreaderAutoCorrectEntry.pas' {fmProofreaderAutoCorrectEntry},
  GX_ProofreaderConfig in 'CodeProofreader\GX_ProofreaderConfig.pas' {fmProofreaderConfig},
  GX_ProofreaderCorrection in 'CodeProofreader\GX_ProofreaderCorrection.pas',
  GX_ProofreaderData in 'CodeProofreader\GX_ProofreaderData.pas',
  GX_ProofreaderDefaults in 'CodeProofreader\GX_ProofreaderDefaults.pas',
  GX_ProofreaderExpert in 'CodeProofreader\GX_ProofreaderExpert.pas',
  GX_ProofreaderKeyboard in 'CodeProofreader\GX_ProofreaderKeyboard.pas',
  GX_ProofreaderUtils in 'CodeProofreader\GX_ProofreaderUtils.pas',
  GX_Replace in 'Grep\GX_Replace.pas',
  GX_ReplaceComp in 'ReplaceComponents\GX_ReplaceComp.pas' {fmReplaceComp},
  GX_ReplaceCompData in 'ReplaceComponents\GX_ReplaceCompData.pas',
  GX_ReplaceCompLog in 'ReplaceComponents\GX_ReplaceCompLog.pas' {fmReplaceCompLog},
  GX_ReplaceCompMapDets in 'ReplaceComponents\GX_ReplaceCompMapDets.pas' {fmReplaceCompMapDets},
  GX_ReplaceCompMapGrpList in 'ReplaceComponents\GX_ReplaceCompMapGrpList.pas' {fmReplaceCompMapGrpList},
  GX_ReplaceCompMapList in 'ReplaceComponents\GX_ReplaceCompMapList.pas' {fmReplaceCompMapList},
  GX_ReplaceCompUtils in 'ReplaceComponents\GX_ReplaceCompUtils.pas',
  GX_SetComponentProps in 'SetComponentProperties\GX_SetComponentProps.pas',
  GX_SetComponentPropsConfig in 'SetComponentProperties\GX_SetComponentPropsConfig.pas' {fmSetComponentPropsConfig},
  GX_SetComponentPropsStatus in 'SetComponentProperties\GX_SetComponentPropsStatus.pas' {fmSetComponentPropsStatus},
  GX_SetFocusControl in 'Experts\GX_SetFocusControl.pas',
  GX_SharedImages in 'Framework\GX_SharedImages.pas' {dmSharedImages: TDataModule},
  GX_SourceExport in 'SourceExport\GX_SourceExport.pas' {fmSourceExport},
  GX_SourceExportOptions in 'SourceExport\GX_SourceExportOptions.pas' {fmSourceExportOptions},
  GX_SynMemoUtils in 'Framework\GX_SynMemoUtils.pas',
  GX_TabOrder in 'Experts\GX_TabOrder.pas' {fmTabOrder},
  GX_ToDo in 'ToDoList\GX_ToDo.pas' {fmToDo},
  GX_ToDoOptions in 'ToDoList\GX_ToDoOptions.pas' {fmToDoOptions},
  GX_Toolbar in 'EditorToolbar\GX_Toolbar.pas',
  GX_ToolbarConfig in 'EditorToolbar\GX_ToolbarConfig.pas' {fmToolbarConfig},
  GX_ToolBarDropDown in 'EditorToolbar\GX_ToolBarDropDown.pas',
  GX_UnitPositions in 'Framework\GX_UnitPositions.pas',
  GX_UsesManager in 'Framework\GX_UsesManager.pas',
  GX_VerDepConst in 'Framework\GX_VerDepConst.pas',
  GX_XmlUtils in 'Utils\GX_XmlUtils.pas',
  GX_Zipper in 'BackupProject\GX_Zipper.pas',
  GX_IdeSearchPathEnhancer in 'IDE\GX_IdeSearchPathEnhancer.pas',
  GX_HideNavbar in 'Experts\GX_HideNavbar.pas',
  GX_dzVclUtils in 'Utils\GX_dzVclUtils.pas',
  GX_ePasteAs in 'Editor\GX_ePasteAs.pas' {fmPasteAsConfig},
  GX_IdeProjectOptionsEnhancer in 'IDE\GX_IdeProjectOptionsEnhancer.pas',
  GX_IdeToolPropertiesEnhancer in 'IDE\GX_IdeToolPropertiesEnhancer.pas';

begin
end.

