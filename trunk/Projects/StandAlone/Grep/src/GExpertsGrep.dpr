program GExpertsGrep;

uses
  DropSource in '..\..\..\..\ExternalSource\DropSource.pas',
  GrepMain in 'GrepMain.pas',
  GX_About in '..\..\..\..\Source\Framework\GX_About.pas' {fmAbout},
  GX_AboutExperimental in '..\..\..\..\Source\Formatter\GX_AboutExperimental.pas' {fmAboutExperimental},
  GX_ActionBroker in 'GX_ActionBroker.pas',
  GX_BaseForm in '..\..\..\..\Source\Framework\GX_BaseForm.pas' {fmBaseForm},
  GX_CodeFormatterUnicode in '..\..\..\..\Source\Formatter\Engine\GX_CodeFormatterUnicode.pas',
  GX_ConfigurationInfo in '..\..\..\..\Source\Framework\GX_ConfigurationInfo.pas',
  GX_DbugIntf in '..\..\..\..\Source\Utils\GX_DbugIntf.pas',
  GX_EditorEnhancements in 'GX_EditorEnhancements.pas',
  GX_Experts in 'GX_Experts.pas',
  GX_FeedbackWizard in '..\..\..\..\Source\Framework\GX_FeedbackWizard.pas' {fmFeedbackWizard},
  GX_GenericClasses in '..\..\..\..\Source\Utils\GX_GenericClasses.pas',
  GX_GenericUtils in '..\..\..\..\Source\Utils\GX_GenericUtils.pas',
  GX_GetIdeVersion in '..\..\..\..\Source\Framework\GX_GetIdeVersion.pas',
  GX_GExperts in 'GX_GExperts.pas',
  GX_GrepBackend in '..\..\..\..\Source\Grep\GX_GrepBackend.pas',
  GX_GrepExpert in '..\..\..\..\Source\Grep\GX_GrepExpert.pas',
  GX_GrepMenuEntry in 'GX_GrepMenuEntry.pas',
  GX_GrepOptions in '..\..\..\..\Source\Grep\GX_GrepOptions.pas' {fmGrepOptions},
  GX_GrepPrinting in '..\..\..\..\Source\Grep\GX_GrepPrinting.pas',
  GX_GrepProgress in '..\..\..\..\Source\Grep\GX_GrepProgress.pas' {fmGrepProgress},
  GX_GrepRegExSearch in '..\..\..\..\Source\Grep\GX_GrepRegExSearch.pas',
  GX_GrepReplace in '..\..\..\..\Source\Grep\GX_GrepReplace.pas' {fmGrepReplace},
  GX_GrepResults in '..\..\..\..\Source\Grep\GX_GrepResults.pas' {fmGrepResults},
  GX_GrepResultsOptions in '..\..\..\..\Source\Grep\GX_GrepResultsOptions.pas' {fmGrepResultsOptions},
  GX_GrepSearch in '..\..\..\..\Source\Grep\GX_GrepSearch.pas' {fmGrepSearch},
  GX_GrepSearchExpert in '..\..\..\..\Source\Grep\GX_GrepSearchExpert.pas',
  GX_GrepSelect in '..\..\..\..\Source\Grep\GX_GrepSelect.pas' {fmGrepSelect},
  GX_GxUtils in '..\..\..\..\Source\Utils\GX_GxUtils.pas',
  GX_IdeDock in 'GX_IdeDock.pas' {fmIdeDockForm},
  GX_IdeUtils in 'GX_IdeUtils.pas',
  GX_LibrarySource in 'GX_LibrarySource.pas',
  GX_MemoEscFix in '..\..\..\..\Source\Utils\GX_MemoEscFix.pas',
  GX_MessageBox in '..\..\..\..\Source\Framework\GX_MessageBox.pas' {fmGxMessageBox},
  GX_OtaUtils in 'GX_OtaUtils.pas',
  GX_Replace in '..\..\..\..\Source\Grep\GX_Replace.pas',
  GX_SharedImages in '..\..\..\..\Source\Framework\GX_SharedImages.pas' {dmSharedImages: TDataModule},
  GX_StringList in '..\..\..\..\Source\Utils\GX_StringList.pas',
  GX_TestRegEx in '..\..\..\..\Source\Grep\GX_TestRegEx.pas' {fmTestRegEx},
  GX_VerDepConst in '..\..\..\..\Source\Framework\GX_VerDepConst.pas',
  ToolsAPI in 'ToolsAPI.pas';

{$R *_icon.res}
{$R *_version.res}
{$R *_manifest.res}

begin
  Main;
end.
