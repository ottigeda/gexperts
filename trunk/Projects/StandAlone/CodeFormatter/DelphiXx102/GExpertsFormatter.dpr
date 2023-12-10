program GExpertsFormatter;

uses
  Forms,
  w_GExpertsFormatterMain in 'w_GExpertsFormatterMain.pas' {f_GExpertsFormatterMain},
  w_GExpertsFormatterAbout in 'w_GExpertsFormatterAbout.pas' {f_GExpertsFormatterAbout},
  GX_CodeFormatterDefaultSettings in '..\..\..\..\Source\Formatter\GX_CodeFormatterDefaultSettings.pas',
  GX_CodeFormatterTokenList in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterTokenList.pas',
  GX_CodeFormatterTypes in '..\..\..\..\Source\Formatter\Engine\GX_CodeFormatterTypes.pas',
  GX_CodeFormatterEngine in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterEngine.pas',
  GX_CodeFormatterTokens in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterTokens.pas',
  GX_CodeFormatterSettings in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterSettings.pas',
  GX_CodeFormatterFormatter in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterFormatter.pas',
  GX_CodeFormatterStack in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterStack.pas',
  GX_CodeFormatterParser in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterParser.pas',
  GX_CodeFormatterConfigHandler in '..\..\..\..\Source\Formatter\GX_CodeFormatterConfigHandler.pas',
  GX_CodeFormatterUnicode in '..\..\..\..\Source\Formatter\engine\GX_CodeFormatterUnicode.pas',
  GX_GenericUtils in '..\..\..\..\Source\Utils\GX_GenericUtils.pas',
  GX_StringList in '..\..\..\..\Source\Utils\GX_StringList.pas',
  GX_Logging in '..\..\..\..\Source\Framework\GX_Logging.pas',
  GX_BaseForm in 'GX_BaseForm.pas' {fmBaseForm},
  GX_CodeFormatterConfig in '..\..\..\..\Source\Formatter\GX_CodeFormatterConfig.pas',
  GX_EnhancedEditor in '..\..\..\..\ExternalSource\GX_EnhancedEditor.pas',
  GX_SynMemoUtils in 'GX_SynMemoUtils.pas',
  GX_CodeFormatterEditCapitalization in '..\..\..\..\Source\Formatter\GX_CodeFormatterEditCapitalization.pas' {fmCodeFormatterEditCapitalization},
  GX_OtaUtils in 'GX_OtaUtils.pas',
  GX_CodeFormatterGXConfigWrapper in 'GX_CodeFormatterGXConfigWrapper.pas',
  GX_DbugIntf in '..\..\..\..\Source\Utils\GX_DbugIntf.pas';

{$R *.res}

begin
  Main;
end.
