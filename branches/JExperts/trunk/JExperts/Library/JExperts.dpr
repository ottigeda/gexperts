library JExperts;

uses
  ShareMem,
  SysUtils,
  Classes,
  Dialogs,
  JeExpert_Types in '..\Package\JeExpert_Types.pas',
  JeExpert_Strings in 'Experts\JeExpert_Strings.pas',
  JeExpert_DllExported in 'Experts\JeExpert_DllExported.pas',
  JeClass_Options in 'Classes\JeClass_Options.pas',
  JeClass_CodeToHtml in 'Classes\JeClass_CodeToHtml.pas',
  JeClass_TransHeader in 'Classes\JeClass_TransHeader.pas',
  JeUnit_Utils in 'Units\JeUnit_Utils.pas',
  JeFormDate in 'Forms\JeFormDate.pas' {foDate},
  JeFormTime in 'Forms\JeFormTime.pas' {foTime},
  JeFormHint in 'Forms\JeFormHint.pas' {foHintForm},
  JeFormCursorEditor in 'Forms\JeFormCursorEditor.pas' {foCursorEditor},
  JeFormDateTime in 'Forms\JeFormDateTime.pas' {foDateTime},
  JeFormFilter in 'Forms\JeFormFilter.pas' {foFilter},
  JeFormSpeedButton in 'Forms\JeFormSpeedButton.pas' {foSpeedButton},
  JeFormBitBtn in 'Forms\JeFormBitBtn.pas' {foBitBtn},
  JeFormPopup in 'Forms\JeFormPopup.pas' {foPopup},
  JeFormMessageDlg in 'Forms\JeFormMessageDlg.pas' {foMessageDialog},
  JeFormRecord in 'Forms\JeFormRecord.pas' {foRecord},
  JeFormUnion in 'Forms\JeFormUnion.pas' {foUnion},
  JeFormEnumeration in 'Forms\JeFormEnumeration.pas' {foEnumeration},
  JeFormFind in 'Forms\JeFormFind.pas' {foFindFirst},
  JeFormTryExcept in 'Forms\JeFormTryExcept.pas' {foTryExcept},
  JeFormCaseOf in 'Forms\JeFormCaseOf.pas' {foCaseOf},
  JeFormSeparators in 'Forms\JeFormSeparators.pas' {foSeparator},
  JeFormFavorites in 'Forms\JeFormFavorites.pas' {foFavorites},
  JeFormSnippet in 'Forms\JeFormSnippet.pas' {foSnippet},
  JeFormAddLibrary in 'Forms\JeFormAddLibrary.pas' {foAddCodeLibrary},
  JeFormLibrary in 'Forms\JeFormLibrary.pas' {foLibrary},
  JeFormOptions in 'Forms\JeFormOptions.pas' {foOptions},
  JeFormAbout in 'Forms\JeFormAbout.pas' {foAbout},
  JeFormRegister in 'Forms\JeFormRegister.pas' {foRegister},
  JeFormCodeToHtml in 'Forms\JeFormCodeToHtml.pas' {foCodeToHtml},
  JeFormASCII in 'Forms\JeFormASCII.pas' {foAscii},
  JeFormRuntime in 'Forms\JeFormRuntime.pas' {foRuntime},
  JeFormLexical in 'Forms\JeFormLexical.pas' {foLexical},
  JeFormBatch in 'Forms\JeFormBatch.pas' {foBatch},
  JeFormReport in 'Forms\JeFormReport.pas' {foBugReport},
  JeFormAutoSave in 'Forms\JeFormAutoSave.pas' {foAutoSave},
  JeFormTodo in 'Forms\JeFormTodo.pas' {foToDo},
  JeFormToDo_Add in 'Forms\JeFormToDo_Add.pas' {foToDoAdd},
  JeFormComponentSearch in 'Forms\JeFormComponentSearch.pas' {foCompoSearch},
  JeFormTimeLogger in 'Forms\JeFormTimeLogger.pas' {foTimeLogger},
  JeFormTransHeader in 'Forms\JeFormTransHeader.pas' {foTransHeader},
  JeFormLayout in 'Forms\JeFormLayout.pas' {foLayoutChooser},
  JeFormLayoutManage in 'Forms\JeFormLayoutManage.pas' {foLayoutManage},
  JeFormCrlf in 'Forms\JeFormCrlf.pas' {foCrlf},
  JeFormDirectoryCleaner in 'Forms\JeFormDirectoryCleaner.pas' {foDirCleaner};

{$R Resources\Datas.res}
{$R Htmls\HtmlPages.res}

function ExpertDllEntry(DelphiApis: TDelphiApis): TDllFunctions; stdcall;
begin
  JeFormPopup.DelphiApis := DelphiApis;
  DllFunctions := TDllExported.Create;
  result := DllFunctions;
end;

exports
  ExpertDllEntry index 1 name 'ExpertDllEntry';

begin
end.

