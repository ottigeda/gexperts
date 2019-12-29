program GExpertsPeInformation;

uses
  Forms,
  GX_PeInfo in 'src\GX_PeInfo.pas',
  GX_PeInformation in 'src\GX_PeInformation.pas' {fmPeInformation},
  GX_PeInfoPrint in 'src\GX_PeInfoPrint.pas' {f_PeInfoPrint},
  GX_SharedImages in 'src\GX_SharedImages.pas' {dmSharedImages: TDataModule},
  GX_GenericUtils in '..\..\..\Source\Utils\GX_GenericUtils.pas',
  GX_DbugIntf in '..\..\..\Source\Utils\GX_DbugIntf.pas',
  GX_Debug in '..\..\..\Source\Utils\GX_Debug.pas',
  GX_dzSelectDirectoryFix in '..\..\..\Source\Utils\GX_dzSelectDirectoryFix.pas',
  GX_dzVclUtils in '..\..\..\Source\Utils\GX_dzVclUtils.pas',
  GX_dzOsUtils in '..\..\..\Source\Utils\GX_dzOsUtils.pas',
  GX_dzVersionInfo in '..\..\..\Source\Utils\GX_dzVersionInfo.pas',
  GX_dzPackageInfo in '..\..\..\Source\Utils\GX_dzPackageInfo.pas',
  GX_dzClassUtils in '..\..\..\Source\Utils\GX_dzClassUtils.pas',
  GX_dzNamedThread in '..\..\..\Source\Utils\GX_dzNamedThread.pas';

{$R *_version.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmPeInformation, fmPeInformation);
  Application.Run;
end.
