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
  u_dzVersionInfo in '..\..\..\ExternalSource\dzlib\u_dzVersionInfo.pas',
  u_dzVclUtils in '..\..\..\ExternalSource\dzlib\u_dzVclUtils.pas',
  u_dzSelectDirectoryFix in '..\..\..\ExternalSource\dzlib\u_dzSelectDirectoryFix.pas',
  u_dzPackageInfo in '..\..\..\ExternalSource\dzlib\u_dzPackageInfo.pas',
  u_dzOsUtils in '..\..\..\ExternalSource\dzlib\u_dzOsUtils.pas',
  u_dzNamedThread in '..\..\..\ExternalSource\dzlib\u_dzNamedThread.pas',
  u_dzClassUtils in '..\..\..\ExternalSource\dzlib\u_dzClassUtils.pas',
  u_dzTranslator in '..\..\..\ExternalSource\dzlib\u_dzTranslator.pas',
  u_dzTypes in '..\..\..\ExternalSource\dzlib\u_dzTypes.pas',
  u_dzConvertUtils in '..\..\..\ExternalSource\dzlib\u_dzConvertUtils.pas',
  u_dzDateUtils in '..\..\..\ExternalSource\dzlib\u_dzDateUtils.pas',
  u_dzStringUtils in '..\..\..\ExternalSource\dzlib\u_dzStringUtils.pas',
  u_dzFileUtils in '..\..\..\ExternalSource\dzlib\u_dzFileUtils.pas',
  u_dzMiscUtils in '..\..\..\ExternalSource\dzlib\u_dzMiscUtils.pas',
  u_dzFileStreams in '..\..\..\ExternalSource\dzlib\u_dzFileStreams.pas',
  u_dzSortProvider in '..\..\..\ExternalSource\dzlib\u_dzSortProvider.pas',
  u_dzQuicksort in '..\..\..\ExternalSource\dzlib\u_dzQuicksort.pas',
  u_dzSortUtils in '..\..\..\ExternalSource\dzlib\u_dzSortUtils.pas',
  u_dzLineBuilder in '..\..\..\ExternalSource\dzlib\u_dzLineBuilder.pas',
  u_dzVariantUtils in '..\..\..\ExternalSource\dzlib\u_dzVariantUtils.pas',
  u_dzTypesUtils in '..\..\..\ExternalSource\dzlib\u_dzTypesUtils.pas',
  u_dzErrorThread in '..\..\..\ExternalSource\dzlib\u_dzErrorThread.pas';

{$R *_version.res}
{$R *_icon.res}
{$R *_manifest.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmSharedImages, dmSharedImages);
  Application.CreateForm(TfmPeInformation, fmPeInformation);
  Application.Run;
end.
