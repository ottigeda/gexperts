program GExpertsPeInformation;

uses
  Forms,
  GX_PeInfo in 'src\GX_PeInfo.pas',
  GX_PeInformation in 'src\GX_PeInformation.pas' {fmPeInformation},
  GX_PeInfoPrint in 'src\GX_PeInfoPrint.pas' {f_PeInfoPrint},
  GX_SharedImages in 'src\GX_SharedImages.pas' {dmSharedImages: TDataModule};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmPeInformation, fmPeInformation);
  Application.Run;
end.
