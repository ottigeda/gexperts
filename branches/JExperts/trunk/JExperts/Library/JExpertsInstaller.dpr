program JExpertsInstaller;

uses
  Forms,
  JeUnit1 in 'JeUnit1.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'JExperts Installer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
