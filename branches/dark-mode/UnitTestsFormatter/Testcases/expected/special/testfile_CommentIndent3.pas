unit testfile_CommentIndent;

interface

type
  TDelforExpert = class(TNotifierObject, IOTAWizard)
  private
    procedure Execute;

    // IOTANotifier implemented by TNotifierObject

    // IOTAIdeNotifier
//    procedure AfterCompile(Succeeded: Boolean);
//    procedure BeforeCompile(const Project: IOTAProject;
  end;

implementation

end.