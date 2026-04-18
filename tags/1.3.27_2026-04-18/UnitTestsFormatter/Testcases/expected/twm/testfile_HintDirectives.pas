unit HintDirectives;

interface

procedure OldProc(bla: blub); deprecated;
procedure PlatformProc(blub: bla); platform;
procedure LibraryProc(a: integer); library;
procedure ExperimentalProc(x, y: string); experimental;

function OldFunc(bla: blub): integer; deprecated;
function PlatformFunc(blub: bla): word; platform;
function LibraryFunc(a: integer): string; library;
function ExperimentalFunc(x, y: string): TObject; experimental;

type
  TBla = class(TBlub)
  strict private
    procedure OldProc(bla: blub); deprecated;
    procedure PlatformProc(blub: bla); platform;
    procedure LibraryProc(a: integer); library;
    procedure ExperimentalProc(x, y: string); experimental;

    function OldFunc(bla: blub): integer; deprecated;
    function PlatformFunc(blub: bla): word; platform;
    function LibraryFunc(a: integer): string; library;
    function ExperimentalFunc(x, y: string): TObject; experimental;
  end;

implementation

end.
