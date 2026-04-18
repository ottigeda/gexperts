unit testfile_MultilineCommentIndent;

interface

type
  TMyClass = class
    procedure functionx(); { this is multiline comment
    ---- continuation line
    }
    procedure functiony();
  end;

type
  TMyClass2 = class
    procedure functionx();
    { this is own-line comment
    ---- continuation line
    }
    procedure functiony();
  end;

type
  TMyClass3 = class
    procedure functionx(); (* this is star comment
    ---- continuation line
    *)
    procedure functiony();
  end;

implementation

procedure TMyClass.functionx();
begin
  x := 1; { mid-line in begin/end
  continuation
  }
  y := 2;
end;

{ top-level own-line comment
  with indented continuation
}
procedure TMyClass.functiony();
begin
end;

end.
