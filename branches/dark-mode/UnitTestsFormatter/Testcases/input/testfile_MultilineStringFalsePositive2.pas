unit MultilineStringFalsePositive

interface

implementation

begin
  case FInputSource.NextCodePoint of
    STRING_TERMINATOR_CODE: begin
        FTokenType := XML_START_TAG_TOKEN;
        FErrorType := ET_UNCLOSED_ELEMENT;
        if SQ_Open then begin
          FClue := ''' ';
        end else if DQ_Open then begin
          FClue := '">';
        end else
          FClue := '>';
    end;
  end;
end.