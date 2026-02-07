unit testfile_NoGeneric;

interface

implementation

procedure xmlAnalyseTag(source: wideString;
  var tagName,
  AttribSequence: wideString);
// 'Source': The tag, to be analyzed.
// 'tagName': Returns the namen of the tag.
// 'AttribSequence': Returns the Attributes, if existing.
var
  i, j, sourceLength: integer;
begin
  sourceLength := length(Source); // buffer storage to increase performance

  // Evaluate tagName:
  i := 1;
  while i <= sourceLength do begin
    if IsXmlWhiteSpace(Source[i]) then
      break;
    inc(i);
  end;

  tagName := copy(Source, 1, i - 1);

  // Evaluate Attributes:
  while i < sourceLength do begin
    inc(i);
    if not IsXmlWhiteSpace(Source[i]) then
      break;
  end;
  j := length(Source);
  while j >= i do begin
    if not IsXmlWhiteSpace(Source[j]) then
      break;
    dec(j);
  end;

  AttribSequence := copy(Source, i, j - i + 1);
end;

end.