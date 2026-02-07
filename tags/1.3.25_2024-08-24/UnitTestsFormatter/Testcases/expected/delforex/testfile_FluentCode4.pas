unit testfile_FluentCode4;

interface

implementation

begin
  GetCharset()
    .Add;

  GetCharset()['Symbols']
    .Add;

  GetCharset(Result)['Symbols'].AsObj.AddObj(Key)
    .Add('Description', Descr, Descr <> '')
    .AddList('Images');

  GetCharset(Result)['Symbols'].AsObj.AddObj(Key)
    .Add('Description', Descr, Descr <> '');

  GetCharset(Result)['Symbols'].AsObj
    .Add('Description', Descr, Descr <> '');

  GetCharset(Result)['Symbols']
    .Add('Description', Descr, Descr <> '');

  GetCharset(Result)
    .Add('Description', Descr, Descr <> '');
end.