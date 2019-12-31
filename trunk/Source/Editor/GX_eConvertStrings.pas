unit GX_eConvertStrings;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  GX_BaseForm,
  GX_MemoEscFix,
  Menus,
  ActnList,
  Actions;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TfmEConvertStrings = class(TfmBaseForm)
    m_Input: TMemo;
    l_Input: TLabel;
    m_Output: TMemo;
    l_Output: TLabel;
    chk_QuoteStrings: TCheckBox;
    chk_AppendSpace: TCheckBox;
    b_CopyToClipboard: TButton;
    b_Insert: TButton;
    b_Close: TButton;
    chk_ExtractRaw: TCheckBox;
    l_Prefix: TLabel;
    ed_Prefix: TEdit;
    b_PasteFromClipboard: TButton;
    chk_TrimLeft: TCheckBox;
    chk_TrimRight: TCheckBox;
    chk_Indent: TCheckBox;
    b_Favorites: TButton;
    pm_Favorites: TPopupMenu;
    N1: TMenuItem;
    mi_FavoritesSaveAs: TMenuItem;
    TheActionList: TActionList;
    act_Favorites: TAction;
    mi_Opendirectory: TMenuItem;
    ed_PrefixFirst: TEdit;
    l_Suffix: TLabel;
    chk_PrefixFirst: TCheckBox;
    ed_Suffix: TEdit;
    chk_SuffixLast: TCheckBox;
    ed_SuffixLast: TEdit;
    procedure chk_ExtractRawClick(Sender: TObject);
    procedure rg_ConvertTypeClick(Sender: TObject);
    procedure b_CopyToClipboardClick(Sender: TObject);
    procedure ed_PrefixChange(Sender: TObject);
    procedure m_InputChange(Sender: TObject);
    procedure b_InsertClick(Sender: TObject);
    procedure chk_QuoteStringsClick(Sender: TObject);
    procedure chk_AppendSpaceClick(Sender: TObject);
    procedure b_PasteFromClipboardClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure chk_TrimLeftClick(Sender: TObject);
    procedure chk_TrimRightClick(Sender: TObject);
    procedure chk_IndentClick(Sender: TObject);
    procedure mi_FavoritesSaveAsClick(Sender: TObject);
    procedure act_FavoritesExecute(Sender: TObject);
    procedure pm_FavoritesPopup(Sender: TObject);
    procedure mi_OpendirectoryClick(Sender: TObject);
    procedure chk_PrefixFirstClick(Sender: TObject);
    procedure ed_PrefixFirstChange(Sender: TObject);
    procedure ed_SuffixChange(Sender: TObject);
    procedure chk_SuffixLastClick(Sender: TObject);
    procedure ed_SuffixLastChange(Sender: TObject);
  private
    FUpdating: Boolean;
    procedure SetData(_sl: TStrings);
    procedure ConvertStrings;
    procedure ExtractRawStrings(_sl: TStrings; _AddBaseIndent, _TrimLeft, _TrimRight: Boolean);
    function DetermineIndent(_sl: TStrings): Integer;
    procedure ConvertToCode(_sl: TStrings; _Indent: Boolean;
      _QuoteStrings: Boolean; _AppendSpace: Boolean;
      const _FirstPrefix, _Prefix, _Suffix, _LastSuffix: string);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure TrimStrings(_sl: TStrings; _TrimLeft, _TrimRight: Boolean);
    procedure OnFavoriteClick(_Sender: TObject);
    procedure CopyDefaultsFromResource(const _ConfigDir, _ResName, _Filename: string);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    class procedure Execute(_bmp: TBitmap; _sl: TStrings);
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  Clipbrd,
  ToolsAPI,
  IniFiles,
  ShellAPI,
  u_dzVclUtils,
  u_dzFileUtils,
  u_dzClassUtils,
  GX_GenericUtils,
  GX_OtaUtils,
  GX_EditorExpert,
  GX_ConfigurationInfo;

type
  TConvertStringsExpert = class(TEditorExpert)
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    // Returns false
    function HasConfigOptions: Boolean; override;
  end;

const
  SINGLE_QUOTE = '''';

class procedure TfmEConvertStrings.Execute(_bmp: TBitmap; _sl: TStrings);
var
  frm: TfmEConvertStrings;
begin
  frm := TfmEConvertStrings.Create(Application);
  try
    ConvertBitmapToIcon(_bmp, frm.Icon);
    frm.SetData(_sl);
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmEConvertStrings.Create(_Owner: TComponent);
begin
  inherited;
  TControl_SetMinConstraints(Self);
end;

destructor TfmEConvertStrings.Destroy;
begin
  try
    SaveSettings;
  except
    // we are being called in the destructor -> ignore any exceptions
  end;
  inherited;
end;

procedure TfmEConvertStrings.FormResize(Sender: TObject);
var
  cw: Integer;
  w: Integer;
  X: Integer;
  m: Integer;
begin
  m := m_Input.Left;
  cw := ClientWidth;
  X := (cw - b_Favorites.Width) div 2;
  b_Favorites.Left := X;
  chk_ExtractRaw.Left := X;
  chk_TrimLeft.Left := X;
  chk_TrimRight.Left := X;
  chk_Indent.Left := X;
  chk_QuoteStrings.Left := X;
  chk_AppendSpace.Left := X;
  l_Prefix.Left := X;
  ed_Prefix.Left := X;
  chk_PrefixFirst.Left := X;
  l_Suffix.Left := X;
  ed_PrefixFirst.Left := X;
  ed_Suffix.Left := X;
  chk_SuffixLast.Left := X;
  ed_SuffixLast.Left := X;

  w := X - 2 * m;
  m_Input.Width := w;
  m_Output.Width := w;

  X := cw - w - m;
  l_Output.Left := X;
  m_Output.Left := X;
end;

procedure TfmEConvertStrings.mi_FavoritesSaveAsClick(Sender: TObject);
var
  NewFavName: string;
  FavDir: string;
  fn: string;
  Section: TIniSection;
  Exists: Boolean;
begin
  if not InputQuery('Favorite Name', 'New Favorite', NewFavName) then
    Exit; //==>

  NewFavName := Trim(NewFavName);
  if NewFavName = '' then
    Exit; //==>

  FavDir := ConfigInfo.ConfigPath + TConvertStringsExpert.ConfigurationKey;
  ForceDirectories(FavDir);
  fn := AddSlash(FavDir) + NewFavName + '.INI';
  Exists := FileExists(fn);
  if Exists then
    if mrYes <> MessageDlg('A favorite with this name already exists.' + #13 + #10 + 'Overwrite it?', mtWarning, [mbYes, mbCancel], 0) then
      Exit; //==>

  Section := TIniSection.Create(fn, 'Favorite');
  try
    Section.WriteBool('ExtractRaw', chk_ExtractRaw.Checked);
    Section.WriteBool('TrimLeft', chk_TrimLeft.Checked);
    Section.WriteBool('TrimRight', chk_TrimRight.Checked);
    Section.WriteBool('KeepIndent', chk_Indent.Checked);
    Section.WriteBool('QuoteStrings', chk_QuoteStrings.Checked);
    Section.WriteBool('AppendSpace', chk_AppendSpace.Checked);
    Section.WriteString('Prefix', ed_Prefix.Text);
    Section.WriteString('Prefix', ed_Prefix.Text);
    Section.WriteBool('SamePrefix', chk_PrefixFirst.Checked);
    Section.WriteString('FirstPrefix', ed_PrefixFirst.Text);
    Section.WriteString('Suffix', ed_Suffix.Text);
    Section.WriteBool('SameSuffix', chk_SuffixLast.Checked);
    Section.WriteString('LastSuffix', ed_SuffixLast.Text);
    Section.UpdateFile;
  finally
    FreeAndNil(Section);
  end;

  if not Exists then
    TPopupMenu_AppendMenuItem(pm_Favorites, NewFavName, OnFavoriteClick);
end;

procedure TfmEConvertStrings.OnFavoriteClick(_Sender: TObject);
var
  mi: TMenuItem;
  FavName: string;
  fn: string;
  Section: TIniSection;
begin
  mi := _Sender as TMenuItem;
  FavName := Menus.StripHotkey(mi.Caption);
  fn := AddSlash(ConfigInfo.ConfigPath + TConvertStringsExpert.ConfigurationKey) + FavName + '.ini';
  Section := TIniSection.Create(fn, 'Favorite');
  try
    chk_ExtractRaw.Checked := Section.ReadBool('ExtractRaw', False);
    chk_TrimLeft.Checked := Section.ReadBool('TrimLeft', False);
    chk_TrimRight.Checked := Section.ReadBool('TrimRight', False);
    chk_Indent.Checked := Section.ReadBool('KeepIndent', False);
    chk_QuoteStrings.Checked := Section.ReadBool('QuoteStrings', False);
    chk_AppendSpace.Checked := Section.ReadBool('AppendSpace', False);
    ed_Prefix.Text := Section.ReadString('Prefix', '');
    chk_PrefixFirst.Checked := Section.ReadBool('SamePrefix', True);
    ed_PrefixFirst.Text := Section.ReadString('FirstPrefix', '');
    ed_Suffix.Text := Section.ReadString('Suffix', '');
    chk_SuffixLast.Checked := Section.ReadBool('SameSuffix', True);
    ed_SuffixLast.Text := Section.ReadString('LastSuffix', '')
  finally
    FreeAndNil(Section);
  end;
end;

procedure TfmEConvertStrings.mi_OpendirectoryClick(Sender: TObject);
var
  ConfigDir: string;
begin
  ConfigDir := AddSlash(ConfigInfo.ConfigPath + TConvertStringsExpert.ConfigurationKey);
  ShellExecute(0, nil, 'explorer.exe', PChar('/d,' + ConfigDir), nil, SW_SHOWNORMAL)
end;

procedure TfmEConvertStrings.pm_FavoritesPopup(Sender: TObject);
const
  NilEvent: TMethod = (Code: nil; Data: nil);
var
  ConfigDir: string;
  Favs: TStringList;
  i: Integer;
  FavName: string;
  fn: string;
  cnt: Integer;
begin
  inherited;
  pm_Favorites.Items.Clear;
  TPopupMenu_AppendMenuItem(pm_Favorites, 'Save as ...', mi_FavoritesSaveAsClick);
  TPopupMenu_AppendMenuItem(pm_Favorites, '-', TNotifyEvent(NilEvent));

  ConfigDir := AddSlash(ConfigInfo.ConfigPath + TConvertStringsExpert.ConfigurationKey);
  if not DirectoryExists(ConfigDir) then begin
    ForceDirectories(ConfigDir);
    CopyDefaultsFromResource(ConfigDir, 'ConvertStringsAddToSql', 'Add-to-SQL.ini');
    CopyDefaultsFromResource(ConfigDir, 'ConvertStringsSqlToAdd', 'SQL-to-Add.ini');
    CopyDefaultsFromResource(ConfigDir, 'ConvertStringsSqlToString', 'SQL-to-string.ini');
    CopyDefaultsFromResource(ConfigDir, 'ConvertStringsStringToSql', 'String-to-SQL.ini');
  end;

  Favs := TStringList.Create;
  try
    cnt := TSimpleDirEnumerator.EnumFilesOnly(ConfigDir + '*.ini', Favs, True);
    for i := 0 to cnt - 1 do begin
      fn := Favs[i];
      FavName := ChangeFileExt(ExtractFileName(fn), '');
      TPopupMenu_AppendMenuItem(pm_Favorites, FavName, OnFavoriteClick);
    end;
  finally
    FreeAndNil(Favs);
  end;

  if cnt > 0 then
    TPopupMenu_AppendMenuItem(pm_Favorites, '-', TNotifyEvent(NilEvent));
  TPopupMenu_AppendMenuItem(pm_Favorites, 'Open directory', mi_OpendirectoryClick);
end;

procedure TfmEConvertStrings.CopyDefaultsFromResource(const _ConfigDir, _ResName, _Filename: string);
var
  ResStream: TResourceStream;
  FileStream: TFileStream;
begin
  FileStream := nil;
  ResStream := TResourceStream.Create(hInstance, _ResName, RT_RCDATA);
  try
    FileStream := TFileStream.Create(AddSlash(_ConfigDir) + _Filename, fmOpenReadWrite or fmCreate);
    FileStream.CopyFrom(ResStream, ResStream.Size);
  finally
    FreeAndNil(FileStream);
    FreeAndNil(ResStream);
  end;
end;

procedure TfmEConvertStrings.SaveSettings;
var
  Settings: IExpertSettings;
begin
  // Do not localize any of the following lines.
  Settings := TConvertStringsExpert.GetSettings;
  Settings.SaveForm('Window', Self);
  Settings.WriteBool('ExtractRaw', chk_ExtractRaw.Checked);
  Settings.WriteBool('TrimLeft', chk_TrimLeft.Checked);
  Settings.WriteBool('TrimRight', chk_TrimRight.Checked);
  Settings.WriteBool('KeepIndent', chk_Indent.Checked);
  Settings.WriteBool('QuoteStrings', chk_QuoteStrings.Checked);
  Settings.WriteBool('AppendSpace', chk_AppendSpace.Checked);
  Settings.WriteString('Prefix', ed_Prefix.Text);
  Settings.WriteBool('SamePrefix', chk_PrefixFirst.Checked);
  Settings.WriteString('FirstPrefix', ed_PrefixFirst.Text);
  Settings.WriteString('Prefix', ed_Prefix.Text);
  Settings.WriteString('Suffix', ed_Suffix.Text);
  Settings.WriteBool('SameSuffix', chk_SuffixLast.Checked);
  Settings.WriteString('LastSuffix', ed_SuffixLast.Text);
end;

procedure TfmEConvertStrings.SetData(_sl: TStrings);
var
  Prefix: string;
  p: Integer;
begin
  FUpdating := True;
  try
    m_Input.Lines.Assign(_sl);

    Prefix := Trim(GxOtaGetCurrentSelection(False));
    if Prefix = '' then begin
      Prefix := Trim(GxOtaGetCurrentLine);
      GxOtaSelectCurrentLine(GxOtaGetCurrentSourceEditor);
    end;
    // multiple lines? Only take the first
    p := Pos(CR, Prefix);
    if p > 0 then
      Prefix := LeftStr(Prefix, p - 1);
    if Prefix <> '' then begin
      // Does it contain a '('? -> cut it there, we don't want parameters
      p := Pos('(', Prefix);
      if p > 0 then
        Prefix := LeftStr(Prefix, p - 1);
      // now look up the last '.', that's where we append the .Add()
      p := LastDelimiter('.', Prefix);
      if p > 0 then
        Prefix := LeftStr(Prefix, p)
      else begin
        // no '.'? -> add one
        Prefix := Prefix + '.';
      end;
      ed_Prefix.Text := Prefix;
    end;

    LoadSettings;
  finally
    FUpdating := False;
  end;
  ConvertStrings;
end;

procedure TfmEConvertStrings.LoadSettings;
var
  Settings: IExpertSettings;
begin
  // Do not localize any of the following lines.
  Settings := TConvertStringsExpert.GetSettings;
  Settings.LoadForm('Window', Self);

  chk_ExtractRaw.Checked := Settings.ReadBool('ExtractRaw', True);
  chk_TrimLeft.Checked := Settings.ReadBool('TrimLeft', True);
  chk_TrimRight.Checked := Settings.ReadBool('TrimRight', True);
  chk_Indent.Checked := Settings.ReadBool('KeepIndent', False);
  chk_QuoteStrings.Checked := Settings.ReadBool('QuoteStrings', True);
  chk_AppendSpace.Checked := Settings.ReadBool('AppendSpace', True);
  ed_Prefix.Text := Settings.ReadString('Prefix', '');
  chk_PrefixFirst.Checked := Settings.ReadBool('SamePrefix', True);
  ed_PrefixFirst.Text := Settings.ReadString('FirstPrefix', '');
  ed_Suffix.Text := Settings.ReadString('Suffix', '');
  chk_SuffixLast.Checked := Settings.ReadBool('SameSuffix', True);
  ed_SuffixLast.Text := Settings.ReadString('LastSuffix', '');
end;

function TfmEConvertStrings.DetermineIndent(_sl: TStrings): Integer;
var
  i: Integer;
  Line: string;
  FCP: Integer;
begin
  Result := MaxInt;
  for i := 0 to _sl.Count - 1 do begin
    Line := _sl[i];
    FCP := GetFirstCharPos(Line, [' ', #09], False);
    if FCP < Result then
      Result := FCP;
  end;
end;

procedure TfmEConvertStrings.ExtractRawStrings(_sl: TStrings; _AddBaseIndent, _TrimLeft, _TrimRight: Boolean);
var
  i, FirstCharPos, FirstQuotePos, LastQuotePos: Integer;
  Line, BaseIndent: string;
begin
  if _sl.Count = 0 then
    Exit;

  FirstCharPos := DetermineIndent(_sl);
  // this works, because FirstCharPos is the smallest Indent for all lines
  BaseIndent := LeftStr(_sl[0], FirstCharPos - 1);

  for i := 0 to _sl.Count - 1 do begin
    Line := Copy(_sl[i], FirstCharPos);
    if _TrimLeft then
      Line := TrimLeft(Line);
    if _TrimRight then
      Line := TrimRight(Line);

    FirstQuotePos := GetFirstCharPos(Line, [SINGLE_QUOTE], True);
    LastQuotePos := GetLastCharPos(Line, [SINGLE_QUOTE], True);
    if (FirstQuotePos > 0) and (LastQuotePos > 0) then begin
      Line := Copy(Line, FirstQuotePos, LastQuotePos - FirstQuotePos + 1);
      Line := AnsiDequotedStr(Line, SINGLE_QUOTE);
      Line := TrimRight(Line);
      if _AddBaseIndent then
        Line := BaseIndent + Line;
      _sl[i] := Line;
    end;
  end;
end;

procedure TfmEConvertStrings.ConvertToCode(_sl: TStrings;
  _Indent, _QuoteStrings, _AppendSpace: Boolean;
  const _FirstPrefix, _Prefix, _Suffix, _LastSuffix: string);
var
  cnt: Integer;
  i: Integer;
  FirstCharPos: Integer;
  Line, BaseIndent: string;
begin
  if _Indent then
    FirstCharPos := DetermineIndent(_sl)
  else
    FirstCharPos := 1;
  // this works, because FirstCharPos is the smallest Indent for all lines
  BaseIndent := LeftStr(_sl[0], FirstCharPos - 1);

  cnt := _sl.Count;
  for i := 0 to cnt - 1 do begin
    Line := Copy(_sl[i], FirstCharPos);

    if _QuoteStrings then
      Line := AnsiQuotedStr(Line + IfThen(_AppendSpace, ' '), SINGLE_QUOTE);

    if i = 0 then
      Line := _FirstPrefix + Line
    else
      Line := _Prefix + Line;

    if i = cnt - 1 then
      Line := Line + _LastSuffix
    else
      Line := Line + _Suffix;

    _sl[i] := BaseIndent + Line;
  end;
end;

procedure TfmEConvertStrings.TrimStrings(_sl: TStrings; _TrimLeft, _TrimRight: Boolean);
var
  i: Integer;
  s: string;
begin
  if not _TrimLeft and not _TrimRight then
    Exit; //==>

  for i := 0 to _sl.Count - 1 do begin
    s := _sl[i];
    if _TrimLeft then begin
      if _TrimRight then
        s := Trim(s)
      else
        s := TrimLeft(s);
    end else begin
      if _TrimRight then
        s := TrimRight(s);
    end;
    _sl[i] := s;
  end;
end;

procedure TfmEConvertStrings.ConvertStrings;
var
  sl: TStrings;
  Prefix: string;
  FirstPrefix: string;
  Suffix: string;
  LastSuffix: string;
begin
  if FUpdating then
    Exit;

  sl := TStringList.Create;
  try
    sl.Assign(m_Input.Lines);
    if chk_ExtractRaw.Checked then
      ExtractRawStrings(sl, chk_Indent.Checked, chk_TrimLeft.Checked, chk_TrimRight.Checked)
    else
      TrimStrings(sl, chk_TrimLeft.Checked, chk_TrimRight.Checked);

    if sl.Count > 0 then begin
      Prefix := ed_Prefix.Text;
      if chk_PrefixFirst.Checked then
        FirstPrefix := Prefix
      else
        FirstPrefix := ed_PrefixFirst.Text;

      Suffix := ed_Suffix.Text;
      if chk_SuffixLast.Checked then
        LastSuffix := Suffix
      else
        LastSuffix := ed_SuffixLast.Text;
      ConvertToCode(sl, chk_Indent.Checked, chk_QuoteStrings.Checked, chk_AppendSpace.Checked,
        FirstPrefix, Prefix, Suffix, LastSuffix);
    end;
    m_Output.Lines.Assign(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmEConvertStrings.m_InputChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.ed_PrefixChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.ed_PrefixFirstChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.ed_SuffixChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.ed_SuffixLastChange(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.rg_ConvertTypeClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_AppendSpaceClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_ExtractRawClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_IndentClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_PrefixFirstClick(Sender: TObject);
begin
  ed_PrefixFirst.Enabled := not chk_PrefixFirst.Checked;
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_QuoteStringsClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_SuffixLastClick(Sender: TObject);
begin
  ed_SuffixLast.Enabled := not chk_SuffixLast.Checked;
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_TrimLeftClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.chk_TrimRightClick(Sender: TObject);
begin
  ConvertStrings;
end;

procedure TfmEConvertStrings.act_FavoritesExecute(Sender: TObject);
var
  Point: TPoint;
begin
  Point.X := b_Favorites.Width;
  Point.Y := 0;
  Point := b_Favorites.ClientToScreen(Point);
  pm_Favorites.Popup(Point.X, Point.Y);
end;

procedure TfmEConvertStrings.b_CopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := m_Output.Lines.Text;
end;

procedure TfmEConvertStrings.b_InsertClick(Sender: TObject);
var
  i: Integer;
  Lines: TStrings;
begin
  Lines := m_Output.Lines;
  for i := 0 to Lines.Count - 1 do begin
    GxOtaInsertLineIntoEditor(Lines[i] + sLineBreak);
  end;
  ModalResult := mrOk;
end;

procedure TfmEConvertStrings.b_PasteFromClipboardClick(Sender: TObject);
begin
  m_Input.Lines.Text := Clipboard.AsText;
end;

{ TConvertStringsExpert }

procedure TConvertStringsExpert.Execute(Sender: TObject);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := GxOtaGetCurrentSelection(False);
    if sl.Count = 0 then
      sl.Text := Clipboard.AsText;
    TfmEConvertStrings.Execute(GetBitmap, sl);
  finally
    FreeAndNil(sl);
  end;
  IncCallCount;
end;

function TConvertStringsExpert.GetDisplayName: string;
resourcestring
  SConvertStringsName = 'Convert Strings';
begin
  Result := SConvertStringsName;
end;

function TConvertStringsExpert.GetHelpString: string;
resourcestring
  SConvertStringsHelp =
    '  This expert takes the selected code lines (or the text on the clipboard), ' +
    'optionally removes the strings that are used to make them proper Delphi code, ' +
    'leaving you with just the raw strings.' + sLineBreak +
    '  It then uses the selected string prefix/suffix combination to create new strings, ' +
    'that can be pasted back the editor or copied to the clipboard.' + sLineBreak +
    '  To use it, select the string constants in the Delphi editor and ' +
    'activate this expert.';
begin
  Result := SConvertStringsHelp;
end;

class function TConvertStringsExpert.GetName: string;
begin
  Result := 'ConvertStrings';
end;

function TConvertStringsExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterEditorExpert(TConvertStringsExpert);
end.

