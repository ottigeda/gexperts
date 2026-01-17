unit testfile_NestedFunctionIndent;

interface

implementation

procedure Test;
begin
  if Tf_dzDialog.ShowMessage(mtConfirmation,
    Format(_('The segment length (%d m) from' + #13#10 +
        '%s' + #13#10 +
        'does not match this vehicle type (expected %d m).' + #13#10 + #13#10 +
        'Do you want to continue anyway?'),
      [_IstLenM, _GmsFn, _SollLenM]),
    [dbeYes, dbeNo], nil, '', 1) <> mrYes then begin
    SysUtils.Abort;
  end;
end;

end.
