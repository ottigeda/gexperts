unit testfile_FluentCode3;

interface

implementation

procedure bla;
begin
  FLaser[LaserNo] := FData.AddDataSeries(Format('LASER#%d', [LaserNo]), Format(_('Laser%d'), [LaserNo]))
    .SetVisibility(not DauerhaftInv, False);
  FLaserInvalid[LaserNo] := FData.AddDataSeries(Format('INV#%d', [LaserNo]), Format(_('Inv%d'), [LaserNo]))
    .SetVisibility(False, False)
    .SetCalculated;
end;

end.