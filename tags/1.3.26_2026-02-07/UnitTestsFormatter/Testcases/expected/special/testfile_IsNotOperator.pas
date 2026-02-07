unit testfile_IsNotOperator;

interface

type
  TAnimal = class
  end;

  TDog = class(TAnimal)
  end;

  TCat = class(TAnimal)
  end;

procedure TestIsNot;

implementation

procedure TestIsNot;
var
  Animal: TAnimal;
begin
  Animal := TDog.Create;
  try
    if  Animal is not TCat then
      WriteLn('Not a cat');

    if  Animal is not TDog then
      WriteLn('Not a dog')
    else
      WriteLn('Is a dog');

    // Combined with other conditions
    if  (Animal is not TCat) and (Animal is TDog) then
      WriteLn('Definitely a dog');
  finally
    Animal.Free;
  end;
end;

end.
