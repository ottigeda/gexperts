{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JeMSAgent.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Last Modified: 2002-02-28

You may retrieve the latest version of this file at the JEDI Experts home page,
located at http://jexperts.sourceforge.net/

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JeMSAgent;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AgentObjects_TLB;

type
  TJeMSAgent = class(TComponent)
  private
    FAgent: string;
    FLanguage: Integer;
  protected
    procedure BalloonHide(Sender: TObject;
      const CharacterID: WideString);virtual;
  public
    procedure SayWithAgent(Value: string);
    constructor Create(AOwner: TComponent);override;
    property Language:Integer read FLanguage write FLanguage default LANG_ENGLISH;
  published
    property Agent:string read FAgent write FAgent;
  end;

implementation

{************************************************************}
procedure TJeMSAgent.BalloonHide(Sender: TObject;
  const CharacterID: WideString);
begin
  with (Sender as TAgent) do
  begin
    if Characters.Get_Item(FAgent)<>nil then
     with Characters.Get_Item(FAgent) do
     begin
       Characters.Unload(FAgent);
       Connected := false;
     end;
    Free;
  end;
end;
{************************************************************}
constructor TJeMSAgent.Create(AOwner: TComponent);
begin
  inherited;
  FAgent := 'Merlin';
  FLanguage := LANG_ENGLISH;
end;
{************************************************************}
procedure TJeMSAgent.SayWithAgent(Value: string);
begin
  try
    with TAgent.Create(nil) do
    begin
      OnBalloonHide := BalloonHide;
      Connected := true;
      Characters.Load(FAgent,FAgent+'.acs');
      with Characters.Character(FAgent) do
      begin
//        LanguageID := FLanguage;         // didn't work on Win2k, maybe on Win95
        Language := FLanguage;
        Show(true);
        MoveTo(Screen.Width-(Width+20),Screen.Height-(Height+20), 0);
        Speak(Value,'');
      end;
    end;
  except
  end;
end;
{************************************************************}
end.
 