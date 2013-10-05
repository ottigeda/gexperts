unit GX_CodeFormatterRegisterExpert;

{$I GX_CondDefine.inc}

interface

implementation

uses
  GX_Experts,
  GX_Formatter;

initialization
  RegisterGX_Expert(TGxCodeFormatterExpert);
end.

