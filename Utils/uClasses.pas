unit uClasses;

interface

uses
  System.SysUtils;

type
  TProcedure = procedure of Object;
  TVCLProcedure = procedure(ASender: TObject) of Object;

  function Random64: Int64;
  procedure NormalizeAngle(var AAngle: Single);

implementation

procedure NormalizeAngle(var AAngle: Single);
begin
  if AAngle < -180 then
  begin
    AAngle := AAngle + 360;
    NormalizeAngle(AAngle);
  end;

  if AAngle > 180 then
  begin
    AAngle := AAngle - 360;
    NormalizeAngle(AAngle);
  end;

end;

function Random64: Int64;
begin
   Int64Rec(result).Words[0] := Random(Word.MaxValue);
   Int64Rec(result).Words[1] := Random(Word.MaxValue);
   Int64Rec(result).Words[2] := Random(Word.MaxValue);
   Int64Rec(result).Words[3] := Random(Word.MaxValue);
end;

end.



