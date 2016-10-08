unit uClasses;

interface

uses
  System.SysUtils, System.Types, System.UITypes;

type
  TBooleanFunction = function: Boolean of object;

type
  TProcedure = procedure of object;

  function Random64: Int64;
  procedure NormalizeAngle(var AAngle: Single);
  function RGBColor(const AR, AG, AB, AA: Byte): TAlphaColorRec;

implementation

function RGBColor(const AR, AG, AB, AA: Byte): TAlphaColorRec;
begin
  Result.R := AR;
  Result.G := AG;
  Result.B := AB;
  Result.A := AA;
end;

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
