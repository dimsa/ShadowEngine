unit uClasses;

interface

uses
  System.SysUtils;

type
  TProcedure = procedure of Object;
  TVCLProcedure = procedure(ASender: TObject) of Object;

  function Random64: Int64;

implementation

function Random64: Int64;
begin
   Int64Rec(result).Words[0] := Random(Word.MaxValue);
   Int64Rec(result).Words[1] := Random(Word.MaxValue);
   Int64Rec(result).Words[2] := Random(Word.MaxValue);
   Int64Rec(result).Words[3] := Random(Word.MaxValue);
end;

end.



