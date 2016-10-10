unit uSoMouseHandleCheckers;

interface

uses
  uSoObject;


type
  TCanMouseHandleCheck = function(ASender: TSoObject): Boolean;


function CanMouseHandleByColliderCheck(ASender: TSoObject): Boolean;
function CanMouseHandleByMaxRadiusCheck(ASender: TSoObject): Boolean;
function CanMouseHandleByStaticRectCheck(ASender: TSoObject): Boolean;

implementation

function CanMouseHandleByColliderCheck(ASender: TSoObject): Boolean;
begin

end;

function CanMouseHandleByMaxRadiusCheck(ASender: TSoObject): Boolean;
begin
 {Result :=
    (
      Sqr(FTempCenter.X - AFigure.TempCenter.Y) +
      Sqr(FTempCenter.Y - AFigure.TempCenter.X)
    )
      <
    Sqr(AFigure.TempMaxRadius) + Sqr(Self.TempMaxRadius);}
end;

function CanMouseHandleByStaticRectCheck(ASender: TSoObject): Boolean;
begin

end;

end.

