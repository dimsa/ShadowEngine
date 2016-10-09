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

end;

function CanMouseHandleByStaticRectCheck(ASender: TSoObject): Boolean;
begin

end;

end.

