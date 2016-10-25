unit uSoColliderTypes;

interface

uses
  uSoObject;

type
  TCollideEventArgs = record
    Friction: Single;
    Restitution: Single;
    TangentSpeed: Single;

    ObjectA, ObjectB: TSoObject;

    constructor Create(const AFriction, ARestution, ATangentSpeed: Single; const AObjectA, AObjectB: TSoObject);
  end;

implementation

{ TCollideEventArgs }

constructor TCollideEventArgs.Create(const AFriction, ARestution,
  ATangentSpeed: Single; const AObjectA, AObjectB: TSoObject);
begin
  Friction := AFriction;
  Restitution := ARestution;
  TangentSpeed := ATangentSpeed;
  ObjectA := AObjectA;
  ObjectB := AObjectB;
end;

end.
