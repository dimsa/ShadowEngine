unit uSoColliderTypes;

interface

uses
  uSoColliderObject;

type
  TPairCollidedEventArgs = record
    Friction: Single;
    Restitution: Single;
    TangentSpeed: Single;

    ObjectA, ObjectB: TSoColliderObj;

    constructor Create(const AFriction, ARestution, ATangentSpeed: Single; const AObjectA, AObjectB: TSoColliderObj);
  end;

implementation

constructor TPairCollidedEventArgs.Create(const AFriction, ARestution,
  ATangentSpeed: Single; const AObjectA, AObjectB: TSoColliderObj);
begin
  Friction := AFriction;
  Restitution := ARestution;
  TangentSpeed := ATangentSpeed;
  ObjectA := AObjectA;
  ObjectB := AObjectB;
end;

end.
