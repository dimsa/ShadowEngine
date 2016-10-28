unit uSoColliderTypes;

interface

uses
  uSoObject;

type
  TPairCollidedEventArgs = record
    Friction: Single;
    Restitution: Single;
    TangentSpeed: Single;

    ObjectA, ObjectB: TSoObject;

    constructor Create(const AFriction, ARestution, ATangentSpeed: Single; const AObjectA, AObjectB: TSoObject);
  end;

  TObjectCollidedEventArgs = record
    Friction: Single;
    Restitution: Single;
    TangentSpeed: Single;

    Opponent: TSoObject;

    constructor Create(const AFriction, ARestution, ATangentSpeed: Single; const AOpponent: TSoObject);
  end;

implementation

{ TCollideEventArgs }

constructor TPairCollidedEventArgs.Create(const AFriction, ARestution,
  ATangentSpeed: Single; const AObjectA, AObjectB: TSoObject);
begin
  Friction := AFriction;
  Restitution := ARestution;
  TangentSpeed := ATangentSpeed;
  ObjectA := AObjectA;
  ObjectB := AObjectB;
end;

{ TObjectCollidedEventArgs }

constructor TObjectCollidedEventArgs.Create(const AFriction, ARestution,
  ATangentSpeed: Single; const AOpponent: TSoObject);
begin
  Friction := AFriction;
  Restitution := ARestution;
  TangentSpeed := ATangentSpeed;
  Opponent := AOpponent;
end;

end.
