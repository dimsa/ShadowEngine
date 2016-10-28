unit uSoColliderObjectTypes;

interface

uses
  uSoObject;

type
  TObjectCollidedEventArgs = record
    Friction: Single;
    Restitution: Single;
    TangentSpeed: Single;

    Opponent: TSoObject;

    constructor Create(const AFriction, ARestution, ATangentSpeed: Single; const AOpponent: TSoObject);
  end;

implementation

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
