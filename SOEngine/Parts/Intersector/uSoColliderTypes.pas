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
  end;

implementation

end.
