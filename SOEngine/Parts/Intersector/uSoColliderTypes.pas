unit uSoColliderTypes;

interface

uses
  uSoObject;

type
  TCollideEventArgs = record
    Friction: Single;
    FRestitution: Single;
    FTangentSpeed: Single;

    ObjectA, ObjectB: TSoObject;
  end;

implementation

end.
