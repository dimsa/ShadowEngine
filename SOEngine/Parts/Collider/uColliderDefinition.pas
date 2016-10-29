unit uColliderDefinition;

interface

uses
  uRawShapes;

type
  TColliderDefinition = class
  private
    FShape: TRawShape;
    FFriction, FDensity: Single;
  public
    property Shape: TRawShape read FShape;
    property Friction: Single read FFriction;
    property Density: Single read FDensity;

    constructor Create(const AShape: TRawShape; const AFriction, ADensity: Single);
  end;


implementation

{ TColliderDefinition }

constructor TColliderDefinition.Create(const AShape: TRawShape; const AFriction,
  ADensity: Single);
begin
  FShape := AShape.Clone;
  FFriction := AFriction;
  FDensity := ADensity;
end;

end.
