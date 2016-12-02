unit uColliderDefinition;

interface

uses
  System.Generics.Collections,
  uRawShapes;

type
  TBodyType = (btStatic, btKinematic, btDynamic);

  TColliderDefinition = class
  private
    FShapes: TList<TRawShape>;
    FFriction, FDensity, FRestitution: Single;
    FIsSensor: Boolean;
    FBodyType: TBodyType;
  public
    property Shape: TList<TRawShape> read FShapes;
    property Friction: Single read FFriction; // Трение
    property Restitution: Single read FRestitution; // Коэффициент упругости
    property Density: Single read FDensity; // Плотность
    property IsSensor: Boolean read FIsSensor; // Будет ли участвовать в коллайде или только фикировать его
    property BodyType: TBodyType read FBodyType;

    constructor Create(const AShapes: TList<TRawShape>; const AFriction, ADensity, ARestitution: Single; const AIsSensor: Boolean; const ABodyType: TBodyType);
    destructor Destroy; override;
  end;


implementation

{ TColliderDefinition }

constructor TColliderDefinition.Create(const AShapes: TList<TRawShape>; const AFriction, ADensity, ARestitution: Single; const AIsSensor: Boolean; const ABodyType: TBodyType);
var
  i: Integer;
begin
  FShapes := TList<TRawShape>.Create;
  for i := 0 to AShapes.Count - 1 do
    FShapes.Add(AShapes[i].Clone);

  FFriction := AFriction;
  FDensity := ADensity;
  FRestitution := ARestitution;
  FIsSensor := AIsSensor;
  FBodyType := ABodyType;
end;

destructor TColliderDefinition.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShapes.Count - 1 do
    FShapes[i].Free;
  FShapes.Free;
  inherited;
end;

end.
