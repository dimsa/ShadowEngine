unit uUnitCreator;

interface

uses
  uSoTypes, uGeometryClasses,
  uEngine2DClasses, uUnitManager, uWorldManager, uModel, uSoObject;

type
  TGameUnitFriend = class(TGameUnit);

  TUnitCreator = class
  private
//    FSimpleManager: TSoSimpleManager;
    FUnitManager: TUnitManager;
  public
    function NewShip: TShip;
    function NewSpaceDebris(const ASize: integer): TBigAsteroid;
    function NewSpaceDust: TLtlAsteroid;
    constructor Create(const AUnitManager: TUnitManager);
  end;

implementation

uses
  uLogicAssets;

{ TUnitCreator }

constructor TUnitCreator.Create(const AUnitManager: TUnitManager);
begin
  FUnitManager := AUnitManager;
end;

function TUnitCreator.NewShip: TShip;
begin
  Result := TShip.Create(FUnitManager);
end;

function TUnitCreator.NewSpaceDebris(const ASize: integer): TBigAsteroid;
begin
  Result := TBigAsteroid.Create(FUnitManager);
end;

function TUnitCreator.NewSpaceDust: TLtlAsteroid;
begin
  Result := TLtlAsteroid.Create(FUnitManager);

end;

end.
