unit uUnitCreator;

interface

uses
  System.SysUtils, System.Types, FMX.Objects,
  uEngine2DClasses, uUnitManager, uWorldManager, uModel, uSoObject;

type
  TGameUnitFriend = class(TGameUnit);

  TUnitCreator = class
  private
//    FSimpleManager: TSoSimpleManager;
    FUnitManager: TUnitManager;
 //   FWorldManager: TWorldManager;
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
//  FSimpleManager := ASimpleManager;
  FUnitManager := AUnitManager;
//  FWorldManager :=  AWorldManager;
end;

function TUnitCreator.NewShip: TShip;
begin

  // vName is the name of template
{  vName := 'Ship';
  with ManageNew('Ship') do begin}
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
