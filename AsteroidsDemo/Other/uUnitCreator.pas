unit uUnitCreator;

interface

uses
  System.SysUtils, System.Types, FMX.Objects,
  uEngine2DClasses, uUnitManager, uWorldManager, uModel, uSoObject;

type
  TGameUnitFriend = class(TGameUnit);

  TUnitCreator = class
  private
    FUnitManager: TUnitManager;
    FWorldManager: TWorldManager;
  // Delegates
    ManageByName: TManageByNameDelegate;
    ManageNew: TManageNewDelegate;
    Manage: TManageDelegate;
    procedure OnResize(ASender: TObject; AImage: TImage);
  public
    function NewShip: TShip;
    function NewSpaceDebris(const ASize: integer): TBigAsteroid;
    function NewSpaceDust: TLtlAsteroid;
    constructor Create(const AUnitManager: TUnitManager; const AWorldManager: TWorldManager);
  end;

implementation

{ TUnitCreator }

constructor TUnitCreator.Create(const AUnitManager: TUnitManager; const AWorldManager: TWorldManager);
begin
  FUnitManager := AUnitManager;
  FWorldManager :=  AWorldManager;
  Manage := FUnitManager.Manage;
  ManageNew := FUnitManager.ManageNew;
  ManageByName := FUnitManager.Manage;
end;

function TUnitCreator.NewShip: TShip;
var
  vName: string;
begin
  // vName is the name of template
  vName := 'Ship';
  with ManageNew('Ship') do begin
    Result := TShip.Create(ActiveContainer);
    AddRendition(vName);
//    AddColliderObj(vName);
    AddNewLogic(TGameUnitFriend(Result).OnLogicTick);
    AddProperty('Lifes', 3);
    AddProperty('WorldWidth', FWorldManager.Size.X);
    AddProperty('WorldHeight', FWorldManager.Size.Y);
  end;

end;

function TUnitCreator.NewSpaceDebris(const ASize: integer): TBigAsteroid;
var
  vName: string;
begin
  // vName is the name of template
  vName := 'Asteroid';
  with ManageNew do begin
    Result := TBigAsteroid.Create(ActiveContainer);
    ActiveContainer.Scale := ASize * 0.25;
    AddRendition(vName);
    AddColliderObj(vName);
    AddNewLogic(TGameUnitFriend(Result).OnLogicTick);
  end;
end;

function TUnitCreator.NewSpaceDust: TLtlAsteroid;
var
  vName: string;
begin
  // vName is the name of template
  if Random(2) = 0 then
    vName := 'Star'
  else
    vName := 'LittleAsteroid' + IntToStr(Random(3));

  with ManageNew do begin
    Result := TLtlAsteroid.Create(ActiveContainer);
    AddRendition(vName);
    AddColliderObj(vName);
    AddNewLogic(TGameUnitFriend(Result).OnLogicTick);
  end;
end;

procedure TUnitCreator.OnResize(ASender: TObject; AImage: TImage);
begin
  with ManageByName('Ship') do
  begin
    AddProperty('WorldWidth', FWorldManager.Size.X);
    AddProperty('WorldHeight', FWorldManager.Size.Y);
  end;
end;

end.
