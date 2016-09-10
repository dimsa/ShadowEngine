unit uUnitCreator;

interface

uses
  System.SysUtils, System.Types,
  uUnitManager, uModel;

type
  TUnitCreator = class
  private
    FUnitManager: TUnitManager;
    FManage: TManageDelegate;
    FManageNew: TManageNewDelegate;
  public
    function NewShip: TShip;
    function NewSpaceDebris(const ASize: integer): TBigAsteroid;
    function NewSpaceDust: TLtlAsteroid;

    constructor Create(const AUnitManager: TUnitManager);
  end;

implementation

{ TUnitCreator }

constructor TUnitCreator.Create(const AUnitManager: TUnitManager);
begin
  FUnitManager := AUnitManager;
  FManage := FUnitManager.Manage;
  FManageNew := FUnitManager.ManageNew;
end;

function TUnitCreator.NewShip: TShip;
var
  vName: string;
begin
  // vName is the name of template
  vName := 'Ship';
  with FManageNew('Ship') do begin
    AddRendition(vName);
    AddColliderObj(vName);
    AddLogic(vName);
  end;
end;

function TUnitCreator.NewSpaceDebris(const ASize: integer): TBigAsteroid;
var
  vName: string;
begin
  // vName is the name of template
  vName := 'Asteroid';
  with FManageNew do begin
    ActiveContainer.Scale := ASize * 0.25;
    AddRendition(vName);
    AddColliderObj(vName);
    AddLogic(vName);
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

  with FManageNew do begin
    AddRendition(vName);
    AddColliderObj(vName);
    AddLogic(vName);
  end;
end;

end.
