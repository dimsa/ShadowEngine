unit uModel;

interface

uses
  uSoObject, uSoTypes, uLogicAssets, uUnitManager, System.SysUtils, uSoObjectDefaultProperties,
  FMX.Dialogs, uGeometryClasses;

type
  TGameUnit = class
  protected
    FContainer: TSoObject;
    FManager: TUnitManager;
    procedure Init; virtual; abstract;
  public
    constructor Create(const AManager: TUnitManager); virtual;
  end;

  TMovingUnit = class(TGameUnit)
  protected
    FAcceleration: TAcceleration;
  public
    constructor Create(const AManager: TUnitManager); override;
  end;

  TLtlAsteroid = class(TMovingUnit)
  protected
    procedure Init; override;
  end;

  TBigAsteroid = class(TMovingUnit)
  protected
    procedure Init; override;
  end;

  TShipFire = class(TGameUnit)
  private
    FPower: Single;
    procedure SetPower(const Value: Single);
  protected
    procedure Init; override;
  public
    property Power: Single read FPower write SetPower;
  end;

  TLeftFire = class(TShipFire)
  protected
    procedure Init; override;
  end;

  TRightFire = class(TShipFire)
  protected
    procedure Init; override;
  end;

  TShip = class(TMovingUnit)
  private
    FLeftFire, FRightFire: TShipFire;
    FDest: TList<TPointF>;
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    procedure Init; override;
  public
    procedure AddDestination(const APosition: TPointF);
  end;

implementation

{ TGameUnit }

constructor TGameUnit.Create(const AManager: TUnitManager);
begin
  FManager := AManager;
  Init;
end;

{ TMovingUnit }

constructor TMovingUnit.Create(const AManager: TUnitManager);
begin
  FManager := AManager;
  FAcceleration := TAcceleration.Create;
  Init;
end;

{ TShip }

procedure TShip.AddDestination(const APosition: TPointF);
begin
  FDest.Add(APosition);
end;

procedure TShip.Init;
var
  vTemplateName: string;
begin
  FDest := TList<TPointF>.Create;
  vTemplateName := 'Ship';
  with FManager.New(vTemplateName) do begin
    AddRendition(vTemplateName);
   // AddColliderObj(vTemplateName);
    AddProperty('Acceleration', FAcceleration);
    AddProperty('Destinations', FDest);
    AddProperty('World', FManager.ObjectByName('World'));
    AddNewLogic(MovingToDestination, 'MovingThroughSides');
    AddMouseHandler(ByStaticRect).OnMouseDown := OnMouseDown;
    ActiveContainer.Scale := 1;
  end;
end;

procedure TShip.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ShowMessage('Click');
end;

{ TBigAsteroid }

procedure TBigAsteroid.Init;
var
  vTemplateName: string;
begin
  inherited;
  vTemplateName := 'Asteroid';
  with FManager.New do begin
    AddRendition(vTemplateName);
    AddColliderObj(vTemplateName);
    AddProperty('Acceleration', FAcceleration);
    AddProperty('World', FManager.ObjectByName('World'));
    AddNewLogic(MovingByAcceleration);
  end;
end;

procedure TShipFire.Init;
begin
  with FManager.New do begin
    AddRendition('FireLeft');
    AddRendition('FireRight');
    AddProperty('Ship', FManager.ObjectByName('Ship'));
  end;
end;

procedure TShipFire.SetPower(const Value: Single);
begin
  FPower := Value;
  FContainer.Scale := FPower;
end;

{ TLtlAsteroid }

procedure TLtlAsteroid.Init;
var
  vTemplateName: string;
begin
  inherited;
  if Random(2) = 0 then
    vTemplateName := 'Star'
  else
    vTemplateName := 'LittleAsteroid' + IntToStr(Random(3));

 with FManager.New do begin
    AddRendition(vTemplateName);
    AddColliderObj(vTemplateName);
    AddProperty('Acceleration', FAcceleration);
    AddProperty('World', FManager.ObjectByName('World'));
    AddNewLogic(MovingByAcceleration);
  end;
end;

{ TLeftFire }

procedure TLeftFire.Init;
begin
  inherited;

end;

{ TRightFire }

procedure TRightFire.Init;
begin
  inherited;

end;

end.
