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
    FContainer: TSoObject;
  //  procedure FollowTheShip(ASender: TObject; APosition: TPosition);
    function GetPosition: TPointF;
    procedure SetPosition(const Value: TPointF);
    procedure SetPower(const Value: Single);
  protected
    FShip: TSoObject;
    procedure Init; override;
  public
    constructor Create(const AManager: TUnitManager; AShip: TSoObject);
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

uses
  uSoSprite;

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
  FAcceleration.Dx := 4;
  FAcceleration.Dy := 4;
  FAcceleration.Da := 4;
  Init;
end;

{ TShip }

procedure TShip.AddDestination(const APosition: TPointF);
begin
  if FDest.Count <= 0 then
    FDest.Add(APosition)
  else
    FDest[0] := APosition;
end;

procedure TShip.Init;
var
  vTemplateName: string;
begin
  FDest := TList<TPointF>.Create;
  vTemplateName := 'Ship';

  with FManager.New(vTemplateName) do begin
    FContainer := ActiveContainer;
    AddRendition(vTemplateName);
   // AddColliderObj(vTemplateName);
    AddProperty('Acceleration', FAcceleration);
    AddProperty('Destinations', FDest);
    AddProperty('World', FManager.ObjectByName('World'));
    AddNewLogic(MovingToDestination, 'MovingThroughSides');
    AddMouseHandler(ByStaticRect).OnMouseDown := OnMouseDown;
  end;

  FLeftFire := TLeftFire.Create(FManager, FContainer);
  FRightFire := TRightFire.Create(FManager, FContainer);
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
    FContainer := ActiveContainer;
    AddRendition(vTemplateName);
  //  AddColliderObj(vTemplateName);
    AddProperty('Acceleration', FAcceleration);
    FAcceleration.Dx := Random(3) + Random  - 2;
    FAcceleration.Dy := Random(3) + Random  - 2;
    ActiveContainer.X := Random(Round(FManager.ObjectByName('World').Width));
    ActiveContainer.Y := Random(Round(FManager.ObjectByName('World').Height));
    AddProperty('World', FManager.ObjectByName('World'));
    AddNewLogic(MovingByAcceleration);
  end;
end;

constructor TShipFire.Create(const AManager: TUnitManager; AShip: TSoObject);
begin
  FShip := AShip;
  inherited Create(AManager);

//  AShip.AddChangePositionHandler(FollowTheShip);
end;

{procedure TShipFire.FollowTheShip(ASender: TObject; APosition: TPosition);
begin
  FContainer.Center := APosition.XY;
  FContainer.Rotate := APosition.Rotate;

  FContainer[Rendition].Val<TSoSprite>.NextFrame;
end;  }

function TShipFire.GetPosition: TPointF;
begin
  Result := FContainer.Center;
end;

procedure TShipFire.Init;
begin
  FPower := 0.4;
end;

procedure TShipFire.SetPosition(const Value: TPointF);
begin
  FContainer.Center := Value;
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
    FContainer := ActiveContainer;
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

  with FManager.New do begin
    FContainer := ActiveContainer;
    AddRendition('FireLeft');
    AddNewLogic(FollowTheShip);
    AddProperty('Ship', FShip);
    FContainer[Rendition].Val<TSoSprite>.BringToBack;
  end;
end;

{ TRightFire }

procedure TRightFire.Init;
begin
  inherited;

  with FManager.New do begin
    FContainer := ActiveContainer;
    AddRendition('FireRight');
    AddNewLogic(FollowTheShip);
    AddProperty('Ship', FShip);
    FContainer[Rendition].Val<TSoSprite>.BringToBack;
  end;
end;

end.
