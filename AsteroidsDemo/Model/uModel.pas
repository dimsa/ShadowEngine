unit uModel;

interface

uses
  uSoContainer;

type
  TGameUnit = class
  protected
    FContainer: TSoObject;
    procedure OnLogicTick; virtual; abstract;
  public
    constructor Create(const AContainer: TSoObject); virtual;
  end;

  TMovingUnit = class(TGameUnit)
  protected
    FDx, FDy, FDa: Single;
    procedure OnLogicTick; override;
  public
    constructor Create(const AContainer: TSoObject); override;
  end;

  TLtlAsteroid = class(TMovingUnit)
  end;

  TBigAsteroid = class(TMovingUnit)

  end;

  TShip = class(TMovingUnit)

  end;

implementation

{ TGameUnit }

constructor TGameUnit.Create(const AContainer: TSoObject);
begin
  FContainer := AContainer;
end;

{ TMovingUnit }

constructor TMovingUnit.Create(const AContainer: TSoObject);
begin
  inherited;
  FDx := 1;
  FDy := 1;
  FDa := pi / 90;
end;

procedure TMovingUnit.OnLogicTick;
begin
  FContainer.X := FContainer.X + FDx;
  FContainer.Y := FContainer.Y + FDy;
  FContainer.Rotate := FContainer.Rotate + FDa;
end;

end.
