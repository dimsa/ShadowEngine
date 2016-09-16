unit uModel;

interface

uses
  uSoObject;

type
  TGameUnit = class
  protected
    FContainer: TSoObject;
    procedure OnLogicTick(ASender: TSoObject); virtual; abstract;
  public
    constructor Create(const AContainer: TSoObject); virtual;
  end;

  TMovingUnit = class(TGameUnit)
  protected
    FDx, FDy, FDa: Single;
    procedure OnLogicTick(ASender: TSoObject); override;
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

procedure TMovingUnit.OnLogicTick(ASender: TSoObject);
var
   vObj: TSoObject;
begin
  vObj := TSoObject(ASender);
  with vObj do begin
    X := X + FDx;
    Y := Y + FDy;
    Rotate := Rotate + FDa;
  end;
end;

end.
