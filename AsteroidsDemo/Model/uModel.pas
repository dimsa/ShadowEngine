unit uModel;

interface

uses
  uSoObject, uSoTypes;

type
  TGameUnit = class
  protected
    FContainer: TSoObject;
    procedure OnLogicTick(ASender: TSoObject); virtual; abstract;
  public
    procedure SetWorldSize(const ASize: TPointF);
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

procedure TGameUnit.SetWorldSize(const ASize: TPointF);
begin
  FContainer['WorldWidth'].AsDouble := ASize.X;
  FContainer['WorldHeight'].AsDouble := ASize.Y;
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

   if X < - vObj['Width'].AsDouble * 0.5 then
     X := vObj['WorldWidth'].AsDouble + vObj['Width'].AsDouble;

   if Y  < - vObj['Height'].AsDouble * 0.5 then
     Y := vObj['WorldHeight'].AsDouble + vObj['Height'].AsDouble;

   if X > vObj['WorldWidth'].AsDouble + vObj['Width'].AsDouble  then
     X := - vObj['Width'].AsDouble * 0.5;

   if Y > vObj['WorldHeight'].AsDouble + vObj['Height'].AsDouble then
     Y := - vObj['Height'].AsDouble * 0.5;
  end;




end;

end.
