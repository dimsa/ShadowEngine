unit uIntersectorFigure;

interface

uses
  System.Types, System.Generics.Collections, FMX.Objects,
  uIntersectorClasses, uIntersectorShapeModificator;

type
  // Это класс, хранящий в себе информацию о геометрической фигуре
  TFigure = class abstract
  private
    FAutoCalcRadius: Boolean;
  protected
    FCenter: TPointF;
    FMaxRadius: Single;
    procedure SetCenterX(const Value: Single); virtual;
    procedure SetCenterY(const Value: Single); virtual;
    procedure SetCenter(const Value: TPointF); virtual;
    procedure CalculateMaxRadius; virtual; abstract;
  public
    // На случай, если кто-то решит расширять набор фигур
    property X: Single read FCenter.X write SetCenterX;// FCenter.X; // write FCenter.X; // Центр фигуры, от которого считаются сдвиги
    property Y: Single read FCenter.Y write SetCenterY;//FCenter.Y;// write FCenter.Y; // Центр фигуры, от которого считаются сдвиги
    property Center: TPointF read FCenter write SetCenter;// write FCenter; // Заданный центр. Модифкаторы работают от него
    property MaxRadius: Single read FMaxRadius;
    property AutoCalcMaxRadius: Boolean read FAutoCalcRadius write FAutoCalcRadius;

    procedure Translate(const AValue: TPointF); virtual; // Сдвигает центр
    procedure Rotate(const AValue: Single); virtual;
    procedure Scale(const AValue: TPointF); virtual;
    //function InGlobal(const AScale: TPointF; const ARotate: Single; const ATranslate: TPoint): TFigure; virtual; abstract;
//    procedure Translate(const AValue: TPointF); virtual; abstract;
//    procedure FastMigration(const AScale: TPointF; const ARotate: Single); virtual; abstract; // Выполняет действия в одной последовательности.
//    function BelongPointLocal(const AX, AY: Single): Boolean; overload; virtual; abstract;
    function InGlobal(const AScale: TPointF; const ARotate: Single; const ATranslate: TPointF): TFigure; virtual; abstract;
    function BelongPointLocal(const APoint: TPointF): Boolean; virtual; abstract;
    procedure Draw(AImage: TImage); virtual; abstract;

    procedure Assign(const AFigure: TFigure); virtual;
    function Clone: TFigure; virtual;
    constructor Create; virtual;
  end;

implementation

{ TFigure }

procedure TFigure.Assign(const AFigure: TFigure);
begin
  Self.FCenter := AFigure.Center;
  Self.CalculateMaxRadius;
end;

function TFigure.Clone: TFigure;
var
  vRes: TFigure;
begin
  vRes := TFigure.Create;
  vRes.Assign(Self);
  Result := vRes;
end;

constructor TFigure.Create;
begin
  FAutoCalcRadius := True;
  FCenter.X := 0;
  FCenter.Y := 0;
end;

procedure TFigure.Rotate(const AValue: Single);
begin
  if FAutoCalcRadius then
    CalculateMaxRadius;
end;

procedure TFigure.Scale(const AValue: TPointF);
begin
  if FAutoCalcRadius then
    CalculateMaxRadius;
end;

procedure TFigure.SetCenter(const Value: TPointF);
begin
  FCenter := Value;
  if FAutoCalcRadius then
    CalculateMaxRadius;
end;

procedure TFigure.SetCenterX(const Value: Single);
begin
  FCenter.X := Value;
  if FAutoCalcRadius then
    CalculateMaxRadius;
end;

procedure TFigure.SetCenterY(const Value: Single);
begin
  FCenter.Y := Value;
  if FAutoCalcRadius then
    CalculateMaxRadius;
end;

procedure TFigure.Translate(const AValue: TPointF);
begin
  Center := FCenter + AValue;
  if FAutoCalcRadius then
    CalculateMaxRadius;
end;

end.

{    function IntersectWith(const AFigure: TFigure): Boolean; virtual; abstract;
    function BelongPoint(const AX, AY: Double): Boolean; virtual; abstract;  }

