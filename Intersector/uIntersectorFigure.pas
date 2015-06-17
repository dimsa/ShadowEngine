unit uIntersectorFigure;

interface

uses
  System.Types, System.Generics.Collections, FMX.Objects,
  uIntersectorClasses, uIntersectorShapeModificator;

type
  TFigure = class abstract
  private
    function GetSize: Single; virtual; abstract;
    procedure SetSize(const Value: Single); virtual; abstract;
  protected
    FCenter: TPointF;
  public
    // На случай, если кто-то решит расширять набор фигур
    property X: Single read FCenter.X write FCenter.X; // write FCenter.X; // Центр фигуры, от которого считаются сдвиги
    property Y: Single read FCenter.Y write FCenter.Y;// write FCenter.Y; // Центр фигуры, от которого считаются сдвиги
    property Size: Single read GetSize write SetSize;
    property Center: TPointF read FCenter write FCenter;// write FCenter; // Заданный центр. Модифкаторы работают от него

    procedure Rotate(const AValue: Single); virtual; abstract;
    procedure Scale(const AValue: TPointF); virtual; abstract;
    //function InGlobal(const AScale: TPointF; const ARotate: Single; const ATranslate: TPoint): TFigure; virtual; abstract;
//    procedure Translate(const AValue: TPointF); virtual; abstract;
//    procedure FastMigration(const AScale: TPointF; const ARotate: Single); virtual; abstract; // Выполняет действия в одной последовательности.
    function BelongPointLocal(const AX, AY: Single): Boolean; virtual; abstract;
    procedure Draw(AImage: TImage); virtual; abstract;

    procedure Assign(const AFigure: TFigure); virtual;
    function Clone: TFigure; virtual;
    function FigureRect: TRectF; virtual; abstract;
    constructor Create; virtual;
  const
    pi180 = 0.01745329251;
  end;

implementation

{ TFigure }

procedure TFigure.Assign(const AFigure: TFigure);
begin
  Self.FCenter := AFigure.Center;
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
{  FPositionChange.X := 0;
  FPositionChange.Y := 0;
  FPositionChange.ScaleX := 1;
  FPositionChange.ScaleY := 1;
  FPositionChange.Rotate := 0; }
end;

end.

{    function IntersectWith(const AFigure: TFigure): Boolean; virtual; abstract;
    function BelongPoint(const AX, AY: Double): Boolean; virtual; abstract;  }

