unit uIntersectorFigure;

interface

uses
  System.Types, System.Generics.Collections,
  uIntersectorClasses, uIntersectorShapeModificator;

type
  TFigure = class abstract
  private
  //  FPositionChange: TPosition;
    //FIntersectionComparer: Pointer;
  //  FModificators: TList<TShapeModificator>;
  //  procedure SetPositionChange(const Value: TPosition);
 //   function GetCalced: TPointF;
    function GetSize: Single; virtual; abstract;
    procedure SetSize(const Value: Single); virtual; abstract;
    function GetCalced: TPointF;
//    procedure SetPositionChange(const Value: TPosition);
  protected
    FCenter: TPointF;
  public
    // На случай, если кто-то решит расширять набор фигур
    property X: Single read FCenter.X;// write FCenter.X; // Центр фигуры, от которого считаются сдвиги
    property Y: Single read FCenter.Y;// write FCenter.Y; // Центр фигуры, от которого считаются сдвиги
    property Size: Single read GetSize write SetSize;
    property Center: TPointF read FCenter;// write FCenter; // Заданный центр. Модифкаторы работают от него

    procedure Rotate(const AValue: Single); virtual; abstract;
    procedure Scale(const AValue: TPointF); virtual; abstract;
    procedure Translate(const AValue: TPointF); virtual; abstract;

    procedure Assign(const AFigure: TFigure); virtual;
    function Clone: TFigure; virtual;
    function FigureRect: TRectF; virtual; abstract;

    constructor Create; virtual;
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

function TFigure.GetCalced: TPointF;
begin

end;

{function TFigure.GetPoints: TArray<TPointF>;
begin
//  Compute;
  Result := FPoints;
end;  }

{procedure TFigure.SetPositionChange(const Value: TPosition);
begin
  FPositionChange := Value;
end;   }

end.

{    function IntersectWith(const AFigure: TFigure): Boolean; virtual; abstract;
    function BelongPoint(const AX, AY: Double): Boolean; virtual; abstract;  }

