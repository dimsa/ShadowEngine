unit uIntersectorFigure;

interface

uses
  System.Types,
  uIntersectorClasses;

type
  TFigure = class abstract
  private
    FPosition: TPosition;
    FIntersectionComparer: Pointer;
    procedure SetPosition(const Value: TPosition);
  protected
    procedure Recalc; virtual; abstract;
  public
    // На случай, если кто-то решит расширять набор фигур
    property IntersectionComparer: Pointer read FIntersectionComparer write FIntersectionComparer;
    property Position: TPosition read FPosition write SetPosition;
    function FigureRect: TRectF; virtual; abstract;
    function IntersectWith(const AFigure: TFigure): Boolean; virtual; abstract;
    function BelongPoint(const AX, AY: Double): Boolean; virtual; abstract;

    constructor Create;
  end;

implementation

{ TFigure }

constructor TFigure.Create;
begin

end;

procedure TFigure.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
  Recalc;
end;

end.
