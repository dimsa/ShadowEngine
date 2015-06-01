unit uIntersectorFigure;

interface

uses
  System.Types,
  uIntersectorClasses;

type
  TFigure = class
  private
    FPosition: TPosition;
  protected
    FIntersectionComparer: Pointer; // На случай, если кто-то решит расширять набор фигур
  public
    property Position: TPosition read FPosition write FPosition;
    function FigureRect: TRectF; virtual; abstract;
    function IntersectWith(const AFigure: TFigure): Boolean; virtual; abstract;
    function BelongPoint(const AX, AY: Integer): Boolean; virtual; abstract;

    constructor Create;
  end;

implementation

{ TFigure }

constructor TFigure.Create;
begin

end;

end.
