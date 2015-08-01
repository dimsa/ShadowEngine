unit uEngine2DShapedSprite;

interface

uses
  uEngine2DSprite, uEngine2DObjectShape, uIntersectorCircle,
  uIntersectorRectangle;

implementation

type
  TCircleSprite = class(TSprite)
  protected
///    procedure SetCurRes(const Value: Integer); override;
    procedure ShapeCreating; override; // ѕо умолчанию создает форму без фигур
  end;

  TRectangleSprite = class(TSprite)
  protected
//    procedure SetCurRes(const Value: Integer); override;
    procedure ShapeCreating; override; // ѕо умолчанию создает форму без фигур
  end;

{ TCircleSprite }

procedure TCircleSprite.ShapeCreating;
begin
  inherited;
 // Shape.AddFigure(TCircleFigure.Create);
end;

{ TRectangleSprite }

procedure TRectangleSprite.ShapeCreating;
begin
  inherited;

end;

end.
