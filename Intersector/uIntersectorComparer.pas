unit uIntersectorComparer;

interface

uses
  uIntersectorFigure, uIntersectorCircle;

type
  TIntersectionComparer = class
  strict private
    class var FInstance: TIntersectionComparer;
    constructor Create;
  public
    class function Instance: TIntersectionComparer;
    class function CircleCircleIntersect(const AFigure1, AFigure2: TCircleFigure): Boolean;
    function IsFiguresIntersect(const AFigure1, AFigure2: TFigure): Boolean; virtual;
  end;

implementation

{ TIntersectionComparer }

class function TIntersectionComparer.CircleCircleIntersect(const AFigure1,
  AFigure2: TCircleFigure): Boolean;
begin
  Result :=
    (sqr(AFigure1.Position.X-AFigure2.Position.X)+sqr(AFigure1.Position.Y-AFigure2.Position.Y))
    <=
    (AFigure1.Radius + AFigure2.Radius);
end;

constructor TIntersectionComparer.Create;
begin

end;

class function TIntersectionComparer.Instance: TIntersectionComparer;
begin
  if not Assigned(FInstance) then
    FInstance := TIntersectionComparer.Create;
  Result := FInstance;
end;

function TIntersectionComparer.IsFiguresIntersect(const AFigure1,
  AFigure2: TFigure): Boolean;
begin
  Result := False;
  if (AFigure1 is TCircleFigure) and (AFigure2 is TCircleFigure) then
    Result := CircleCircleIntersect(TCircleFigure(AFigure1), TCircleFigure(AFigure2));
end;

end.
