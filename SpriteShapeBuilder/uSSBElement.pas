unit uSSBElement;

interface

uses
  FMX.Objects, FMX.Controls, System.Classes, System.Generics.Collections,
  {$IFDEF VER290} System.Math.Vectors, {$ENDIF} System.Math, FMX.Types,
  System.Types, FMX.Graphics,
  uNewFigure, uIntersectorClasses;

type
  TSSBElement = class(TImage)
  private
    FFigures: TList<TNewFigure>;
    procedure DoShapeRepaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  public
    procedure AddCircle;
    procedure AddPoly;
    function FigureByCoord(const APoint: TPointF): TNewFigure;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TSSBElement }

procedure TSSBElement.AddCircle;
var
  vFigure: TNewFigure;
  vCircle: TCircle;
begin
  vFigure := TNewFigure.Create(TNewFigure.cfCircle);
  vCircle.X := Self.Width / 2;
  vCircle.Y := Self.Height / 2;
  vCircle.Radius := Min(Width, Height) / 2;

  vFigure.SetData(vCircle);
  FFigures.Add(vFigure);
  Self.Repaint;
end;

procedure TSSBElement.AddPoly;
var
  vFigure: TNewFigure;
  vPoly: TPolygon;
begin
  vFigure := TNewFigure.Create(TNewFigure.cfPoly);
  SetLength(vPoly, 3);
  vPoly[0] := PointF(Self.Width* 0.5, Self.Height * 0.33);
  vPoly[1] := PointF(Self.Width* 0.75, Self.Height * 0.66);
  vPoly[2] := PointF(Self.Width* 0.25, Self.Height * 0.66);

  vFigure.SetData(vPoly);
  FFigures.Add(vFigure);
  Self.Repaint;
end;

constructor TSSBElement.Create(AOwner: TComponent);
begin
  inherited;
  FFigures := TList<TNewFigure>.Create;
  Self.OnPaint := DoShapeRepaint;
end;

destructor TSSBElement.Destroy;
var
  vFigure: TNewFigure;
begin
  for vFigure in FFigures do
  begin
    FFigures.Remove(vFigure);
    vFigure.Free;
  end;
  FFigures.Clear;

  inherited;
end;

procedure TSSBElement.DoShapeRepaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  i: Integer;
begin
  for i := 0 to FFigures.Count - 1 do
    FFigures[i].Draw(Canvas);
end;

function TSSBElement.FigureByCoord(const APoint: TPointF): TNewFigure;
var
  i: Integer;
begin
  for i := 0 to FFigures.Count - 1 do
  begin
    if FFigures[i].BelongPointLocal(APoint) then
    begin
      Result := FFigures[i];
      Exit;
    end;
  end;
  Result := nil;
end;

end.
