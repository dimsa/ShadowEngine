unit uSSBElement;

interface

uses
  FMX.Objects, FMX.Controls, System.Classes, System.Generics.Collections,
  System.Math, FMX.Types, System.Types, FMX.Graphics,
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
begin
  vFigure := TNewFigure.Create(TNewFigure.cfPoly);
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

end.
