unit uSSBModels;

interface

uses
  FMX.Objects, FMX.StdCtrls, FMX.Controls, System.Types,
  uNamedList, uClasses;

type
  TSSBImagerModel = class
  private
    FSelected: TImage;
    FImages: TNamedList<TImage>; //Картинки из которых состоит подложока объектов
  public
    constructor Create;
    destructor Destroy;
    procedure Adjust(AControl: TControl);
    procedure DelSelected;
    function Select(const AImage: TImage): Boolean;
    property Selected: TImage read FSelected;
  const
    CPrec = 5;
end;

implementation

{ TSSBImagerModel }

procedure TSSBImagerModel.Adjust(AControl: TControl);
var
  i, vX, vY: Integer;
begin

  for i := 0 to FImages.Count - 1 do
    if FImages[i] <> AControl then
    begin
      for vX := 0 to 3 do
        for vY := 0 to 3 do
          with AControl do
          begin
            if (Points[vX].X <= FImages[i].Points[vY].X + CPrec) and
              (Points[vX].X >= FImages[i].Points[vY].X - CPrec) then
              Points[vX] := PointF(FImages[i].Points[vY].X, Points[vX].Y);

            if (Points[vX].Y <= FImages[i].Points[vY].Y + CPrec) and
              (Points[vX].Y >= FImages[i].Points[vY].Y - CPrec) then
              Points[vX] := PointF(Points[vX].X, FImages[i].Points[vY].Y);
          end;
    end;
end;

constructor TSSBImagerModel.Create;
begin
  FImages := TNamedList<TImage>.Create;
end;

procedure TSSBImagerModel.DelSelected;
begin
  if FImages.IsHere(FSelected) then
  begin
    FImages.Delete(FSelected);
    FSelected.Free;
    FSelected := nil;
  end;
end;

destructor TSSBImagerModel.Destroy;
var
  vImg: TImage;
begin
  for vImg in FImages do
    vImg.Free;
  FImages.Clear;
  FImages.Free;
end;

function TSSBImagerModel.Select(const AImage: TImage): Boolean;
begin
  if FImages.IsHere(AImage) then
    FSelected := AImage;
end;

end.
