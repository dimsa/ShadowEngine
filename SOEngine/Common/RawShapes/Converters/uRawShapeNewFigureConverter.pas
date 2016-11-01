unit uRawShapeNewFigureConverter;

interface

uses
  uRawShapeBaseConverter, uNewFigure, uRawShapes;

type
  TRawShapeNewFigureConverter = class(TRawShapeConverter<TNewFigure>)
  public
    class function ConvertFrom(const AObject: TNewFigure): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): TNewFigure; override;
  end;

implementation

{ TRawShapeNewFigureConverter }

class function TRawShapeNewFigureConverter.ConvertFrom(
  const AObject: TNewFigure): TRawShape;
begin

end;

class function TRawShapeNewFigureConverter.ConvertTo(
  const AShape: TRawShape): TNewFigure;
begin

end;

end.
