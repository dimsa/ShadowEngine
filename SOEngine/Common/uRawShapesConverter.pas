unit uRawShapesConverter;

interface

uses
  uRawShapes, System.JSON, UPhysics2D, uNewFigure;

type
  TRawShapeConverter<T> = class abstract
  public
    class function ConvertFrom(const AObject: T): TRawShape; virtual; abstract;
    class function ConvertTo(const AShape: TRawShape): T; virtual; abstract;
  end;

  TRawShapeJsonConverter = class(TRawShapeConverter<TJsonValue>)
  public
    class function ConvertFrom(const AObject: TJsonValue): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): TJsonValue; override;
  end;

  TRawShapeBox2DShapeConverter = class(TRawShapeConverter<Tb2Shape>)
  public
    class function ConvertFrom(const AObject: Tb2Shape): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): Tb2Shape; override;
  end;

  TRawShapeNewFigureConverter = class(TRawShapeConverter<TNewFigure>)
  public
    class function ConvertFrom(const AObject: TNewFigure): TRawShape; override;
    class function ConvertTo(const AShape: TRawShape): TNewFigure; override;
  end;


implementation

{ TRawShapeJsonConverter }

class function TRawShapeJsonConverter.ConvertFrom(
  const AObject: TJsonValue): TRawShape;
begin

end;

class function TRawShapeJsonConverter.ConvertTo(const AShape: TRawShape): TJsonValue;
begin

end;

{ TRawShapeBox2DShapeConverter }

class function TRawShapeBox2DShapeConverter.ConvertFrom(
  const AObject: Tb2Shape): TRawShape;
begin

end;

class function TRawShapeBox2DShapeConverter.ConvertTo(
  const AShape: TRawShape): Tb2Shape;
begin

end;

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
