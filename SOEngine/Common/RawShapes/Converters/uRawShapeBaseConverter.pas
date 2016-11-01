unit uRawShapeBaseConverter;

interface

uses
  uRawShapes;

type
  TRawShapeConverter<T> = class abstract
  protected const
    CConvertNotImplementedErrorMessage = 'Converting from %class1 to %class2 of some shape not implemented yet';
  public
    class function ConvertFrom(const AObject: T): TRawShape; virtual; abstract;
    class function ConvertTo(const AShape: TRawShape): T; virtual; abstract;
  end;

implementation

end.
