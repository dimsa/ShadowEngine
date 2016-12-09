unit uSoPositionConverter;

interface

uses
  uSoTypes, uGeometryClasses;

type
  ISoPositionConverter = interface
    function CalculatePosition: TPosition;
  end;

implementation

end.
