unit uSoBasePosition;

interface

uses
  uSoPositionConverter, uGeometryClasses;

type
  TSoBasePosition = class abstract
  private
    FSoPositionConverter: ISoPositionConverter;
    FPosition: TPosition;
  protected
    procedure OnPositionChanged;
  public
    property Position: TPosition read FPosition;
    constructor Create(const AConverter: ISoPositionConverter);
    destructor Destroy; override;
  end;

implementation

{ TSoPosition }

constructor TSoBasePosition.Create(const AConverter: ISoPositionConverter);
begin
  FSoPositionConverter := AConverter;
end;

destructor TSoBasePosition.Destroy;
begin
  FSoPositionConverter := nil;
  inherited;
end;

procedure TSoBasePosition.OnPositionChanged;
begin
  FPosition := FSoPositionConverter.CalculatePosition;
end;

end.
