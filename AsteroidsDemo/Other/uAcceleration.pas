unit uAcceleration;

interface

type

  TAcceleration = class
  private
    FDx, FDy, FDa: Single;
    procedure SetDa(const Value: Single);
    procedure SetDx(const Value: Single);
    procedure SetDy(const Value: Single);
  public
    property Dx: Single read FDx write SetDx;
    property Dy: Single read FDy write SetDy;
    property Da: Single read FDa write SetDa;
    constructor Create;
  end;

implementation

{ TAccelerate }

constructor TAcceleration.Create;
begin
  FDx := 1;
  FDy := 1;
  FDa := pi / 90;
end;

procedure TAcceleration.SetDa(const Value: Single);
begin
  FDa := Value;
end;

procedure TAcceleration.SetDx(const Value: Single);
begin
  FDx := Value;
end;

procedure TAcceleration.SetDy(const Value: Single);
begin
  FDy := Value;
end;

end.
