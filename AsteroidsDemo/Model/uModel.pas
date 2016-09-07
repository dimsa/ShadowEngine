unit uModel;

interface

uses
  uSoContainer;

type
  TGameUnit = class
  protected
    FContainer: TSoContainer;
    procedure OnLogicTick; virtual; abstract;
  public
    constructor Create(const AContainer: TSoContainer); virtual;
  end;

  TLtlAsteroid = class(TGameUnit)

  end;

  TBigAsteroid = class(TGameUnit)

  end;

  TShip = class(TGameUnit)

  end;

implementation

{ TGameUnit }

constructor TGameUnit.Create(const AContainer: TSoContainer);
begin
  FContainer := AContainer;
end;

end.
