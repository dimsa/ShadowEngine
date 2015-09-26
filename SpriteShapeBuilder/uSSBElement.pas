unit uSSBElement;

interface

uses
  FMX.Objects, FMX.Controls, System.Classes;

type
  TSSBElement = class(TImage)
  private

  public
    procedure AddCircle;
    procedure AddPoly;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TSSBElement }

procedure TSSBElement.AddCircle;
begin

end;

procedure TSSBElement.AddPoly;
begin

end;

constructor TSSBElement.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSSBElement.Destroy;
begin

  inherited;
end;

end.
