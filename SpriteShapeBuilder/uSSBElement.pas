unit uSSBElement;

interface

uses
  FMX.Objects, FMX.Controls, System.Classes;

type
  TSSBElement = class(TImage)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TSSBElement }

constructor TSSBElement.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TSSBElement.Destroy;
begin

  inherited;
end;

end.
