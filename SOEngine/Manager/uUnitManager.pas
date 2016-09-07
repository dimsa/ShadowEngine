unit uUnitManager;

interface

uses
  uSoModel;

type
  TUnitManager = class
  private
    FModel: TSoModel;
  public
    constructor Create(const AModel: TSoModel);
  end;

implementation

{ TUnitManager }

constructor TUnitManager.Create(const AModel: TSoModel);
begin
  FModel:= AModel;
end;

end.
