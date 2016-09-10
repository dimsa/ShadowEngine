unit uGame;

interface

uses
  uClasses, uEngine2DClasses, uWorldManager, uUnitManager, uEasyDevice, uMapPainter, uUnitCreator;

type
  TGame = class
  private
    FMapPainter: TMapPainter; // Some object to draw parallax or map or etc
    FUnitCreator: TUnitCreator;
    FWorldManager: TWorldManager;
    FUnitManager: TUnitManager;
  public
    constructor Create(const AWorldManager: TWorldManager; const AUnitManager: TUnitManager);
  end;

implementation

{ TGame }

constructor TGame.Create(const AWorldManager: TWorldManager; const AUnitManager: TUnitManager);
begin
  FWorldManager := AWorldManager;
  FUnitManager :=  AUnitManager;

  //Prepairing of background
  FMapPainter := TMapPainter.Create(FWorldManager, UniPath('../../../../art/back.jpg') );
  FUnitCreator := TUnitCreator.Create(FUnitManager);
end;

end.
