unit uGame;

interface

uses
  uClasses, uEngine2DClasses, uWorldManager, uUnitManager, uMapPainter, uUnitCreator, uTemplateManager,
  uUtils;

type
  TGame = class
  private
    FMapPainter: TMapPainter; // Some object to draw parallax or map or etc
    FUnitCreator: TUnitCreator;
    FWorldManager: TWorldManager;
    FUnitManager: TUnitManager;
    FTemplateManager: TTemplateManager;
    procedure StartGame;
  public
    constructor Create(const ATemplateManager: TTemplateManager; const AWorldManager: TWorldManager;
      const AUnitManager: TUnitManager);
  end;

implementation

{ TGame }

constructor TGame.Create(const ATemplateManager: TTemplateManager; const AWorldManager: TWorldManager;
  const AUnitManager: TUnitManager);
begin
  FWorldManager := AWorldManager;
  FUnitManager :=  AUnitManager;
  FTemplateManager := ATemplateManager;
  //Prepairing of background
  FMapPainter := TMapPainter.Create(FWorldManager, ResourcePath('Back.jpg'));
  FUnitCreator := TUnitCreator.Create(FUnitManager, FWorldManager);

  FTemplateManager.LoadSeJson(ResourcePath('Asteroids.sejson'));
  FTemplateManager.LoadSeCss( ResourcePath('Formatters.secss'));

  StartGame;
end;

procedure TGame.StartGame;
begin
  FUnitCreator.NewShip;
end;

end.
