unit uGame;

interface

uses
  uClasses, uSoTypes, uEngine2DClasses, uWorldManager, uUnitManager, uMapPainter, uUnitCreator, uTemplateManager,
  uUtils, uModel;

type
  TGame = class
  private
    FShip: TShip;
    FAsteroids: TList<TBigAsteroid>;
    FDecorations: TList<TLtlAsteroid>;
    FMapPainter: TMapPainter; // Some object to draw parallax or map or etc
    FUnitCreator: TUnitCreator;
    FWorldManager: TWorldManager;
    FUnitManager: TUnitManager;
    FTemplateManager: TTemplateManager;
    procedure StartGame;
    procedure OnResize(ASender: TObject; AImage: TAnonImage);
  public
    constructor Create(const ATemplateManager: TTemplateManager; const AWorldManager: TWorldManager;
      const AUnitManager: TUnitManager);
    destructor Destroy; override;
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

  FAsteroids := TList<TBigAsteroid>.Create;
  FDecorations := TList<TLtlAsteroid>.Create;

  FWorldManager.OnResize.Add(OnResize);
  StartGame;

end;

destructor TGame.Destroy;
begin
  FDecorations.Free;
  FAsteroids.Free;
  inherited;
end;

procedure TGame.OnResize(ASender: TObject; AImage: TAnonImage);
var
  i: Integer;
  vPoint: TPointF;
begin
  vPoint := TPointF.Create(AImage.Width, AImage.Height);
  FShip.SetWorldSize(vPoint);

  for i := 0 to FAsteroids.Count - 1 do
    FAsteroids[i].SetWorldSize(vPoint);

  for i := 0 to FDecorations.Count - 1 do
    FDecorations[i].SetWorldSize(vPoint);
end;

procedure TGame.StartGame;
var
  i: Integer;
begin
  FShip := FUnitCreator.NewShip;

{  for i := 0 to 3 do
    FAsteroids.Add(FUnitCreator.NewSpaceDebris(Random(3)));   }

{  for i:= 0 to 19 do
    FDecorations.Add(FUnitCreator.NewSpaceDust);  }
end;

end.
