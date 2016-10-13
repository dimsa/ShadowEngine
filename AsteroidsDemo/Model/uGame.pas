unit uGame;

interface

uses
  uClasses, uSoTypes, uEngine2DClasses, uWorldManager, uUnitManager, uMapPainter, uUnitCreator, uTemplateManager,
  uUtils, uModel, uSoManager;

type
  TGame = class
  private
    FShip: TShip;
    FAsteroids: TList<TBigAsteroid>;
    FDecorations: TList<TLtlAsteroid>;
    FMapPainter: TMapPainter; // Some object to draw parallax or map or etc
    FUnitCreator: TUnitCreator;
    FManager: TSoManager;
 //   FWorldManager: TWorldManager;
 //   FUnitManager: TUnitManager;
 //   FTemplateManager: TTemplateManager;
//    FSimpleManager: TSoSimpleManager;
    procedure StartGame;
    procedure OnResize(ASender: TObject);
    procedure OnMouseDown(Sender: TObject; AEventArgs: TMouseEventArgs);
  public
    constructor Create(const AManager: TSoManager);
    destructor Destroy; override;
  end;

implementation

{ TGame }

constructor TGame.Create(const AManager: TSoManager);
begin
  FManager := AManager;

  with FManager do begin
    FMapPainter := TMapPainter.Create(WorldManager, ResourcePath('Back.jpg'));
    FUnitCreator := TUnitCreator.Create(UnitManager);

    TemplateManager.LoadSeJson(ResourcePath('Asteroids.sejson'));
    TemplateManager.LoadSeCss( ResourcePath('Formatters.secss'));

    FAsteroids := TList<TBigAsteroid>.Create;
    FDecorations := TList<TLtlAsteroid>.Create;

    WorldManager.OnResize.Add(OnResize);
    WorldManager.OnMouseDown.Add(OnMouseDown);

    StartGame;
  end;

end;

destructor TGame.Destroy;
begin
  FDecorations.Free;
  FAsteroids.Free;
  inherited;
end;

procedure TGame.OnMouseDown(Sender: TObject; AEventArgs: TMouseEventArgs);
begin

end;

procedure TGame.OnResize(ASender: TObject);
var
  i: Integer;
  vPoint: TPointF;
begin
  vPoint := TPointF.Create(TAnonImage(ASender).Width, TAnonImage(ASender).Height);
{  FShip.SetWorldSize(vPoint);

  for i := 0 to FAsteroids.Count - 1 do
    FAsteroids[i].SetWorldSize(vPoint);

  for i := 0 to FDecorations.Count - 1 do
    FDecorations[i].SetWorldSize(vPoint);  }
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
