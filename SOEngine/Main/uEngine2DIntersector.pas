unit uEngine2DIntersector;

interface

uses
  System.Generics.Collections,
  uEngine2DClasses, uEngine2DObject, uEngine2DObjectShape, uCommonClasses;

type

  TObjectCollidedEventArgs = record
    Object1, Object2: TEngine2DObject;
  end;

  // It should be abstraction for Engine2D or Box2D shape
  TCollidableObject = class
  private
    FOwner: tEngine2DObject;
    FCollided: TEvent<tEngine2DObject>;
  public
    property Owner: tEngine2DObject read FOwner write FOwner;
    property Collided: TEvent<tEngine2DObject> read FCollided write FCollided; // Event returns object with which one was collide
    procedure SetShape(const AObjectShape: TObjectShape); // You can set object shape in Engine2D style, but it can SetShape for Box2D
  end;

  TEngine2DIntersector = class
  private
    FCollidableList: TList<TCollidableObject>;
    FClickableList: TList<TCollidableObject>;
    FObjectsCollided: TEvent<Integer>;
  public
    property ObjectsCollided: TEvent<Integer> read FObjectsCollided write FObjectsCollided; // Event returns quantity of colliding
    procedure DoWork; virtual;
    procedure AddCollidable(const AObject: TCollidableObject); // Only for collide test
    procedure AddClickable(const AObject: TCollidableObject); // Only for click test
    function GetCollided: TList<TCollidableObject>; // Returns list of collided pairs
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEngine2DIntersector }

procedure TEngine2DIntersector.AddClickable(const AObject: TCollidableObject);
begin
  FClickableList.Add(AObject);
end;

procedure TEngine2DIntersector.AddCollidable(const AObject: TCollidableObject);
begin
  FCollidableList.Add(AObject);
end;

constructor TEngine2DIntersector.Create;
begin
  FClickableList := TList<TCollidableObject>.Create;
  FCollidableList := TList<TCollidableObject>.Create;
end;

destructor TEngine2DIntersector.Destroy;
begin
  FCollidableList.Free;
  FCollidableList.Free;
  inherited;
end;

procedure TEngine2DIntersector.DoWork;
begin

end;

function TEngine2DIntersector.GetCollided: TList<TCollidableObject>;
begin

end;

{ TCollidableObject }

procedure TCollidableObject.SetShape(const AObjectShape: TObjectShape);
begin

end;

end.
