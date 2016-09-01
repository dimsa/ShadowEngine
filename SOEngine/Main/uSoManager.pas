unit uSoManager;

interface

uses
  uSoModel, uSoContainer,
  uE2DRendition;

type
  TSoManager = class
  private
    FModel: TSoModel;
    FActiveContainer: TSoContainer;
//    function GetItems(Index: Integer): TSoContainer;
  public
    function AddRendition(const AName: string): TEngine2DRendition;
    procedure AddShape(const AName: string);
    procedure AddMouseHandler(const AName: string);
    procedure AddKeyBoardHandler(const AName: string);
    procedure Activate(const AContainer: TSoContainer);


    {
        AddRendition('AsteroidPicture');
    AddShape('Asteroid', DoSmth2);
    AddMouseDownHandler(DoSmth);
    AddKeyboardHandler('h',DoSmth3);

    }
//    property Items[Index: Integer]: TSoContainer read GetItems; default;
    constructor Create(const AModel: TSoModel);
  end;

implementation

{ TSoManager }

procedure TSoManager.Activate(const AContainer: TSoContainer);
begin
  FActiveContainer := AContainer;
end;

procedure TSoManager.AddKeyBoardHandler(const AName: string);
begin

end;

procedure TSoManager.AddMouseHandler(const AName: string);
begin

end;

function TSoManager.AddRendition(const AName: string): TEngine2DRendition;
begin

  FModel.Renderer.AddFromTemplate(FActiveContainer, AName);

end;

procedure TSoManager.AddShape(const AName: string);
begin

end;

constructor TSoManager.Create(const AModel: TSoModel);
begin
  FModel := AModel;
end;

end.
