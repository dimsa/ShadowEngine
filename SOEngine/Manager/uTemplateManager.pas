unit uTemplateManager;

interface

uses
  uSoModel;

type
  TSoModelFriend = class(TSoModel);

  TTemplateManager = class
  private
    FModel: TSoModelFriend;
  public
    procedure LoadSeJson(const AFileName: string);
    procedure LoadSeCss(const AFileName: string);
    constructor Create(const AModel: TSoModel);
  end;

implementation

{ TTemplateLoader }

constructor TTemplateManager.Create(const AModel: TSoModel);
begin
  FModel := TSoModelFriend(AModel);
end;

procedure TTemplateManager.LoadSeCss(const AFileName: string);
begin
  FModel.Formattor.LoadTemplates(AFileName);
end;

procedure TTemplateManager.LoadSeJson(const AFileName: string);
begin
  FModel.Renderer.LoadTemplateFromSeJson(AFileName);
  FModel.Collider.LoadTemplateFromSeJson(AFileName);
end;

end.
