unit uTemplateManager;

interface

uses
  System.JSON, System.SysUtils,
  uSoTypes, uSoModel, uEngine2DClasses;

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
var
  vJSON: TJSONObject;
  vColliders, vResources: TJSONArray;
  vFile: TStringList;
  i: Integer;
  vImg: TAnonImage;
begin
  vFile := TStringList.Create;
  vFile.LoadFromFile(AFileName);
  vJSON := TJSONObject.ParseJSONValue(vFile.Text) as TJsonObject;
  vFile.Free;

  vImg := TAnonImage.Create(nil);
  vImg.Bitmap.LoadFromFile(vJSON.GetValue('ImageFile').ToString);

  vResources := vJSON.GetValue('Resources') as TJSONArray;
  vColliders := vJSON.GetValue('Colliders') as TJSONArray;

  for i := 0 to vResources.Count - 1 do
  begin
    FModel.Renderer.AddTemplateFromJson(vImg, TJSONObject(vResources.Items[i]));
  end;

  for i := 0 to vColliders.Count - 1 do
  begin
    FModel.Collider.AddTemplateFromJson(TJSONObject(vColliders.Items[i]));
  end;

  vImg.Free;
end;

end.
