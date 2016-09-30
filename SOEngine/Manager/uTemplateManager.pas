unit uTemplateManager;

interface

uses
  System.JSON, System.SysUtils, System.StrUtils,
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
  vColliders, vResources, vTemplates: TJSONArray;
  vFile: TStringList;
  i: Integer;
  vImg: TAnonImage;
  s: string;
  vNum: Int;
begin
  vFile := TStringList.Create;
  vFile.LoadFromFile(AFileName);
  vJSON := TJSONObject.ParseJSONValue(vFile.Text) as TJsonObject;
  vFile.Free;

  vImg := TAnonImage.Create(nil);
  s := AFileName;

  // There is no good function that returns dir of AFileName or I can nor find it. So it's Analog.
  vNum := PosEx('/', ReverseString(AFileName));
  Delete(s, Length(s) - vNum + 1, vNum );
  vImg.Bitmap.LoadFromFile(s + '/' + vJSON.GetValue('ImageFile').Value);

  vResources := vJSON.GetValue('Resources') as TJSONArray;
  vColliders := vJSON.GetValue('Colliders') as TJSONArray;
  vTemplates := vJSON.GetValue('Templates') as TJSONArray;

  for i := 0 to vResources.Count - 1 do
    FModel.Renderer.AddResourceFromJson(vImg.Bitmap, TJSONObject(vResources.Items[i]));

  for i := 0 to vColliders.Count - 1 do
    FModel.Collider.AddTemplateFromJson(TJSONObject(vColliders.Items[i]));

  for i := 0 to vTemplates.Count - 1 do
    FModel.Renderer.AddTemplateFromJson(TJSONObject(vTemplates.Items[i]));

  vImg.Free;
end;

end.
