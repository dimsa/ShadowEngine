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
    procedure LoadResources(const AFileName: string; const AJson: TJSONObject);
    procedure LoadColliders(const AJson: TJSONObject);
    procedure LoadRenditions(const AJson: TJSONObject);
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

procedure TTemplateManager.LoadColliders(const AJson: TJSONObject);
var
  vArr: TJSONArray;
  i: Integer;
begin
  vArr := AJSON.GetValue('Colliders') as TJSONArray;
  for i := 0 to vArr.Count - 1 do
    FModel.Collider.AddTemplateFromJson(TJSONObject(vArr.Items[i]));
end;

procedure TTemplateManager.LoadRenditions(const AJson: TJSONObject);
var
  vArr: TJSONArray;
  i: Integer;
begin
  vArr := AJSON.GetValue('Templates') as TJSONArray;
  for i := 0 to vArr.Count - 1 do
    FModel.Renderer.AddTemplateFromJson(TJSONObject(vArr.Items[i]));

  vArr := AJSON.GetValue('Resources') as TJSONArray;
  for i := 0 to vArr.Count - 1 do
    FModel.Renderer.AddTemplateFromJson(TJSONObject(vArr.Items[i]));
end;

procedure TTemplateManager.LoadResources(const AFileName: string; const AJson: TJSONObject);
var
  vArr: TJSONArray;
  i: Integer;
  vImg: TAnonImage;
  vNum: Int;
  vS: string;
begin
  vImg := TAnonImage.Create(nil);

  // There is no good function that returns dir of AFileName or I can nor find it. So it's Analog.
  vS := AFileName;
  vNum := PosEx('/', ReverseString(AFileName));
  Delete(vS, Length(vS) - vNum + 1, vNum );
  vImg.Bitmap.LoadFromFile(vS + '/' + AJSON.GetValue('ImageFile').Value);

  vArr := AJSON.GetValue('Resources') as TJSONArray;

  for i := 0 to vArr.Count - 1 do
    FModel.Renderer.AddResourceFromJson(vImg.Bitmap, TJSONObject(vArr.Items[i]));

  vImg.Free;
end;

procedure TTemplateManager.LoadSeCss(const AFileName: string);
begin
  FModel.Formattor.LoadTemplates(AFileName);
end;

procedure TTemplateManager.LoadSeJson(const AFileName: string);
var
  vJSON: TJSONObject;
  vColliders, vResources, vRenditions: TJSONArray;
  vFile: TStringList;
  i: Integer;
  s: string;
begin
  vFile := TStringList.Create;
  vFile.LoadFromFile(AFileName);
  vJSON := TJSONObject.ParseJSONValue(vFile.Text) as TJsonObject;
  vFile.Free;

  LoadResources(AFileName, vJSON);
  LoadColliders(vJSON);
  LoadRenditions(vJSON);
end;

end.
