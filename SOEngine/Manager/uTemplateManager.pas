unit uTemplateManager;

interface

uses
  System.JSON, System.SysUtils, System.StrUtils,
  uSoTypes, uSoModel, uEngine2DClasses, uSoObjectDefaultProperties, uJsonUtils;

type
  TSoModelFriend = class(TSoModel);

  TTemplateManager = class
  private
    FModel: TSoModelFriend;
    FBasePath: string;
    procedure LoadResources(const AFileName: string; const AJson: TJSONObject);
    procedure LoadColliders(const AJson: TJSONObject);
    procedure LoadRenditions(const AJson: TJSONObject);
    procedure LoadSound(const AJson: TJSONObject);
    procedure GetBasePath(const AFileName: string);
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

procedure TTemplateManager.GetBasePath(const AFileName: string);
var
  vS: string;
  vNum: Integer;
begin
  // There is no good function that returns dir of AFileName or I can nor find it. So it's Analog.
  vS := AFileName;
  vNum := PosEx('/', ReverseString(AFileName));
  Delete(vS, Length(vS) - vNum + 1, vNum);

  FBasePath := vS + '/';
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
  vArr, vBodyArr: TJSONArray;
  i: Integer;
  vJson, vBody: TJSONObject;
  vJsonVal: TJSONValue;
  vName: string;
begin
  vArr := AJSON.GetValue('Templates') as TJSONArray;
  for i := 0 to vArr.Count - 1 do
    FModel.Renderer.AddTemplateFromJson(TJSONObject(vArr.Items[i]));

  vArr := AJSON.GetValue('Resources') as TJSONArray;
  for i := 0 to vArr.Count - 1 do
    if vArr.Items[i].TryGetValue('Name', vJsonVal) then
    begin
      vName := JsonToString(vJsonVal);
      vJson := TJSONObject.Create;
      vJson.AddPair('Type', 'Sprite');
      vJson.AddPair('Name', vName);

      vBody := TJSONObject.Create;
      vBodyArr := TJSONArray.Create;
      vBodyArr.Add(vName);
      vBody.AddPair('Resources', vBodyArr);
      vJson.AddPair('Body', vBody);

      FModel.Renderer.AddTemplateFromJson(vJson);
    end;
end;

procedure TTemplateManager.LoadResources(const AFileName: string; const AJson: TJSONObject);
var
  vArr: TJSONArray;
  i: Integer;
  vImg: TAnonImage;
begin
  vImg := TAnonImage.Create(nil);

  vImg.Bitmap.LoadFromFile(FBasePath + '/' + AJSON.GetValue('ImageFile').Value);

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

  GetBasePath(AFileName);

  LoadResources(AFileName, vJSON);
  LoadColliders(vJSON);
  LoadRenditions(vJSON);
  LoadSound(vJSON);
end;

procedure TTemplateManager.LoadSound(const AJson: TJSONObject);
var
  vArr: TJSONArray;
  i: Integer;
  vName, vPath: TJSONValue;
begin
  vArr := AJSON.GetValue('Sounds') as TJSONArray;
  for i := 0 to vArr.Count - 1 do
  begin
    if (vArr.Items[i].TryGetValue('Name', vName)) and (vArr.Items[i].TryGetValue('Path', vPath)) then
      FModel.SoundKeeper.AddTemplate(JsonToString(vName), FBasePath + JsonToString(vPath));
  end;
end;

end.
