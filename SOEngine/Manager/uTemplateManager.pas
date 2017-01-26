unit uTemplateManager;

interface

uses
  System.JSON, System.SysUtils, System.StrUtils, uSoIDelegateCollector,
  uSoTypes, uEngine2DClasses, uSoObjectDefaultProperties, uJsonUtils,
  uSoTemplateLoader;

type
  TTemplateManager = class
  private
    FAddByTemplateCreator: ISoTemplateLoader;
    FBasePath: string;
    procedure LoadResources(const AFileName: string; const AJson: TJSONObject);
    procedure LoadColliders(const AJson: TJSONObject);
    procedure LoadRenditions(const AJson: TJSONObject);
    procedure LoadSound(const AJson: TJSONObject);
    procedure GetBasePath(const AFileName: string);
  public
    procedure LoadSeJson(const AFileName: string);
    procedure LoadSeCss(const AFileName: string);
    constructor Create(const AAddByTemplateCreator: ISoTemplateLoader);
  end;

implementation

{ TTemplateLoader }

constructor TTemplateManager.Create(const AAddByTemplateCreator: ISoTemplateLoader);
begin
  FAddByTemplateCreator := AAddByTemplateCreator;
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
  vNodeName: string;
begin
  vNodeName := 'Colliders';
  vArr := AJSON.GetValue(vNodeName) as TJSONArray;
  for i := 0 to vArr.Count - 1 do
    FAddByTemplateCreator.AddJsonTemplate(vNodeName, TJSONObject(vArr.Items[i]));
//    .AddByTemplate();
  //  FModel.Collider.AddTemplateFromJson(TJSONObject(vArr.Items[i]));
end;

procedure TTemplateManager.LoadRenditions(const AJson: TJSONObject);
var
  vArr, vBodyArr: TJSONArray;
  i: Integer;
  vJson, vBody: TJSONObject;
  vJsonVal: TJSONValue;
  vName: string;
  vNodeName: string;
begin
  vNodeName := 'Templates';
  vArr := AJSON.GetValue(vNodeName) as TJSONArray;
  for i := 0 to vArr.Count - 1 do
    FAddByTemplateCreator.AddJsonTemplate(vNodeName, TJSONObject(vArr.Items[i]));

  vNodeName := 'Templates';
  vArr := AJSON.GetValue(vNodeName) as TJSONArray;
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

      FAddByTemplateCreator.AddJsonTemplate(vNodeName, vJson);
    end;
end;

procedure TTemplateManager.LoadResources(const AFileName: string; const AJson: TJSONObject);
var
  vArr: TJSONArray;
  i: Integer;
  vNodeName: string;
  vJson: TJSONObject;
begin
  vNodeName := 'Resources';
  vArr := AJSON.GetValue(vNodeName) as TJSONArray;

  vJson := TJSONObject.Create;
  vJson.AddPair('BasePath', FBasePath);
  vJson.AddPair(vNodeName, vArr);

  FAddByTemplateCreator.AddJsonTemplate(vNodeName, vJson);
end;

procedure TTemplateManager.LoadSeCss(const AFileName: string);
begin
  FAddByTemplateCreator.AddTemplateFromFile('Formatters', AFileName);
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
  vNodeName: string;
begin
  vNodeName := 'Sounds';
  FAddByTemplateCreator.AddJsonTemplate(vNodeName, AJson);

{  vArr := AJSON.GetValue('Sounds') as TJSONArray;
  for i := 0 to vArr.Count - 1 do
  begin
    if (vArr.Items[i].TryGetValue('Name', vName)) and (vArr.Items[i].TryGetValue('Path', vPath)) then
      FModel.SoundKeeper.AddTemplate(JsonToString(vName), FBasePath + JsonToString(vPath));
  end;    }
end;

end.
