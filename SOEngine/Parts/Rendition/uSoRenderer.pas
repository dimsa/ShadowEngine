// This class contains unit renditions that will be repainted on eash tick.
unit uSoRenderer;

interface

uses
  System.SyncObjs, System.SysUtils, System.JSON, {$I 'Utils\DelphiCompatability.inc'}
  uCommonClasses, uSoTypes, uEasyDevice, uEngine2DClasses, uE2DRendition, uSoBaseOperator, uSoObject,
  uSoContainerTypes, uSoBasePart, uSoRenditionTemplate, uJsonUtils, uSoObjectDefaultProperties;

type
  TSoRenderer = class(TSoOperator<TEngine2DRendition>)
  private type
    TSoRenditionFriend = class(TEngine2DRendition);
  private
    FTemplates: TDict<string, TSoRenditionTemplate>;
    FResources: TDict<string, TBitmap>;
//    FRenditionsBySubject: TDict<TSoObject, TList<TEngine2DRendition>>;
    FImage: TAnonImage;
    FBackground: TBitmap; // Background of Engine that paints on every tick. Not sure if it should be here // Бэкграунд. Всегда рисуется в Repaint на весь fImage
    FOnPaintBackground, FOnBeginPaint, FOnEndPaint: TEvent<TAnonImage>;
//    procedure OnItemDestroy(ASender: TObject);

    procedure BringToBack(ASender: TObject);
    procedure SendToFront(ASender: TObject);
    procedure SetBackground(const Value: TBitmap);
    procedure SetOnBeginPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnEndPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnPaintBackground(const Value: TEvent<TAnonImage>);
    function CutBitmapFrom(const ABitmap: TBitmap; const ARect: TRect; AFlipX: Boolean = False; AFlipY: Boolean = False): TBitmap;
    function OnAllRenditionRequest(ASender: TSoObject): TRectF;// TList<TEngine2DRendition>;
    function PropertyName: string; override;
  public
    property OnPaintBackground: TEvent<TAnonImage> write SetOnPaintBackground;
    property OnBeginPaint: TEvent<TAnonImage> write SetOnBeginPaint;
    property OnEndPaint: TEvent<TAnonImage> write SetOnEndPaint;
    property Background: TBitmap write SetBackground;
    procedure AddTemplateFromJson(const AJson: TJSONObject);
    procedure AddResourceFromJson(const ABitmap: TBitmap; const AJson: TJSONObject);
    constructor Create(const ACritical: TCriticalSection; const AImage: TAnonImage);
    destructor Destroy; override;
    procedure Execute; // Render On Tick
    procedure Add(const AItem: TEngine2DRendition; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TEngine2DRendition; override;
  end;

implementation

uses
  uSoContainer, System.Math, uSoProperty;

{ TSoRenderer }

procedure TSoRenderer.Add(const AItem: TEngine2DRendition; const AName: string);
var
  vName: string;
begin

  AddAsProperty(AItem, AName);

  TSoRenditionFriend(AItem).OnBringToBack := BringToBack;
  TSoRenditionFriend(AItem).OnSendToFront := SendToFront;

  {$I .\Template\uItemAdd.inc}
  TSoRenditionFriend(AItem).OnRequestAllRenditions := OnAllRenditionRequest;
end;

function TSoRenderer.AddFromTemplate(const ASubject: TSoObject;
  const ATemplateName: string; const AName: string = ''): TEngine2DRendition;
begin
  Result := FTemplates[ATemplateName].Instantiate(ASubject, FImage);// TEngine2DRendition.Create(ASubject, FImage);
  Add(Result, AName);
end;

function TSoRenderer.CutBitmapFrom(const ABitmap: TBitmap; const ARect: TRect; AFlipX: Boolean; AFlipY: Boolean): TBitmap;
var
  m: TMatrix;
begin
  Result := tBitmap.Create;
  with Result do begin
    Width := Abs(ARect.Width);
    Height := Abs(ARect.Height);
    Canvas.BeginScene;
    Clear(1);
    Canvas.SetMatrix(
      TMatrix.CreateScaling(-2 * Ord(AFlipX) + 1, (-2 * Ord(AFlipY) + 1)) *
      TMatrix.CreateTranslation(Width * Ord(AFlipX), Height * Ord(AFlipY)));

    Canvas.DrawBitmap(
      ABitmap,
      TRectF.Create(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom),
      TRectF.Create(0, 0, Abs(ARect.Width), Abs(ARect.Height)), 1, True);

    Canvas.EndScene;
  end;
end;

procedure TSoRenderer.AddResourceFromJson(const ABitmap: TBitmap; const AJson: TJSONObject);
var
  vBmp: tBitmap;
  vRect: TRect;
  vVal: TJSONValue;
begin
  vRect := JsonToRectF((TJSONObject(AJson.GetValue('Body')).GetValue('Position'))).Round;
  vBmp := CutBitmapFrom(ABitmap, vRect);
  if AJson.TryGetValue('Name', vVal) then
    FResources.Add(vVal.Value, vBmp);

  // Parameter of Mirror property is name the of mirrored sprite
  if AJson.TryGetValue('Mirror-x', vVal) then
    FResources.Add(vVal.Value, CutBitmapFrom(ABitmap, vRect, True));

  if AJson.TryGetValue('Mirror-y', vVal) then
    FResources.Add(vVal.Value, CutBitmapFrom(ABitmap, vRect, False, True));

  if AJson.TryGetValue('Mirror-xy', vVal) then
    FResources.Add(vVal.Value, CutBitmapFrom(ABitmap, vRect, True, True));
end;

procedure TSoRenderer.AddTemplateFromJson(const AJson: TJSONObject);
var
  vVal: TJSONValue;
  vTemplate: TSoRenditionTemplate;
  vType: TRenditionType;
begin
  if AJson.TryGetValue('Type', vVal) then
    vType := JsonToRenditionType(vVal);

  if AJson.TryGetValue('Body', vVal) then
    case vType of
      rtSprite: vTemplate := TSoSpriteTemplate.Create(vVal, FResources);
      rtText: vTemplate := TSoTextTemplate.Create(vVal);
      rtShape: vTemplate := TSoShapeTemplate.Create(vVal);
    end;

  if AJson.TryGetValue('Name', vVal) then
    if not FTemplates.ContainsKey(vVal.Value) then
      FTemplates.Add(vVal.Value, vTemplate);
end;

procedure TSoRenderer.BringToBack(ASender: TObject);
begin
  FList.Move(FList.IndexOf(TEngine2DRendition(ASender)), 0);
end;

{function TSoRenderer.OnAllRenditionRequest(ASender: TSoObject): TRectF;
var
  i: Integer;
  vXLeft, vYTop, vXRight, vYBottom: Single;
  vRend: TEngine2DRendition;
  cont: TSoContainer;                                                                                                ,,
begin
  Result := TRectF.Empty;//TList<TEngine2DRendition>.Create;

  cont := TSoContainer(TSoObject(ASender).Container);
   for  i := 0 to cont.Items[TEngine2DRendition].Count - 1 do
 // for i in TSoContainer(ASender.Container).Items[TEngine2DRendition].Items do
  begin
    vRend := TEngine2DRendition(TSoContainer(ASender.Container).Items[TEngine2DRendition].Items[i]);
    Result.Left := Min(Result.Left, Abs((vRend.Width / 2)  * CJustifyPoints[vRend.Justify].Left) + vRend.Margin.X);
    Result.Right := Max(Result.Right, Abs((vRend.Width / 2)  * CJustifyPoints[vRend.Justify].Right) + vRend.Margin.X);
    Result.Top := Min(Result.Top, Abs((vRend.Height / 2)  * CJustifyPoints[vRend.Justify].Top) + vRend.Margin.Y);
    Result.Bottom := Max(Result.Bottom, Abs((vRend.Height / 2)  * CJustifyPoints[vRend.Justify].Bottom) + vRend.Margin.Y);
  end;

end;   }

function TSoRenderer.OnAllRenditionRequest(ASender: TSoObject): TRectF;
var
  i: Integer;
  vXLeft, vYTop, vXRight, vYBottom: Single;
  vRend: TEngine2DRendition;
  vList: TList<TSoBasePart>;
begin
  Result := TRectF.Empty;

//  cont := TSoContainer(TSoObject(ASender).Container);
  vList := FElementBySubject[TSoObject(ASender)];
  for  i := 0 to vList.Count - 1 do
  begin
    vRend := TEngine2DRendition(vList[i]);// TEngine2DRendition(TSoContainer(ASender.Container).Items[TEngine2DRendition].Items[i]);
    Result.Left := Min(Result.Left, (vRend.Width / 2)  * CJustifyPoints[vRend.Justify].Left) + vRend.Margin.X;
    Result.Right := Max(Result.Right,(vRend.Width / 2)  * CJustifyPoints[vRend.Justify].Right) + vRend.Margin.X;
    Result.Top := Min(Result.Top, (vRend.Height / 2)  * CJustifyPoints[vRend.Justify].Top) + vRend.Margin.Y;
    Result.Bottom := Max(Result.Bottom, (vRend.Height / 2)  * CJustifyPoints[vRend.Justify].Bottom) + vRend.Margin.Y;
  end;

end;

function TSoRenderer.PropertyName: string;
begin
  Result := Rendition;
end;

constructor TSoRenderer.Create(const ACritical: TCriticalSection;
  const AImage: TAnonImage);
begin
  inherited Create(ACritical);
  FImage := AImage;
  FResources := TDict<string, TBitmap>.Create;
  FTemplates := TDict<string, TSoRenditionTemplate>.Create;
  FBackGround := TBitmap.Create;
  //FRenditionsBySubject := TDict<TSoObject, TList<TEngine2DRendition>>.Create;
end;

destructor TSoRenderer.Destroy;
var
  vBmp: TBitmap;
  vItem: TSoRenditionTemplate;
begin
  for vBmp in FResources.Values do
    vBmp.Free;

  for vItem in FTemplates.Values do
    vItem.Free;

  FTemplates.Free;
  FResources.Free;

  inherited;
end;

procedure TSoRenderer.Execute;
var
  IRend: TEngine2DRendition;
begin
  FCritical.Enter;
    if FImage.Bitmap.Canvas.BeginScene() then
    with FImage do
      try
        if Assigned(FOnBeginPaint) then
          FOnBeginPaint(Self, FImage);
        if Assigned(FOnPaintBackground) then
          FOnPaintBackground(Self, FImage);

        for IRend in FList do
          if IRend.Enabled then
          begin

            Bitmap.Canvas.SetMatrix(
              tMatrix.CreateTranslation(-IRend.Subject.x, -IRend.Subject.y) *
              tMatrix.CreateScaling(IRend.Subject.ScaleX, IRend.Subject.ScaleY) *
              tMatrix.CreateRotation(IRend.Subject.rotate * pi180) *
              tMatrix.CreateTranslation(IRend.Subject.x, IRend.Subject.y)
            );

            {$IFDEF DEBUG}
         //   if FOptions.ToDrawFigures then
        //      vSpr := ObjectList[FModel.ObjectOrder[i]];
            {$ENDIF}
            IRend.Repaint;

            {$IFDEF DEBUG}
          //  if FOptions.ToDrawFigures then
          //    vSpr.RepaintWithShapes;
            {$ENDIF}
          end;
      finally
        if Assigned(FOnEndPaint) then
          FOnEndPaint(Self, FImage);

        Bitmap.Canvas.EndScene();

        {$IFDEF POSIX}
        InvalidateRect(TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height));
        {$ENDIF}
      end;
      FCritical.Leave;
end;

{procedure TSoRenderer.OnItemDestroy(ASender: TObject);
begin
  FCritical.Enter;
  FList.Delete(TEngine2DRendition(ASender));
  FCritical.Leave;
end;}

procedure TSoRenderer.SendToFront(ASender: TObject);
begin
  FList.Move(FList.IndexOf(TEngine2DRendition(ASender)), FList.Count - 1);
end;

procedure TSoRenderer.SetBackground(const Value: TBitmap);
begin
  FCritical.Enter;
  FBackground := Value;
  FCritical.Leave;
end;

procedure TSoRenderer.SetOnBeginPaint(const Value: TEvent<TAnonImage>);
begin
  FCritical.Enter;
  FOnBeginPaint := Value;
  FCritical.Leave;
end;

procedure TSoRenderer.SetOnEndPaint(const Value: TEvent<TAnonImage>);
begin
  FCritical.Enter;
  FOnEndPaint := Value;
  FCritical.Leave;
end;

procedure TSoRenderer.SetOnPaintBackground(const Value: TEvent<TAnonImage>);
begin
  FCritical.Enter;
  FOnPaintBackground := Value;
  FCritical.Leave;
end;

end.
