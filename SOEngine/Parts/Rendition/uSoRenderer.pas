// This class contains unit renditions that will be repainted on eash tick.
unit uSoRenderer;

interface

uses
  System.SyncObjs, System.SysUtils, System.JSON, {$I 'Utils\DelphiCompatability.inc'}
  uCommonClasses, uSoTypes, uEasyDevice, uEngine2DClasses, uE2DRendition, uSoBaseOperator, uSoObject,
  uSoContainerTypes, uSoBasePart, uSoRenditionTemplate, uJsonUtils;

type
  TSoRenderer = class(TSoOperator<TEngine2DRendition>)
  private
    FTemplates: TDict<string, TSoRenditionTemplate>;
    FResources: TDict<string, TBitmap>;
    FImage: TAnonImage;
    FBackground: TBitmap; // Background of Engine that paints on every tick. Not sure if it should be here // Бэкграунд. Всегда рисуется в Repaint на весь fImage
    FOnPaintBackground, FOnBeginPaint, FOnEndPaint: TEvent<TAnonImage>;
   procedure OnItemDestroy(ASender: TObject);
    procedure OnImageResize(ASender: TObject);
    procedure SetBackground(const Value: TBitmap);
    procedure SetOnBeginPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnEndPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnPaintBackground(const Value: TEvent<TAnonImage>);
    function CutBitmapFrom(const ABitmap: TBitmap; ARect: TRect): TBitmap;
  public
    property OnPaintBackground: TEvent<TAnonImage> write SetOnPaintBackground;
    property OnBeginPaint: TEvent<TAnonImage> write SetOnBeginPaint;
    property OnEndPaint: TEvent<TAnonImage> write SetOnEndPaint;
    property Background: TBitmap write SetBackground;
    procedure AddTemplateFromJson(const AJson: TJSONObject);
    procedure AddResourceFromJson(const ABitmap: TBitmap; const AJson: TJSONObject);
    procedure LoadTemplateFromSeJson(const AFilename: string);
    constructor Create(const ACritical: TCriticalSection; const AImage: TAnonImage);
    destructor Destroy; override;
    procedure Execute; // Render On Tick
    procedure Add(const AItem: TEngine2DRendition; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TEngine2DRendition; override;
  end;

implementation

{ TSoRenderer }

procedure TSoRenderer.Add(const AItem: TEngine2DRendition; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

function TSoRenderer.AddFromTemplate(const ASubject: TSoObject;
  const ATemplateName: string; const AName: string = ''): TEngine2DRendition;
begin
  Result := FTemplates[ATemplateName].Instantiate(ASubject, FImage);// TEngine2DRendition.Create(ASubject, FImage);
  Add(Result, AName);
end;

function TSoRenderer.CutBitmapFrom(const ABitmap: TBitmap; ARect: TRect): TBitmap;
begin
  Result := tBitmap.Create;
  with Result do begin
    Width := ARect.Width;
    Height := ARect.Height;
    Canvas.BeginScene;
    Clear(1);
    Canvas.DrawBitmap(ABitmap, TRectF.Create(ARect.Left, ARect.Top, ARect.Left + ARect.Width, ARect.Top + ARect.Height), TRectF.Create(0, 0, ARect.Width, ARect.Height), 1, False);
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

  // Parameter of Mirror porperty is name of mirrored sprite
  if AJson.TryGetValue('Mirror-x', vVal) then
    FResources.Add(vVal.Value, CutBitmapFrom(ABitmap, TRect.Create(vRect.Right, vRect.Top, vRect.Left, vRect.Bottom)));

  if AJson.TryGetValue('Mirror-y', vVal) then
    FResources.Add(vVal.Value, CutBitmapFrom(ABitmap, TRect.Create(vRect.Left, vRect.Bottom, vRect.Right, vRect.Top)));

  if AJson.TryGetValue('Mirror-xy', vVal) then
    FResources.Add(vVal.Value, CutBitmapFrom(ABitmap, TRect.Create(vRect.Right, vRect.Bottom, vRect.Left, vRect.Top)));
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
    FTemplates.Add(vVal.Value, vTemplate);
end;

constructor TSoRenderer.Create(const ACritical: TCriticalSection;
  const AImage: TAnonImage);
begin
  inherited Create(ACritical);
  FImage := AImage;
  FImage.OnResize := OnImageResize;
  FResources := TDict<string, TBitmap>.Create;
  FTemplates := TDict<string, TSoRenditionTemplate>.Create;
  OnImageResize(FImage);
  FBackGround := TBitmap.Create;
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
        InvalidateRect(RectF(0, 0, Bitmap.Width, Bitmap.Height));
        {$ENDIF}
      end;
      FCritical.Leave;
end;

procedure TSoRenderer.LoadTemplateFromSeJson(const AFilename: string);
begin

end;

procedure TSoRenderer.OnImageResize(ASender: TObject);
begin
  FImage.Bitmap.Width := Round(FImage.Width * getScreenScale);
  FImage.Bitmap.Height := ROund(FImage.Height * getScreenScale);
end;

procedure TSoRenderer.OnItemDestroy(ASender: TObject);
begin
  FCritical.Enter;
  FList.Delete(TEngine2DRendition(ASender));
  FCritical.Leave;
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
