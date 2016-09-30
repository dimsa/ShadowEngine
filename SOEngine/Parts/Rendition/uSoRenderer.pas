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
  Result := TEngine2DRendition.Create(ASubject, FImage);
  Add(Result, AName);
end;

procedure TSoRenderer.AddResourceFromJson(const ABitmap: TBitmap; const AJson: TJSONObject);
var
  vBmp: tBitmap;
  vRect: TRect;
  vVal: TJSONValue;
begin
  vRect := JsonToRectF((TJSONObject(AJson.GetValue('Body')).GetValue('Position'))).Round;
  vBmp := tBitmap.Create;
  vBmp.Width := vRect.Width;
  vBmp.Height := vRect.Height;
  vBmp.Canvas.BeginScene();
  vBmp.Clear(1);
  vBmp.Canvas.DrawBitmap(
    ABitmap,
    TRectF.Create(vRect.Left, vRect.Top, vRect.Left + vRect.Width, vRect.Top + vRect.Height),
    TRectF.Create(0, 0, vRect.Width, vRect.Height), 1, False);
  vBmp.Canvas.EndScene;
  if AJson.TryGetValue('Name', vVal) then
    FResources.Add(vVal.Value, vBmp);
end;

procedure TSoRenderer.AddTemplateFromJson(const AJson: TJSONObject);
var
  vRend: TEngine2DRendition;
  vVal: TJSONValue;
begin

{  if AJson.TryGetValue('Position', vVal)  AJson.GetValue.TryGetValue('Position') then
  begin
    FTemplates.Add(Aj);
    vRend := TEngine2DRendition.Create(AJson);
  end;      }
end;

constructor TSoRenderer.Create(const ACritical: TCriticalSection;
  const AImage: TAnonImage);
begin
  inherited Create(ACritical);
  FImage := AImage;
  FImage.OnResize := OnImageResize;
  FResources := TDict<string, TBitmap>.Create;
  OnImageResize(FImage);
  FBackGround := TBitmap.Create;
end;

destructor TSoRenderer.Destroy;
var
  vBmp: TBitmap;
begin
  for vBmp in FResources.Values do
    vBmp.Free;

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
