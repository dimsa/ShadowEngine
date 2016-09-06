// This class contains unit renditions that will be repainted on eash tick.
unit uSoRenderer;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils, {$I 'Utils\DelphiCompatability.inc'}
  FMX.Graphics,
  uEngine2DClasses, uE2DRendition, uSoBaseOperator, uSoContainer;

type

  TSoRenderer = class(TSoOperator<TEngine2DRendition>)
  private
    FImage: TAnonImage;
    FBackground: TBitmap; // Background of Engine that paints on every tick. Not sure if it should be here // Бэкграунд. Всегда рисуется в Repaint на весь fImage
    FOnPaintBackground, FOnBeginPaint, FOnEndPaint: TEvent<TAnonImage>;
    procedure OnItemDestroy(ASender: TObject);
    procedure SetBackground(const Value: TBitmap);
    procedure SetOnBeginPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnEndPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnPaintBackground(const Value: TEvent<TAnonImage>);
  public
    property OnPaintBackground: TEvent<TAnonImage> write SetOnPaintBackground;
    property OnBeginPaint: TEvent<TAnonImage> write SetOnBeginPaint;
    property OnEndPaint: TEvent<TAnonImage> write SetOnEndPaint;
  //  property BackgroundBehavior: TProcedure write SetBackgroundBehavior;
//    property InBeginPaintBehavior: TProcedure write SetInBeginPaintBehavior;
//    property InEndPaintBehavior: TProcedure write SetInBeginPaintBehavior;
    property Background: TBitmap write SetBackground;
    constructor Create(const ACritical: TCriticalSection; const AImage: TAnonImage);
    procedure Execute; // Render On Tick
    procedure Add(const AItem: TEngine2DRendition; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoContainer; const ATemplateName: string; const AName: string = ''): TEngine2DRendition; override;
  end;

implementation

{ TSoRenderer }

procedure TSoRenderer.Add(const AItem: TEngine2DRendition; const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
end;

function TSoRenderer.AddFromTemplate(const ASubject: TSoContainer;
  const ATemplateName: string; const AName: string = ''): TEngine2DRendition;
begin
  Result := TEngine2DRendition.Create(ASubject, FImage);
end;

constructor TSoRenderer.Create(const ACritical: TCriticalSection;
  const AImage: TAnonImage);
begin
  inherited Create(ACritical);

  FBackGround := TBitmap.Create;
  FImage := AImage;
end;

procedure TSoRenderer.Execute;
var
  IRend: TEngine2DRendition;
  m: tMatrix;
begin
  with FImage do
  begin
    if Bitmap.Canvas.BeginScene() then
      try
        if Assigned(FOnBeginPaint) then
          FOnBeginPaint(Self, FImage);
        if Assigned(FOnPaintBackground) then
          FOnPaintBackground(Self, FImage);

        for IRend in FList do
          if IRend.Enabled then
          begin
            m := tMatrix.CreateTranslation(-IRend.Subject.x, -IRend.Subject.y) *
              tMatrix.CreateScaling(IRend.Subject.ScaleX, IRend.Subject.ScaleY) *
              tMatrix.CreateRotation(IRend.Subject.rotate * pi180) *
              tMatrix.CreateTranslation(IRend.Subject.x, IRend.Subject.y);

            Bitmap.Canvas.SetMatrix(m);

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
  end;
end;

procedure TSoRenderer.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TEngine2DRendition(ASender));
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
