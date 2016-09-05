// This class contains unit renditions that will be repainted on eash tick.
unit uSoRenderer;

interface

uses
  System.SyncObjs, System.Classes, System.SysUtils, {$I 'Utils\DelphiCompatability.inc'}
  uEngine2DClasses, uE2DRendition, uSoBaseOperator, uSoContainer;

type

  TSoRenderer = class(TSoOperator<TEngine2DRendition>)
  private
    FImage: TAnonImage;
    procedure OnItemDestroy(ASender: TObject);
  public
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
  FImage := AImage;
end;

procedure TSoRenderer.Execute;
var
  i: Integer;
  IRend: TEngine2DRendition;
  m: tMatrix;
begin
  with FImage do
  // with FModel do
  begin
    if Bitmap.Canvas.BeginScene() then
      try

        // FInBeginPaintBehavior;
        // FBackgroundBehavior;

        // l := (ObjectList.Count - 1);
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
      //  FInEndPaintBehavior;

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

end.
