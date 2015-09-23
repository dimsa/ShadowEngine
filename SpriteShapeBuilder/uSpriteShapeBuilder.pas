unit uSpriteShapeBuilder;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types,
  uSSBElement, uNamedList;

type
  TSpriteShapeBuilder = class
  private
    FPanel: TPanel;
    FElements: TNamedList<TSSBElement>;
    FSelectedElement: TSSBElement;
    procedure DoSelect(ASender: TObject);
    procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  public
    property SelectedElement: TSSBElement read FSelectedElement write FSelectedElement;
    procedure AddElement(const AFileName: string);
    procedure LoadProject(const AFileName: string);
    procedure SaveProject(const AFileName: string);
    constructor Create;
    procedure Init(const APanel: TPanel);
    destructor Destroy; override;
  end;


implementation

{ TSpriteShapeBuilder }

procedure TSpriteShapeBuilder.AddElement(const AFileName: string);
var
  vSSBElement: TSSBElement;
begin
  vSSBElement := TSSBElement.Create(FPanel);
  vSSBElement.Parent := FPanel;
  vSSBElement.Bitmap.LoadFromFile(AFileName);
  vSSBElement.Width := vSSBElement.Bitmap.Width;
  vSSBElement.Height := vSSBElement.Bitmap.Height;

  vSSBElement.OnClick := DoSelect;
  vSSBElement.OnMouseWheel := DoZoom;

  FElements.Add(vSSBElement);
end;

constructor TSpriteShapeBuilder.Create;
begin
  FElements := TNamedList<TSSBElement>.Create;
end;

destructor TSpriteShapeBuilder.Destroy;
var
  vSSBElement: TSSBElement;
begin
  for vSSBElement in FElements do
    vSSBElement.Free;
  FElements.Clear;
  FElements.Free;

  inherited;
end;

procedure TSpriteShapeBuilder.DoSelect(ASender: TObject);
begin
  if ASender is TSSBElement then
    FSelectedElement := TSSBElement(ASender);
end;

procedure TSpriteShapeBuilder.DoZoom(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if FPanel.Scale.X + ((WheelDelta / 120) * 0.1) > 0.1 then
  begin
    FPanel.Scale.X := FPanel.Scale.X + ((WheelDelta / 120) * 0.1);
    FPanel.Scale.Y := FPanel.Scale.X;
  end;
end;

procedure TSpriteShapeBuilder.Init(const APanel: TPanel);
begin
  FPanel := APanel;
  FPanel.OnMouseWheel := DoZoom;
  try
    FPanel.Canvas.BeginScene;
    FPanel.Canvas.Fill.Color := TAlphaColorRec.Blanchedalmond;
    FPanel.Canvas.FillRect(FPanel.BoundsRect, 0, 0, [], 1, FMX.Types.TCornerType.Round);
  finally
    FPanel.Canvas.EndScene;
  end;
end;

procedure TSpriteShapeBuilder.LoadProject(const AFileName: string);
begin

end;

procedure TSpriteShapeBuilder.SaveProject(const AFileName: string);
begin

end;

end.


