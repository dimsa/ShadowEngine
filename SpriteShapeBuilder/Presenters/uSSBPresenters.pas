unit uSSBPresenters;

interface

uses
  System.Classes, System.UITypes, System.Types, System.SysUtils, System.Generics.Collections,
  FMX.StdCtrls, FMX.Controls, FMX.Graphics, FMX.Objects, FMX.Dialogs, FMX.Types,
  uSSBModels, uClasses, uEasyDevice, uIView;


type

 { TSSBController = class abstract
  public
    // Обработчики мыши
    procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean); virtual; abstract;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); virtual; abstract;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single); virtual; abstract;
    procedure DoCommand(const ACommandName: string; AParams: array of Const); overload; virtual; abstract;
    procedure DoCommand(const ACommandName: string); overload;
    procedure InitView(
      AAddButton, ADelButton: TControl;
      AMainPanel: TPanel); virtual; abstract;
  end; }
  TSSBPresenter = class
  protected
    FView: ISSBView;
  public
    constructor Create(AView: ISSBView); virtual;
    procedure Init; virtual;
    procedure OnModelUpdate(ASender: TObject); virtual;
    destructor Destroy; override;
  end;

  TSSBImagerPresenter = class(TSSBPresenter)
  private
    FModel: TSSBImagerModel;
    FPanel: TPanel;
    FIsMouseDown: Boolean;
    FMouseStartPoint: TPointF;
    FMouseElementPoint: TPointF;
    FElementStartPosition: TPointF;
    FSelected: TImage;
    FElements: TDictionary<ISSBViewElement, TImage>;

    // Методы на клик
    procedure DoSelectImage(ASender: TObject);
    procedure DoDelImage(ASender: TObject);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    // Обработчики мыши
    {procedure DoZoom(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean); override;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single); override;
    procedure Adjust(AControl: TControl); }
  public
    //procedure DoCommand(const ACommandName: string; AParams: array of Const); override;
//    procedure AddImage;//(const AFileName: string); overload;
    procedure AddImg;
    procedure SelImg;
    procedure DelImg;
    procedure DragImg;
    procedure StartDragImg;
    procedure FinishDragImg;
    procedure Init; override;
    constructor Create(AView: ISSBView); override;
    destructor Destroy; override;
  end;

  TSSBObjecterPresenter = class(TSSBPresenter)
  public
    procedure SelObj;
    procedure AddObj;
    procedure DelObj;
    procedure EditObj;
    procedure ImgAutoConvert;
  end;

  TSSBShaperPresenter = class(TSSBPresenter)
  public
    procedure SelObj;
    procedure AddShape;
    procedure DelShape;
    procedure SelShape;
    procedure EditShape;
  end;

implementation

{ TSSBImagerController }

{rocedure TSSBImagerPresenter.AddImage(const AFileName: string);
var
  vImg: TImage;
begin
  vImg := TImage.Create(FPanel);
  vImg.Parent := FPanel;
  vImg.Position.Point := Point(0, 0);

  vImg.Bitmap.LoadFromFile(AFileName);
  vImg.Width := vImg.Bitmap.Width;
  vImg.Height:= vImg.Bitmap.Height;
  AddImage(vImg);
end;

procedure TSSBImagerPresenter.AddImage(const AImg: TImage);
var
  vImg: TImage;
begin
  vImg := AImg;
  with vImg do
  begin
    Parent := FPanel;
    OnClick := DoSelectImage;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnMouseMove := DoMouseMove;
    OnMouseWheel := DoZoom;
  end;

  FModel.Add(vImg);
  //FImages.Add(AImage);
end;  }

procedure TSSBImagerPresenter.AddImg;
var
  vS: string;
  vImg: TImage;
  vViewItem: ISSBViewElement;
begin
  vS := FView.FilenameFromDlg;
  if vS <> '' then
  begin
    vImg := TImage.Create(nil);
    vViewItem := FView.AddElement;
    vViewItem.Left := 0;
    vViewItem.Top := 0;
    vViewItem.Width := Round(vImg.Width);
    vViewItem.Height:= Round(vImg.Height);

    FElements.Add(vViewItem, vImg);
    try
      vImg.Bitmap.LoadFromFile(vS);
      FModel.Add(vImg);
      vViewItem.AssignBitmap(vImg.Bitmap);
    except
      vImg.Free;
      FView.RemoveElement(vViewItem);
    end;
  end;
end;

{procedure TSSBImagerPresenter.Adjust(AControl: TControl);
var
  i, vX, vY: Integer;
begin
  for i := 0 to FModel.ImageCount - 1 do
    with FModel do
    if Images[i] <> AControl then
    begin
      for vX := 0 to 3 do
        for vY := 0 to 3 do
          with AControl do
          begin
            if (Points[vX].X <= Images[i].Points[vY].X + CPrec) and
              (Points[vX].X >= Images[i].Points[vY].X - CPrec) then
              Points[vX] := PointF(Images[i].Points[vY].X, Points[vX].Y);

            if (Points[vX].Y <= Images[i].Points[vY].Y + CPrec) and
              (Points[vX].Y >= Images[i].Points[vY].Y - CPrec) then
              Points[vX] := PointF(Points[vX].X, Images[i].Points[vY].Y);
          end;
    end;
end; }

constructor TSSBImagerPresenter.Create(AView: ISSBView);
begin
  inherited;
  FElements := TDictionary<ISSBViewElement, TImage>.Create;
  FModel := TSSBImagerModel.Create(OnModelUpdate);
end;

procedure TSSBImagerPresenter.DelImg;
begin

end;

destructor TSSBImagerPresenter.Destroy;
begin
  FElements.Free;
  FModel.Free;
  inherited;
end;

{procedure TSSBImagerPresenter.DoCommand(const ACommandName: string;
  AParams: array of Const);
var
  vName: string;
  vS: ansistring;
begin
  vName := LowerCase(ACommandName);

  if vName = 'add' then
  begin
    vS := string(AParams[0].VString);
    //dImage(vS);
  end;

 if vName = 'remove' then
    DoDelImage(nil);
end;  }

procedure TSSBImagerPresenter.DoDelImage(ASender: TObject);
begin
  FModel.DelSelected;
end;

procedure TSSBImagerPresenter.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SelImg;
end;

procedure TSSBImagerPresenter.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  StartDragImg;
end;

procedure TSSBImagerPresenter.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FinishDragImg;
end;

{procedure TSSBImagerPresenter.DoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  vSelected: TImage;
begin
  FIsMouseDown := True;

//  ShowMessage(Sender.ClassName);
//  vSelected := Sender;
  Exit;
  FMouseStartPoint := MousePos / FPanel.Scale.X;//Point;
  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;

  if Sender is TImage then
  begin
    if FModel.Select(TImage(Sender)) then
    begin
      vSelected := TImage(Sender);

      FElementStartPosition := vSelected.Position.Point;
      FMouseElementPoint.X := X;
      FMouseElementPoint.Y := Y;
    end;

    {vFigure := SelectedElement.FigureByCoord(FMouseElementPoint, True);
    if vFigure <> nil then
    begin
      if vFigure.KeyPointLocal(FMouseElementPoint, vPoint, CPrec*2, True) then
      begin
        FSelectedElement.AddPointToDraw(vPoint, TAlphaColorRec.Red);
        FElementStartPosition := vPoint ;
        FLockPoint := True;
      end;
    end;
   FSelectedElement.Repaint; }
 { end;

end; }

{procedure TSSBImagerPresenter.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  i: Integer;
  vX, vY: Integer;
  vPoint: TPointF;
begin
  if Sender = FModel.Selected  then
  begin
    if FIsMouseDown then
      with FModel.Selected do
      begin
        Position.Point := FElementStartPosition + MousePos / FPanel.Scale.X - FMouseStartPoint;
        Adjust(FModel.Selected);
      end
   end;

  FMouseElementPoint.X := X;
  FMouseElementPoint.Y := Y;
end; }

{procedure TSSBImagerPresenter.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := False;
end;   }

procedure TSSBImagerPresenter.DoSelectImage(ASender: TObject);
begin
{  if ASender is TImage then
    FModel.Select(ASender as TImage)     }
end;

{procedure TSSBImagerPresenter.DoZoom(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin

end;  }

procedure TSSBImagerPresenter.DragImg;
begin

end;

procedure TSSBImagerPresenter.FinishDragImg;
begin

end;

procedure TSSBImagerPresenter.Init;
begin
  inherited;

  FView.ChangeImageMouseDownHandler(DoMouseDown);
  FView.ChangeImageMouseUpHandler(DoMouseUp);
  FView.ChangeImageMouseMoveHandler(DoMouseMove);
end;

procedure TSSBImagerPresenter.SelImg;
var
  i: Integer;
begin
  for i := 0 to FModel.ImageCount - 1 do
    if FModel.Images[i].PointInObject(FView.MousePos.X, FView.MousePos.Y) then
    begin
      FSelected := FModel.Images[i];
      FView.SelectElement(FView.ElementUnderMouse);
      Exit;
    end;
    FSelected := nil;
end;

procedure TSSBImagerPresenter.StartDragImg;
begin

end;

{ TSSBPresenter }

constructor TSSBPresenter.Create(AView: ISSBView);
begin
  FView := AView;
end;

destructor TSSBPresenter.Destroy;
begin
  FView := nil;
  inherited;
end;

procedure TSSBPresenter.Init;
begin

end;

procedure TSSBPresenter.OnModelUpdate(ASender: TObject);
begin

end;

{ TSSBObjecterPresenter }

procedure TSSBObjecterPresenter.AddObj;
begin

end;

procedure TSSBObjecterPresenter.DelObj;
begin

end;

procedure TSSBObjecterPresenter.EditObj;
begin

end;

procedure TSSBObjecterPresenter.ImgAutoConvert;
begin

end;

procedure TSSBObjecterPresenter.SelObj;
begin

end;

{ TSSBShaperPresenter }

procedure TSSBShaperPresenter.AddShape;
begin

end;

procedure TSSBShaperPresenter.DelShape;
begin

end;

procedure TSSBShaperPresenter.EditShape;
begin

end;

procedure TSSBShaperPresenter.SelObj;
begin

end;

procedure TSSBShaperPresenter.SelShape;
begin

end;

end.
