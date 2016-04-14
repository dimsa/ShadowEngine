unit uSpriteShapeBuilder;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes, FMX.Forms,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types, System.Types, FMX.Graphics,
  System.JSON, FMX.Controls, FMX.Layouts,
  uNamedList, uEasyDevice, uClasses, uStreamUtil,
  uSSBModels, uView, uSSBTypes, uImagerPresenter, uObjecterPresenter;

type
  TSpriteShapeBuilder = class
  private
    FStatus: TSSBStatus;
    FPanel: TPanel;
    FForm: TForm;

    // Контролы для переключения статуса
    FPanels: array[TSSBStatus] of TLayout;
    FTabsRect: array[TSSBStatus] of TRectangle;
    FTabsImg: array[TSSBStatus] of TImage;

    FView: TView;
    FModel: TSSBModel;
    FControllers: array[TSSBStatus] of TImagerPresenter;
    FIsMouseDown: Boolean;

    FObjecter: IInterface;
    FImager: IInterface;
//    FObjecter: TObjecterPresenter;
//    FImager: TImagerPresenter;

    FResourceFileName: string;

    procedure DoChangeStatus(ASender: TObject);

    procedure SetStatus(const Value: TSSBStatus);
    function GetController: TImagerPresenter;
    function FormTopLeft: TPointF;
    procedure OnModelUpdate(ASender: TObject);
    function GetImager: TImagerPresenter;
    function GetObjecter: TObjecterPresenter;
  public
    property Status: TSSBStatus read FStatus write SetStatus;
    property IsMouseDown: Boolean read FIsMouseDown write FIsMouseDown;
    property Controller: TImagerPresenter read GetController;
    property Imager: TImagerPresenter read GetImager;
    property Objecter: TObjecterPresenter read GetObjecter;
    procedure LoadProject(const AFileName: string);
    procedure SaveProject(const AFileName: string);
    procedure SaveForEngine(const AFileName: string);
    constructor Create(AForm: TForm; APanel: TPanel; ABackground, ASelected: TImage;
      AOpenDialog: TOpenDialog);
    procedure Init(const AProgForm: TForm);
    destructor Destroy; override;
  const
    CPrec = 5;
  end;


implementation

uses
  SSBMainForm;

{ TSpriteShapeBuilder }

constructor TSpriteShapeBuilder.Create(AForm: TForm; APanel: TPanel; ABackground,
  ASelected: TImage; AOpenDialog: TOpenDialog);
begin
  FForm := AForm;
  FView := TView.Create(APanel, ABackground, ASelected, AOpenDialog, FormTopLeft);
  FModel := TSSBModel.Create(OnModelUpdate);
  FImager := TImagerPresenter.Create(FView, FModel);
  FObjecter := TObjecterPresenter.Create(FView, FModel);
  FResourceFileName := 'NoName';
end;

destructor TSpriteShapeBuilder.Destroy;
begin
  FView := nil;//.Free;
  FImager := nil; //.Free;

  inherited;
end;

procedure TSpriteShapeBuilder.DoChangeStatus(ASender: TObject);
var
  vName: String;
begin

  vName := LowerCase(TControl(ASender).Name);
  if vName.Contains('picture') then
    Status := sPicture;

  if vName.Contains('object') then
    Status := sObject;

  if vName.Contains('shape') then
    Status := sShape;
end;

function TSpriteShapeBuilder.FormTopLeft: TPointF;
begin
  Result := FForm.ClientToScreen(TPoint.Zero);
end;

function TSpriteShapeBuilder.GetController: TImagerPresenter;
begin
  Result := FControllers[FStatus];
end;

function TSpriteShapeBuilder.GetImager: TImagerPresenter;
begin
  Result := TImagerPresenter(FImager);
end;

function TSpriteShapeBuilder.GetObjecter: TObjecterPresenter;
begin
  Result := TObjecterPresenter(FObjecter);
end;

procedure TSpriteShapeBuilder.Init(const AProgForm: TForm);
begin
  FPanel := TPanel(AProgForm.FindComponent('MainPanel'));
  with FPanel do
  begin
    try
      Canvas.BeginScene;
      Canvas.Fill.Color := TAlphaColorRec.Blanchedalmond;
      Canvas.FillRect(FPanel.BoundsRect, 0, 0, [], 1, FMX.Types.TCornerType.Round);
    finally
      Canvas.EndScene;
    end;

  end;

  FPanels[sPicture] := TLayout(AProgForm.FindComponent('Picture_Inst'));
  FPanels[sObject] := TLayout(AProgForm.FindComponent('Object_Inst'));
  FPanels[sShape] := TLayout(AProgForm.FindComponent('Shape_Inst'));

  Status := sPicture;

  FTabsRect[sPicture] := TRectangle(AProgForm.FindComponent('Picture_Rect'));
  FTabsRect[sObject] := TRectangle(AProgForm.FindComponent('Object_Rect'));
  FTabsRect[sShape] := TRectangle(AProgForm.FindComponent('Shape_Rect'));

  FTabsImg[sPicture] := TImage(AProgForm.FindComponent('Picture_Img'));
  FTabsImg[sObject] := TImage(AProgForm.FindComponent('Object_Img'));
  FTabsImg[sShape] := TImage(AProgForm.FindComponent('Shape_Img'));

  Status := TSSBStatus.sPicture;
  Imager.Init;
end;

procedure TSpriteShapeBuilder.LoadProject(const AFileName: string);
var
  vStream: TStreamUtil;
  i, vInt: Integer;
  vS, vSTmp, vPar: string;
  vN: Integer;
  vImageElement: TItemImageModel;
 // vTmp: IInterface;
  vBmp: TBitmap;
begin
  vStream := TStreamUtil.Create(AFileName);
  with vStream do
  begin
    StartRead;
    vS := 'SpriteShapeBuilderProjectFile';
    vSTmp := ReadStrWithLength(Length(vS));
    if vS <> vSTmp then
    begin
      ShowMessage('This file format not supported!');
      vStream.Free;
      Exit;
    end;

    ReadStr('Version');
    vInt := ReadInt;

    if vInt <> 1 then
    begin
      ShowMessage('This version of SpriteShapeBuilderProjectFile not supported!');
      vStream.Free;
      Exit;
    end;
    ReadStr('Resources');
    vN := ReadInt;

    for i := 0 to vN - 1 do
    begin
      ReadStr('Resource');

//      vImageElement := FModel.AddImageElement;
      vBmp := ReadBitmap;
//      vTmp := vImageElement;
//      vImageElement.ReadFromStream(vStream);
  //    vTmp := nil;
//      vTmp := FImager;
     // Imager.AddImg(vImageElement);
    end;

//    ReadStr('ResourceFileName');
//    FResourceFileName := ReadStr;
//    ReadStr('Objects');
//    vN := ReadInt;

//    for i := 0 to vN - 1 do
//      FModel.Elements[i].ReadFromStream(vStream);

    Stop;
  end;

  vStream.Free;

  {Look at SSBProjectFormatDescription.txt !!!}
end;

procedure TSpriteShapeBuilder.OnModelUpdate(ASender: TObject);
begin

end;

procedure TSpriteShapeBuilder.SaveForEngine(const AFileName: string);
var
  vS: String;
  vList: TStringList;
begin
  vList := TStringList.Create;
  vS := FModel.ToJson;
  vList.Add(vS);
  vList.SaveToFile(AFileName);
  vList.Free;
end;

procedure TSpriteShapeBuilder.SaveProject(const AFileName: string);
var
  vStream: TStreamUtil;
  i: Integer;
  vBmp: TBitmap;
  vTmp: TStream;
begin
  vStream := TStreamUtil.Create(AFileName);
  with vStream do
  begin
    StartWrite;
    WriteStrOnly('SpriteShapeBuilderProjectFile');
    WriteStr('Version');
    WriteInt(1);
    WriteStr('Resources');
    WriteInt(FModel.ImageElementCount);
    for i := 0 to FModel.ImageElementCount - 1 do
    begin
      WriteStr('Resource');
      FModel.ImageElements[i].WriteToStream(vStream);
    end;

    WriteStr('ResourceFileName');
    WriteStr(FResourceFileName);
    WriteStr('Objects');
    WriteInt(FModel.ElementCount);

    for i := 0 to FModel.ElementCount - 1 do
    begin
      FModel.Elements[i].WriteToStream(vStream);
    end;

    Stop;
  end;

  vStream.Free;

  {Look at SSBProjectFormatDescription.txt !!!}
end;

procedure TSpriteShapeBuilder.SetStatus(const Value: TSSBStatus);
begin
  FPanels[FStatus].Visible := False;
  FStatus := Value;
  FPanels[FStatus].Visible := True;

  if Value = TSSBStatus.sShape then
    Objecter.ShowShapes
  else
    Objecter.HideShapes;
//  FPanels[Value].Visible := True;
end;

end.


{

100% workable streamread

//  vBmp := TBitmap.Create;
//  vBmp.LoadFromFile(AFileName);
//  vTmp := TMemoryStream.Create;
//  vBmp.SaveToStream(vTmp);
//  vStream.StartWrite;
//  vStream.WriteStr('123abc');
//  vStream.WriteInt(vTmp.Size);
//  vStream.WriteStream(vTmp);
//  vStream.Stop;
//  vBmp.Free;
//  vTmp.Free;
//
//  vStream.StartRead;
//  vStream.ReadStr('123abc');
//  i := vStream.ReadInt;
//  vTmp := vStream.ReadStream(i);
//  vTmp.Position := 0;
//  vBmp := TBitmap.Create;
//  vBmp.LoadFromStream(vTmp);
//  vBmp.SaveToFile(AFileName+'123.bmp');
//  vStream.Free;
//
// Exit;

}
