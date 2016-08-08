unit uMainPresenter;

interface

uses
  System.Generics.Collections, FMX.Objects, FMX.StdCtrls, System.Classes, FMX.Forms,
  FMX.Dialogs, System.SysUtils, System.UITypes, FMX.Types, System.Types, FMX.Graphics,
  System.JSON, FMX.Controls, FMX.Layouts,
  uNamedList, uEasyDevice, uClasses, uStreamUtil, uMainModel, uIMainView, uIWorkSpaceView,
  uSSBModels, uWorkSpaceView, uSSBTypes, uImagerPresenter, uObjecterPresenter;

type
  TMainPresenter = class
  private
    FStatus: TSSBStatus;
    FView: IMainView;
    FModel: TSSBModel;
    FIsMouseDown: Boolean;

    FObjecter: TObjecterPresenter;
    FImager: TImagerPresenter;

    FResourceFileName: string;

    procedure DoChangeStatus(ASender: TObject);
    function FormTopLeft: TPointF;
    procedure OnModelUpdate(ASender: TObject);
    function GetStatus: TSSBStatus;
  public
    procedure InitImager;
    procedure InitObjecter;
    procedure InitShaper;
    procedure LoadProject;
    procedure SaveProject;
    procedure SaveForEngine;
    constructor Create(const AView: IMainView; const AWorkSpaceView: IWorkSpaceView);
    property Imager: TImagerPresenter read FImager;
    property Objecter: TObjecterPresenter read FObjecter;
    destructor Destroy; override;
  const
    CPrec = 5;
  end;


implementation

uses
  SSBMainForm;

{ TSpriteShapeBuilder }

constructor TMainPresenter.Create(const AView: IMainView; const AWorkSpaceView: IWorkSpaceView);
//; APanel: TPanel; ABackground, ASelected: TImage; AOpenDialog: TOpenDialog);
begin
  FView := AView;
//  FForm := AForm;
 // FView := TView.Create(APanel, ABackground, ASelected, AOpenDialog, FormTopLeft);
  FModel := TSSBModel.Create(OnModelUpdate);
  FImager := TImagerPresenter.Create(AWorkSpaceView, FModel, GetStatus);
  FObjecter := TObjecterPresenter.Create(AWorkSpaceView, FModel, GetStatus);
  FResourceFileName := 'NoName';
  InitImager;
end;

destructor TMainPresenter.Destroy;
begin
  FView := nil;//.Free;
  FImager := nil; //.Free;
  FObjecter := nil;

  inherited;
end;

procedure TMainPresenter.DoChangeStatus(ASender: TObject);
var
  vName: String;
begin

end;

function TMainPresenter.FormTopLeft: TPointF;
begin
  Result := FView.ClientToScreenPoint(TPoint.Zero); //FForm.ClientToScreen(TPoint.Zero);
end;

function TMainPresenter.GetStatus: TSSBStatus;
begin
  Result := FStatus;
end;

procedure TMainPresenter.InitImager;
begin
  FStatus := TSSBStatus.sPicture;
  FObjecter.HideShapes;
  FView.SetStatus(TSSBStatus.sPicture);
end;

procedure TMainPresenter.InitObjecter;
begin
  FStatus := TSSBStatus.sObject;
  FObjecter.HideShapes;
  FView.SetStatus(TSSBStatus.sObject);
end;

procedure TMainPresenter.InitShaper;
begin
  FStatus := TSSBStatus.sShape;
  FObjecter.ShowShapes;
  FView.SetStatus(TSSBStatus.sShape);
end;

procedure TMainPresenter.LoadProject;//(const AFileName: string);
var
  vStream: TStreamUtil;
  i, vInt: Integer;
  vS, vSTmp, vPar: string;
  vN: Integer;
  vImageElement: TItemImageModel;
  vElement: TResourceModel;
  vFileName: string;
begin
  if FView.FilenameFromDlg(vFileName) then
    try
      vStream := TStreamUtil.Create(vFileName);
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

        vImageElement := FModel.AddImageElement;
        vImageElement.ReadFromStream(vStream);
        FImager.AddImg(vImageElement);
        vImageElement.RaiseUpdateEvent;
      end;

      ReadStr('ResourceFileName');
      FResourceFileName := ReadStr;
      ReadStr('Objects');
      vN := ReadInt;

      for i := 0 to vN - 1 do
      begin
        vElement := FModel.AddResource;
        vElement.ReadFromStream(vStream);
        FObjecter.AddObj(vElement);
        FObjecter.ShowShapes;
        vElement.RaiseUpdateEvent;
      end;

      Stop;
    end;

  finally
      vStream.Free;
  end;

  InitImager;
  {Look at SSBProjectFormatDescription.txt !!!}
end;

procedure TMainPresenter.OnModelUpdate(ASender: TObject);
begin

end;

procedure TMainPresenter.SaveForEngine;//(const AFileName: string);
var
  vS: String;
  vList: TStringList;
  vBmp: TBitmap;
  vFileName: string;
begin
  if FView.FilenameFromDlg(vFileName) then
  begin
    vList := TStringList.Create;
    vS := FModel.ToJson;
    vList.Add(vS);
    vList.SaveToFile(vFileName);
    vList.Free;
    vBmp := FModel.GenerateWholeBitmap;

    vS := ExtractFileDir(vFileName) + '\' + FModel.ImageFileName;
    if not FileExists(vS) then
      vBmp.SaveToFile(vS);
    vBmp.Free;
  end;
end;

procedure TMainPresenter.SaveProject;//(const AFileName: string);
var
  vStream: TStreamUtil;
  i: Integer;
  vBmp: TBitmap;
  vTmp: TStream;
  vFileName: string;
begin
  if FView.FilenameFromDlg(vFileName) then
  begin
    vStream := TStreamUtil.Create(vFileName);
    FModel.SaveProjectToStream(vStream);
    vStream.Free;
  end;

  { Look at SSBProjectFormatDescription.txt !!!}
end;

{procedure TMainPresenter.SetStatus(const Value: TSSBStatus);
begin
  FPanels[FStatus].Visible := False;
  FStatus := Value;
  FPanels[FStatus].Visible := True;

  if Value = TSSBStatus.sShape then
    Objecter.ShowShapes
  else
    Objecter.HideShapes;
end;  }

end.
