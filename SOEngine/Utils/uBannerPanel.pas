unit uBannerPanel;

interface

uses
 FMX.Advertising, System.Types, System.Classes, FMX.Types, FMX.Controls, FMX.StdCtrls,
 System.Generics.Collections, FMX.Forms, System.UITypes, FMX.Layouts, FMX.Objects,
 System.SysUtils, FMX.Dialogs,
 uEasyDevice;

type
  TBannerPanel = class
  private
    FHeader, FFooter: TRectangle;
    FPanel, FBody: TLayout;
    FCloseButton: TButton;
    FLabel: TLabel;
    FForm: TForm;
    FList: TList<TBannerAd>;
    FPrepared: Integer; // Количество приготовленных баннеров
    procedure OnLoad(Sender: TObject);
    procedure Hide(Sender: TObject);
    procedure ReArrange;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  public
    property Visible: Boolean read GetVisible write SetVisible;
    procedure Show;
    function IsReadyToShow: Boolean;
    procedure Resize;
    constructor Create(AOwner: TForm);
    destructor Destroy; override;
    procedure Add(AAdCode: String);
    procedure Prepare;
  const
    CPadding = 3;
  end;

implementation

{ TBannerPanel }


procedure TBannerPanel.Add(AAdCode: String);
var
  vBanner: TBannerAd;
begin
  vBanner := TBannerAd.Create(FBody);
  vBanner.Parent := FBody;
  FBody.Controls.Add(vBanner);

  vBanner.AdUnitID := AAdCode;
  vBanner.AdSize := TBannerAdSize.Auto;
  vBanner.Align := TAlignLayout.MostTop;
  {$IFDEF DEBUG}
    //vBanner.TestMode := True;
  {$ENDIF}
  //vBanner.Align := TAlignLayout.Horizontal;
  vBanner.OnDidLoad := OnLoad;
  vBanner.Visible := True;

  FList.Add(vBanner)
end;

constructor TBannerPanel.Create(AOwner: TForm);
begin
  FList := TList<TBannerAd>.Create;
  FForm := AOwner;
  FPanel := TLayout.Create(AOwner); //TPanel.Create(AOwner);
  FPanel.Parent := AOwner;
  FPanel.Visible := False;
  FPanel.Align := TAlignLayout.VertCenter;
  FPanel.BringToFront;

  FHeader := TRectangle.Create(FPanel);
  FHeader.Parent := FPanel;
  FHeader.Align := TAlignLayout.MostTop;
  FHeader.Fill.Color := TAlphaColor($ff323232);
  FHeader.Stroke.Color := TAlphaColor($ff323232);
  FHeader.Height := 60;
  FHeader.Margins.Bottom := FHeader.Height * 0.25;

  FBody := TLayout.Create(FPanel);
  FBody.Parent := FPanel;
  FBody.Align := TAlignLayout.MostTop;
  FBody.Height := 240;

  FFooter := TRectangle.Create(FPanel);
  FFooter.Parent := FPanel;
  FFooter.Align := TAlignLayout.MostTop;
  FFooter.Height := 60;
  FFooter.Fill.Color := TAlphaColor($ff323232);
  FFooter.Stroke.Color := TAlphaColor($ff323232);

  FCloseButton := TButton.Create(FFooter);
  FCloseButton.Parent := FFooter;
  FFooter.Controls.Add(FCloseButton);
  FCloseButton.Align := TAlignLayout.Client;
  FCloseButton.Text := 'Thanks! Nothing interesting.';
  FCloseButton.OnClick := Hide;
  FCloseButton.Opacity := 1;
  FCloseButton.StyledSettings := FCloseButton.StyledSettings - [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.FontColor, TStyledSetting.Other, TStyledSetting.Style];
  FCloseButton.Font.Size := 16;

  FLabel := TLabel.Create(FHeader);
  FLabel.Parent := FHeader;
  FHeader.Controls.Add(FLabel);
  FLabel.Align := TAlignLayout.Client;

  FLabel.WordWrap := True;
  FLabel.Position.Y := 0;
  FLabel.StyledSettings := FLabel.StyledSettings - [TStyledSetting.Family,TStyledSetting.Size, TStyledSetting.FontColor, TStyledSetting.Other, TStyledSetting.Style];
  FLabel.Text := 'Advertisments which you might be interested';
  FLabel.Font.Size := 16;
  FLabel.Font.Style := [TFontStyle.fsBold];
  FLabel.FontColor := TAlphaColorRec.White;
  FLabel.Opacity := 1;
  FLabel.TextAlign := TTextAlign.Center;
  FLabel.VertTextAlign := TTextAlign.Center;
end;

destructor TBannerPanel.Destroy;
var
  i: Integer;
  vBanner: TBannerAd;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    vBanner := FList[i];
    FList.Delete(i);
    vBanner.Free;
  end;

  FCloseButton.Free;
  FPanel.Free;

  inherited;
end;

function TBannerPanel.GetVisible: Boolean;
begin
  Result := FPanel.Visible;
end;

procedure TBannerPanel.Hide(Sender: TObject);
begin
  Self.Visible := False;
end;

function TBannerPanel.IsReadyToShow: Boolean;
var
  i: Integer;
begin
  FPrepared := 0;

  for i := 0 to FList.Count - 1 do
    if FList[i].IsLoaded then
      Inc(FPrepared)
    else
      FList[i].LoadAd;

  Result := (FPrepared >= FList.Count);
end;

procedure TBannerPanel.OnLoad(Sender: TObject);
begin
 // FPrepared := FPrepared + 1;
  if IsReadyToShow then
    ReArrange;
end;

procedure TBannerPanel.Prepare;
var
  i: Integer;
begin
  if FList.Count <= 0 then
    Exit;

  FPrepared := 0;

  for i := 0 to FList.Count - 1 do
    FList[i].LoadAd;
end;

procedure TBannerPanel.ReArrange;
var
  i: Integer;
  vSize: TPointF;
begin
  if FList.Count <= 0 then
    Exit;

  vSize := uEasyDevice.getDisplaySizeInPx;

  for i := 0 to FList.Count - 1 do
  begin
    FList[i].RecalcSize;
    FList[i].Margins.Rect := RectF(0, FList[i].Height * 0.5, 0, FList[i].Height * 0.5);
    FList[i].BringToFront;
  end;

  FPanel.Align := TAlignLayout.Client;
  FHeader.Align := TAlignLayout.MostTop;
  FBody.Align := TAlignLayout.MostTop;
  FFooter.Align := TAlignLayout.MostBottom;
end;

procedure TBannerPanel.Resize;
begin
  if Visible then
    ReArrange;
end;

procedure TBannerPanel.SetVisible(const Value: Boolean);
var
  i: Integer;
begin
  FPanel.Visible := Value;
  for i := 0 to FList.Count - 1 do
  begin
    FList[i].Visible := Value;
    if Value = True then
      if not FList[i].IsLoaded then
        FList[i].LoadAd;
  end;

  FPanel.Visible := Value;
end;

procedure TBannerPanel.Show;
begin
  Self.Visible := True;
  ReArrange;
end;

end.
