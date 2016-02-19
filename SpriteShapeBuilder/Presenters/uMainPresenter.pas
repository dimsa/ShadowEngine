unit uMainPresenter;

interface

uses
  System.Classes, System.UITypes, System.Types, System.SysUtils, System.Generics.Collections,
  FMX.StdCtrls, FMX.Controls, FMX.Graphics, FMX.Objects, FMX.Dialogs, FMX.Types,
  uSSBModels, uClasses, uEasyDevice, uIView, uIItemView, uMVPFrameWork, uItemPresenterProxy;


type
  TMainPresenter = class(TPresenter)
  private
    function GetView: IMainView;
  protected
    FModel: TSSBModel;
    FPresenterProxy: TItemPresenterProxy;
    property View: IMainView read GetView;
  public
    procedure Init; virtual;
    constructor Create(AView: IView; AModel: TSSBModel); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSSBPresenter }

constructor TMainPresenter.Create(AView: IView; AModel: TSSBModel);
begin
  inherited Create(AView);
  FModel := AModel;
end;

destructor TMainPresenter.Destroy;
begin
  FModel := nil;
  inherited;
end;

function TMainPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TMainPresenter.Init;
begin

end;

end.
