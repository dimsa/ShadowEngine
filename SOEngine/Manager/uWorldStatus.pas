unit uWorldStatus;

interface

uses
  uSoModel;

type

TSoModelFriend = class(TSoModel);

TWorldStatus = class
private
  FWidth, FHeight: Integer;
  PWidth, PHeight: PInteger;
  FModel: TSoModelFriend;
public
  procedure Resize;
  property Width: Integer read FWidth;
  property Height: Integer read FHeight;

  constructor Create(const AModel: TSoModel; const AWidth, AHeight: PInteger);
end;


implementation

{ TWorldStatus }

constructor TWorldStatus.Create(const AModel: TSoModel; const AWidth, AHeight: PInteger);
begin
  FModel := TSoModelFriend(AModel);
  PWidth := AWidth;
  PHeight := AHeight;

  Resize;
end;

procedure TWorldStatus.Resize;
begin
  FWidth := PWidth^;
  FHeight := PHeight^;
end;

end.
