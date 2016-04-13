unit uStreamUtil;

interface

uses
  System.Classes, System.SysUtils;

type

  TStreamUtilStatus = (usUnknown, usRead, usWrite);

  TStreamUtil = class
  private
    FFileStream: TFileStream;
    FFileName: string;
    FStatus: TStreamUtilStatus;
    procedure TestCanWrite;
  public
    destructor Destroy; override;
    constructor Create(const AFileName: string);
    procedure StartRead;
    procedure StartWrite;
    procedure Stop;
    property Status: TStreamUtilStatus read FStatus;
    procedure WriteStr(const AText: string);
    procedure WriteStrOnly(const AText: string);
    procedure WriteInt(const AInt: Integer);
  end;

implementation

{ TStreamUtil }

constructor TStreamUtil.Create(const AFileName: string);
begin
  FFileName := AFileName;
  FStatus := usUnknown;
end;

destructor TStreamUtil.Destroy;
begin
   if FFileStream <> nil then
     FFileStream.Free;

  inherited;
end;

procedure TStreamUtil.StartRead;
begin
  if FFileStream <> nil then
    FFileStream.Free;

  FFileStream := TFileStream.Create(FFileName, fmOpenRead);
  FFileStream.Seek(0, soFromBeginning);

  FStatus := usRead;
end;

procedure TStreamUtil.StartWrite;
begin
  if FFileStream <> nil then
    FFileStream.Free;

  FFileStream := TFileStream.Create(FFileName, fmOpenWrite);
  FFileStream.Seek(0, soFromBeginning);

  FStatus := usWrite;
end;

procedure TStreamUtil.Stop;
begin
  if FFileStream <> nil then
    FFileStream.Free;

  FStatus := usUnknown;
end;

procedure TStreamUtil.TestCanWrite;
begin
  if (FFileStream = nil) or (FStatus <> usWrite) then
    raise Exception.Create('TStreamUtil not ready for Write!');
end;

procedure TStreamUtil.WriteInt(const AInt: Integer);
begin
  TestCanWrite;
  FFileStream.WriteBuffer(AInt, SizeOf(AInt));
end;

procedure TStreamUtil.WriteStr(const AText: string);
var
  vInt: Integer;
begin
  TestCanWrite;
  vInt := Length(AText);
  FFileStream.WriteBuffer(vInt, SizeOf(vInt));
  FFileStream.WriteBuffer(AText, vInt * SizeOf(Copy(AText, 1, 1)));
end;

procedure TStreamUtil.WriteStrOnly(const AText: string);
var
  vInt: Integer;
begin
  TestCanWrite;
  vInt := Length(AText);
  FFileStream.WriteBuffer(AText, vInt * SizeOf(Copy(AText, 1, 1)));
end;

end.
