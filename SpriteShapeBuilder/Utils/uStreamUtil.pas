unit uStreamUtil;

interface

uses
  System.Classes, System.SysUtils, FMX.Graphics;

type

  TStreamUtilStatus = (usUnknown, usRead, usWrite);

  TStreamUtil = class
  private
    FFileStream: TFileStream;
    FFileName: string;
    FStatus: TStreamUtilStatus;
    FEncoding: TEncoding;
    procedure TestCanWrite;
    procedure TestCanRead;

  public
      procedure Test(ABmp: TBitmap);
    destructor Destroy; override;
    constructor Create(const AFileName: string);
    procedure StartRead;
    procedure StartWrite;
    procedure Stop;
    property Status: TStreamUtilStatus read FStatus;
    function ReadStr: string; overload; // Read string if Size is writed
    function ReadStr(const ACheck: string): string; overload;// Read string if Size is writed and compares it with parameter ACheck
    function ReadInt: Int64; // Read string if Size is writed
    function ReadStream(const ASize: Int64): TStream;
    function ReadStrWithLength(const ASize: Integer): string; // Read string if Size is writed
    procedure WriteStr(const AText: string);
    procedure WriteStrOnly(const AText: string);
    procedure WriteInt(const AInt: Int64);
    procedure WriteStream(AStream: TStream);
  end;

implementation

{ TStreamUtil }

constructor TStreamUtil.Create(const AFileName: string);
begin
  FFileName := AFileName;
  FStatus := usUnknown;
  FEncoding := TEncoding.UTF8;
end;

destructor TStreamUtil.Destroy;
begin
   if FFileStream <> nil then
     FreeAndNil(FFileStream);

  inherited;
end;

function TStreamUtil.ReadInt: Int64;
var
  vInt: Int64;
  a,b: Int64;
begin
  TestCanRead;
  try
    a := FFileStream.Position;
    FFileStream.ReadBuffer(vInt, SizeOf(vInt));
    b := FFileStream.Position;
    Result := vInt;
  except
    if FFileStream <> nil then
      FreeAndNil(FFileName);

    raise Exception.Create('Can not read Int from stream');
  end;
end;

function TStreamUtil.ReadStr: string;
var
  vStr: string;
  vSize, vLength: Integer;
begin
  TestCanRead;
  try
    FFileStream.ReadBuffer(vLength, SizeOf(vLength));
    vSize := vLength * SizeOf(Copy(vStr, 1, 1));
    SetLength(vStr, vLength);
    FFileStream.ReadBuffer(Pointer(vStr)^, vSize);
    Result := vStr;//TEncoding.UTF8.GetString(Pointer(vStr));
  except
    if FFileStream <> nil then
      FreeAndNil(FFileName);

    raise Exception.Create('Can not read String with Size before It from stream');
  end;
end;

function TStreamUtil.ReadStr(const ACheck: string): string;
begin
  Result := ReadStr;

  if Result <> ACheck then
    raise Exception.Create('Read parameter not equal to check parameter');
end;

function TStreamUtil.ReadStream(const ASize: Int64): TStream;
var
  vStream: TMemoryStream;
  a: Integer;
begin
  TestCanRead;
  try
    vStream := TMemoryStream.Create;
    vStream.Position := 0;
    vStream.Size := ASize;
    a := FFileStream.Position;
    vStream.CopyFrom(FFileStream, ASize);
    a := FFileStream.Position;

    vStream.Position := 0;
//    FFileStream.ReadBuffer(vStream, ASize);
    Result := vStream;//TEncoding.UTF8.GetString(Pointer(vStr));
  except
    if FFileStream <> nil then
      FreeAndNil(FFileName);
    vStream.Free;

    raise Exception.Create('Can not read String with Size before It from stream');
  end;
end;

function TStreamUtil.ReadStrWithLength(const ASize: Integer): string;
var
  vStr: string;
  vSize: Integer;
begin
  TestCanRead;
  try
    vSize := ASize * SizeOf(Copy(vStr, 1, 1));
    SetLength(vStr, ASize);
    FFileStream.ReadBuffer(Pointer(vStr)^, vSize);
    Result := vStr;//TEncoding.UTF8.GetString(Pointer(vStr));
  except
    if FFileStream <> nil then
      FreeAndNil(FFileName);

    raise Exception.Create('Can not read String with Size before It from stream');
  end;
end;

procedure TStreamUtil.StartRead;
begin
  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  FFileStream := TFileStream.Create(FFileName, fmOpenRead);
  FFileStream.Position := 0;

  FStatus := usRead;
end;

procedure TStreamUtil.StartWrite;
begin
  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  FFileStream := TFileStream.Create(FFileName, fmCreate);
  FFileStream.Position := 0;

  FStatus := usWrite;
end;

procedure TStreamUtil.Stop;
begin
  if FFileStream <> nil then
    FreeAndNil(FFileStream);

  FStatus := usUnknown;
end;

procedure TStreamUtil.Test(ABMp: TBitmap);
var
  vBmp: TBitmap;
begin
  FFileStream := TFileStream.Create(FFileName, fmCreate);
  ABmp.SaveToStream(FFileStream);
  FFileStream.Free;

  FFileStream := TFileStream.Create(FFileName, fmOpenRead);
  FFileStream.Position := 0;

  vBmp := TBitmap.Create;
  vBmp.LoadFromStream(FFileStream);
end;

procedure TStreamUtil.TestCanRead;
begin
  if (FFileStream = nil) or (FStatus <> usRead) then
    raise Exception.Create('TStreamUtil not ready for Read!');
end;

procedure TStreamUtil.TestCanWrite;
begin
  if (FFileStream = nil) or (FStatus <> usWrite) then
    raise Exception.Create('TStreamUtil not ready for Write!');
end;

procedure TStreamUtil.WriteInt(const AInt: Int64);
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
  FFileStream.WriteBuffer(Pointer(AText)^, vInt * SizeOf(Copy(AText, 1, 1)));
end;

procedure TStreamUtil.WriteStream(AStream: TStream);
begin
  TestCanWrite;
  AStream.Position := 0;
  FFileStream.CopyFrom(AStream, AStream.Size);
end;

procedure TStreamUtil.WriteStrOnly(const AText: string);
var
  vInt: Integer;
begin
  TestCanWrite;
  vInt := Length(AText) * SizeOf(Copy(AText, 1, 1));
  FFileStream.WriteBuffer(Pointer(AText)^, vInt);
end;

end.
