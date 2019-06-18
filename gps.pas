unit GPS;
{$mode objfpc}{$H+}

interface

uses cthreads, cmem, sysutils, Classes, serial, Miscellaneous;

type
  TGPSSource = class(TThread)
  private
    { Private declarations }
	CurrentPosition: TGPSPosition;
    procedure ExtractGPSPosition(serialhandle: LongInt; Line: String);
  protected
    { Protected declarations }
    procedure Execute; override;
	procedure SetFlightMode(serialhandle: LongInt; FlightMode: Boolean);
  public
    { Public declarations }
    Constructor Create(CreateSuspended : boolean);
	function GetPosition: TGPSPosition;
end;

implementation

constructor TGPSSource.Create(CreateSuspended : boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

function FixPosition(Position: Double): Double;
var
    Minutes, Seconds: Double;
begin
	Position := Position / 100;

	Minutes := Trunc(Position);
	Seconds := Frac(Position);

	Result := Minutes + Seconds * 5 / 3;
end;

function TGPSSource.GetPosition: TGPSPosition;
begin
	Result := CurrentPosition;
end;

procedure DisableGSV(serialhandle: LongInt);
const
	SetGSV: Array[0..15] of Byte = ($B5, $62, $06, $01, $08, $00, $F0, $03, $00, $00, $00, $00, $00, $01, $03, $39);
begin
	SerWrite(serialHandle, SetGSV, 16);
end;

procedure DisableGLL(serialhandle: LongInt);
const
	SetGLL: Array[0..15] of Byte = ($B5, $62, $06, $01, $08, $00, $F0, $01, $00, $00, $00, $00, $00, $01, $01, $2B);
begin
	SerWrite(serialHandle, SetGLL, 16);
end;

procedure DisableGSA(serialhandle: LongInt);
const
	SetGSA: Array[0..15] of Byte = ($B5, $62, $06, $01, $08, $00, $F0, $02, $00, $00, $00, $00, $00, $01, $02, $32);
begin
	SerWrite(serialHandle, SetGSA, 16);
end;

procedure DisableVTG(serialhandle: LongInt);
const
	SetVTG: Array[0..15] of Byte = ($B5, $62, $06, $01, $08, $00, $F0, $05, $00, $00, $00, $00, $00, $01, $05, $47);
begin
	SerWrite(serialHandle, SetVTG, 16);
end;


procedure TGPSSource.ExtractGPSPosition(serialhandle: LongInt; Line: String);
var
    Fields: TStringList;
	Position: TGPSPosition;
begin
	Position := CurrentPosition;
	
    try
        if Copy(Line, 1, 2) = '$G' then begin
            // Looks like an NME sentence so far
            if Copy(Line, 4, 3) = 'RMC' then begin
                // Just get direction from RMC
                Fields := TStringList.Create;
                ExtractStrings([','], [], pChar(Line), Fields);
                if Fields.Count >= 10 then begin
                    Position.Speed := StrToFloat(Fields[7]);
                    if Position.Speed > 2 then begin
                        Position.Direction := StrToFloat(Fields[8]);
                    end;
					CurrentPosition := Position;
                end;
                Fields.Free;
            end else if Copy(Line, 4, 3) = 'GGA' then begin
                Fields := TStringList.Create;
                ExtractStrings([','], [], pChar(Line), Fields);
                if Fields.Count >= 10 then begin
                    Position.Satellites := StrToIntDef(Fields[7], 0);
                    if Position.Satellites >= 3 then begin
                        Position.TimeStamp := EncodeTime(StrToIntDef(Copy(Fields[1], 1, 2), 0),
                                                         StrToIntDef(Copy(Fields[1], 3, 2), 0),
                                                         StrToIntDef(Copy(Fields[1], 5, 2), 0),
                                                         0);

                        Position.Latitude := FixPosition(StrToFloat(Fields[2]));
                        if Fields[3] = 'S' then Position.Latitude := -Position.Latitude;

                        Position.Longitude := FixPosition(StrToFloat(Fields[4]));
                        if Fields[5] = 'W' then Position.Longitude := -Position.Longitude;

                        if Fields[9] = 'M' then begin
                            Position.Altitude := StrToFloat(Fields[8]);
                        end else begin
                            Position.Altitude := StrToFloat(Fields[9]);
                        end;

                        CurrentPosition := Position;
                    end;
                end;
                Fields.Free;
            end else if Copy(Line, 4, 3) = 'GSV' then begin
				DisableGSV(serialhandle);
            end else if Copy(Line, 4, 3) = 'GLL' then begin
				DisableGLL(serialhandle);
            end else if Copy(Line, 4, 3) = 'GSA' then begin
				DisableGSA(serialhandle);
            end else if Copy(Line, 4, 3) = 'VTG' then begin
				DisableVTG(serialhandle);
            end;
        end;
    finally
        //
    end;
end;

procedure TGPSSource.SetFlightMode(serialhandle: LongInt; FlightMode: Boolean);
const
	SetNav: Array[0..43] of Byte = ($B5, $62, $06, $24, $24, $00, $FF, $FF, $06, $03, $00, $00, $00, $00, $10, $27, $00, $00, $05, $00, $FA, $00, $FA, $00, $64, $00, $2C, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $16, $DC);
begin
	// SetNav: String = {0xB5, 0x62, 0x06, 0x24, 0x24, 0x00, 0xFF, 0xFF, 0x06, 0x03, 0x00, 0x00, 0x00, 0x00, 0x10, 0x27, 0x00, 0x00, 0x05, 0x00, 0xFA, 0x00, 0xFA, 0x00, 0x64, 0x00, 0x2C, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x16, 0xDC};
		
	SerWrite(serialHandle, setNav, 44);
		
	WriteLn('Setting UBlox flight mode');
end;


procedure TGPSSource.Execute;         
var
    Line: AnsiString;
    // Position: THABPosition;
    CommPort: String;
    Buffer : array[1..100] of Ansichar;
    i, Count: Integer;
    serialhandle: LongInt;
    Flags: TSerialFlags;
	PositionCount: Integer;
begin
	FillChar(CurrentPosition, SizeOf(CurrentPosition), 0);
	CommPort := '/dev/ttyAMA0';
	serialhandle := SerOpen(CommPort);
	WriteLn('Handle = ' + IntToStr(serialhandle));
	
	if (serialhandle >= 0) then begin
		Flags:= [];
		SerSetParams(serialhandle,9600,8,NoneParity,1,Flags); 
		Line := '';
		PositionCount := 0;

		while not Terminated do begin
			Count := SerRead(serialhandle, Buffer, 100);
			for i := 1 to Count do begin
				if Buffer[i] = #13 then begin
					ExtractGPSPosition(serialHandle, String(Line));
					// WriteLn(Line);
					Inc(PositionCount);
					if PositionCount >= 60 then begin
						PositionCount := 0;
						SetFlightMode(serialhandle, CurrentPosition.Altitude > 2000);
					end;
					Line := '';
				end else if Buffer[i] = '$' then begin
					Line := Buffer[i];
				end else if Line <> '' then begin
					Line := Line + Buffer[i];
				end else if Buffer[i] <> #10 then begin
					WriteLn(IntToHex(Ord(Buffer[i]), 2));
				end;
            end;
            Sleep(100);
        end;
    end;
end;


end.
