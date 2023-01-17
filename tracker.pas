program tracker;
{$mode objfpc}{$H+}
uses
  cthreads,
  cmem, // the c memory manager is on some systems much faster for multi-threading
  sysutils,
  GPS, Miscellaneous, LoRa;
  
var
  GPSSource: TGPSSource;
  Position: TGPSPosition;
  LoRa1: TLoRa;
  SentenceCounter: Integer;
  PayloadID, Sentence: AnsiString;
begin
  WriteLn('FPC Tracker V1.0');
  
  SentenceCounter := 0;
  PayloadID := 'FPC';
  
  GPSSource := TGPSSource.Create(False);

  LoRa1 := TLoRa.Create(0, 0, 1, 434.450);  
  
  while True do begin
	// Wait for LoRa
	// while LoRa1.IsSending do begin
	// 	Sleep(100);
	// end;

	WriteLn('Getting a position ....');
	// Get position
	Position := GPSSource.GetPosition;
	WriteLn(FormatDateTime('hh:nn:ss', Position.TimeStamp) + ', ' +
			FormatFloat('0.0000', Position.Latitude) + ', ' +
			FormatFloat('0.0000', Position.Longitude) + ', ' +
			FormatFloat('0', Position.Altitude) + ', ' +
			IntToStr(Position.Satellites));
			
	// Create sentence
	Inc(SentenceCounter);
	Sentence := PayloadID + ',' +
				IntToStr(SentenceCounter) + ',' +
				FormatDateTime('hh:nn:ss', Position.TimeStamp) + ',' +
				FormatFloat('0.0000', Position.Latitude) + ',' +
				FormatFloat('0.0000', Position.Longitude) + ',' +
				FormatFloat('0', Position.Altitude) + ',' +
				IntToStr(Position.Satellites);
				
	// Convert to UKHAS format
	Sentence := '$$' + Sentence + '*' + CRC16(Sentence) + #0;
	WriteLn(Sentence);

	WriteLn('Sending a packet ...');
	LoRa1.SendText(Sentence);
	WriteLn('** SENT **');
	Sleep(3000);
  end;
end.