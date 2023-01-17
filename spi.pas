unit SPI;
{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, BaseUnix;
	
type
  spi_ioc_transfer = record // Control register required for FileIO
    tx_buf: uint64; //pointer;
    rx_buf: uint64; //pointer; // always 64-bit
    len: uint32; // Number of characters
    speed: uint32; // data rate in Hz
    delay: uint16; // delay CS in usec
    bpw: uint8; // bits per word
    csc: uint8; // CS change
    txn: uint8;
    rxn: uint8;
    pad: uint16;
end; // a total of 32 bytes

	
type
  TSPI = class
  private
    { Private declarations }
	spihnd: longint;
  public
    { Public declarations }
    constructor Create(Bus, Channel: Integer);
	function ReadRegister(Reg: byte): byte;
	procedure WriteRegister(Reg, Value: byte);
	procedure WriteBuffer(Data: Array of Byte; Len: Integer);
end;

const
  // SPI mode flags

  SPI_CPHA = $01;
  SPI_CPOL = $02;

  SPI_MODE_0 = (0 or 0);
  SPI_MODE_1 = (0 or SPI_CPHA);
  SPI_MODE_2 = (SPI_CPOL or 0);
  SPI_MODE_3 = (SPI_CPOL or SPI_CPHA);

  SPI_CS_HIGH = $04;
  SPI_LSB_FIRST = $08;
  SPI_3WIRE = $10;
  SPI_LOOP = $20;
  SPI_NO_CS = $40;
  SPI_READY = $80;

  SPI_CTRL = $6B; // is the "magic byte"

  // control register
  // Read / Write + Size + MagicByte + Register

  SPI_RD_MODE: uint32 = $80016B01;
  SPI_WR_MODE: uint32 = $40016B01;
  SPI_RD_LSB_FIRST: uint32 = $80016B02;
  SPI_WR_LSB_FIRST: uint32 = $40016B02;
  SPI_RD_BITS_PER_WORD: uint32 = $80016B03;
  SPI_WR_BITS_PER_WORD: uint32 = $40016B03;
  SPI_RD_MAX_SPEED_HZ: uint32 = $80046B04;
  SPI_WR_MAX_SPEED_HZ: uint32 = $40046B04;

  SPI_SPEED = 1000000; // data rate in Hz
  SPI_BITS = 8; // data bits
  SPI_LSBF = 0; // LSB first = -1
  SPI_MODE = SPI_MODE_0;

implementation

constructor TSPI.Create(Bus, Channel: Integer);
var
	DeviceName: String;
	val8: byte;
	val32: longword;
begin
	DeviceName := '/dev/spidev' + IntToStr(Bus) + '.' + IntToStr(Channel);
	
	WriteLn('SPI Device = ' + DeviceName);
	
    spihnd := fpOpen(DeviceName, O_RdWr); // Open the interface for read / write

    if spihnd < 0 then begin
		WriteLn('Error opening ' + DeviceName);
	end else begin
		WriteLn('Opened ' + DeviceName + ' OK');
		val8 := SPI_MODE;
		FpIOCtl (spihnd, SPI_WR_MODE, @ val8); // set fashion
		val8 := SPI_BITS;
		FpIOCtl (spihnd, SPI_WR_BITS_PER_WORD, @ val8); // set data bits per byte
		val8 := SPI_LSBF; //-1
		FpIOCtl (spihnd, SPI_WR_LSB_FIRST, @ val8); // Set MSB or LSB first
		val32 := SPI_SPEED;
		FpIOCtl (spihnd, SPI_WR_MAX_SPEED_HZ, @ val32); // set speed	
	end;
end;

function TSPI.ReadRegister(Reg: byte): byte;
var
	inbuf, outbuf: array[0..1] of byte;
	transfer: spi_ioc_transfer;
begin
	outbuf[0] := Reg;
	outbuf[1] := 0;
	inbuf[0] := 0;
	inbuf[1] := 0;
	FillByte (transfer, SizeOf (transfer), 0);
	transfer.tx_buf := uint64(@outbuf[0]);
	transfer.rx_buf := uint64(@inbuf[0]);
	transfer.len := 2;
	transfer.delay := 110;
	transfer.speed := SPI_SPEED;
	transfer.bpw := SPI_BITS;
	transfer.csc := 0;

	FpIOCtl (spihnd, $40206B00, @transfer);
	
	Result := inbuf[1];
end;

procedure TSPI.WriteRegister(Reg, Value: byte);
var
	inbuf, outbuf: array[0..1] of byte;
	transfer: spi_ioc_transfer;
begin
	outbuf[0] := Reg or $80;
	outbuf[1] := Value;
	inbuf[0] := 0;
	inbuf[1] := 0;
	FillByte (transfer, SizeOf (transfer), 0);
	transfer.tx_buf := uint64(@outbuf[0]);
	transfer.rx_buf := uint64(@inbuf[0]);
	transfer.len := 2;
	transfer.delay := 110;
	transfer.speed := SPI_SPEED;
	transfer.bpw := SPI_BITS;
	transfer.csc := 0;

	FpIOCtl (spihnd, $40206B00, @transfer);
end;

procedure TSPI.WriteBuffer(Data: Array of Byte; Len: Integer);
var
	transfer: spi_ioc_transfer;
begin
	FillByte (transfer, SizeOf (transfer), 0);
	transfer.tx_buf := uint64(@Data[0]);
	transfer.rx_buf := uint64(@Data[0]);
	transfer.len := Len;
	transfer.delay := 110;
	transfer.speed := SPI_SPEED;
	transfer.bpw := SPI_BITS;
	transfer.csc := 0;

	FpIOCtl (spihnd, $40206B00, @transfer);
end;


end.
