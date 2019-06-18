FPC Pi HAB Tracker
==================

(Very) simple Pascal tracker for the Pi.

Runs on a Raspberry Pi with LoRa modules attached to the SPI port, and with UBlox GPS on serial port.

Connections
===========

Connect the LoRa module like so:

	LORA     PI
	----     --
	3.3V	3.3V Power
	GND		Ground
	MOSI	MOSI (pin 19)
	MISO	MISO (pin 21)
	NSS		CE0 (pin 24) (CE1 (pin 26) for 2nd module)
	SCK		SLCK

DIO pins are not used in this implementation.

Installation
============

Enable SPI in raspi-config.

Extract this repository, cd to the project folder, and compile with:

	fpc tracker.pas


Execution
=========

cd to the project folder, and run with:

	./tracker
	