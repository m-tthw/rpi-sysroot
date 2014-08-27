(* Mathematica Package *)

(* $Id: Serial.m,v 1.1.2.10 2014/01/21 22:58:16 lambertc Exp $ *)

(*BeginPackage["SerialPort`", { "SerialLink`"}]*)
BeginPackage["SerialPort`"]

Needs["SerialLink`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 


(* ::Section:: *) (* API Registration Function *)
DeviceAPI`DeviceClassRegister[ "Serial",
	"OpenFunction" -> iSerialPortOpen,
	"CloseFunction" -> iSerialPortClose,
	"ReadFunction" -> iSerialPortRead,
	"WriteFunction" -> iSerialPortWrite,
	"ReadBufferFunction" -> iSerialPortReadBuffer,
	"WriteBufferFunction" -> iSerialPortWriteBuffer,
	"ExecuteFunction" -> iSerialPortMethods
]

DeviceRead::serialargs = "No arguments are expected for DeviceRead on a Serial device.";
DeviceWrite::serialargs = "A single byte or a Item[\"Break\"] is expected."
DeviceReadBuffer::serialargs = "The number of bytes to read or a \"ReadTerminator\" is expected.";
DeviceWriteBuffer::serialargs = "A list of bytes or a string is expected.";
DeviceExecute::serialargs = "DeviceExecute was called with an unrecognized method or unexpected arguments.";
DeviceExecute::serialargs = "DeviceExecute of `1` expects `2`.";

(* ::Section:: *) (* Device API Adapter Functions *)

(* ::SubSection:: *) (* Port Management *)

$defaultSerialAddress = Which[
	$OperatingSystem === "Windows", "COM3",
	$SystemID === "LinuxARM","/dev/ttyAMA0",
	$OperatingSystem === "MacOSX" || $OperatingSystem === "Unix", "/dev/ttyS0"
]

(*iSerialPortOpen[ iHandle_]:= Check[  SerialPortOpen[ $defaultSerialAddress], $Failed];*)

iSerialPortOpen[ iHandle_]:= SerialPortOpen["/dev/ttyAMA0"];

iSerialPortOpen[ iHandle_, args___]:= Check[ SerialPortOpen[ args], $Failed]

iSerialPortClose[ {iHandle_, dHandle_}, args___]:= Check[ SerialPortClose[ dHandle], $Failed]

(* ::SubSection:: *) (* Synchronous Read *)

Options[ iSerialPortRead]:= Options[ SerialPortRead] 

(* Sync Read a single byte.  ReadTerminator is ignored *)
iSerialPortRead[{ iHandle_, port_SerialPort}, opts:OptionsPattern[]]:= iSerialPortRead[{ iHandle, port}, "Byte", opts]
iSerialPortRead[{ iHandle_, port_SerialPort}, "Byte", opts:OptionsPattern[]]:= SerialPortRead[ port, "Byte", 1, opts]
iSerialPortRead[{ iHandle_, port_SerialPort}, "String", opts:OptionsPattern[]]:= SerialPortRead[ port, "String", 1, opts]
iSerialPortRead[{ iHandle_, port_SerialPort}, args___]:= (
	Message[ DeviceRead::serialargs];
	Return[$Failed];
)
(*iSerialPortRead[{ iHandle_, port_SerialPort}, "String", opts:OptionsPattern[]]:= SerialPortRead[ port, "String", 1, opts]*)

(* Sync Read a list of bytes already buffered and available. *)

iSerialPortReadBuffer[{ iHandle_, port_SerialPort}]:= SerialPortRead[ port, "Byte"];
iSerialPortReadBuffer[{ iHandle_, port_SerialPort}, n_Integer]:= SerialPortRead[ port, "Byte", n]
iSerialPortReadBuffer[{ iHandle_, port_SerialPort}, "ReadTerminator" -> rt_]:= SerialPortRead[ port, "Byte", "ReadTerminator"->rt]

iSerialPortReadBuffer[{ iHandle_, port_SerialPort}, args___]:= (
	Message[ DeviceReadBuffer::serialargs];
	Return[$Failed];
)

(* Read a list of bytes and format as a string *)
(*iSerialPortReadBuffer[{ iHandle_, port_SerialPort}, "String", opts:OptionsPattern[]]:= SerialPortRead[ port, "String", opts]*)

(*
(* Sync Read a list of byteCount bytes. *)
iSerialPortReadBuffer[{ iHandle_, port_SerialPort}, "Byte", byteCount_Integer, opts:OptionsPattern[]]:= SerialPortRead[ port, "Byte", byteCount, opts]:=
	SerialPortRead[ port, "Byte", byteCount, opts]

(* Sync Read a list of byteCount bytes and format as a string. *)
iSerialPortReadBuffer[{ iHandle_, port_SerialPort}, "String", byteCount_Integer, opts:OptionsPattern[]]:= SerialPortRead[ port, "String", byteCount, opts]*)

(* ::SubSection:: *) (* Synchronous Write*)

iSerialPortWrite[{ iHandle_, port_SerialPort}, byte_Integer]:= SerialPortWrite[ port, { byte}]

iSerialPortWrite[{ iHandle_, port_SerialPort}, Item["Break"]]:= SerialPortWrite[ port, Break]

iSerialPortWrite[{ iHandle_, port_SerialPort}, bytes__Integer]:= SerialPortWrite[ port, {bytes}]

iSerialPortWrite[{ iHandle_, port_SerialPort}, str_String]:= SerialPortWrite[ port, str]

iSerialPortWrite[{ iHandle_, port_SerialPort}, args___]:= (
	Message[ DeviceWrite::serialargs];
	Return[$Failed];
)

(* ::SubSection:: *) (* Synchronous Write Buffer*)
	
(*Options[ iSerialPortWriteBuffer] = { "Timeout" -> 0}*)

iSerialPortWriteBuffer[{ iHandle_, port_SerialPort}, bytes__Integer]:= SerialPortWrite[ port, {bytes}]

iSerialPortWriteBuffer[{ iHandle_, port_SerialPort}, string_String]:= SerialPortWrite[ port, string]

iSerialPortWriteBuffer[{ iHandle_, port_SerialPort}, args___]:= (
	Message[ DeviceWriteBuffer::serialargs];
	Return[$Failed];
)
	
(* ::SubSection:: *) (* Synchronous Write *)
	
iSerialPortMethods[ { iHandle_, port_SerialPort}, "SerialReadyQ", args___]:= iSerialPortReadyQ[ port, args]
iSerialPortReadyQ[ port_SerialPort]:= SerialPortReadyQ[ port]
iSerialPortReadyQ[ port_SerialPort, numBytes_Integer]:= SerialPortReadyQ[ port, numBytes]
iSerialPortReadyQ[ { iHandle_, port_SerialPort}, args___]:=  (
	Message[ DeviceExecute::serialargs, "SerialReadyQ", "no arguments or the number of bytes available"];
	Return[$Failed];
)


iSerialPortMethods[ { iHandle_, port_SerialPort}, "ReadFlush", args___]:= SerialPortFlush[ port, "Read"]

iSerialPortMethods[ port_SerialPort, args___]:= (
	Message[ DeviceExecute::serialargs];
	Return[$Failed];
)



End[] (* End Private Context *)

EndPackage[]