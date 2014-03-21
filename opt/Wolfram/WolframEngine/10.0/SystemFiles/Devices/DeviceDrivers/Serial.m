(* Mathematica Package *)

(* $Id: Serial.m,v 1.1.2.2 2013/11/16 17:09:00 lambertc Exp $ *)

(*BeginPackage["SerialPort`", { "SerialLink`"}]*)
BeginPackage["SerialPort`"]

Needs["SerialLink`"]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

(* ::Section:: *) (* API Registration Function *)
DeviceAPI`DeviceClassRegister[ "Serial", Null, 
	"OpenFunction" -> iSerialPortOpen,
	"CloseFunction" -> iSerialPortClose,
	"ReadFunction" -> iSerialPortRead,
	"ReadBufferFunction" -> iSerialPortReadList,
	"WriteBufferFunction" -> iSerialPortWrite,
	"ExecuteMethodFunction" -> iSerialPortMethods,
	"ReadAsynchronousFunction" -> iRunSerialPortReadAsynchronousTask,
	"WriteAsynchronousFunction" -> iRunSerialPortWriteAsynchronousTask
]

(* ::Section:: *) (* Device API Adapter Functions *)

(* ::SubSection:: *) (* Port Management *)

iSerialPortOpen[ iHandle_, args___]:= Check[ SerialPortOpen[ args], $Failed]

iSerialPortClose[ iHandle_, args___]:= Check[ SerialPortClose[ args], $Failed]

(* ::SubSection:: *) (* Synchronous Read *)

Options[ iSerialPortRead]:= Options[ SerialPortRead] 

(* Sync Read a single byte.  ReadTerminator is ignored *)
iSerialPortRead[{ iHandle_, port_SerialPort}, opts:OptionsPattern[]]:= iSerialPortRead[{ iHandle, port}, "Byte", opts]
iSerialPortRead[{ iHandle_, port_SerialPort}, "Byte", opts:OptionsPattern[]]:= SerialPortRead[ port, "Byte", 1, opts]
iSerialPortRead[{ iHandle_, port_SerialPort}, "String", opts:OptionsPattern[]]:= SerialPortRead[ port, "String", 1, opts]

(* Sync Read a list of bytes already buffered and available. *)
iSerialPortReadList[{ iHandle_, port_SerialPort}, opts:OptionsPattern[]]:= SerialPortRead[ port, "Byte", opts]
iSerialPortReadList[{ iHandle_, port_SerialPort}, "Byte", opts:OptionsPattern[]]:= SerialPortRead[ port, "Byte", opts]

(* Read a list of bytes and format as a string *)
iSerialPortReadList[{ iHandle_, port_SerialPort}, "String", opts:OptionsPattern[]]:= SerialPortRead[ port, "String", opts]

(* Sync Read a list of byteCount bytes. *)
iSerialPortReadList[{ iHandle_, port_SerialPort}, "Byte", byteCount_Integer, opts:OptionsPattern[]]:= SerialPortRead[ port, "Byte", byteCount, opts]:=
	SerialPortRead[ port, "Byte", byteCount, opts]

(* Sync Read a list of byteCount bytes and format as a string. *)
iSerialPortReadList[{ iHandle_, port_SerialPort}, "String", byteCount_Integer, opts:OptionsPattern[]]:= SerialPortRead[ port, "String", byteCount, opts]

(* ::SubSection:: *) (* Synchronous Write *)
	
Options[ iSerialPortWrite] = { "Timeout" -> 0}

iSerialPortWrite[{ iHandle_, port_SerialPort}, byte_Integer, opts:OptionsPattern[]]:= SerialPortWrite[ port, byte, opts]

iSerialPortWrite[{ iHandle_, port_SerialPort}, byte__Integer, opts:OptionsPattern[]]:= SerialPortWrite[ port, {byte}, opts]

iSerialPortWrite[{ iHandle_, port_SerialPort}, string_String, opts:OptionsPattern[]]:= SerialPortWrite[ port, string, opts]

iSerialPortWrite[{ iHandle_, port_SerialPort}, Break, opts:OptionsPattern[]]:= SerialPortWrite[ port, Break, opts]

Options[ iSerialPortWriteList] = { "Timeout" -> 0}

iSerialPortWriteList[{ iHandle_, port_SerialPort}, bytes__Integer, opts:OptionsPattern[]]:= SerialPortWrite[port, {bytes}, opts]

iSerialPortWriteList[{ iHandle_, port_SerialPort}, bytes:{ _Integer..}, opts:OptionsPattern[]]:= SerialPortWrite[ port, bytes, opts]

iSerialPortWriteList[{ iHandle_, port_SerialPort}, strings:{ _String..}, opts:OptionsPattern[]]:= 
	Do[ SerialPortWrite[ port, strings[[i]], opts], {i, Length@strings}]
	
(* ::SubSection:: *) (* Synchronous Write *)
	
iSerialPortMethods[ port_SerialPort, "SerialPortReadyQ", args_List]:= iSerialPortReadyQ[ port, Sequence@@args]
iSerialPortReadyQ[ port_SerialPort]:= SerialPortReadyQ[ port]
iSerialPortReadyQ[ port_SerialPort, numBytes_Integer]:= SerialPortReadyQ[ port, numBytes]

iSerialPortMethods[ port_SerialPort, "SerialPortFlush", args_List]:= iSerialPortFlush[ port, Sequence@@args]
iSerialPortFlush[ port_SerialPort, "Read"]:= SerialPortFlush[ port, "Read"]

(* ::SubSection:: *) (* Asynchronous Write *)
Options[ iRunSerialPortWriteAsynchronousTask]:= Options[ CreateSerialWriteAsynchronousTask]

iRunSerialPortWriteAsynchronousTask[{ iHandle_, port_SerialPort}, opts:OptionsPattern[]]:= 
	RunSerialPortWriteAsynchronousTask[ port, opts]
	
iSerialPortMethods[ port_SerialPort, "SerialPortWriteAsynchronous", args_List]:= 
	iSerialPortWriteAsynchronous[ port, Sequence@@args]

iSerialPortWriteAsynchronous[ port_SerialPort, asyncObj_AsynchronousTaskObject, data:(_String | {_Integer..})]:= SerialPortWriteAsynchronous[ asyncObj, data]
	
(*iSerialPortWriteAsynchronous[ asyncObj_AsynchronousTaskObject, data:(_String | {_Integer..})]:= SerialPortWriteAsynchronous[ port, data]*)

iSerialPortWriteAsynchronous[ port_SerialPort, asyncObj_AsynchronousTaskObject, Break]:= SerialPortWriteAsynchronous[ asyncObj, Break]

(*iSerialPortWriteAsynchronous[ asyncObj_AsynchronousTaskObject, Break]:= SerialPortWriteAsynchronous[ port, Break]*)

(* ::SubSection:: *) (* Asynchronous Read *)

Options[ iRunSerialPortReadAsynchronousTask]:= Options[ RunSerialPortReadAsynchronousTask]

iRunSerialPortReadAsynchronousTask[{ iHandle_, port_SerialPort}, 
	format_:"Byte", repeatSpec_:Infinity, numBytesPerBuffer_:Automatic, opts:OptionsPattern[]]:=
	RunSerialPortReadAsynchronousTask[ port, format, repeatSpec, numBytesPerBuffer, opts]
	
iSerialPortMethods[ port_SerialPort, "CurrentSerialPortData", args_List]:= iCurrentSerialPortData[ port, Sequence@@args]

iCurrentSerialPortData[ port_SerialPort, asyncObj_AsynchronousTaskObject]:= CurrentSerialPortData[ asyncObj]
 
(* SerialPortFlush method also works for async read *)
	

End[] (* End Private Context *)

EndPackage[]