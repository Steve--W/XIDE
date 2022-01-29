(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit InterfaceTypesDll;
(*
   Interface declarations.
   For inclusion inside the dynamically created dll (fpc compiler), providing functions available in user event code
   (also see units CompileUserCode and DllInterface).
 *)
{$mode objfpc}{$H+}
{$INTERFACES CORBA}
interface

uses
  Classes, SysUtils, EventsInterface
  ;

{$ifdef Dll}
type
Tshowmessage=procedure(msg:String) of object;                                                        stdcall;
Tsetpropertyvalue=procedure(nodeName:String;propName:String;newValue:String) of object;              stdcall;
TsetpropertyvalueIndexed=procedure(nodeName:String;propName:String;newValue:TStringArray;x,y:integer) of object;   stdcall;
Tconfirm=function(TextMessage:string):boolean of object;                                             stdcall;
Tgetpropertyvalue=function(nodeName:String;propName:String):string  of object;                        stdcall;
Tprompt=function(TextMessage,promptString:string):string of object;                                  stdcall;
Tshowxform=procedure(XFormID:String; modal:Boolean) of object;                                       stdcall;
Tclosexform=procedure(XFormID:String) of object;                                                     stdcall;
TCopyToClip=procedure(str:String) of object;                                                         stdcall;
TCopyFromClip=function(e:TEventStatus):String of object;                                             stdcall;
TLoadTableFromExcelCopy=procedure(TableName,CopiedString:String) of object;                         stdcall;
TGetTableDataForExcel=function(TableName:String):String of object                                   stdcall;
TLoadTableFromNumArray=procedure(TableName:String;NumArray:T2DNumArray) of object;                  stdcall;
TGetTableDataArray=function(TableName:String;SkipHeader:Boolean):T2DStringArray of object;       stdcall;
TDoEvent=procedure(EventType,NodeId,myValue:String) of object;                                      stdcall;
TMoveComponent=procedure(nodeId:string;NewParentId:string) of object;                                stdcall;
TCopyComponent=procedure(nodeId,NewParentId,NewName:string) of object;                                stdcall;
TDeleteComponent=function(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean of object;             stdcall;
TGetGPUParamNumValue=function(GPUName,pName:String):TNumArray of object;                                stdcall;
TGetGPUParam2DNumValue=function(GPUName,pName:String):T2DNumArray of object;                          stdcall;
TGetGPUConstIntValue=function(GPUName,pName:String):integer of object;                                stdcall;
TSetGPUParamNumValue=procedure(GPUName,pName:String;pValue:TNumArray) of object;                        stdcall;
TSetGPUParam2DNumValue=procedure(GPUName,pName:String;pValue:T2DNumArray) of object;                        stdcall;
TSetGPUConstIntValue=procedure(GPUName,pName:String;pValue:integer) of object;                        stdcall;
TStartMain=procedure(e:TEventStatus) of object;                        stdcall;
TShowBusy=procedure(e:TEventStatus) of object;                        stdcall;
THideBusy=procedure of object;                        stdcall;
TProcessMessages=procedure of object;                        stdcall;
TUserSystemAsString=function():String of object; stdcall;
TLoadUserSystemString=procedure(SystemString:String) of object; stdcall;
TConsoleLog=procedure(txt:String) of object; stdcall;
TArray2DToString=function(arr:T2DNumArray):String of object;                                 stdcall;
TGetGPUPixelArray=function(GPUName:String):T3DNumArray of object;                                stdcall;
TGetGPUPixelArrayAsString=function(GPUName:String):String of object;                                stdcall;
TGetGPUStageArray=function(GPUName:String):T3DNumArray of object;                                stdcall;
TGetGPUStageArrayAsString=function(GPUName:String):String of object;                                stdcall;
TGetGPUInitStageArray=function(GPUName:String):T3DNumArray of object;                                stdcall;
TDebugStart=procedure of object; stdcall;
TRunPython=procedure(str:String) of object; stdcall;
TSetImageSource=procedure(nm,str:String) of object; stdcall;
TWobbleCEF=procedure(nm:String)of object;    stdcall;
TPyodideLoadPackage=procedure(nm:String) of object;  stdcall;
TPyodidePackageLoaded=function(nm:String):Boolean of object;  stdcall;
TDSFetchRow=function(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean of object;  stdcall;
TDSAppendRow=function(e:TEventStatus;DSName:String;recObject:TObject):Boolean of object; stdcall;
TDSDeleteRow=function(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean of object; stdcall;
TDSDeleteAllRows=function(e:TEventStatus;DSName:String):Boolean of object; stdcall;

//TDSDatasetToString=function(e:TEventStatus;dsName:String):Boolean of object; stdcall;

type
IMyMethodInterface = interface(IInterface)
    procedure mmiSetEventsNameSpace(NameSpace:String);  stdcall;
    procedure mmiShowMessage(msg:String); stdcall;
    procedure mmiShowXForm(XFormID:String; modal:Boolean);  stdcall;
    procedure mmiCloseXForm(XFormID:String);  stdcall;
    procedure mmiSetPropertyValue(nodeName:String;propName:String;newValue:String);  stdcall;
    procedure mmiSetPropertyValueIndexed(nodeName:String;propName:String;newValue:TStringArray; x,y:integer);  stdcall;
    function mmiGetPropertyValue(nodeName:String;propName:String):string;  stdcall;
    Function mmiconfirm(TextMessage:string):boolean;  stdcall;
    Function mmiprompt(TextMessage,promptString:string):string;  stdcall;
    procedure mmiCopyToClip(str:String);  stdcall;
    function mmiCopyFromClip(e:TEventStatus):String;  stdcall;
    procedure mmiLoadTableFromExcelCopy(TableName,CopiedString:String); stdcall;
    function mmiGetTableDataForExcel(TableName:String):String;  stdcall;
    procedure mmiLoadTableFromNumArray(TableName:String;NumArray:T2DNumArray); stdcall;
    function mmiGetTableDataArray(TableName:String;SkipHeader:Boolean):T2DStringArray; stdcall;
    procedure mmiDoEvent(EventType,NodeId,myValue:String);   stdcall;
    procedure mmiMoveComponent(nodeId:string;NewParentId:string);  stdcall;
    procedure mmiCopyComponent(nodeId,NewParentId,NewName:string);  stdcall;
    function mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;  stdcall;
    function mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;  stdcall;
    function mmiGetGPUParam2DNumValue(GPUName,pName:String):T2DNumArray;  stdcall;
    function mmiGetGPUConstIntValue(GPUName,pName:String):integer;  stdcall;
    procedure mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);  stdcall;
    procedure mmiSetGPUParam2DNumValue(GPUName,pName:String;pValue:T2DNumArray);  stdcall;
    procedure mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);  stdcall;
    procedure mmiStartMain(e:TEventStatus); stdcall;
    procedure mmiShowBusy(e:TEventStatus); stdcall;
    procedure mmiHideBusy; stdcall;
    procedure mmiProcessMessages; stdcall;
    function mmiUserSystemAsString():String; stdcall;
    procedure mmiLoadUserSystemString(SystemString:String); stdcall;
    procedure mmiConsoleLog(txt:String);  stdcall;
    function mmiArray2DToString(arr:T2DNumArray):String;         stdcall;
    function mmiGetGPUPixelArray(GPUName:String):T3DNumArray;                                stdcall;
    function mmiGetGPUPixelArrayAsString(GPUName:String):String;                             stdcall;
    function mmiGetGPUStageArray(GPUName:String):T3DNumArray;                                stdcall;
    function mmiGetGPUStageArrayAsString(GPUName:String):String;                             stdcall;
    function mmiGetGPUInitStageArray(GPUName:String):T3DNumArray;                                stdcall;
    procedure mmiDebugStart; stdcall;
    procedure mmiRunPython(str:String); stdcall;
    procedure mmiSetImageSource(nm,str:String); stdcall;
    procedure mmiWobbleCEF(nm:String);    stdcall;
    procedure mmiPyodideLoadPackage(nm:String);  stdcall;
    function mmiPyodidePackageLoaded(nm:String):Boolean; stdcall;
    function mmiDSFetchRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean;  stdcall;
    function mmiDSAppendRow(e:TEventStatus;DSName:String;recObject:TObject):Boolean; stdcall;
    function mmiDSDeleteRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean; stdcall;
    function mmiDSDeleteAllRows(e:TEventStatus;DSName:String):Boolean; stdcall;
//    function mmiDSDatasetToString(e:TEventStatus;dsName:String):Boolean; stdcall;
end;


var AppMethods : IMyMethodInterface;

var
showmessage:Tshowmessage;
showxform:Tshowxform;
closexform:Tclosexform;
confirm:Tconfirm;
prompt:Tprompt;
setpropertyvalue:Tsetpropertyvalue;
setpropertyvalueindexed:Tsetpropertyvalueindexed;
getpropertyvalue:Tgetpropertyvalue;
copytoclip:TCopyToClip;
copyfromclip:TCopyFromClip;
LoadTableFromExcelCopy:TLoadTableFromExcelCopy;
GetTableDataForExcel:TGetTableDataForExcel;
LoadTableFromNumArray:TLoadTableFromNumArray;
GetTableDataArray:TGetTableDataArray;
doevent:TDoEvent;
movecomponent:TMoveComponent;
copycomponent:TCopyComponent;
deletecomponent:TDeleteComponent;
getgpuparamnumvalue:TGetGPUParamNumValue;
getgpuparam2dnumvalue:TGetGPUParam2DNumValue;
getgpuconstintvalue:TGetGPUConstIntValue;
setgpuparamnumvalue:TSetGPUParamNumValue;
setgpuparam2Dnumvalue:TSetGPUParam2DNumValue;
setgpuconstintvalue:TSetGPUConstIntValue;
startmain:TStartMain;
showbusy:TShowBusy;
hidebusy:THideBusy;
ProcessMessages:TProcessMessages;
UserSystemAsString:TUserSystemAsString;
LoadUserSystemString:TLoadUserSystemString;
ConsoleLog:TConsoleLog;
Array2DToString:TArray2DToString;
GetGPUPixelArray:TGetGPUPixelArray;
GetGPUPixelArrayAsString:TGetGPUPixelArrayAsString;
GetGPUStageArray:TGetGPUStageArray;
GetGPUStageArrayAsString:TGetGPUStageArrayAsString;
GetGPUInitStageArray:TGetGPUInitStageArray;
DebugStart:TDebugStart;
RunPython:TRunPython;
SetImageSource:TSetImageSource;
WobbleCEF:TWobbleCEF;
PyodideLoadPackage:TPyodideLoadPackage;
PyodidePackageLoaded:TPyodidePackageLoaded;
DSFetchRow:TDSFetchRow;
DSAppendRow:TDSAppendRow;
DSDeleteRow:TDSDeleteRow;
DSDeleteAllRows:TDSDeleteAllRows;

//DSDatasetToString:TDSDatasetToString;


procedure SetDllContext(mmi : IMyMethodInterface); stdcall;
{$endif}


implementation

{$ifdef Dll}
procedure SetDllContext(mmi : IMyMethodInterface); stdcall;
var
  dummy:String;
begin
  // Map Appmethods onto the interface object passed in
  AppMethods := mmi;

  showmessage:=@AppMethods.mmishowmessage;
  showxform:=@appmethods.mmishowxform;
  closexform:=@appmethods.mmiclosexform;
  setpropertyvalue:=@AppMethods.mmisetpropertyvalue;
  setpropertyvalueindexed:=@AppMethods.mmisetpropertyvalueindexed;
  getpropertyvalue:=@AppMethods.mmigetpropertyvalue;
  confirm:=@AppMethods.mmiconfirm;
  prompt:=@AppMethods.mmiprompt;
  copytoclip:=@AppMethods.mmiCopyToClip;
  copyfromclip:=@AppMethods.mmiCopyFromClip;
  LoadTableFromExcelCopy:=@AppMethods.mmiLoadTableFromExcelCopy;
  GetTableDataForExcel:=@AppMethods.mmiGetTableDataForExcel;
  LoadTableFromNumArray:=@AppMethods.mmiLoadTableFromNumArray;
  GetTableDataArray:=@AppMethods.mmiGetTableDataArray;
  doevent:=@appmethods.mmiDoEvent;
  movecomponent:=@appmethods.mmiMoveComponent;
  copycomponent:=@appmethods.mmiCopyComponent;
  deletecomponent:=@appmethods.mmiDeleteComponent;
  getgpuparamnumvalue:=@appmethods.mmiGetGPUParamNumValue;
  getgpuparam2dnumvalue:=@appmethods.mmiGetGPUParam2DNumValue;
  getgpuconstintvalue:=@appmethods.mmiGetGPUConstIntValue;
  setgpuparamnumvalue:=@appmethods.mmiSetGPUParamNumValue;
  setgpuparam2Dnumvalue:=@appmethods.mmiSetGPUParam2DNumValue;
  setgpuconstintvalue:=@appmethods.mmiSetGPUConstIntValue;
  startmain:=@appmethods.mmiStartMain;
  showbusy:=@appmethods.mmiShowBusy;
  hidebusy:=@appmethods.mmiHideBusy;
  ProcessMessages:=@appmethods.mmiProcessMessages;
  UserSystemAsString:=@appmethods.mmiUserSystemAsString;
  LoadUserSystemString:=@appmethods.mmiLoadUserSystemString;
  ConsoleLog:=@appmethods.mmiConsoleLog;
  Array2DToString:=@appmethods.mmiArray2DToString;
  GetGPUPixelArray:=@appmethods.mmiGetGPUPixelArray;
  GetGPUPixelArrayAsString:=@appmethods.mmiGetGPUPixelArrayAsString;
  GetGPUStageArray:=@appmethods.mmiGetGPUStageArray;
  GetGPUStageArrayAsString:=@appmethods.mmiGetGPUStageArrayAsString;
  GetGPUInitStageArray:=@appmethods.mmiGetGPUInitStageArray;
  DebugStart:=@appmethods.mmiDebugStart;
  RunPython:=@appmethods.mmiRunPython;
  SetImageSource:=@appmethods.mmiSetImageSource;
  WobbleCEF:=@appmethods.mmiWobbleCEF;
  PyodideLoadPackage:=@appmethods.mmiPyodideLoadPackage;
  PyodidePackageLoaded:=@appmethods.mmiPyodidePackageLoaded;
  DSFetchRow:=@appmethods.mmiDSFetchRow;
  DSAppendRow:=@appmethods.mmiDSAppendRow;
  DSDeleteRow:=@appmethods.mmiDSDeleteRow;
  DSDeleteAllRows:=@appmethods.mmiDSDeleteAllRows;
//  DSDatasetToString:=@appmethods.mmiDSDatasetToString;

end;
{$endif}

end.

