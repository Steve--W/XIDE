(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit InterfaceTypes;
(*
   Interface declarations
   for use with the dynamically created units, used for executing user event code (also see unit CompileUserCode)
 *)

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, EventsInterface;

{$ifndef Dll}
type
Tshowmessage=procedure(msg:String) of object;
Tsetpropertyvalue=procedure(nodeName:String;propName:String;newValue:String) of object;
TsetpropertyvalueIndexed=procedure(nodeName:String;propName:String;newValue:TStringArray;x,y:integer) of object;
Tconfirm=function(TextMessage:string):boolean of object;
Tgetpropertyvalue=function(nodeName:String;propName:String):string  of object;
Tprompt=function(TextMessage,promptString:string):string of object;
Tshowxform=procedure(XFormID:String; modal:Boolean) of object;
Tclosexform=procedure(XFormID:String) of object;
TCopyToClip=procedure(str:String) of object;
TCopyFromClip=function(e:TEventStatus):String of object;
TLaunchHTMLPage=procedure(DataString:String) of object;
TLoadTableFromExcelCopy=procedure(TableName,CopiedString:String) of object;
TGetTableDataForExcel=function(TableName:String):String of object;
TLoadTableFromNumArray=procedure(TableName:String;NumArray:T2DNumArray) of object;
TLoad3DTableFromNumArray=procedure(TableName:String;NumArray:T3DNumArray) of object;
TLoadTableFromStringArray=procedure(TableName:String;StrArray:T2DStringArray) of object;
TGetTableDataArray=function(TableName:String;SkipHeader:Boolean):T2DStringArray of object;
TGet3DTableNumArray=function(TableName:String):T3DNumArray of object;
TDoEvent=procedure(EventType,NodeId,myValue:String) of object;
TMoveComponent=procedure(nodeId:string;NewParentId:string) of object;
TCopyComponent=procedure(nodeId,NewParentId,NewName:string) of object;
TDeleteComponent=function(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean of object;
TDeleteSelectedTreeNode=procedure(TreeName:string) of object;
TGetGPUParamNumValue=function(GPUName,pName:String):TNumArray of object;
TGetGPUParam2DNumValue=function(GPUName,pName:String):T2DNumArray of object;
TGetGPUConstIntValue=function(GPUName,pName:String):integer of object;
TSetGPUParamNumValue=procedure(GPUName,pName:String;pValue:TNumArray) of object;
TSetGPUParamNumValueFromStr=procedure(GPUName,pName:String;pValue:String) of object;
TSetGPUParam2DNumValue=procedure(GPUName,pName:String;pValue:T2DNumArray) of object;
TSetGPUParam2DNumValueFromStr=procedure(GPUName,pName:String;pValue:String) of object;
TSetGPUConstIntValue=procedure(GPUName,pName:String;pValue:integer) of object;
TStartMain=procedure(e:TEventStatus) of object;
TShowBusy=procedure(e:TEventStatus) of object;
THideBusy=procedure of object;
TProcessMessages=procedure of object;
TUserSystemAsString=function():String of object;
TLoadUserSystemString=procedure(SystemString:String) of object;
TConsoleLog=procedure(txt:String) of object;
TArray2DToString=function(arr:T2DNumArray):String of object;
TGetGPUPixelArray=function(GPUName:String):T3DNumArray of object;
TGetGPUPixelArrayAsString=function(GPUName:String):String of object;
TGetGPUStageArray=function(GPUName:String):T3DNumArray of object;
TGetGPUStageArrayAsString=function(GPUName:String):String of object;
TGetGPUInitStageArray=function(GPUName:String):T3DNumArray of object;
TDebugStart=procedure of object;
TRunPython=procedure(str:String) of object;
TSetImageSource=procedure(nm,str:String) of object;
TWobbleCEF=procedure(nm:String) of object;
TPyodideLoadPackage=procedure(nm:String) of object;
TPyodidePackageLoaded=function(nm:String):Boolean of object;
//TDSFetchRow=function(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean of object;
//TDSAppendRow=function(e:TEventStatus;DSName:String;recObject:TObject):Boolean of object;
//TDSDeleteRow=function(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean of object;
//TDSDeleteAllRows=function(e:TEventStatus;DSName:String):Boolean of object;
{$endif}

{$ifdef JScript}
var
ShowMessage:Tshowmessage;
ShowXForm:Tshowxform;
CloseXForm:Tclosexform;
Confirm:Tconfirm;
Prompt:Tprompt;
SetPropertyValue:Tsetpropertyvalue;
SetPropertyValueIndexed:Tsetpropertyvalueindexed;
GetPropertyValue:Tgetpropertyvalue;
CopyToClip:TCopyToClip;
CopyFromClip:TCopyFromClip;
LaunchHTMLPage:TLaunchHTMLPage;
LoadTableFromExcelCopy:TLoadTableFromExcelCopy;
GetTableDataForExcel:TGetTableDataForExcel;
LoadTableFromNumArray:TLoadTableFromNumArray;
Load3DTableFromNumArray:TLoad3DTableFromNumArray;
LoadTableFromStringArray:TLoadTableFromStringArray;
GetTableDataArray:TGetTableDataArray;
Get3DTableNumArray:TGet3DTableNumArray;
DoEvent:TDoEvent;
MoveComponent:TMoveComponent;
CopyComponent:TCopyComponent;
DeleteComponent:TDeleteComponent;
DeleteSelectedTreeNode:TDeleteSelectedTreeNode;
GetGPUParamNumValue:TGetGPUParamNumValue;
GetGPUParam2DNumValue:TGetGPUParam2DNumValue;
GetGPUConstIntValue:TGetGPUConstIntValue;
SetGPUParamNumValue:TSetGPUParamNumValue;
SetGPUParamNumValueFromStr:TSetGPUParamNumValueFromStr;
SetGPUParam2DNumValue:TSetGPUParam2DNumValue;
SetGPUParam2DNumValueFromStr:TSetGPUParam2DNumValueFromStr;
SetGPUConstIntValue:TSetGPUConstIntValue;
ShowBusy:TShowBusy;
HideBusy:THideBusy;
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
//DSFetchRow:TDSFetchRow;
//DSAppendRow:TDSAppendRow;
//DSDeleteRow:TDSDeleteRow;
//DSDeleteAllRows:TDSDeleteAllRows;

//DSDatasetToString:TDSDatasetToString;

//var EventsNameSpace:String;   //moved into XComponents

type TMethodsClass = class(TObject)
 public
 CurrentEventType:String;
 CurrentNodeId:String;

 procedure mmiSetEventsNameSpace(NameSpace:String);
 procedure mmishowmessage(msg:String);
 procedure mmiShowXForm(XFormID:String; modal:Boolean);
 procedure mmiCloseXForm(XFormID:String);
 procedure mmisetpropertyvalue(nodeName:String;propName:String;newValue:String);
 procedure mmisetpropertyvalueindexed(nodeName:String;propName:String;newValue:TStringArray; x,y:integer);
 function mmigetpropertyvalue(nodeName:String;propName:String):string;
 Function mmiconfirm(TextMessage:string):boolean;
 Function mmiprompt(TextMessage,promptString:string):string;
 procedure mmiCopyToClip(str:String);
 function mmiCopyFromClip(e:TEventStatus):String;
 procedure mmiLaunchHTMLPage(DataString:String);
 procedure mmiLoadTableFromExcelCopy(TableName,CopiedString:String);
 function mmiGetTableDataForExcel(TableName:String):String;
 procedure mmiLoadTableFromNumArray(TableName:String;NumArray:T2DNumArray);
 procedure mmiLoad3DTableFromNumArray(TableName:String;NumArray:T3DNumArray);
 procedure mmiLoadTableFromStringArray(TableName:String;StrArray:T2DStringArray);
 function mmiGetTableDataArray(TableName:String;SkipHeader:Boolean):T2DStringArray;
 function mmiGet3DTableNumArray(TableName:String):T3DNumArray;
 procedure mmiDoEvent(EventType,NodeId,myValue:String);
 procedure mmiMoveComponent(nodeId:string;NewParentId:string);
 procedure mmiCopyComponent(nodeId,NewParentId,NewName:string);
 function mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;
 procedure mmiDeleteSelectedTreeNode(TreeName:string);
 function mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;
 function mmiGetGPUParam2DNumValue(GPUName,pName:String):T2DNumArray;
 function mmiGetGPUConstIntValue(GPUName,pName:String):integer;
 procedure mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);
 procedure mmiSetGPUParamNumValueFromStr(GPUName,pName:String;pValue:String);
 procedure mmiSetGPUParam2DNumValue(GPUName,pName:String;pValue:T2DNumArray);
 procedure mmiSetGPUParam2DNumValueFromStr(GPUName,pName:String;pValue:String);
 procedure mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);
 procedure mmiStartMain(e:TEventStatus);
 procedure mmiShowBusy(e:TEventStatus);
 procedure mmiHideBusy;
 procedure mmiProcessMessages;
 function mmiUserSystemAsString():String;
 procedure mmiLoadUserSystemString(SystemString:String);
 procedure mmiConsoleLog(txt:String);
 function mmiArray2DToString(arr:T2DNumArray):String;
 function mmiGetGPUPixelArray(GPUName:String):T3DNumArray;
 function mmiGetGPUPixelArrayAsString(GPUName:String):String;
 function mmiGetGPUStageArray(GPUName:String):T3DNumArray;
 function mmiGetGPUStageArrayAsString(GPUName:String):String;
 function mmiGetGPUInitStageArray(GPUName:String):T3DNumArray;
 procedure mmiDebugStart;
 procedure mmiRunPython(str:String);
 procedure mmiSetImageSource(nm,str:String);
 procedure mmiPyodideLoadPackage(nm:String);
 function mmiPyodidePackageLoaded(nm:String):Boolean;
 //function mmiDSFetchRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean;
 //function mmiDSAppendRow(e:TEventStatus;DSName:String;recObject:TObject):Boolean;
 //function mmiDSDeleteRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean;
 //function mmiDSDeleteAllRows(e:TEventStatus;DSName:String):Boolean;
 //function mmiDSDatasetToString(e:TEventStatus;DSName:String):Boolean;
end;

type AnsiString=String;

var appmethods : TMethodsClass;

procedure DSReturnToEvent(ReturnEvent:TEventStatus;ProcName:String);

{$endif}


implementation

{$ifdef JScript}
procedure SetInterfaceContext;
begin
  appmethods:=TMethodsClass.Create;
  ShowMessage:=@AppMethods.mmishowmessage;
  showxform:=@appmethods.mmishowxform;
  closexform:=@appmethods.mmiclosexform;
  setpropertyvalue:=@AppMethods.mmisetpropertyvalue;
  setpropertyvalueindexed:=@AppMethods.mmisetpropertyvalueindexed;
  getpropertyvalue:=@AppMethods.mmigetpropertyvalue;
  confirm:=@AppMethods.mmiconfirm;
  prompt:=@AppMethods.mmiprompt;
  copytoclip:=@AppMethods.mmiCopyToClip;
  copyfromclip:=@AppMethods.mmiCopyFromClip;
  LaunchHTMLPage:=@AppMethods.mmiLaunchHTMLPage;
  LoadTableFromExcelCopy:=@AppMethods.mmiLoadTableFromExcelCopy;
  GetTableDataForExcel:=@AppMethods.mmiGetTableDataForExcel;
  LoadTableFromNumArray:=@AppMethods.mmiLoadTableFromNumArray;
  Load3DTableFromNumArray:=@AppMethods.mmiLoad3DTableFromNumArray;
  LoadTableFromStringArray:=@AppMethods.mmiLoadTableFromStringArray;
  GetTableDataArray:=@AppMethods.mmiGetTableDataArray;
  Get3DTableNumArray:=@AppMethods.mmiGet3DTableNumArray;
  doevent:=@appmethods.mmiDoEvent;
  movecomponent:=@appmethods.mmiMoveComponent;
  copycomponent:=@appmethods.mmiCopyComponent;
  deletecomponent:=@appmethods.mmiDeleteComponent;
  deleteselectedtreenode:=@appmethods.mmiDeleteSelectedTreeNode;
  getgpuparamnumvalue:=@appmethods.mmiGetGPUParamNumValue;
  getgpuparam2dnumvalue:=@appmethods.mmiGetGPUParam2DNumValue;
  getgpuconstintvalue:=@appmethods.mmiGetGPUConstIntValue;
  setgpuparamnumvalue:=@appmethods.mmiSetGPUParamNumValue;
  setgpuparamnumvaluefromstr:=@appmethods.mmiSetGPUParamNumValueFromStr;
  setgpuparam2Dnumvalue:=@appmethods.mmiSetGPUParam2DNumValue;
  setgpuparam2Dnumvaluefromstr:=@appmethods.mmiSetGPUParam2DNumValueFromStr;
  setgpuconstintvalue:=@appmethods.mmiSetGPUConstIntValue;
  showbusy:=@appmethods.mmiShowBusy;
  hidebusy:=@appmethods.mmiHideBusy;
  processmessages:=@appmethods.mmiProcessMessages;
  UserSystemAsString:=@appmethods.mmiUserSystemAsString;
  LoadUserSystemString:=@appmethods.mmiLoadUserSystemString;
  ConsoleLog:=@appmethods.mmiConsoleLog;
  Array2DToString:=@appmethods.mmiArray2DToString;
  GetGPUPixelArray:=@appmethods.mmiGetGPUPixelArray;
  GetGPUPixelArrayAsString:=@appmethods.mmiGetGPUPixelArrayAsString;
  GetGPUStageArray:=@appmethods.mmiGetGPUStageArray;
  GetGPUStageArrayAsString:=@appmethods.mmiGetGPUStageArrayAsString;
  GetGPUinitStageArray:=@appmethods.mmiGetGPUInitStageArray;
  DebugStart:=@appmethods.mmiDebugStart;
  RunPython:=@appmethods.mmiRunPython;
  SetImageSource:=@appmethods.mmiSetImageSource;
  PyodideLoadPackage:=@appmethods.mmiPyodideLoadPackage;
  PyodidePackageLoaded:=@appmethods.mmiPyodidePackageLoaded;
  //DSFetchRow:=@appmethods.mmiDSFetchRow;
  //DSAppendRow:=@appmethods.mmiDSAppendRow;
  //DSDeleteRow:=@appmethods.mmiDSDeleteRow;
  //DSDeleteAllRows:=@appmethods.mmiDSDeleteAllRows;
  //DSDatasetToString:=@appmethods.mmiDSDatasetToString;
end;

procedure DSReturnToEvent(ReturnEvent:TEventStatus;ProcName:String);
begin
  if (ReturnEvent<>nil) then
  begin
     ReturnEvent.ClearAsync(ProcName);
     asm
     pas.Events.handleEvent(ReturnEvent,
                 ReturnEvent.EventType,
                 ReturnEvent.NodeId,
                 ReturnEvent.NameSpace,
                 '','');
     end;
  end;

end;

procedure TMethodsClass.mmiSetEventsNameSpace(NameSpace:String);
begin
  asm
  pas.EventsInterface.EventsNameSpace=NameSpace;
  end;
end;
procedure TMethodsClass.mmishowmessage(msg:String);
begin
  asm
  alert(msg);
  end;
end;
procedure TMethodsClass.mmisetpropertyvalue(nodeName:String;propName:String;newValue:String);
begin
  asm
  pas.NodeUtils.EditAttributeValue2(nodeName,pas.EventsInterface.EventsNameSpace,propName,newValue);
  end;
end;
procedure TMethodsClass.mmisetpropertyvalueindexed(nodeName:String;propName:String;newValue:TStringArray; x,y:integer);
begin
  asm
  pas.NodeUtils.EditAttributeValueIndexed(nodeName,pas.EventsInterface.EventsNameSpace,propName,newValue,x,y);


  if (pas.EventsInterface.EventsNameSpace!='') {

     var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.UIRootNode,nodeName,pas.EventsInterface.EventsNameSpace,true);
     if (mynode!=null) {
       if (myNode.NodeType=='TXCompositeIntf') {
          // find the composite container
          var CompositeContainer=pas.XComposite.FindCompositeContainer(myNode);
          // set the equivalent property
          if (CompositeContainer!=null) {
            CompositeContainer.SetAttributeValue(propName,newValue);
          }
       }
     }
  }
  end;
end;
function TMethodsClass.mmigetpropertyvalue(nodeName:String;propName:String):String;
var
  val:String;
begin
  asm
  //var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,nodeName,pas.InterfaceTypes.EventsNameSpace,true);
  var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.UIRootNode,nodeName,pas.EventsInterface.EventsNameSpace,true);
  if (myNode!=null) {
    // special case for actual height/width attributes...
    if ((propName=='ActualHeight')||(propName=='ActualWidth')) {
      //alert('mmigetpropertyvalue resetting h/w');
      pas.XIFrame.ResetHWAttributes(myNode);
    }
    var attr=myNode.GetAttributeAnyCase(propName,false);
    if (attr!=null) {
      val=attr.AttribValue;
      }
      else val='';
    }
  else  {
    val='';
    }
  end;
  result:=val;
end;
Function TMethodsClass.mmiconfirm(TextMessage:string):boolean;
var
  ok:boolean;
begin
  asm
  //ok=pas.StringUtils.confirm(TextMessage);
  ok=pas.XObjectInsp.XIDEConfirm(TextMessage);
  end;
  result:=ok;
end;
Function TMethodsClass.mmiprompt(TextMessage,promptString:string):string;
var
  res:String;
begin
  asm
  //res=pas.StringUtils.prompt(TextMessage,promptString);
  res=pas.XObjectInsp.XIDEPrompt(TextMessage,promptString);
  end;
  result:=res;
end;
procedure TMethodsClass.mmiShowXForm(XFormID:String; modal:Boolean);
begin
  asm
  //alert('calling showxform '+XFormID+' ns='+pas.EventsInterface.EventsNameSpace);
  pas.XForm.ShowXForm(XFormID,modal,pas.EventsInterface.EventsNameSpace);
  end;
end;
procedure TMethodsClass.mmiCloseXForm(XFormID:String);
begin
  asm
  pas.XForm.CloseXForm(XFormID,pas.EventsInterface.EventsNameSpace);
  end;
end;
procedure TMethodsClass.mmiCopyToClip(str:String);
begin
  asm
  pas.NodeUtils.myCopyToClip('String',str);
  end;
end;
function TMethodsClass.mmiCopyFromClip(e:TEventStatus):String;
var
  str:String;
begin
  // CopyFromClip is an async function (required for browser use), so it must be coded in the
  // 'Init' section of an event handler. The result here is a blank string.
  // mygetClipboardData pops up the paste dialog form, where the user must press Ctrl-V.
  // The pasted string is then held in e.ReturnString, which cn be picked up in the
  // 'Main' section of the event handler.
  str:='';
  e:=glbEvent;  ///// in case called from Python, where passing 'e' in and out via function args is tricky...
  asm
    {
      if ((e!=null)&&(e!=undefined ))
      {
        if (e.InitRunning==false) {
          alert('Warning: CopyFromClip must be called from the "Init" section of an event handler');
          }
        e.AsyncProcsRunning.Add('CopyFromClip');
        //alert('mmiCopyFromClip. completion event is '+e.EventType);
        pas.PasteDialogUnit.CompletionEvent=e;
        var str=pas.NodeUtils.mygetClipboardData('String');
      }
      else
        alert('CopyFromClip must be called with parameter "e"');
    }
  end;
  result:=str;   // have to await user pressing ctrl-v to get pasted data
end;
procedure TMethodsClass.mmiLaunchHTMLPage(DataString:String);
begin
      asm
        //alert('open window with name LaunchedHTML');
        var win=window.open("LaunchedHTML");                  // third (blank) parameter makes a new window
        win.document.write(DataString);
      end;
end;

procedure TMethodsClass.mmiLoadTableFromExcelCopy(TableName,CopiedString:String);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TableName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXTable'))
    {
      myNode.LoadTableFromExcelCopy(CopiedString);
    }
  end;
end;
function TMethodsClass.mmiGetTableDataForExcel(TableName:String):String;
var
  dta:String;
begin
  dta:='';
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TableName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXTable'))
    {
      dta = myNode.GetTableDataForExcel;
    }
  end;
  result:=dta;
end;

procedure TMethodsClass.mmiLoadTableFromNumArray(TableName:String;NumArray:T2DNumArray);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TableName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXTable'))
    {
      myNode.LoadTableFromNumArray(NumArray);
    }
  end;
end;

procedure TMethodsClass.mmiLoad3DTableFromNumArray(TableName:String;NumArray:T3DNumArray);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TableName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TX3DTable'))
    {
      myNode.LoadTableFrom3DNumArray(NumArray);
      //myNode.Construct3DTableStringFromArray  ########## use NumArray, or JSON string,  or string array?????????????????
    }
  end;
end;

procedure TMethodsClass.mmiLoadTableFromStringArray(TableName:String;StrArray:T2DStringArray);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TableName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXTable'))
    {
      myNode.LoadTableFromStringArray(StrArray);
    }
  end;
end;

function TMethodsClass.mmiGetTableDataArray(TableName:String;SkipHeader:Boolean):T2DStringArray;
var
  arr:T2DStringArray;
begin
  setlength(arr,0);
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TableName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXTable'))
    {
      arr = myNode.GetCellsAsArray(SkipHeader);
    }
  end;
  result:=arr;
end;
function TMethodsClass.mmiGet3DTableNumArray(TableName:String):T3DNumArray;
var
  arr:T3DNumArray;
begin
  setlength(arr,0);
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TableName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TX3DTable'))
    {
      arr = myNode.Get3DNumArray(0);
      //console.log('mmiGet3DTableNumArray done');
      //console.log(arr);
    }
  end;
  result:=arr;
end;

procedure TMethodsClass.mmiDoEvent(EventType,NodeId,myValue:String);
begin
  // timeout here basically (eg.) because TreeNodeClick opens the tree node on a timeout, and we want to see
  // those changes on screen before continuing.
  asm
    myTimeout(pas.Events.handleEvent,5,'handleEvent',0,null,EventType,NodeId,pas.EventsInterface.EventsNameSpace,myValue);
  end;
end;

procedure TMethodsClass.mmiMoveComponent(nodeId:string;NewParentId:string);
begin
  asm
  pas.XObjectInsp.OIMoveItem(nodeId,pas.EventsInterface.EventsNameSpace,NewParentId);
  end;
end;
procedure TMethodsClass.mmiCopyComponent(nodeId,NewParentId,NewName:string);
begin
  asm
  pas.XObjectInsp.OICopyToNewParent(nodeId,pas.EventsInterface.EventsNameSpace,NewParentId,NewName);
  end;
end;
function TMethodsClass.mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;
var
  Deleted:Boolean;
begin
  asm
  Deleted=pas.XObjectInsp.OIDeleteItem(nodeId,pas.EventsInterface.EventsNameSpace,ShowNotFoundMsg,ShowConfirm);
  end;
  result:=Deleted;
end;
procedure TMethodsClass.mmiDeleteSelectedTreeNode(TreeName:string);
begin
  asm
  var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,TreeName,pas.EventsInterface.EventsNameSpace,true);
  if ((myNode!=null)&&(myNode.NodeType=='TXTree')) {
    myNode.DeleteSelectedNode();
    }
  end;
end;
procedure TMethodsClass.mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      myNode.SetParamNumValue(pName,pValue,true);
    }
  end;
end;
procedure TMethodsClass.mmiSetGPUParamNumValueFromStr(GPUName,pName:String;pValue:String);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      myNode.SetParamNumValueFromStr(pName,pValue,true);
    }
  end;
end;
procedure TMethodsClass.mmiSetGPUParam2DNumValue(GPUName,pName:String;pValue:T2DNumArray);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      myNode.SetParam2DNumValue(pName,pValue,true);
    }
  end;
end;
procedure TMethodsClass.mmiSetGPUParam2DNumValueFromStr(GPUName,pName:String;pValue:String);
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      myNode.SetParam2DNumValueFromStr(pName,pValue,true);
    }
  end;
end;
procedure TMethodsClass.mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);
begin
asm
//alert('mmiSetGPUConstIntValue '+GPUName+' '+pName);
  var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
  if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
    myNode.SetConstIntValue(pName,pValue);
  }
end;
end;
function TMethodsClass.mmiGetGPUConstIntValue(GPUName,pName:String):integer;
var
  cval:integer;
begin
  cval:=0;
  asm
  //alert('mmiSetGPUConstIntValue '+GPUName+' '+pName);
  var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
  if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
    cval=myNode.GetConstIntValue(pName);
  }
  end;
  result:=cval;
end;
function TMethodsClass.mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;
var
  pval:TNumArray;
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pval=myNode.GetParamNumValue(pName);
    }
  end;
  result:=pval;
end;
function TMethodsClass.mmiGetGPUParam2DNumValue(GPUName,pName:String):T2DNumArray;
var
  pval:T2DNumArray;
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pval=myNode.GetParam2DNumValue(pName);
    }
  end;
  result:=pval;
end;

procedure TMethodsClass.mmiStartMain(e:TEventStatus);
begin
  // eg. after the async function in an event 'Init' section is complete, fire off
  // the event handler again to process the 'Main' section.
  asm
    //console.log(this.EventType+' '+this.NodeId);
    //console.log(this);
    var evt = e.EventType;
    var nid = e.NodeId;
    var ns = e.NameSpace;
    var eob = e;
    setTimeout(function(){pas.Events.handleEvent(eob,
                             evt,
                             nid,
                             ns,
                             '','');  }, 100);
  end;
end;
procedure TMethodsClass.mmiShowBusy(e:TEventStatus);
begin
  // ShowBusy is an async function (required for browser use), so it must be coded in the
  // 'Init' section of an event handler.
  asm
    {
      if (e!=null)
      {
        if (e.InitRunning==false) {
          alert('Warning: ShowBusy must be called from the "Init" section of an event handler');
          }
        //console.log('  adding async proc');
        e.AsyncProcsRunning.Add('ShowBusy');
        var ob=document.getElementById('Grey99');
        if (ob==null) {
          //console.log('  creating Grey99');
          pas.HTMLUtils.ShowGreyOverlay('UIRoot','Grey99','Please Wait...');
        }
        else  console.log('  Grey99 already exists');
      }
      else
      {
        alert('ShowBusy must be called with parameter "e"');
      }
    }
  end;
end;
procedure TMethodsClass.mmiHideBusy;
begin
  asm
    pas.HTMLUtils.DeleteGreyOverlay('Grey99');
  end;
end;
procedure TMethodsClass.mmiProcessMessages;
begin
  asm
    //!!!!
  end;
end;

function TMethodsClass.mmiUserSystemAsString():String;
var
  SystemString:String;
begin
  asm
  SystemString=pas.XObjectInsp.BuildSystemString(false);
  end;
  result:=SystemString;
end;

procedure TMethodsClass.mmiLoadUserSystemString(SystemString:String);
begin
  asm
    pas.NodeUtils.StartingUp=false;
    pas.XObjectInsp.DoSystemLoad(SystemString,'');
  end;
end;

procedure TMethodsClass.mmiConsoleLog(txt:String);
begin
  asm
    console.log(txt);
  end;
end;

function TMethodsClass.mmiArray2DToString(arr:T2DNumArray):String;
var
  str:String;
begin
  asm
    str=pas.StringUtils.Num2dArrayToString(arr);
  end;
  result:=str;
end;

function TMethodsClass.mmiGetGPUPixelArray(GPUName:String):T3DNumArray;
var
  pxval:T3DNumArray;
begin
  pxval:=nil;
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pxval=pas.XGPUCanvas.GetOutputArrayValue(GPUName);
    }
  end;
  result:=pxval;
end;

function TMethodsClass.mmiGetGPUPixelArrayAsString(GPUName:String):String;
var
  pxval:String;
begin
  pxval:='';
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pxval=pas.XGPUCanvas.GetOutputArrayString(GPUName);
    }
  end;
  result:=pxval;
end;

function TMethodsClass.mmiGetGPUStageArray(GPUName:String):T3DNumArray;
var
  pxval:T3DNumArray;
begin
  pxval:=nil;
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pxval=pas.XGPUCanvas.GetStageArrayValue(GPUName);
    }
  end;
  result:=pxval;
end;

function TMethodsClass.mmiGetGPUStageArrayAsString(GPUName:String):String;
var
  pxval:String;
begin
  pxval:='';
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pxval=pas.XGPUCanvas.GetStageArrayString(GPUName);
    }
  end;
  result:=pxval;
end;
function TMethodsClass.mmiGetGPUInitStageArray(GPUName:String):T3DNumArray;
var
  pxval:T3DNumArray;
begin
  pxval:=nil;
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.EventsInterface.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pxval=pas.XGPUCanvas.GetInitStageArrayValue(GPUName);
    }
  end;
  result:=pxval;
end;
procedure TMethodsClass.mmiDebugStart;
begin
  asm
    debugger;
  end;
end;
procedure TMethodsClass.mmiRunPython(str:String);
begin
  asm
    pyodide.runPython(str);
  end;
end;
procedure TMethodsClass.mmiSetImageSource(nm,str:String);
begin
  asm
    var ob=document.getElementById(nm+"Contents");
    if (ob!=null) {
      ob.src=str;
    }
  end;
end;

procedure TMethodsClass.mmiPyodideLoadPackage(nm:String);
begin
  asm
    try {
      pyodide.loadPackage(nm).then(() => {
        if (nm in pyodide.loadedPackages) {console.log(nm+" is now available"); }
        else {alert('Pyodide failed to load package '+nm+' please check console for details');}
      }).catch(err => alert(err.message+' in PyodideLoadPackage '+nm));
    } catch(err) { alert(err.message+'  in PyodideLoadPackage '+nm);
           }
  end;
end;
function TMethodsClass.mmiPyodidePackageLoaded(nm:String):Boolean;
var
  found:Boolean;
begin
  found:=false;
  asm
    if (nm in pyodide.loadedPackages) { found=true; }
  end;
  result:=found;
end;

begin
  SetInterfaceContext;
  {$endif}
end.

