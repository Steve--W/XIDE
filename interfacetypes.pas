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
   for use with the dynamically created dll, used for executing user event code (also see unit CompileUserCode)
 *)
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, EventsInterface;

type
Tshowmessage=procedure(msg:String) of object;
Tsetpropertyvalue=procedure(nodeName:String;propName:String;newValue:String) of object;
TsetpropertyvalueIndexed=procedure(nodeName:String;propName:String;newValue:TStringArray;x,y:integer) of object;
Tconfirm=function(TextMessage:string):boolean of object;
Tgetpropertyvalue=function(nodeName:String;propName:String):string  of object;
//TgetpropertyvalueIndexed=function(nodeName:String;propName:String; x,y,w,h:integer):TStringArray  of object;
Tprompt=function(TextMessage,promptString:string):string of object;
Tshowxform=procedure(XFormID:String; modal:Boolean) of object;
Tclosexform=procedure(XFormID:String) of object;
TCopyToClip=procedure(str:String) of object;
TCopyFromClip=function(e:TEventStatus):String of object;
TDoEvent=procedure(EventType,NodeId,myValue:String) of object;
TMoveComponent=procedure(nodeId:string;NewParentId:string) of object;
TCopyComponent=procedure(nodeId,NewParentId,NewName:string) of object;
TDeleteComponent=function(nodeId:string;ShowNotFoundMsg:Boolean=true):Boolean of object;
TGetGPUParamNumValue=function(GPUName,pName:String):TNumArray of object;
TGetGPUConstIntValue=function(GPUName,pName:String):integer of object;
TGetGPUParamImgValue=function(GPUName,pName:String):TImgArray of object;
TSetGPUParamNumValue=procedure(GPUName,pName:String;pValue:TNumArray) of object;
TSetGPUConstIntValue=procedure(GPUName,pName:String;pValue:integer) of object;
TSetGPUParamImgValue=procedure(GPUName,pName:String;pValue:TImgArray) of object;
TShowBusy=procedure of object;
THideBusy=procedure of object;
TProcessMessages=procedure of object;
TMovePointerBetweenComponents=procedure(NodeName1,NodeName2,Sub1,Sub2:String) of object;
THidePointer=procedure of object;
TUserSystemAsString=function():String of object;
TLoadUserSystemString=procedure(SystemString:String) of object;
TConsoleLog=procedure(txt:String) of object;

{$ifdef JScript}
var
showmessage:Tshowmessage;
showxform:Tshowxform;
closexform:Tclosexform;
confirm:Tconfirm;
prompt:Tprompt;
setpropertyvalue:Tsetpropertyvalue;
setpropertyvalueindexed:Tsetpropertyvalueindexed;
getpropertyvalue:Tgetpropertyvalue;
//getpropertyvalueindexed:Tgetpropertyvalueindexed;
copytoclip:TCopyToClip;
copyfromclip:TCopyFromClip;
doevent:TDoEvent;
movecomponent:TMoveComponent;
copycomponent:TCopyComponent;
deletecomponent:TDeleteComponent;
getgpuparamnumvalue:TGetGPUParamNumValue;
getgpuconstintvalue:TGetGPUConstIntValue;
getgpuparamimgvalue:TGetGPUParamImgValue;
setgpuparamnumvalue:TSetGPUParamNumValue;
setgpuconstintvalue:TSetGPUConstIntValue;
setgpuparamimgvalue:TSetGPUParamImgValue;
showbusy:TShowBusy;
hidebusy:THideBusy;
processmessages:TProcessMessages;
movepointerbetweencomponents:TMovePointerBetweenComponents;
hidepointer:THidePointer;
UserSystemAsString:TUserSystemAsString;
LoadUserSystemString:TLoadUserSystemString;
ConsoleLog:TConsoleLog;


var EventsNameSpace:String;

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
// function mmigetpropertyvalueindexed(nodeName:String;propName:String; x,y,w,h:integer):TstringArray;
 Function mmiconfirm(TextMessage:string):boolean;
 Function mmiprompt(TextMessage,promptString:string):string;
 procedure mmiCopyToClip(str:String);
 function mmiCopyFromClip(e:TEventStatus):String;
 procedure mmiDoEvent(EventType,NodeId,myValue:String);
 procedure mmiMoveComponent(nodeId:string;NewParentId:string);
 procedure mmiCopyComponent(nodeId,NewParentId,NewName:string);
 function mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true):Boolean;
 function mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;
 function mmiGetGPUConstIntValue(GPUName,pName:String):integer;
 function mmiGetGPUParamImgValue(GPUName,pName:String):TImgArray;
 procedure mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);
 procedure mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);
 procedure mmiSetGPUParamImgValue(GPUName,pName:String;pValue:TImgArray);
 procedure mmiShowBusy;
 procedure mmiHideBusy;
 procedure mmiProcessMessages;
 procedure mmiMovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2:String);
 procedure mmiHidePointer;
 function mmiUserSystemAsString():String;
 procedure mmiLoadUserSystemString(SystemString:String);
 procedure mmiConsoleLog(txt:String);
end;

type AnsiString=String;

var appmethods : TMethodsClass;


{$endif}


implementation

{$ifdef JScript}
procedure SetInterfaceContext;
begin
  appmethods:=TMethodsClass.Create;
  showmessage:=@AppMethods.mmishowmessage;
  showxform:=@appmethods.mmishowxform;
  closexform:=@appmethods.mmiclosexform;
  setpropertyvalue:=@AppMethods.mmisetpropertyvalue;
  setpropertyvalueindexed:=@AppMethods.mmisetpropertyvalueindexed;
  getpropertyvalue:=@AppMethods.mmigetpropertyvalue;
//  getpropertyvalueindexed:=@AppMethods.mmigetpropertyvalueindexed;
  confirm:=@AppMethods.mmiconfirm;
  prompt:=@AppMethods.mmiprompt;
  copytoclip:=@AppMethods.mmiCopyToClip;
  copyfromclip:=@AppMethods.mmiCopyFromClip;
  doevent:=@appmethods.mmiDoEvent;
  movecomponent:=@appmethods.mmiMoveComponent;
  copycomponent:=@appmethods.mmiCopyComponent;
  deletecomponent:=@appmethods.mmiDeleteComponent;
  getgpuparamnumvalue:=@appmethods.mmiGetGPUParamNumValue;
  getgpuconstintvalue:=@appmethods.mmiGetGPUConstIntValue;
  getgpuparamimgvalue:=@appmethods.mmiGetGPUParamImgValue;
  setgpuparamnumvalue:=@appmethods.mmiSetGPUParamNumValue;
  setgpuconstintvalue:=@appmethods.mmiSetGPUConstIntValue;
  setgpuparamimgvalue:=@appmethods.mmiSetGPUParamImgValue;
  showbusy:=@appmethods.mmiShowBusy;
  hidebusy:=@appmethods.mmiHideBusy;
  processmessages:=@appmethods.mmiProcessMessages;
  movepointerbetweencomponents:=@appmethods.mmimovepointerbetweencomponents;
  hidepointer:=@appmethods.mmihidepointer;
  UserSystemAsString:=@appmethods.mmiUserSystemAsString;
  LoadUserSystemString:=@appmethods.mmiLoadUserSystemString;
  ConsoleLog:=@appmethods.mmiConsoleLog;
end;

procedure TMethodsClass.mmiSetEventsNameSpace(NameSpace:String);
begin
  asm
  pas.InterfaceTypes.EventsNameSpace=NameSpace;
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
  pas.NodeUtils.EditAttributeValue2(nodeName,pas.InterfaceTypes.EventsNameSpace,propName,newValue);
  end;
end;
procedure TMethodsClass.mmisetpropertyvalueindexed(nodeName:String;propName:String;newValue:TStringArray; x,y:integer);
begin
  asm
  pas.NodeUtils.EditAttributeValueIndexed(nodeName,pas.InterfaceTypes.EventsNameSpace,propName,newValue,x,y);


  if (pas.InterfaceTypes.EventsNameSpace!='') {

     var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,nodeName,pas.InterfaceTypes.EventsNameSpace,true);
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
  var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,nodeName,pas.InterfaceTypes.EventsNameSpace,true);
  if (myNode!=null) {
    // special case for actual height/width attributes...
    if ((propName=='ActualHeight')||(propName=='ActualWidth')) {
      //alert('mmigetpropertyvalue resetting h/w');
      pas.XIFrame.ResetHWAttributes(myNode);
    }

    val=myNode.GetAttributeAnyCase(propName,true).AttribValue;

    }
  else  {
    val='';
    }
  end;
  result:=val;
end;
//function TMethodsClass.mmigetpropertyvalueindexed(nodeName:String;propName:String; x,y,w,h:integer):TstringArray;
//begin
//  //!!!!
//  asm
//  end;
//end;
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
  //alert('calling showxform '+XFormID+' ns='+pas.InterfaceTypes.EventsNameSpace);
  pas.XForm.ShowXForm(XFormID,modal,pas.InterfaceTypes.EventsNameSpace);
  end;
end;
procedure TMethodsClass.mmiCloseXForm(XFormID:String);
begin
  asm
  pas.XForm.CloseXForm(XFormID,pas.InterfaceTypes.EventsNameSpace);
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
  asm
    if (pas.EventLogging.MacroEventList.Replaying==false)
    {
      //alert('CopyFromClip. not replaying');
      if (e!=null)
      {
        e.AsyncProcsRunning.Add('CopyFromClip');
        //alert('mmiCopyFromClip. completion event is '+e.EventType);
        pas.PasteDialogUnit.CompletionEvent=e;
        var str=pas.NodeUtils.mygetClipboardData('String');
      }
      else
        alert('CopyFromClip must be called with parameter "e"');
    }
    else
    {
      // replaying an event, so can't handle async stuff...
      // Instead, pop the original pasted string off the eventlist.
      var macroEvent=pas.EventLogging.AdvanceEventLog();
      if (macroEvent.EventType!='MemoPaste')
      {
        alert('oops cannot retrieve original pasted input');
        alert('found event '+macroEvent.EventType+' '+macroEvent.NodeId);
      }
      else
      {
        str=macroEvent.eventvalue;
        e.ReturnString=str;
        //alert('found MemoPaste.  str='+str);
      }
    }
  end;
  result:=str;   // have to await user pressing ctrl-v to get pasted data
end;

procedure TMethodsClass.mmiDoEvent(EventType,NodeId,myValue:String);
begin
  asm
  if (myValue==undefined) {myValue='';}
  pas.EventLogging.ReplayEvent(null,EventType,NodeId,pas.InterfaceTypes.EventsNameSpace,myValue);

  end;
end;

procedure TMethodsClass.mmiMoveComponent(nodeId:string;NewParentId:string);
begin
  asm
  pas.XObjectInsp.OIMoveItem(nodeId,pas.InterfaceTypes.EventsNameSpace,NewParentId);
  end;
end;
procedure TMethodsClass.mmiCopyComponent(nodeId,NewParentId,NewName:string);
begin
  asm
  pas.XObjectInsp.OICopyToNewParent(nodeId,pas.InterfaceTypes.EventsNameSpace,NewParentId,NewName);
  end;
end;
function TMethodsClass.mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true):Boolean;
var
  Deleted:Boolean;
begin
  asm
  Deleted=pas.XObjectInsp.OIDeleteItem(nodeId,pas.InterfaceTypes.EventsNameSpace,ShowNotFoundMsg);
  end;
  result:=Deleted;
end;
procedure TMethodsClass.mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);
begin
  asm
  //alert('mmiSetGPUParamNumValue '+GPUName+' '+pName);
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.InterfaceTypes.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      //alert('calling SetParamNumValue '+pName+' '+pValue);
      myNode.SetParamNumValue(pName,pValue,true);
    }
  end;
end;
procedure TMethodsClass.mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);
begin
asm
//alert('mmiSetGPUConstIntValue '+GPUName+' '+pName);
  var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.InterfaceTypes.EventsNameSpace,true);
  if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
    //alert('calling SetConstIntValue '+pName+' '+pValue);
    myNode.SetConstIntValue(pName,pValue);
  }
end;
end;
procedure TMethodsClass.mmiSetGPUParamImgValue(GPUName,pName:String;pValue:TImgArray);
begin
  asm
  //alert('mmiSetGPUParamImgValue '+GPUName+' '+pName);
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.InterfaceTypes.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      //alert('calling SetParamImgValue '+pName+' '+pValue);
      myNode.SetParamImgValue(pName,pValue,true);
    }
  end;
end;
function TMethodsClass.mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;
var
  pval:TNumArray;
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.InterfaceTypes.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pval=myNode.GetParamNumValue(pName);
    }
  end;
  result:=pval;
end;
function TMethodsClass.mmiGetGPUConstIntValue(GPUName,pName:String):integer;
var
  pval:integer;
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.InterfaceTypes.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pval=myNode.GetConstIntValue(pName);
    }
  end;
  result:=pval;
end;

function TMethodsClass.mmiGetGPUParamImgValue(GPUName,pName:String):TImgArray;
var
  pval:TImgArray;
begin
  asm
    var myNode=pas.NodeUtils.FindDataNodeById(pas.NodeUtils.SystemNodeTree,GPUName,pas.InterfaceTypes.EventsNameSpace,true);
    if ((myNode!=null)&&(myNode.NodeType=='TXGPUCanvas')) {
      pval=myNode.GetParamImgValue(pName);
    }
  end;
  result:=pval;
end;
procedure TMethodsClass.mmiShowBusy;
begin
  asm
    var ob=document.getElementById('Grey99');
    if (ob==null) {
    pas.HTMLUtils.ShowGreyOverlay('UIRoot','Grey99');
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

procedure TMethodsClass.mmiMovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2:String);      //!!!!namespace???
begin
  asm
    pas.EventLogging.MovePointer(EventsNameSpace,NodeName1,NodeName2,Sub1,Sub2,false);
  end;
end;

procedure TMethodsClass.mmiHidePointer;
begin
  asm
    var ptr = document.getElementById("AutomatedCursor");
    ptr.style.display = "none";
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


begin
  EventsNameSpace:='';
  SetInterfaceContext;
  {$endif}
end.
