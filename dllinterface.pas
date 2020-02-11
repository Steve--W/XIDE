(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit DllInterface;
// Included in the uses list in XIDEMain, to provide interface methods referenced in InterfaceTypesDll.

{$ifndef JScript}
{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, StringUtils, ExtCtrls, Dialogs, Clipbrd, Forms, Controls,
  NodeUtils, LazsUtils, EventsInterface, Events, XForm, XBitMap, XObjectInsp, XGPUCanvas, XTable,
  XComposite, EventLogging,
  MouseAndKeyInput, LCLType, TypInfo;

type
IMyMethodInterface = interface(IInterface)
    procedure mmiSetEventsNameSpace(NameSpace:String);   stdcall;
    procedure mmiShowMessage(msg:String); stdcall;
    procedure mmiShowXForm(XFormID:String; modal:Boolean);  stdcall;
    procedure mmiCloseXForm(XFormID:String);  stdcall;
    procedure mmiSetPropertyValue(nodeName:String;propName:String;newValue:String);  stdcall;
    procedure mmiSetPropertyValueIndexed(nodeName:String;propName:String;newValue:TStringArray; x,y:integer);  stdcall;
    function mmiGetPropertyValue(nodeName:String;propName:String):string;  stdcall;
//    function mmiGetPropertyValueIndexed(nodeName:String;propName:String; x,y,w,h:integer):TstringArray;  stdcall;
    Function mmiconfirm(TextMessage:string):boolean;  stdcall;
    Function mmiprompt(TextMessage,promptString:string):string;  stdcall;
    procedure mmiCopyToClip(str:String);  stdcall;
    function mmiCopyFromClip(e:TEventStatus):String;  stdcall;
    procedure mmiLoadTableFromExcelCopy(TableName,CopiedString:String);  stdcall;
    procedure mmiDoEvent(EventType,NodeId,myValue:String);   stdcall;
    procedure mmiMoveComponent(nodeId:string;NewParentId:string);  stdcall;
    procedure mmiCopyComponent(nodeId,NewParentId,NewName:string);  stdcall;
    function mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true):Boolean;  stdcall;
    function mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;  stdcall;
    function mmiGetGPUConstIntValue(GPUName,pName:String):integer;  stdcall;
    function mmiGetGPUParamImgValue(GPUName,pName:String):TImgArray;  stdcall;
    procedure mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);  stdcall;
    procedure mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);  stdcall;
    procedure mmiSetGPUParamImgValue(GPUName,pName:String;pValue:TImgArray);  stdcall;
    procedure mmiShowBusy;    stdcall;
    procedure mmiHideBusy;   stdcall;
    procedure mmiProcessMessages;   stdcall;
    procedure mmiMovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2:String); stdcall;
    procedure mmiHidePointer; stdcall;
    function mmiUserSystemAsString():String; stdcall;
    procedure mmiLoadUserSystemString(SystemString:String); stdcall;
    procedure mmiConsoleLog(txt:String);  stdcall;
    function mmiArray2DToString(arr:T2DNumArray):String;           stdcall;
    function mmiGetGPUPixelArray(GPUName:String):T3DNumArray;                                stdcall;
    function mmiGetGPUPixelArrayAsString(GPUName:String):String;                             stdcall;
    function mmiGetGPUStageArray(GPUName:String):T3DNumArray;                                stdcall;
    function mmiGetGPUStageArrayAsString(GPUName:String):String;                             stdcall;
end;

type TMyMethodObject = class(TInterfacedObject, IMyMethodInterface)
    public
      procedure mmiSetEventsNameSpace(NameSpace:String);   stdcall;
      procedure mmiShowMessage(msg:String);  stdcall;
      procedure mmiShowXForm(XFormID:String; modal:Boolean);   stdcall;
      procedure mmiCloseXForm(XFormID:String);   stdcall;
      procedure mmiSetPropertyValue(nodeName:String;propName:String;newValue:String);  stdcall;
      procedure mmiSetPropertyValueIndexed(nodeName:String;propName:String;newValue:TStringArray; x,y:integer);  stdcall;
      function mmiGetPropertyValue(nodeName:String;propName:String):string;  stdcall;
//      function mmiGetPropertyValueIndexed(nodeName:String;propName:String; x,y,w,h:integer):TstringArray;  stdcall;
      Function mmiconfirm(Textmessage:string):boolean;   stdcall;
      Function mmiprompt(TextMessage,promptString:string):string;   stdcall;
      procedure mmiCopyToClip(str:String);  stdcall;
      function mmiCopyFromClip(e:TEventStatus):String;    stdcall;
      procedure mmiLoadTableFromExcelCopy(TableName,CopiedString:String);  stdcall;
      procedure mmiDoEvent(EventType,NodeId,myValue:String);   stdcall;
      procedure mmiMoveComponent(nodeId:string;NewParentId:string);  stdcall;
      procedure mmiCopyComponent(nodeId,NewParentId,NewName:string);  stdcall;
      function mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true):Boolean;  stdcall;
      function mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;  stdcall;
      function mmiGetGPUConstIntValue(GPUName,pName:String):integer;  stdcall;
      function mmiGetGPUParamImgValue(GPUName,pName:String):TImgArray;  stdcall;
      procedure mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);  stdcall;
      procedure mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);  stdcall;
      procedure mmiSetGPUParamImgValue(GPUName,pName:String;pValue:TImgArray);  stdcall;
      procedure mmiShowBusy; stdcall;
      procedure mmiHideBusy; stdcall;
      procedure mmiProcessMessages; stdcall;
      procedure mmiMovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2:String); stdcall;
      procedure mmiHidePointer; stdcall;
      function mmiUserSystemAsString():String; stdcall;
      procedure mmiLoadUserSystemString(SystemString:String); stdcall;
      procedure mmiConsoleLog(txt:String);  stdcall;
      function mmiArray2DToString(arr:T2DNumArray):String;         stdcall;
      function mmiGetGPUPixelArray(GPUName:String):T3DNumArray;                                stdcall;
      function mmiGetGPUPixelArrayAsString(GPUName:String):String;                             stdcall;
      function mmiGetGPUStageArray(GPUName:String):T3DNumArray;                                stdcall;
      function mmiGetGPUStageArrayAsString(GPUName:String):String;                             stdcall;
  end;

type TEventTimerTag = class
        public
          e:TEventStatus;
          ProcName:String;
        end;
type
  TTimerEventWrapper = class
    procedure DoEventTimer(Sender:TObject);
  end;
  var mmo : IMyMethodInterface;
  var DllEventTimer:TTimer;
  TimerEventWrapper:TTimerEventWrapper;

implementation

var
EventsNameSpace:String;


procedure TTimerEventWrapper.DoEventTimer(Sender:TObject);
var
  rtnval:PChar;
  myTag:TEventTimerTag;
  i:integer;
begin
  DllEventTimer.Enabled:=false;
  myTag:=TEventTimerTag(DllEventTimer.Tag);

  i:=myTag.e.AsyncProcsRunning.IndexOf(myTag.ProcName);
  if i>-1 then
    myTag.e.AsyncProcsRunning.Delete(i);

  // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
  rtnval:=PChar(myTag.e.ReturnString);
  HandleEvent(myTag.e,myTag.e.EventType,myTag.e.NodeId,myTag.e.NameSpace,rtnval);
end;

procedure TMyMethodObject.mmiSetEventsNameSpace(NameSpace:String);  stdcall;
begin
  EventsNameSpace:=NameSpace;
end;


procedure TMyMethodObject.mmiShowMessage(msg:String);  stdcall;
begin
  showmessage(msg);
end;
procedure TMyMethodObject.mmiShowXForm(XFormID:String; modal:Boolean);   stdcall;
begin
  //showmessage('calling ShowXForm '+XFormID+' ns='+EventsNameSpace);
  XForm.ShowXForm(XFormID,modal,EventsNameSpace);
end;

procedure TMyMethodObject.mmiCloseXForm(XFormID:String);   stdcall;
begin
  XForm.CloseXForm(XFormID,EventsNameSpace);
end;

procedure TMyMethodObject.mmiSetPropertyValue(nodeName:String;propName:String;newValue:String);  stdcall;
var
  nv:PChar;
  myNode:TDataNode;
  SourceAttrib:TNodeAttribute;
begin
  // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
  myNode:=FindDataNodeById(SystemNodetree,nodename,EventsNameSpace,true);
  if myNode<>nil then
  begin
    nv:=PChar(newValue);
    EditAttributevalue(myNode,propName,nv);

    if (myNode.NodeType='TXCompositeIntf')
    or (myNode.NodeType='TXComposite') then
    begin
      CompositePropertyChanged(myNode,propName);
    end;

  end;
end;
procedure TMyMethodObject.mmiSetPropertyValueIndexed(nodeName:String;propName:String;newValue:TStringArray; x,y:integer);  stdcall;
var
  myNode:TDataNode;
  bitmapcomponent:TXBitMap;
begin
  if propName='MapPixelArray' then
  begin
    myNode:=FindDataNodeById(SystemNodetree,nodename,EventsNameSpace,true);
    if mynode<>nil then
    begin
      bitmapcomponent:=TXBitMap(myNode.ScreenObject);
      bitmapComponent.SetMapPixelArraySection(newValue,x,y);    //!!!! newValue is a TStringArray - does this cross the dll/main boundary safely??
    end;
  end
  else
   showmessage('SetPropertyValueIndexed not valid for property '+propName);
end;

function TMyMethodObject.mmiGetPropertyValue(nodeName:String;propName:String):string;  stdcall;
var
 myNode:TDataNode;
 localStr:String;
begin
  myNode:=FindDataNodeById(SystemNodetree,nodename,EventsNameSpace,true);
  if mynode<>nil then
    localStr:=mynode.GetAttributeAnyCase(propName).AttribValue
  else
    localStr:='';
  result:=localStr;
end;
(*
function TMyMethodObject.mmiGetPropertyValueIndexed(nodeName:String;propName:String; x,y,w,h:integer):TstringArray;  stdcall;
// attribute type is expected to be "StringArray".
// attribute value is a string, comma-delimited.
// For efficiency, fetch the value from the underlying component property.
// Return array rows y to y+h, at character positions (x+1) to (x+1)+w.
var
 myNode:TDataNode;
 arr:TstringArray;
// arr0,arr:TstringArray;
// i,j,r:integer;
 bitmapcomponent:TXBitMap;
begin
 if propName='MapPixelArray' then
 begin
//   setlength(arr0,0);
//   setlength(arr,0);
   myNode:=FindDataNodeById(SystemNodetree,nodename,EventsNameSpace,true);
   if mynode<>nil then
   begin
     bitmapcomponent:=TXBitMap(myNode.ScreenObject);
     arr:=bitmapComponent.GetMapPixelArraySection(x,y,w,h);
   end;
 end
 else
   showmessage('GetPropertyValueIndexed not valid for property '+propName);

 result:=arr;
end;
*)
   Function TMyMethodObject.mmiconfirm(Textmessage:string):boolean;    stdcall;
   begin
     result:=XobjectInsp.XIDEConfirm(Textmessage);
   end;

   Function TMyMethodObject.mmiprompt(TextMessage,promptString:string):string;   stdcall;
   begin
     //result:=StringUtils.prompt(TextMessage,promptString);
     result:=XObjectInsp.XIDEPrompt(TextMessage,promptString);
   end;

   procedure TMyMethodObject.mmiCopyToClip(str:String);   stdcall;
   begin
     myCopyToClip('String',str);
   end;

   function TMyMethodObject.mmiCopyFromClip(e:TEventStatus):String;   stdcall;
   // CopyFromClip is an async function (required for browser use), so it must be coded in the
   // 'Init' section of an event handler. The result here is a blank string.
   // The clipboard string is held in e.ReturnString, which cn be picked up in the
   // 'Main' section of the event handler.
   var
     s:String;
     myTag:TEventTimerTag;
     macroevent:TMacroEvent;
   begin
     if  not EventLogging.MacroEventList.Replaying then
     begin
       //showmessage('CopyFromClip');
       e.AsyncProcsRunning.add('CopyFromClip');
       s :=  Clipboard.AsText;
       e.ReturnString:=s;

       // simulate an async function (as the JS equivalent is async)
       HandleEvent(nil,'MemoPaste','UIRootNode','',s);       // to mirror the browser paste action
       myTag:=TEventTimerTag.Create;
       myTag.ProcName:='CopyFromClip';
       myTag.e:=e;
       DllEventTimer.Tag:=WinSizeDependentInt(myTag);
       DllEventTimer.Enabled:=true;
       result:='';
     end
     else
     begin
       // replaying an event, so can't handle async stuff...
       // Instead, pop the original pasted string off the eventlist.
       macroEvent:=EventLogging.AdvanceEventLog;
       if (macroEvent.EventType<>'MemoPaste')
       then
       begin
         showmessage('oops cannot retrieve original pasted input');
         showmessage('found event '+macroevent.EventType+' '+macroevent.NodeId);
       end
       else
       begin
         e.ReturnString:=macroevent.eventvalue;
         result:='';
       end;
     end;
   end;

   procedure TMyMethodObject.mmiLoadTableFromExcelCopy(TableName,CopiedString:String);   stdcall;
   var
    myNode:TDataNode;
   begin
     myNode:=FindDataNodeById(SystemNodetree,TableName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXTable') then
     begin
       TXTable(myNode.ScreenObject).LoadTableFromExcelCopy(CopiedString);
     end;
   end;

   procedure TMyMethodObject.mmiDoEvent(EventType,NodeId,myValue:String);   stdcall;
   var
     mv:PChar;
   begin
     // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
     mv:=PChar(myValue);
     ReplayEvent(nil,EventType,NodeId,EventsNameSpace,mv);
   end;

   procedure TMyMethodObject.mmiMoveComponent(nodeId:string;NewParentId:string);  stdcall;
   begin
     OIMoveItem(nodeId,EventsNameSpace,NewParentId);
   end;

   procedure TMyMethodObject.mmiCopyComponent(nodeId,NewParentId,NewName:string);  stdcall;
   var
     nm:PChar;
   begin
     // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
     nm:=PChar(NewName);
     OICopyToNewParent(nodeId,EventsNameSpace,NewParentId,nm);
   end;

   function TMyMethodObject.mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true):Boolean;  stdcall;
   var
     Deleted:Boolean;
   begin
     Deleted:=OIDeleteItem(nodeId,EventsNameSpace,ShowNotFoundMsg);
     result:=Deleted;
   end;

   procedure TMyMethodObject.mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);  stdcall;
   var
     myNode:TDataNode;
   begin
    //showmessage('mmiSetGPUParamNumValue GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       TXGPUCanvas(myNode.ScreenObject).SetParamNumValue(pName,pValue,true);
     end;
   end;

   procedure TMyMethodObject.mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);  stdcall;
   var
     myNode:TDataNode;
   begin
    //showmessage('mmiSetGPUConstIntValue GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       TXGPUCanvas(myNode.ScreenObject).SetConstIntValue(pName,pValue);
     end;

   end;

   procedure TMyMethodObject.mmiSetGPUParamImgValue(GPUName,pName:String;pValue:TImgArray);  stdcall;
   var
     myNode:TDataNode;
   begin
    //showmessage('mmiSetGPUParamImgValue GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       TXGPUCanvas(myNode.ScreenObject).SetParamImgValue(pName,pValue,true);
     end;

   end;

   function TMyMethodObject.mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;  stdcall;
   var
     myNode:TDataNode;
   begin
     result:=nil;
    //showmessage('mmiSetGPUParamNumValue GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       result:=TXGPUCanvas(myNode.ScreenObject).GetParamNumValue(pName);
     end;
   end;

   function TMyMethodObject.mmiGetGPUConstIntValue(GPUName,pName:String):integer;  stdcall;
   var
     myNode:TDataNode;
   begin
     result:=0;
    //showmessage('mmiSetGPUConstIntValue GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       result:=TXGPUCanvas(myNode.ScreenObject).GetConstIntValue(pName);
     end;
   end;

   function TMyMethodObject.mmiGetGPUParamImgValue(GPUName,pName:String):TImgArray;  stdcall;
   var
     myNode:TDataNode;
   begin
     result:=nil;
    //showmessage('mmiSetGPUParamImgValue GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       result:=TXGPUCanvas(myNode.ScreenObject).GetParamImgValue(pName);
     end;
   end;
   procedure TMyMethodObject.mmiShowBusy; stdcall;
   begin
     Screen.Cursor := crHourglass;
   end;
   procedure TMyMethodObject.mmiHideBusy; stdcall;
   begin
     Screen.Cursor := crDefault;
   end;
   procedure TMyMethodObject.mmiProcessMessages; stdcall;
   begin
     Application.ProcessMessages;
   end;

   procedure TMyMethodObject.mmiMovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2:String); stdcall;
   begin
     MovePointer(EventsNameSpace,NodeName1,NodeName2,Sub1,Sub2,false);
   end;

   procedure TMyMethodObject.mmiHidePointer; stdcall;
   begin
     // not relevant in the Lazarus/desktop environment
   end;

   function TMyMethodObject.mmiUserSystemAsString():String; stdcall;
   var
     SystemString:String;
   begin
     SystemString:=BuildSystemString(false);
     result:=SystemString;
   end;

   procedure TMyMethodObject.mmiLoadUserSystemString(SystemString:String); stdcall;
   begin
     // !! Have to switch system to design mode first
     // (because there are on-going structures and data in run mode that would result in data corruption
     //  eg. the current events dll, built from user code in the system,
     //  and data for sourced attributes)
     if not DesignMode then
       showmessage('Cannot load system string - switch to Design Mode first')
     else
     begin
       DoSystemLoad(SystemString,'');
     end;
   end;

   procedure TMyMethodObject.mmiConsoleLog(txt:String); stdcall;
   var
     oldtxt:String;
   begin
     //showmessage(txt);
     oldtxt:=mmiGetPropertyValue('XMemo1','ItemValue');
     mmiSetPropertyValue('XMemo1','ItemValue',oldtxt+LineEnding+txt);
     //showmessage(txt);
   end;

   function TMyMethodObject.mmiArray2DToString(arr:T2DNumArray):String;    stdcall;
   var
     str:string;
   begin
     str:=StringUtils.Num2DArrayToString(arr);
     result:=str;
   end;

   function TMyMethodObject.mmiGetGPUPixelArray(GPUName:String):T3DNumArray;                                stdcall;
   var
     myNode:TDataNode;
   begin
     result:=nil;
    //showmessage('mmiGetGPUPixelArray GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       {$ifdef Chromium}
       result:=JsonStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUOutputString);
       {$else}
       {$endif}
     end;
   end;

   function TMyMethodObject.mmiGetGPUPixelArrayAsString(GPUName:String):String;                             stdcall;
   var
     myNode:TDataNode;
   begin
     result:='';
    //showmessage('mmiGetGPUPixelArray GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       {$ifdef Chromium}
       result:=TXGPUCanvas(myNode.ScreenObject).GPUOutputString;
       {$else}
       {$endif}
     end;
   end;

   function TMyMethodObject.mmiGetGPUStageArray(GPUName:String):T3DNumArray;                                stdcall;
   var
     myNode:TDataNode;
   begin
     result:=nil;
    //showmessage('mmiGetGPUStageArray GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       {$ifdef Chromium}
       result:=JsonStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUStageString);
       {$else}
       {$endif}
     end;
   end;

   function TMyMethodObject.mmiGetGPUStageArrayAsString(GPUName:String):String;                             stdcall;
   var
     myNode:TDataNode;
   begin
     result:='';
    //showmessage('mmiGetGPUStageArray GPUName='+GPUName);
     myNode:=FindDataNodeById(SystemNodetree,GPUName,EventsNameSpace,true);
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       {$ifdef Chromium}
       result:=TXGPUCanvas(myNode.ScreenObject).GPUStageString;
       {$else}
       {$endif}
     end;
   end;
begin
    EventsNameSpace:='';
    mmo := TMyMethodObject.Create();
    Events.mmi:=IInterface(mmo);

    TimerEventWrapper := TTimerEventWrapper.Create;
    DllEventTimer:=TTimer.Create(nil);
    DllEventTimer.Interval:=10;
    DllEventTimer.OnTimer:=@TimerEventWrapper.DoEventTimer;
    DllEventTimer.Enabled:=false;
{$endif}
end.

