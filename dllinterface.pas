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
  NodeUtils, LazsUtils, EventsInterface, Events, XForm, XBitMap, XObjectInsp, XGPUCanvas, XTable, X3DTable,
  XComposite, XIframe, XTree,
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
    Function mmiconfirm(TextMessage:string):boolean;  stdcall;
    Function mmiprompt(TextMessage,promptString:string):string;  stdcall;
    procedure mmiCopyToClip(str:String);  stdcall;
    function mmiCopyFromClip(e:TEventStatus):String;  stdcall;
    procedure mmiLaunchHTMLPage(DataString:String);  stdcall;
    procedure mmiLoadTableFromExcelCopy(TableName,CopiedString:String);  stdcall;
    function mmiGetTableDataForExcel(TableName:String):String;  stdcall;
    procedure mmiLoadTableFromNumArray(TableName:String;NumArray:T2DNumArray);  stdcall;
    procedure mmiLoad3DTableFromNumArray(TableName:String;NumArray:T3DNumArray);  stdcall;
    procedure mmiLoadTableFromStringArray(TableName:String;StrArray:T2DStringArray);  stdcall;
    function mmiGetTableDataArray(TableName:String;SkipHeader:Boolean):T2DStringArray;  stdcall;
    function mmiGet3DTableNumArray(TableName:String):T3DNumArray;  stdcall;
    procedure mmiDoEvent(EventType,NodeId,myValue:String);   stdcall;
    procedure mmiMoveComponent(nodeId:string;NewParentId:string);  stdcall;
    procedure mmiCopyComponent(nodeId,NewParentId,NewName:string);  stdcall;
    function mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;  stdcall;
    procedure mmiDeleteSelectedTreeNode(TreeName:string);   stdcall;
    function mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;  stdcall;
    function mmiGetGPUParam2DNumValue(GPUName,pName:String):T2DNumArray;  stdcall;
    function mmiGetGPUConstIntValue(GPUName,pName:String):integer;  stdcall;
    procedure mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);  stdcall;
    procedure mmiSetGPUParam2DNumValue(GPUName,pName:String;pValue:T2DNumArray);  stdcall;
    procedure mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);  stdcall;
    procedure mmiStartMain(e:TEventStatus);    stdcall;
    procedure mmiShowBusy(e:TEventStatus);    stdcall;
    procedure mmiHideBusy;   stdcall;
    procedure mmiProcessMessages;   stdcall;
    function mmiUserSystemAsString():String; stdcall;
    procedure mmiLoadUserSystemString(SystemString:String); stdcall;
    procedure mmiConsoleLog(txt:String);  stdcall;
    function mmiArray2DToString(arr:T2DNumArray):String;           stdcall;
    function mmiGetGPUPixelArray(GPUName:String):T3DNumArray;                                stdcall;
    function mmiGetGPUPixelArrayAsString(GPUName:String):String;                             stdcall;
    function mmiGetGPUStageArray(GPUName:String):T3DNumArray;                                stdcall;
    function mmiGetGPUStageArrayAsString(GPUName:String):String;                             stdcall;
    function mmiGetGPUInitStageArray(GPUName:String):T3DNumArray;                            stdcall;
    procedure mmiDebugStart; stdcall;
    procedure mmiRunPython(str:String); stdcall;
    procedure mmiSetImageSource(nm,str:String); stdcall;
    procedure mmiWobbleCEF(nm:String);                  stdcall;
    procedure mmiPyodideLoadPackage(nm:String);  stdcall;
    function mmiPyodidePackageLoaded(nm:String):Boolean; stdcall;
    //function mmiDSFetchRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean;  stdcall;
    //function mmiDSAppendRow(e:TEventStatus;DSName:String;recObject:TObject):Boolean; stdcall;
    //function mmiDSDeleteRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean; stdcall;
    //function mmiDSDeleteAllRows(e:TEventStatus;DSName:String):Boolean; stdcall;
//    function mmiDSDatasetToString(e:TEventStatus;dsName:String):Boolean; stdcall;
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
      Function mmiconfirm(Textmessage:string):boolean;   stdcall;
      Function mmiprompt(TextMessage,promptString:string):string;   stdcall;
      procedure mmiCopyToClip(str:String);  stdcall;
      function mmiCopyFromClip(e:TEventStatus):String;    stdcall;
      procedure mmiLaunchHTMLPage(DataString:String);  stdcall;
      procedure mmiLoadTableFromExcelCopy(TableName,CopiedString:String);  stdcall;
      function mmiGetTableDataForExcel(TableName:String):String;  stdcall;
      procedure mmiLoadTableFromNumArray(TableName:String;NumArray:T2DNumArray);  stdcall;
      procedure mmiLoad3DTableFromNumArray(TableName:String;NumArray:T3DNumArray);  stdcall;
      procedure mmiLoadTableFromStringArray(TableName:String;StrArray:T2DStringArray);  stdcall;
      function mmiGetTableDataArray(TableName:String;SkipHeader:Boolean):T2DStringArray;  stdcall;
      function mmiGet3DTableNumArray(TableName:String):T3DNumArray;  stdcall;
      procedure mmiDoEvent(EventType,NodeId,myValue:String);   stdcall;
      procedure mmiMoveComponent(nodeId:string;NewParentId:string);  stdcall;
      procedure mmiCopyComponent(nodeId,NewParentId,NewName:string);  stdcall;
      function mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;  stdcall;
      procedure mmiDeleteSelectedTreeNode(TreeName:string);   stdcall;
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
      procedure mmiWobbleCEF(nm:String);       stdcall;
      procedure mmiPyodideLoadPackage(nm:String);  stdcall;
      function mmiPyodidePackageLoaded(nm:String):Boolean;  stdcall;
      //function mmiDSFetchRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean;  stdcall;
      //function mmiDSAppendRow(e:TEventStatus;DSName:String;recObject:TObject):Boolean; stdcall;
      //function mmiDSDeleteRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean; stdcall;
      //function mmiDSDeleteAllRows(e:TEventStatus;DSName:String):Boolean; stdcall;
//      function mmiDSDatasetToString(e:TEventStatus;dsName:String):Boolean; stdcall;

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
  //var DllEventTimer:TTimer;
  TimerEventWrapper:TTimerEventWrapper;

procedure DSReturnToEvent(ReturnEvent:TEventStatus;ProcName:String);

implementation
uses PyXUtils;

function CreateEventTimer:TTimer;
var
  newTimer:TTimer;
begin
  newTimer:=TTimer.Create(nil);
  newTimer.Interval:=10;
  newTimer.OnTimer:=@TimerEventWrapper.DoEventTimer;
  newTimer.Enabled:=false;
  result:=newTimer;
end;

procedure TTimerEventWrapper.DoEventTimer(Sender:TObject);
var
  rtnval:PChar;
  myTag:TEventTimerTag;
  ok:boolean;
begin
  TTimer(Sender).Enabled:=false;
  myTag:=TEventTimerTag(TTimer(Sender).Tag);
  TTimer(Sender).Free;

  myTag.e.ClearAsync(myTag.ProcName);

  // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
  rtnval:=PChar(myTag.e.ReturnString);
  ok:=HandleEvent(myTag.e,myTag.e.EventType,myTag.e.NodeId,myTag.e.NameSpace,rtnval);
end;

procedure DSReturnToEvent(ReturnEvent:TEventStatus;ProcName:String);
var
  myTag:TEventTimerTag;
  myTimer:TTimer;
begin
  if (ReturnEvent<>nil) then
  begin
     // simulate an async function (as the JS equivalent is async)
     // to run the 'main' event section
     myTag:=TEventTimerTag.Create;
     myTag.ProcName:=ProcName;
     myTag.e:=ReturnEvent;
     //DllEventTimer.Tag:=WinSizeDependentInt(myTag);
     //DllEventTimer.Enabled:=true;
     myTimer:=CreateEventTimer;
     myTimer.Tag:=WinSizeDependentInt(myTag);
     myTimer.Enabled:=true;
  end;
end;

procedure TMyMethodObject.mmiSetEventsNameSpace(NameSpace:String);  stdcall;
begin
  EventsNameSpace:=PChar(NameSpace);
end;

procedure TMyMethodObject.mmiShowMessage(msg:String);  stdcall;
begin
  showmessage(msg);
end;
procedure TMyMethodObject.mmiShowXForm(XFormID:String; modal:Boolean);   stdcall;
begin
  //showmessage('calling ShowXForm '+XFormID+' ns='+EventsNameSpace);
  XForm.ShowXForm(XFormID,modal,StrPas(EventsNameSpace));
end;

procedure TMyMethodObject.mmiCloseXForm(XFormID:String);   stdcall;
begin
  XForm.XFormClose(XFormID,StrPas(EventsNameSpace));
end;

procedure TMyMethodObject.mmiSetPropertyValue(nodeName:String;propName:String;newValue:String);  stdcall;
var
  nv:PChar;
  myNode:TDataNode;
  SourceAttrib:TNodeAttribute;
begin
  // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
  myNode:=FindDataNodeById(SystemNodeTree,nodename,StrPas(EventsNameSpace),true);    //SystemNodeTree?
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
    myNode:=FindDataNodeById(UIRootNode,nodename,StrPas(EventsNameSpace),true);   //SystemNodeTree?
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
 attr:TNodeAttribute;
begin
  myNode:=FindDataNodeById(UIRootNode,nodename,StrPas(EventsNameSpace),true);      //SystemNodeTree?
  localStr:='';
  if mynode<>nil then
  begin
    attr:=mynode.GetAttributeAnyCase(propName);
    if attr<>nil then
      localStr:=attr.AttribValue
  end;
  result:=localStr;
end;

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
   begin
     begin
       if (e.InitRunning=false) then
         showmessage('Warning: CopyFromClip must be called from the ''Init'' section of an event handler');

       e.AsyncProcsRunning.add('CopyFromClip');
       s :=  Clipboard.AsText;
       e.ReturnString:=s;

       // simulate an async function (as the JS equivalent is async)
       HandleEvent(nil,'MemoPaste',SystemRootName,'',s);       // to mirror the browser paste action
       DSReturnToEvent(e,'CopyFromClip');
       result:='';
     end;
   end;
(*
   function KeyValuesToVarArray(DSName,DSKeyValues:String;var keynames:String;var keys:TVarArray):Boolean;
   var
     keyvalues:TStringList;
     keynodes:TNodesArray;
     att:String;
     ok:Boolean;
     i:integer;
   begin
     ok:=true;
     setlength(keys,0);
     // DSKeyValues is a string delimited by ';' - one value per key field
     keyvalues := stringsplit(DSKeyValues,';');
     keynodes := DMGetKeyFields(DSName);
     if keyvalues.count = length(keynodes) then
     begin
       i:=keyvalues.count;
       setlength(keys,i);
       keynames:='';
       for i:=0 to keyvalues.count-1 do
       begin
         if i>0 then keynames:=keynames+';';
         keynames:=keynames+keynodes[i].NodeName;
         att:= keynodes[i].GetAttribute('AttribType',false).AttribType;
         if att = 'Integer' then
           keys[i] := StrToInt(KeyValues[i])
         else if att = 'Float' then
           keys[i] := StrToFloat(KeyValues[i])
         else if att = 'String' then
           keys[i] := KeyValues[i];
       end;
     end
     else
     begin
       ok:=false;
     end;
     result:=ok;
   end;
*)
(*   function TMyMethodObject.mmiDSFetchRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean;  stdcall;
   // DSFetchRow is an async function (required for browser use), so it must be coded in the
   // 'Init' section of an event handler. The result here is a boolean.
   // The fetched data object is held in e.AsyncReturnObject, which cn be picked up in the
   // 'Main' section of the event handler.
   var
     ok:boolean;
     i:integer;
     s,keynames:String;

     keys:TVarArray;
   begin
     ok:=true;
     if (e.InitRunning=false) then
       showmessage('Warning: DSFetchRow must be called from the ''Init'' section of an event handler');
     e.AsyncProcsRunning.add('DSFetchRow');
     ok:= KeyValuesToVarArray(DSName,DSKeyValues,keynames,keys);


     if ok then
     begin
       // if e.ValueObject is nil, can we dynamically create it here???? type===DSName
       //cls:=classes.GetClass('TDataNode');
       //newob:=cls.Create;         // works
       //cls:=classes.getclass(DSName);
       //newob := cls.Create;       // doesn't work

       ok:=DSGetIndexedRecordAsObject(DSName,'DSFetchRow',keynames,keys,e.ValueObject,e);
     end;
     result:=ok;
   end;

   function TMyMethodObject.mmiDSAppendRow(e:TEventStatus;DSName:String;recObject:TObject):Boolean; stdcall;
   var
     ok:boolean;
   begin
     if (e.InitRunning=false) then
       showmessage('Warning: DSAppendRow must be called from the ''Init'' section of an event handler');
     e.AsyncProcsRunning.add('DSAppendRow');

     ok:=DSAppendRecordFromObject(DSName,'DSAppendRow',recObject,e);
     result:=ok;
   end;

   function TMyMethodObject.mmiDSDeleteRow(e:TEventStatus;DSName:String;DSKeyValues:String):Boolean; stdcall;
   var
     ok:boolean;
     keynames:String;
     keys:TVarArray;
   begin
     if (e.InitRunning=false) then
       showmessage('Warning: DSDeleteRow must be called from the ''Init'' section of an event handler');
     e.AsyncProcsRunning.add('DSDeleteRow');

     ok:= KeyValuesToVarArray(DSName,DSKeyValues,keynames,keys);
     if ok then
       ok:=DSDeleteARow(e,DSName,'DSDeleteRow',keynames,keys);
     result:=ok;
   end;

   function TMyMethodObject.mmiDSDeleteAllRows(e:TEventStatus;DSName:String):Boolean; stdcall;
   var
     ok:boolean;
   begin
     if (e.InitRunning=false) then
       showmessage('Warning: DSDeleteAllRows must be called from the ''Init'' section of an event handler');
     e.AsyncProcsRunning.add('DSDeleteAllRows');

     ok:=DSEmptyDataset(e,DSName,'DSDeleteAllRows');
     result:=ok;
   end;
*)
(*   function TMyMethodObject.mmiDSDatasetToString(e:TEventStatus;dsName:String):Boolean; stdcall;
   var
     ok:Boolean;
   begin
     if (e.InitRunning=false) then
       showmessage('Warning: DSDatasetToString must be called from the ''Init'' section of an event handler');
     e.AsyncProcsRunning.add('DSDatasetToString');
     ok:=DSDataToStringAsync(e,dsName);
     result:=ok;
   end;
*)
   procedure TMyMethodObject.mmiLaunchHTMLPage(DataString:String);   stdcall;
     var
       filename:String;
     begin
         filename:=ProjectDirectory+MainUnitName+'LaunchHTML.html';
         LazsUtils.WriteToFile(filename,DataString);
         filename:='file://'+filename;
         // open in the default browser.
         {$if defined ( windows)}
         WinLaunchBrowser(filename);
         {$else}
         OpenDocument(filename);
         {$endif}
     end;

   procedure TMyMethodObject.mmiLoadTableFromExcelCopy(TableName,CopiedString:String);   stdcall;
   var
    myNode:TDataNode;
   begin
     myNode:=FindDataNodeById(UIRootNode,TableName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXTable') then
     begin
       TXTable(myNode.ScreenObject).LoadTableFromExcelCopy(CopiedString);
     end;
   end;

   function TMyMethodObject.mmiGetTableDataForExcel(TableName:String):String;  stdcall;
   var
     myNode:TDataNode;
   begin
     result:='';
     myNode:=FindDataNodeById(UIRootNode,TableName,StrPas(EventsNameSpace),true);     //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXTable') then
     begin
       result:=TXTable(myNode.ScreenObject).GetTableDataForExcel;
     end;
   end;

   procedure TMyMethodObject.mmiLoadTableFromNumArray(TableName:String; NumArray:T2DNumArray);   stdcall;
   var
    myNode:TDataNode;
   begin
     myNode:=FindDataNodeById(UIRootNode,TableName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXTable') then
     begin
       TXTable(myNode.ScreenObject).LoadTableFromNumArray(NumArray);
     end;
   end;

   procedure TMyMethodObject.mmiLoad3DTableFromNumArray(TableName:String; NumArray:T3DNumArray);   stdcall;
   var
    myNode:TDataNode;
   begin
     myNode:=FindDataNodeById(UIRootNode,TableName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TX3DTable') then
     begin
       TX3DTable(myNode.ScreenObject).LoadTableFrom3DNumArray(NumArray);
     end;
   end;

   procedure TMyMethodObject.mmiLoadTableFromStringArray(TableName:String; StrArray:T2DStringArray);   stdcall;
   var
    myNode:TDataNode;
   begin
     myNode:=FindDataNodeById(UIRootNode,TableName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXTable') then
     begin
       TXTable(myNode.ScreenObject).LoadTableFromStringArray(StrArray);
     end;
   end;

   function TMyMethodObject.mmiGetTableDataArray(TableName:String;SkipHeader:Boolean):T2DStringArray;   stdcall;
   var
    myNode:TDataNode;
    arr:T2DStringArray;
   begin
     myNode:=FindDataNodeById(UIRootNode,TableName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXTable') then
     begin
       arr:=TXTable(myNode.ScreenObject).GetCellsAsArray(SkipHeader);
     end;
     result:=arr;
   end;

   function TMyMethodObject.mmiGet3DTableNumArray(TableName:String):T3DNumArray;   stdcall;
   var
    myNode:TDataNode;
    arr:T3DNumArray;
   begin
     myNode:=FindDataNodeById(UIRootNode,TableName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TX3DTable') then
     begin
       arr:=TX3DTable(myNode.ScreenObject).Get3DNumArray(0);
     end;
     result:=arr;
   end;

   procedure TMyMethodObject.mmiDoEvent(EventType,NodeId,myValue:String);   stdcall;
   var
     mv:PChar;
     ok:boolean;
   begin
     // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
     mv:=PChar(myValue);
     ok:=handleEvent(nil,EventType,nodeId,StrPas(EventsNameSpace),mv);
   end;

   procedure TMyMethodObject.mmiMoveComponent(nodeId:string;NewParentId:string);  stdcall;
   begin
     OIMoveItem(nodeId,StrPas(EventsNameSpace),NewParentId);
   end;

   procedure TMyMethodObject.mmiCopyComponent(nodeId,NewParentId,NewName:string);  stdcall;
   var
     nm:PChar;
   begin
     // !! Must not pass Strings from dll to main, if they need to persist (when the dll is unloaded) - use PChar instead.
     nm:=PChar(NewName);
     OICopyToNewParent(nodeId,StrPas(EventsNameSpace),NewParentId,nm);
   end;

   function TMyMethodObject.mmiDeleteComponent(nodeId:string;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;  stdcall;
   var
     Deleted:Boolean;
   begin
     Deleted:=OIDeleteItem(nodeId,StrPas(EventsNameSpace),ShowNotFoundMsg,ShowConfirm);
     result:=Deleted;
   end;

   procedure TMyMethodObject.mmiDeleteSelectedTreeNode(TreeName:string);  stdcall;
   var
     myNode:TDataNode;
   begin
     myNode:=FindDataNodeById(UIRootNode,TreeName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXTree') then
     begin
       TXTree(myNode.ScreenObject).DeleteSelectedNode;
     end;
   end;

   procedure TMyMethodObject.mmiSetGPUParamNumValue(GPUName,pName:String;pValue:TNumArray);  stdcall;
   var
     myNode:TDataNode;
   begin
    //showmessage('mmiSetGPUParamNumValue GPUName='+GPUName);
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       TXGPUCanvas(myNode.ScreenObject).SetParamNumValue(pName,pValue,true);
     end;
   end;

   procedure TMyMethodObject.mmiSetGPUParam2DNumValue(GPUName,pName:String;pValue:T2DNumArray);  stdcall;
   var
     myNode:TDataNode;
   begin
    //showmessage('mmiSetGPUParam2DNumValue GPUName='+GPUName);
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       TXGPUCanvas(myNode.ScreenObject).SetParam2DNumValue(pName,pValue,true);
     end;
   end;

   procedure TMyMethodObject.mmiSetGPUConstIntValue(GPUName,pName:String;pValue:integer);  stdcall;
   var
     myNode:TDataNode;
   begin
    //showmessage('mmiSetGPUConstIntValue GPUName='+GPUName);
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true); //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       TXGPUCanvas(myNode.ScreenObject).SetConstIntValue(pName,pValue);
     end;

   end;
   function TMyMethodObject.mmiGetGPUConstIntValue(GPUName,pName:String):integer;  stdcall;
   var
     myNode:TDataNode;
     cval:integer;
   begin
     cval:=0;
    //showmessage('mmiSetGPUConstIntValue GPUName='+GPUName);
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       cval:=TXGPUCanvas(myNode.ScreenObject).GetConstIntValue(pName);
     end;
     result:=cval;
   end;

   function TMyMethodObject.mmiGetGPUParamNumValue(GPUName,pName:String):TNumArray;  stdcall;
   var
     myNode:TDataNode;
   begin
     result:=nil;
    //showmessage('mmiSetGPUParamNumValue GPUName='+GPUName);
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       result:=TXGPUCanvas(myNode.ScreenObject).GetParamNumValue(pName);
     end;
   end;

   function TMyMethodObject.mmiGetGPUParam2DNumValue(GPUName,pName:String):T2DNumArray;  stdcall;
   var
     myNode:TDataNode;
   begin
     result:=nil;
    //showmessage('mmiSetGPUParamNumValue GPUName='+GPUName);
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       result:=TXGPUCanvas(myNode.ScreenObject).GetParam2DNumValue(pName);
     end;
   end;

   procedure TMyMethodObject.mmiStartMain(e:TEventStatus); stdcall;
   begin
     HandleEvent(e,e.EventType,e.NodeId,e.NameSpace,'');
   end;

   procedure TMyMethodObject.mmiShowBusy(e:TEventStatus); stdcall;
   begin
     if (e.InitRunning=false) then
       showmessage('Warning: ShowBusy should be called from the ''Init'' section of an event handler');

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
   //var
   //  oldtxt:String;
   begin
   //  oldtxt:=mmiGetPropertyValue('XMemo1','ItemValue');
   //  mmiSetPropertyValue('XMemo1','ItemValue',oldtxt+LineEnding+txt);
     ConsoleString:=ConsoleString + txt + LineEnding;
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
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
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
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
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
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);  //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       {$ifdef Chromium}
       if length( TXGPUCanvas(myNode.ScreenObject).GPUStageArray) > 0 then
         result:=TXGPUCanvas(myNode.ScreenObject).GPUStageArray
       else
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
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       {$ifdef Chromium}
       if TXGPUCanvas(myNode.ScreenObject).GPUStageString<>'' then
         result:=TXGPUCanvas(myNode.ScreenObject).GPUStageString
       else
         result:=Num3DArrayToJsonString(TXGPUCanvas(myNode.ScreenObject).GPUStageArray);
       {$else}
       {$endif}
     end;
   end;
   function TMyMethodObject.mmiGetGPUInitStageArray(GPUName:String):T3DNumArray;                                stdcall;
   var
     myNode:TDataNode;
   begin
     result:=nil;
     myNode:=FindDataNodeById(UIRootNode,GPUName,StrPas(EventsNameSpace),true);   //SystemNodeTree?
     if (mynode<>nil) and (myNode.NodeType='TXGPUCanvas') then
     begin
       {$ifdef Chromium}
         result:=JsonStringTo3DNumArray(myNode.GetAttribute('InitStageData',false).AttribValue);
       {$else}
       {$endif}
     end;
   end;

   procedure TMyMethodObject.mmiDebugStart;                             stdcall;
   begin
   end;
   procedure TMyMethodObject.mmiRunPython(str:String);                  stdcall;
   begin
     {$ifdef Python}
     try            // example, on entry to run mode, some events may fire before the python scripts have been run, so function(s) might not exist yet
       if PyScriptsExecuted then
         PyExeString(str);
     except
       on E: Exception do
       begin
         //showmessage('Python error');
         //raise;   ////makes the whole thing fall over ....
       end;
     end;
     {$endif}
   end;
   procedure TMyMethodObject.mmiSetImageSource(nm,str:String);                  stdcall;
   begin
     {$ifdef Python}
     //!!!! do we need this here?  (required on browser, not desktop??)
     // on desktop, image is populated via the 'source' property on TXImage, which wants a file name.
     // ...can this be done using a pdf-formatted string instead? (eg as returned from a python figure)
     {$endif}
   end;
   procedure TMyMethodObject.mmiWobbleCEF(nm:String);                  stdcall;
   var
     fr:TXIFrame;
     myNode:TDataNode;
   begin
     {$ifdef Chromium}
     myNode:=FindDataNodeById(UIRootNode,nm,StrPas(EventsNameSpace),true);     //SystemNodeTree?
     if myNode<>nil then
     begin
       fr:=TXIframe(myNode.ScreenObject);
       fr.Wobble;
     end;
     {$endif}
   end;

   procedure TMyMethodObject.mmiPyodideLoadPackage(nm:String); stdcall;
   begin
     // this method is applicable only in the browser environment with Pyodide.
     // No action here.
   end;
   function TMyMethodObject.mmiPyodidePackageLoaded(nm:String):Boolean; stdcall;
   begin
     // this method is applicable only in the browser environment with Pyodide.
     // Return True here.
     result := true;
   end;

begin
    mmo := TMyMethodObject.Create();
    Events.mmi:=IInterface(mmo);

    TimerEventWrapper := TTimerEventWrapper.Create;
    //DllEventTimer:=TTimer.Create(nil);
    //DllEventTimer.Interval:=10;
    //DllEventTimer.OnTimer:=@TimerEventWrapper.DoEventTimer;
    //DllEventTimer.Enabled:=false;
{$endif}
end.

