(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XThreads;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Types,NodeUtils,StringUtils, Events, EventsInterface,
  {$ifndef JScript}
  Forms, Controls, StdCtrls, LResources, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls,
  LazsUtils, Dynlibs,
    {$if defined ( windows)}
    windows,
    {$endif}
  {$Else}
  HTMLUtils,
  {$endif}
  WrapperPanel;

type

  {$ifndef JScript}
//  TFunctionParameter = function() : string;

  TMyThread = class(TThread)
    ThreadName:string;
    active:boolean;
    myTXThreads:TComponent;
    ReturnString:String;
    procedure Execute; override;
  end;
  {$else}
  TMyThread = class(TObject)
    ThreadName:string;
    active:boolean;
    myTXThreads:TObject;
    ThreadID:String;
    ReturnString:String;
    CurrentWorker:TObject;
    procedure Execute(TheFunc:TObject);
  end;
  {$endif}

  {$ifndef JScript}
  TXThreads = class(TComponent)
  {$Else}
  TXThreads = class(TWrapperPanel)
  {$endif}
  private
    {$ifndef JScript}
    FmyNode:TDataNode;
    FmyControl:TControl;
    {$endif}

  protected
    FBeginThreads:TEventHandler;
    FHandleReturn:TEventHandler;
    FFinaliseThreads:TEventHandler;
    FHandleThread1:TEventHandler;
    FHandleThread2:TEventHandler;
    FHandleThread3:TEventHandler;
    FHandleThread4:TEventHandler;
    {$ifndef JScript}
    function GetName:string;
    procedure SetMyName(AValue:string);
    {$endif}
    function GetActive:Boolean;
    function GetMaxCopiesThread1:integer;
    function GetMaxCopiesThread2:integer;
    function GetMaxCopiesThread3:integer;
    function GetMaxCopiesThread4:integer;
    function GetThreadVarNums:String;

    procedure SetActive(AValue:Boolean);
    procedure SetMaxCopiesThread1(AValue:integer);
    procedure SetMaxCopiesThread2(AValue:integer);
    procedure SetMaxCopiesThread3(AValue:integer);
    procedure SetMaxCopiesThread4(AValue:integer);
    procedure SetThreadVarNums(AValue:String);

  public
    ThreadCount :integer;
    ThreadList:array of TMyThread;
    {$ifndef JScript}
    myEventTypes:TStringList;
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    function CreateWorkerThread(ThreadNameString:string):TMyThread;
    function getThreadName(ThreadID:LongInt):string;
    procedure DoneThread(Sender:TObject);
    constructor Create(TheOwner: TComponent);  override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean);
    procedure CreateSetOfThreads(ThreadSetNum:integer);
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    procedure MessageReturned(ThreadID:String; edata:TObject);
    procedure ErrorReturned(ThreadID:String);
    function CreateWorkerThread(ThreadNameString:string; TheFunc:TObject):TMyThread;
    procedure CreateSetOfThreads(ThreadSetNum:integer; TheFunc:TObject);
    {$endif}
    procedure StartMyThreads;
    procedure SetMyEventTypes;

  published
    {$ifndef JScript}
    property myControl:TControl read FmyControl write FmyControl;
    property Name: String read GetName write SetMyName;
    property myNode:TDataNode read FmyNode write FmyNode;
    {$endif}

    property Active:Boolean read getActive write setActive;
    property MaxCopiesThread1:integer read GetMaxCopiesThread1 write SetMaxCopiesThread1;
    property MaxCopiesThread2:integer read GetMaxCopiesThread2 write SetMaxCopiesThread2;
    property MaxCopiesThread3:integer read GetMaxCopiesThread3 write SetMaxCopiesThread3;
    property MaxCopiesThread4:integer read GetMaxCopiesThread4 write SetMaxCopiesThread4;
    property ThreadVarNums:String read GetThreadVarNums write SetThreadVarNums;

    // Events to be visible in Lazarus IDE
    property BeginThreads:TEventHandler read FBeginThreads write FBeginThreads;
    property HandleReturn:TEventHandler read FHandleReturn write FHandleReturn;
    property FinaliseThreads:TEventHandler read FFinaliseThreads write FFinaliseThreads;
    property HandleThread1: TEventHandler read FHandleThread1 write FHandleThread1;
    property HandleThread2: TEventHandler read FHandleThread2 write FHandleThread2;
    property HandleThread3: TEventHandler read FHandleThread3 write FHandleThread3;
    property HandleThread4: TEventHandler read FHandleThread4 write FHandleThread4;

  end;

//  {$ifndef JScript}
//  procedure Register;
//  {$endif}
  var
    myDefaultAttribs:TDefaultAttributesArray;

implementation

const MyNodeType='TXThreads';

procedure TXThreads.SetMyEventTypes;
begin
  MyEventTypes.Add('BeginThreads');
  MyEventTypes.Add('HandleReturn');
  MyEventTypes.Add('FinaliseThreads');

  MyEventTypes.Add('Thread1');
  MyEventTypes.Add('Thread2');
  MyEventTypes.Add('Thread3');
  MyEventTypes.Add('Thread4');

end;

{$ifndef JScript}
//procedure Register;
//begin
//  {$I Icons/XThreads.lrs}
//  RegisterComponents('XComponents',[TXThreads]);
//
//  RegisterPropertyEditor(TypeInfo(TControl), TXThreads, 'myControl', THiddenPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(Integer), TXThreads, 'Tag', THiddenPropertyEditor);
//end;

function TXThreads.getThreadName(ThreadID:LongInt):string;
var i:integer;
    tempstr:string;
begin
  tempstr:='';
  for  i := 0 to length(ThreadList)-1 do
  begin
    if (ThreadID = ThreadList[i].ThreadID)
    then tempstr:= ThreadList[i].ThreadName;
  end;
  getThreadName:=tempstr;
end;

procedure TMyThread.Execute;
var
    e:TEventStatus;
begin
  e:=TEventStatus.Create(ThreadName,TXThreads(self.myTXThreads).myNode.NodeName);
  HandleEvent(e,ThreadName,TXThreads(self.myTXThreads).myNode.NodeName,TXThreads(self.myTXThreads).myNode.NameSpace,'');
  // pick up any returnstring that may have been set in the user event code
  self.ReturnString:=e.ReturnString;
end;

procedure TXThreads.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
var
  NewNode:TDataNode;
begin
    NewNode:=TDataNode.Create('NV',self.Name,'','TXThreads',false);
    NewNode.ScreenObject:=self;
    NewNode.MyForm:=TForm(TheOwner);
    NewNode.IsDynamic:=IsDynamic;

    self.MyEventTypes:=TStringList.Create;
    self.SetMyEventTypes;

    SetLength(NewNode.myEventHandlers,self.myEventTypes.Count);
    NewNode.myEventTypes:=self.myEventTypes;


    AddChildToParentNode(SystemNodetree,NewNode,-1);

    self.myNode:=NewNode;

    ThreadCount:=0;
    setlength(ThreadList,0);
end;

constructor TXThreads.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXThreads.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

function TXThreads.GetName:string;
var
  myname:string;
begin
  result:=inherited Name;
end;

procedure TXThreads.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if myNode<>nil then
     myNode.NodeName:=AValue;
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXThreads',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

function TXThreads.CreateWorkerThread(ThreadNameString:string):TMyThread;
var NewThread:TMyThread;
begin

  NewThread :=  TMyThread.Create(true);       //CreateSuspended=true
  NewThread.ThreadName:=ThreadNameString;

  NewThread.myTXThreads:=self;
  NewThread.OnTerminate:=@self.DoneThread;
  NewThread.FreeOnTerminate := true;

  NewThread.active := true;

  ThreadCount := ThreadCount + 1;
  setlength(ThreadList,ThreadCount);
  ThreadList[ThreadCount-1 ]:= NewThread;
  NewThread.Start;

  result:=NewThread;
end;

procedure TXThreads.CreateSetOfThreads(ThreadSetNum:integer);
var
  i,max:integer;
begin
  max:=strToInt(myNode.getAttribute('MaxCopiesThread'+intToStr(ThreadSetNum),true).AttribValue);
  for i:=0 to max-1 do
  begin
    CreateWorkerThread('Thread'+intToStr(ThreadSetNum));
  end;
end;

procedure TXThreads.DoneThread(Sender:TObject);
var
  AnyThreadsActive :boolean;
  CurrentThreadindex,i:integer;
  TiD:integer;
  ThreadName:string;
  ReturnString: string;
begin
  AnyThreadsActive := false;
  TiD:=TMyThread(Sender).ThreadID;
  ReturnString := TMyThread(Sender).ReturnString;
  ThreadName:=getThreadName(TiD);

  TMyThread(Sender).active:=false;

  for  i := 0 to length(ThreadList)-1 do
  begin
    if (ThreadList[i]<>nil) and (ThreadList[i].active = true) then
      AnyThreadsActive := true;
  end;

  // Run any user-supplied message handler
  HandleEvent('HandleReturn',self.MyNode.NodeName,self.MyNode.NameSpace,ReturnString);

  if (AnyThreadsActive = false)
  then
  begin
    self.Active:=false;
    // Run any user-supplied finalisation code
    HandleEvent('FinaliseThreads',self.MyNode.NodeName,self.MyNode.NameSpace,'');
    {$ifndef JScript}
    Screen.Cursor:=crDefault;
    {$endif}
    ThreadCount:=0;
    setlength(ThreadList,0);
  end;
end;

procedure TXThreads.StartMyThreads;
begin
  if (self.HandleThread1<>nil) or (myNode.HasUserEventCode('Thread1')) then CreateSetOfThreads(1);
  if (self.HandleThread2<>nil) or (myNode.HasUserEventCode('Thread2')) then CreateSetOfThreads(2);
  if (self.HandleThread3<>nil) or (myNode.HasUserEventCode('Thread3')) then CreateSetOfThreads(3);
  if (self.HandleThread4<>nil) or (myNode.HasUserEventCode('Thread4')) then CreateSetOfThreads(4);
end;

{$else}

constructor TXThreads.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeClass:='NV';
  self.NodeType:='TXThreads';
  self.MyForm:=MyForm;

  self.myNode.MyEventTypes:=TStringList.Create;
  self.SetMyEventTypes;
  self.IsContainer:=false;

  ThreadCount:=0;
  setlength(ThreadList,0);
end;

function CreateThreadsWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
begin
//showmessage('Create TXThreads widget');

MyNode.ScreenObject:=MyNode;

RefreshComponentProps(myNode);

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXThreads.Create(MyForm,NodeName,NameSpace));
end;

(*function TXThreads.getThreadName(ThreadID:String):string;
var i:integer;
    tempstr:string;
begin
  tempstr:='';
  for  i := 0 to length(ThreadList)-1 do
  begin
    if (ThreadID = ThreadList[i].ThreadID)
    then tempstr:= ThreadList[i].ThreadName;
  end;
  getThreadName:=tempstr;
end; *)

procedure TXThreads.MessageReturned(ThreadID:String; edata:TObject);
var
  AnyThreadsActive :boolean;
  i:integer;
  msg:String;
begin
  asm
  //  msgid =edata.id;
    msg = edata.msg;
  end;
//    showmessage(ThreadID+' MessageReturned '+msgid+' '+msg);

    AnyThreadsActive := false;

    for  i := 0 to length(ThreadList)-1 do
    begin
      if (ThreadList[i]<>nil) and (ThreadList[i].ThreadID = ThreadID) then
      begin
        ThreadList[i].Active := false;
        ThreadList[i].ReturnString:=msg;
      end;
    end;

    for  i := 0 to length(ThreadList)-1 do
    begin
      if (ThreadList[i]<>nil) and (ThreadList[i].active = true) then
        AnyThreadsActive := true;
    end;

    // Run any user-supplied message handler
    HandleEvent('HandleReturn',self.MyNode.NodeName,self.MyNode.NameSpace,msg);

    if (AnyThreadsActive = false)
    then
    begin
      self.Active:=false;
      // Run any user-supplied finalisation code
      HandleEvent('FinaliseThreads',self.MyNode.NodeName,self.MyNode.NameSpace,'');

      for  i := 0 to length(ThreadList)-1 do
      begin
        asm
          //alert('cleaning up');
          this.ThreadList[i].CurrentWorker.terminate();
          this.ThreadList[i].CurrentWorker = undefined;
        end;
      end;


      ThreadCount:=0;
      setlength(ThreadList,0);
    end;
end;

procedure TXThreads.ErrorReturned(ThreadID:String);
var
  AnyThreadsActive :boolean;
  CurrentThreadindex,i:integer;
begin
    //showmessage(ThreadID+' ErrorReturned ');

    AnyThreadsActive := false;

    for  i := 0 to length(ThreadList)-1 do
    begin
      if (ThreadList[i]<>nil) and (ThreadList[i].ThreadID = ThreadID) then
      begin
        ThreadList[i].Active := false;
        ThreadList[i].ReturnString:='';
      end;
    end;

    for  i := 0 to length(ThreadList)-1 do
    begin
      if (ThreadList[i]<>nil) and (ThreadList[i].active = true) then
        AnyThreadsActive := true;
    end;

    if (AnyThreadsActive = false)
    then
    begin
      self.Active:=false;
      // Run any user-supplied finalisation code
      HandleEvent('FinaliseThreads',self.MyNode.NodeName,self.MyNode.NameSpace,'');

      for  i := 0 to length(ThreadList)-1 do
      begin
        asm
          //alert('cleaning up');
          this.ThreadList[i].CurrentWorker.terminate();
          this.ThreadList[i].CurrentWorker = undefined;
        end;
      end;


      ThreadCount:=0;
      setlength(ThreadList,0);
    end;
end;

(*
------------------------------------------------------------
Thread Code Structure....

var loce=null;       // e is object of type TEventStatus

function EventFunc(e,nodeid,str) {
...user code...
}
function setupThread() {
   // onmessage here will handle messages received into the worker thread from the main thread...
   onmessage = function(event) {
                                 //eg. {'cmd': 'start', 'msg': 'Hi'}
                                 loce=event.data;
                                 if (loce.EventType) {
                                   // Execute user event function...
                                   EventFunc(loce,loce.NodeId,"");
                                   // Notify main thread we are done...
                                   postMessage({"id":loce.EventType , "msg":loce.ReturnString});
                                   }
                               };
}
setupThread();
----------------------------------------------------------
*)



procedure TMyThread.Execute(TheFunc:TObject);
var
  e:TEventStatus;
begin
  e:=TEventStatus.Create(self.ThreadID,TXThreads(self.myTXThreads).NodeName);
  e.NameSpace:=TXThreads(self.myTXThreads).NameSpace;
asm
{
  var msgFun = this.myTXThreads.MessageReturned;
  msgFun = msgFun.bind(this.myTXThreads);

  var errFun = this.myTXThreads.ErrorReturned;
  errFun = errFun.bind(this.myTXThreads);

  var myNodeName = this.myTXThreads.NodeName;
  var doFunc = TheFunc;

  var setupThreadFunc =
  'function setupThread() { \n'
  +'  // onmessage here will handle messages received into the worker thread from the main thread... \n'
  +'  onmessage = function(event) { \n'
  +'          var d=event.data; \n'
  +'          if (d.type="EventStart") { \n'
  +'            loce=d.evs; \n'
  +'            if (loce.EventType) {\n'
  +'              // Execute user event function... \n'
  +'              EventFunc(loce,loce.NodeId,"");  \n'
  +'              // Notify main thread we are done... \n'
  +'              postMessage({"id":loce.EventType  ,"msg":loce.ReturnString}); \n'
  +'            } \n'
  +'        }\n'
  +'    }; \n'
  +'};\n';

  //alert('Execute Thread ID='+this.ThreadID);
   // create a URL containing the inlne JavaScript Code instead of loading from a file
   // this can be shared across multiple threads
   function getInlineJS(EventFn)
   {
       var FuncString = EventFn.toString();
       FuncString=FuncString.replace("function", "function EventFunc")

       FuncString = 'var loce = null; \n'
                    + FuncString +'; '
                    + setupThreadFunc + '; '
                    + 'setupThread(); ';
       alert(FuncString);
       var CodeBlob = new Blob([FuncString], { type: "text/javascript" });
       var returnBlob =  URL.createObjectURL(CodeBlob);
       return returnBlob ;
   };

   var localThreadID = this.ThreadID;
   if(typeof(Worker) !== "undefined")
   {
      if ((this.CurrentWorker==null)||(typeof(this.CurrentWorker) == "undefined"))
      {
         if ((TheFunc!=null)&&(TheFunc!=undefined)) {
         this.CurrentWorker =  new Worker(getInlineJS(TheFunc));

         // handle messages received FROM worker...
         this.CurrentWorker.onmessage = function(event) { msgFun(localThreadID, event.data)};  // event.data has id and msg
         // send message TO worker...
         this.CurrentWorker.postMessage({"Type":"EventStart", "evs":e});

         this.CurrentWorker.onerror = function(event) {alert("Error: "+  event.message + "<-- in thread -->"+localThreadID+"<-- at Line " + event.lineno);
                                                        errFun(localThreadID);
                                                       };
         }
         else {alert('Function for thread '+this.ThreadID+' is undefined');}
      }
      else { alert("Error in TMyThread.Execute - CurrentWorker not null"); };
   }
   else { alert("Sorry, your browser does not support Web Workers..."); };
}
;
end;
end;


function TXThreads.CreateWorkerThread(ThreadNameString:string; TheFunc:TObject):TMyThread;
var NewThread:TMyThread;
begin
  NewThread :=  TMyThread.Create;
  NewThread.ThreadName:=ThreadNameString;
  NewThread.ThreadID:=ThreadNameString;

  NewThread.myTXThreads:=self;

  ThreadCount := ThreadCount + 1;
  setlength(ThreadList,ThreadCount);
  ThreadList[ThreadCount-1 ]:= NewThread;
 // showmessage('CreateWorkerThread ID='+ThreadNameString+' is on ThreadList at index '+inttostr(ThreadCount-1));
 // showmessage('length(ThreadList) is '+inttostr(length(ThreadList)));


  NewThread.Execute(TheFunc);   // this creates a Worker instance

  NewThread.active := true;

  result:=NewThread;
end;
procedure TXThreads.CreateSetOfThreads(ThreadSetNum:integer; TheFunc:TObject);
var
  i,max:integer;
begin
  max:=strToInt(myNode.getAttribute('MaxCopiesThread'+intToStr(ThreadSetNum),true).AttribValue);
  for i:=0 to max-1 do
  begin
    CreateWorkerThread(self.NodeName+'Thread'+intToStr(ThreadSetNum)+'_'+intToStr(i),TheFunc);
  end;
end;
procedure TXThreads.StartMyThreads;
var
  fn:TObject;
begin
  fn:=FindEventFunction(self.NameSpace,self.NodeName,'Thread1',self,false);
  if (fn<>nil) then CreateSetOfThreads(1,fn);
  fn:=FindEventFunction(self.NameSpace,self.NodeName,'Thread2',self,false);
  if (fn<>nil) then CreateSetOfThreads(2,fn);
  fn:=FindEventFunction(self.NameSpace,self.NodeName,'Thread3',self,false);
  if (fn<>nil) then CreateSetOfThreads(3,fn);
  fn:=FindEventFunction(self.NameSpace,self.NodeName,'Thread4',self,false);
  if (fn<>nil) then CreateSetOfThreads(4,fn);
end;
{$endif}


function TXThreads.GetActive:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('Active',true).AttribValue);
end;

function TXThreads.GetMaxCopiesThread1:integer;
begin
  result:=StrToInt(MyNode.getAttribute('MaxCopiesThread1',true).AttribValue);
end;

function TXThreads.GetMaxCopiesThread2:integer;
begin
  result:=StrToInt(MyNode.getAttribute('MaxCopiesThread2',true).AttribValue);
end;

function TXThreads.GetMaxCopiesThread3:integer;
begin
  result:=StrToInt(MyNode.getAttribute('MaxCopiesThread3',true).AttribValue);
end;

function TXThreads.GetMaxCopiesThread4:integer;
begin
  result:=StrToInt(MyNode.getAttribute('MaxCopiesThread4',true).AttribValue);
end;

function TXThreads.GetThreadVarNums:String;
begin
  result:=MyNode.getAttribute('ThreadVarNums',true).AttribValue;
end;


procedure TXThreads.SetActive(AValue:Boolean);
begin
  myNode.SetAttributeValue('Active',myBoolToStr(AValue),'Boolean');
  if (StartingUp=false) and (AValue=true) then
  begin
    showmessage('calling event beginthreads');
    HandleEvent('BeginThreads',self.myNode.NodeName,self.myNode.NameSpace,'');  // do any user-initialisation
    showmessage('calling startmythreads');
    self.StartMyThreads;
  end;
end;

procedure TXThreads.SetMaxCopiesThread1(AValue:integer);
begin
  myNode.SetAttributeValue('MaxCopiesThread1',IntToStr(AValue),'Integer');
end;

procedure TXThreads.SetMaxCopiesThread2(AValue:integer);
begin
  myNode.SetAttributeValue('MaxCopiesThread2',IntToStr(AValue),'Integer');
end;

procedure TXThreads.SetMaxCopiesThread3(AValue:integer);
begin
  myNode.SetAttributeValue('MaxCopiesThread3',IntToStr(AValue),'Integer');
end;

procedure TXThreads.SetMaxCopiesThread4(AValue:integer);
begin
  myNode.SetAttributeValue('MaxCopiesThread4',IntToStr(AValue),'Integer');
end;

procedure TXThreads.SetThreadVarNums(AValue:String);
begin
  myNode.SetAttributeValue('ThreadVarNums',AValue,'String');
end;

begin
  AddDefaultAttribute(myDefaultAttribs,'Active','Boolean','True','Set Active to start all threads',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxCopiesThread1','Integer','0','Number of repeats for the Thread1 event code',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxCopiesThread2','Integer','0','Number of repeats for the Thread2 event code',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxCopiesThread3','Integer','0','Number of repeats for the Thread3 event code',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxCopiesThread4','Integer','0','Number of repeats for the Thread4 event code',false);
  AddDefaultAttribute(myDefaultAttribs,'ThreadVarNums','String','','List of variable names (float numeric)',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  {$ifndef JScript}
  Classes.RegisterClass(TXThreads);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateThreadsWidget);
  SuppressDesignerProperty(MyNodeType,'Alignment');
  SuppressDesignerProperty(MyNodeType,'IsVisible');
  SuppressDesignerProperty(MyNodeType,'LabelPos');
  SuppressDesignerProperty(MyNodeType,'LabelText');
  SuppressDesignerProperty(MyNodeType,'Hint');
  {$endif}
end.
