(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XIDEComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XGPUCanvas;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame,
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XTabControl, XMemo, XComboBox, EventsInterface,
    WebTranspilerUtils,
  {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf, Dynlibs,
    LCLType, gettext,
    {$ifdef Chromium}
    uCEFChromium, uCEFInterfaces, uCEFTypes, uCEFProcessMessage, uCEFMiscFunctions,
//    cefXUtils,
    {$endif}
  {$else}
    webfilecache, pas2jswebcompiler,
    HTMLUtils,
  {$endif}
    WrapperPanel, Events, XGPUEditor;


type TGPUNumParam = record
  ParamName:String;
  ParamValue:TNumArray;
  end;
type TGPU2DNumParam = record
  ParamName:String;
  ParamValue:T2DNumArray;
  end;
type TGPUNumParams = Array of TGPUNumParam;
type TGPU2DNumParams = Array of TGPU2DNumParam;
type TGPUIntConst = record
  ConstName:String;
  ConstValue:integer;
  end;
type TGPUIntConsts = Array of TGPUIntConst;

type
  TXGPUCanvas = class(TXIFrame)
  private
    { Private declarations }
    ParamNumArray:TGPUNumParams;
    Param2DNumArray:TGPU2DNumParams;
    ConstIntArray:TGPUIntConsts;
//    {$ifndef JScript}
//    fHandleOnNewFrame:TEventHandler;
//    {$endif}

    function GetAnimationCode:string;
    function GetActive:Boolean;
    function GetAnimated:Boolean;
    function GetParamNumList:string;
    function GetParam2DNumList:string;
    function GetConstIntList:string;
    function GetMaxIterations:integer;
    function GetStartIteration:integer;
    function GetNumFrames:integer;
    function GetMaxFramesPerSec:integer;
    function GetNumKernels:integer;
    function GetInitStageData:string;
    function GetKernelXDims:string;      //array of integer eg. [100,200,300]
    function GetKernelYDims:string;
    function GetKernelZDims:string;
    function GetEmulationMode:Boolean;
    function GetEmulationFrame:Integer;

    procedure SetAnimationCode(AValue:string);
    procedure SetActive(AValue:Boolean);
    procedure SetAnimated(AValue:Boolean);
    procedure SetParamNumList(AValue:string);
    procedure SetConstIntList(AValue:string);
    procedure SetParam2DNumList(AValue:string);
    procedure SetMaxIterations(AValue:integer);
    procedure SetStartIteration(AValue:integer);
    procedure SetNumFrames(AValue:integer);
    procedure SetMaxFramesPerSec(AValue:integer);
    procedure SetNumKernels(AValue:integer);
    procedure SetInitStageData(AValue:string);
    procedure SetKernelXDims(AValue:string);
    procedure SetKernelYDims(AValue:string);
    procedure SetKernelZDims(AValue:string);
    procedure SetEmulationMode(AValue:Boolean);
    procedure SetEmulationFrame(AValue:integer);

    procedure SetMyEventTypes;  override;
    procedure SetPropertyDefaults;
    procedure StartMyGPU;
    procedure StopMyGPU(isdestroying:Boolean);
    function CompileGPUToJS(var GPUJSOutput:String):Boolean;
    function GPUJSCode(AnimCode:TStringList):String;
    function GPUJSAnimationFooter:String;
    procedure setupGPUPage;

    {$ifndef JScript}
    procedure DoGPUCanvasConstructor;
    {$endif}

  protected
    { Protected declarations }
  public
    { Public declarations }
    GeneratedHTML:String;
    GeneratedPascalUnit:String;
    GPUStageArray:T3DNumArray;            // output from non-graphical nested kernels
    GPUOutputArray:T3DNumArray;           // output from main graphical kernel
    GPUStageString:String;
    GPUOutputString:String;
    animCounterString:String;
    Dimensions:TDimsArray;
    {$ifndef JScript}
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    destructor Destroy; override;
    procedure ReLoadURL; override;
    {$ifdef Chromium}
    procedure HandleConsoleMessage(Sender: TObject; const browser: ICefBrowser; level: TCefLogSeverity;
                                   const message, source: ustring; line: Integer; out Result: Boolean) ;
    procedure GPUProcessMessageReceived(
      Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; sourceProcess: TCefProcessId;
      const message: ICefProcessMessage; out Result: Boolean);
    {$endif}
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);  override;
    {$endif}
    function FullParamList:String;
    function FullXMLString:String;
    function GetParamNumValue(pName:String):TNumArray;
    function GetParam2DNumValue(pName:String):T2DNumArray;
    function GetConstIntValue(pName:String):integer;
    procedure SetParamNumValue(pName:String;pValue:TNumArray;ForwardToWidget:Boolean);
    procedure SetParam2DNumValue(pName:String;pValue:T2DNumArray;ForwardToWidget:Boolean);
    procedure SetConstIntValue(pName:String;pValue:integer);
    function CompileAndTrimAnimCode:TStringList;
    function FetchAllAnimCode:TAnimCodeArray;
    function BuildKernelList:String;
    function KernelDimsString(KNum:integer):String;
    function BuildPascalAnimationUnit(Compiler:TObject;RunMode:String):Boolean;
    function BuildDimensionsArray(var xdims,ydims,zdims:TStringList):boolean;
    function CheckForKernels(AnimCode:TStringList):Boolean;
    function JSInitialCode:String;
    function JSDeclareParameters:String;
    function JSGetPixelArray:String;
    function JSPostMessageCode:String;
    function JSFinalCode:String;
    function ExecEmulatorFunc(animcounter,NumZPixels,NumYPixels,NumXPixels:integer):TGPUPixelArray;

published
    { Published declarations }

    // Properties defined for this class...
    property Active: Boolean read GetActive write SetActive;
    property Animated: Boolean read GetAnimated write SetAnimated;
    property AnimationCode: String read GetAnimationCode write SetAnimationCode;
    property ParamNumList: String read GetParamNumList write SetParamNumList;
    property Param2DNumList: String read GetParam2DNumList write SetParam2DNumList;
    property ConstIntList: String read GetConstIntList write SetConstIntList;
    property MaxIterations: integer read GetMaxIterations write SetMaxIterations;
    property StartIteration: integer read GetStartIteration write SetStartIteration;
    property NumFrames: integer read GetNumFrames write SetNumFrames;
    property MaxFramesPerSec: integer read GetMaxFramesPerSec write SetMaxFramesPerSec;
    property NumKernels:integer read GetNumKernels write SetNumKernels;
    property InitStageData: String read GetInitStageData write SetInitStageData;
    property KernelXDims: String read GetKernelXDims write SetKernelXDims;
    property KernelYDims: String read GetKernelYDims write SetKernelYDims;
    property KernelZDims: String read GetKernelZDims write SetKernelZDims;
    property EmulationMode: Boolean read GetEmulationMode write SetEmulationMode;
    property EmulationFrame: integer read GetEmulationFrame write SetEmulationFrame;
    function GPUJSCodeForEmulationMode(AnimCode:TStringList):String;

  end;


procedure SetOutputArrayValue(NodeName:String;const AValue,cval:String);
procedure SetStageArrayValue(NodeName:String;const AValue:String);
procedure SetStageArrayValue2(NodeName:String;AValue:T3DNumArray);
procedure SetCounterValue(NodeName:String;const cval:String);
function GetOutputArrayValue(NodeName:String):T3DNumArray;
function GetOutputArrayString(NodeName:String):String;
function GetStageArrayValue(NodeName:String):T3DNumArray;
function GetStageArrayString(NodeName:String):String;
function GetInitStageArrayValue(NodeName:String):T3DNumArray;

var
  gpujs:String;                 // contents of resource file gpu-browser.js

implementation

const MyNodeType='TXGPUCanvas';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXGPUCanvas.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('OnStart');
  // NB. the following 2 events are called from the GPU at points in its execution, but they are
  // not synchronous.  The GPU process does not wait for completion of these events.
  MyEventTypes.Add('OnFirstStageDone');
  MyEventTypes.Add('OnFrameDone');
end;


function TXGPUCanvas.BuildKernelList:String;
var
  OList:String;
  i:integer;
begin
  OList:='["Graphical (Final)"';
  for i:=0 to numKernels-1 do
    OList:=OList+',"Kernel '+inttostr(i+1)+'"';
  OList:=OList+']';
  result:=OList;
end;

function TXGPUCanvas.KernelDimsString(KNum:integer):String;
var
  xdims,ydims,zdims:TStringList;
  str:string;
begin
  xdims:=JSONStringToStringList(self.KernelxDims);
  ydims:=JSONStringToStringList(self.KernelyDims);
  zdims:=JSONStringToStringList(self.KernelzDims);
  if (xdims.count<KNum+1)
  or (ydims.count<KNum+1)
  or (zdims.count<KNum+1) then
    str:='**err**'
  else
  begin
    str:='['+xdims[KNum]+','+ydims[KNum]+','+zdims[KNum]+']';
  end;
  xdims.Free;
  ydims.Free;
  zdims.Free;
  result:=str;
end;

procedure TXGPUCanvas.SetPropertyDefaults;
begin
end;

{$ifndef JScript}

function CreateGPUCanvasWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXGPUCanvas',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXGPUCanvas.DoGPUCanvasConstructor;
begin
  {$ifdef Chromium}
  myChromium.OnConsoleMessage:=@self.HandleConsoleMessage;
  myChromium.OnProcessMessageReceived:=@self.GPUProcessMessageReceived;
  {$endif}

  self.IsContainer:=false;
  self.myNode.NodeType:='TXGPUCanvas';
  SetLength(ParamNumArray,0);
  SetLength(self.GPUOutputArray,0);
  AddDefaultAttribs(self,self.myNode,mydefaultAttribs);

  SetPropertyDefaults;
end;

constructor TXGPUCanvas.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoGPUCanvasConstructor;
end;

constructor TXGPUCanvas.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoGPUCanvasConstructor;
end;

destructor TXGPUCanvas.Destroy;
begin
  if (not (csDesigning in componentState)) then
  begin
    if Active then
    begin
      self.StopMyGPU(true);
    end;
    myControl.Free;
  end;
  inherited Destroy;
end;

procedure TXGPUCanvas.ReLoadURL;
begin
  if (self.SuspendRefresh)
  {$ifndef JScript}
  or (GlobalSuppressFrameDisplay)
  {$endif}
  then
    EXIT;

  if (StartingUp=false)
  and (self.Active) then
    self.StartMyGPU;
end;

{$ifdef Chromium}

procedure TXGPUCanvas.HandleConsoleMessage(Sender: TObject; const browser: ICefBrowser; level: TCefLogSeverity;
                                 const message, source: ustring; line: Integer; out Result: Boolean) ;
var
  NewText:String;
  MType:String;
  TempMsg : ICefProcessMessage;
begin
  // A frame display has ended.
  if (not (csDesigning in componentState))
  and (not StartingUp)
  and (self.myNode<>nil)
  and (self.Active=true)
  and (browser<>nil)
  and (myChromium<>nil)
  and (FoundString(message,self.myNode.NodeName)=1)
  then
  begin
    NewText:=message;
    Delete(NewText,1,length(self.myNode.NodeName));
    MType:=NewText[1];     // might be 'O' or 'S'
    Delete(NewText,1,1);
    if MType = 'O' then
    begin
      // converting the JSON string back to numeric array takes ages so just storing the
      // string for now.  Do the conversion when the array is wanted.
      // Send a cef message to fetch the new value of the frame output array
      // (Use the ArgumentList property if you need to pass some parameters.)
      TempMsg := TCefProcessMessageRef.New('getGPUData');
      TempMsg.ArgumentList.SetString(0,self.myNode.NodeName);
      myChromium.SendProcessMessage(PID_RENDERER, TempMsg);
    end
    else if MType = 'C' then
    begin
      // this one is used between frames, for an animated GPU
      // Only the animation counter is being sent, for efficiency.
      TempMsg := TCefProcessMessageRef.New('getGPUCounter');
      TempMsg.ArgumentList.SetString(0,self.myNode.NodeName);
      myChromium.SendProcessMessage(PID_RENDERER, TempMsg);
    end;
  end;
end;
procedure TXGPUCanvas.GPUProcessMessageReceived(
  Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
var
  oText,sText,acText:String;
begin
  case message.Name of
    'sendGPUarrays':
    begin
      CefDebugLog('TXGPUCanvas.GPUProcessMessageReceived. id='+self.Name);
      oText := message.ArgumentList.GetString(0);
      sText := message.ArgumentList.GetString(1);
      acText := message.ArgumentList.GetString(2);
      // convert the array string to 3d numeric array
     self.GPUOutputString:=oText;
     self.GPUStageString:=sText;
     self.animCounterString:=acText;
     //self.GPUStageArray:=JSONStringTo3DNumArray(sText);    //!!!! takes ages ..... tbd
     setlength(self.GPUStageArray,0);
     Events.CallHandleEventLater('OnFirstStageDone','', self);     // get back to main thread

    end;
    'sendGPUcounter':
    begin
      CefDebugLog('TXGPUCanvas.GPUProcessMessageReceived. id='+self.Name);
      acText := message.ArgumentList.GetString(0);
     self.animCounterString:=acText;
     self.myNode.SetAttributeValue('LastCounterValue',acText);
     Events.CallHandleEventLater('OnFrameDone','', self);     // get back to main thread
    end;
  else
    inherited;
  end;
end;
{$endif}



{$else} //JScript

constructor TXGPUCanvas.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(MyForm,NodeName,NameSpace);
  self.NodeType:='TXGPUCanvas';
  self.IsContainer:=false;
  SetLength(ParamNumArray,0);

  SetNodePropDefaults(self,myDefaultAttribs);
  SetPropertyDefaults;
end;

function CreateGPUCanvasWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewWidget:TXGPUCanvas;
  h,w:integer;
  scr,GPUMessageHandler:String;
begin
  DoCreateFrameWidget(MyNode, ParentNode,ScreenObjectName,position);
  NewWidget:=TXGPUCanvas(myNode);

  asm
    var nm = MyNode.NodeName;
    var ns = MyNode.NameSpace;
    var nd = MyNode;
    function GPUOutputHandler(ev) {
    if ((ev.data.objid!=undefined)&&(ev.data.objid==ns+nm)) {
//      console.log("handle GPU outbound message "+ev.data.objid+"  "+ev.data.mtype);
      if (ev.data.mtype=="GPUReady") {
        let lGPUStageArray=pas.StringUtils.DelChars(nd.GetAttribute('InitStageData',false).AttribValue,'"');
        let StageArrayValue = JSON.parse(lGPUStageArray);
        lGPUStageArray='';
        ob = document.getElementById(ns+nm+"Contents");
        ob.contentWindow.postMessage({"objid":ns+nm, "mtype":"StartTheGPU", "pName":"", "pValue":StageArrayValue},"*")
      }
      else if (ev.data.mtype=="Debug") {
        console.log("Debug:"+ev.data.dets);
      }
      else if (ev.data.mtype=="FrameOutput") {
        try {
          if (ev.data.outputArray!="") {
            pas.XGPUCanvas.SetOutputArrayValue(ev.data.objid,ev.data.outputArray,ev.data.animcount); }
          else {
            pas.XGPUCanvas.SetCounterValue(ev.data.objid,ev.data.animcount); }
            }catch(err){alert(err.message);
        }
      }
      else if (ev.data.mtype=="StageOutput") {
         try {
           pas.XGPUCanvas.SetStageArrayValue2(ev.data.objid,ev.data.stageArray);
           pas.Events.handleEvent(null,'OnFirstStageDone',nm,ns, '');
         }catch(err){alert(err.message);
         }
       }
      }
    }
    var ob=document.getElementById(NameSpace+ScreenObjectName);
    if (ob!=null) {
      window.addEventListener("message", GPUOutputHandler);
    }
  end;


  RefreshComponentProps(myNode);

  // refresh the actual h/w attributes
  h:=NewWidget.ActualHeight;
  w:=NewWidget.ActualWidth;

  setlength(NewWidget.GPUOutputArray,0);

  result:=myNode;
end;


function CreateinterfaceObjGPU(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXGPUCanvas.Create(MyForm,NodeName,NameSpace));
end;


{$endif}
function TXGPUCanvas.GetAnimationCode:string;
begin
  result:=myNode.getAttribute('AnimationCode',true).AttribValue;
end;
function TXGPUCanvas.GetActive:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('Active',true).AttribValue);
end;
function TXGPUCanvas.GetAnimated:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('Animated',true).AttribValue);
end;
function TXGPUCanvas.GetEmulationMode:Boolean;
begin
  result:=myStrToBool(myNode.getAttribute('EmulationMode',true).AttribValue);
end;
function TXGPUCanvas.GetEmulationFrame:integer;
begin
  result:=StrToInt(myNode.getAttribute('EmulationFrame',true).AttribValue);
end;
//function TXGPUCanvas.GetFetchFrameOutput:Boolean;
//begin
//  result:=myStrToBool(myNode.getAttribute('FetchFrameOutput',true).AttribValue);
//end;
function TXGPUCanvas.GetParamNumList:string;
begin
  result:=myNode.getAttribute('ParamNumList',true).AttribValue;
end;
function TXGPUCanvas.GetParam2DNumList:string;
begin
  result:=myNode.getAttribute('Param2DNumList',true).AttribValue;
end;
function TXGPUCanvas.GetConstIntList:string;
begin
  result:=myNode.getAttribute('ConstIntList',true).AttribValue;
end;
function TXGPUCanvas.GetMaxIterations:integer;
begin
  result:=StrToInt(myNode.getAttribute('MaxIterations',true).AttribValue);
end;
function TXGPUCanvas.GetStartIteration:integer;
begin
  result:=StrToInt(myNode.getAttribute('StartIteration',true).AttribValue);
end;
function TXGPUCanvas.GetNumFrames:integer;
begin
  result:=StrToInt(myNode.getAttribute('NumFrames',true).AttribValue);
end;
function TXGPUCanvas.GetMaxFramesPerSec:integer;
begin
  result:=StrToInt(myNode.getAttribute('MaxFramesPerSec',true).AttribValue);
end;
function TXGPUCanvas.GetNumKernels:integer;
begin
  result:=StrToInt(myNode.getAttribute('NumKernels',true).AttribValue);
end;
function TXGPUCanvas.GetInitStageData:string;
begin
  result:=myNode.getAttribute('InitStageData',true).AttribValue;
end;
function TXGPUCanvas.GetKernelXDims:string;
begin
  result:=myNode.getAttribute('KernelXDims',true).AttribValue;
end;
function TXGPUCanvas.GetKernelYDims:string;
begin
  result:=myNode.getAttribute('KernelYDims',true).AttribValue;
end;
function TXGPUCanvas.GetKernelZDims:string;
begin
  result:=myNode.getAttribute('KernelZDims',true).AttribValue;
end;

function TXGPUCanvas.FullParamList:String;
var
  plist:String;
begin
  plist:='';
  if self.ParamNumList<>'' then plist:=','+self.ParamNumList;
  if self.Param2DNumList<>'' then plist:=plist+','+self.Param2DNumList;
  result:=plist;
end;

function TXGPUCanvas.BuildDimensionsArray(var xdims,ydims,zdims:TStringList):boolean;
var
  TempStr:String;
  n:integer;
begin
  result:=false;
  setlength(self.Dimensions,numKernels+1);
  setlength(self.Dimensions[0],3);
  self.Dimensions[0,0]:=self.ActualWidth;
  self.Dimensions[0,1]:=self.ActualHeight;
  self.Dimensions[0,2]:=1;
  TempStr:=self.KernelXDims;
  xdims:=JSONStringToStringList(TempStr);
  TempStr:=self.KernelyDims;
  ydims:=JSONStringToStringList(TempStr);
  TempStr:=self.KernelzDims;
  zdims:=JSONStringToStringList(TempStr);
  // ... some validation tests ...
  if xdims.Count<>numKernels then
  begin
    showmessage('Number of Kernel X Dimensions must match number of kernels (in '+self.myNode.NodeName+')');
    EXIT;
  end;
  if ydims.Count<>numKernels then
  begin
    showmessage('Number of Kernel Y Dimensions must match number of kernels (in '+self.myNode.NodeName+')');
    EXIT;
  end;
  if zdims.Count<>numKernels then
  begin
    showmessage('Number of Kernel Z Dimensions must match number of kernels (in '+self.myNode.NodeName+')');
    EXIT;
  end;
  for n:=1 to numKernels do
  begin
    if not IsStrFloatNum(xdims[n-1]) then
    begin
      showmessage('GPU KernelXDims item '+inttostr(n)+' is not numeric (in '+self.myNode.NodeName+')');
      EXIT;
    end;
    if not IsStrFloatNum(ydims[n-1]) then
    begin
      showmessage('GPU KernelYDims item '+inttostr(n)+' is not numeric (in '+self.myNode.NodeName+')');
      EXIT;
    end;
    if not IsStrFloatNum(zdims[n-1]) then
    begin
      showmessage('GPU KernelZDims item '+inttostr(n)+' is not numeric (in '+self.myNode.NodeName+')');
      EXIT;
    end;
  end;
  // set up the kernel dimensions...
  for n:=1 to numKernels do
  begin
    setlength(self.Dimensions[n],3);            //x,y,z
    self.Dimensions[n,0]:=strtoint(xdims[n-1]);
    self.Dimensions[n,1]:=strtoint(ydims[n-1]);
    self.Dimensions[n,2]:=strtoint(zdims[n-1]);
  end;
  result:=true;
end;

function TXGPUCanvas.CheckForKernels(AnimCode:TStringList):Boolean;
begin
  result:=true;
  if AnimCode.Count<1 then
  begin
    showmessage('Error: Unable to find any animation code block(s)');
    result:=false;
  end;
  if AnimCode.Count<numKernels+1 then
  begin
    showmessage('Error: Unable to find '+inttostr(numKernels+1)+' animation code blocks');
    result:=false;
  end;
end;

function TXGPUCanvas.JSInitialCode:String;
var
  str:String;
begin
  str:=
  //  'document.domain = "/abc"; ' + LineEnding
    'document.title = "'+myNode.NodeName+' '+myNode.NodeType+'"; ' + LineEnding
    +'/*/ ------------------------------------ Initialise the GPU ---------------------------------/*/ ' + LineEnding
    +'const '+self.MyNode.NodeName+'Matrix = new GPU({mode: ''gpu''});   '+LineEnding
    +'const '+self.MyNode.NodeName+' = new GPU({mode: ''gpu''});   '+LineEnding
    +'let running=true; '+LineEnding;
    str:= str + 'let outputArrayString = ''[]'';'+LineEnding;
    str:= str + 'var GPUIntervalRunner;  '  + LineEnding;
  result:=str;
end;

function TXGPUCanvas.JSDeclareParameters:String;
var
  str,vstr:String;
  vn:TNumArray;
  va:T2DNumArray;
  i,j,k:integer;
begin
  str:= '/*/ -------------------------------- Initialise Parameters List -------------------------/*/ ' + LineEnding;
  // Numeric parameters are 1-D arrays of values
  for i:=0 to length(self.ParamNumArray)-1 do
  begin
    vn:=ParamNumArray[i].ParamValue;
    vstr:='[';
    for j:=0 to length(vn)-1 do
    begin
      if j>0 then vstr:=vstr+',';
      vstr:=vstr+floattostr(vn[j]);
    end;
    vstr:=vstr+']';
    str:=str+'let '+ParamNumArray[i].ParamName+' = '+vstr+';' +LineEnding;
  end;
  // Add the 2D arrays of values
  for i:=0 to length(self.Param2DNumArray)-1 do
  begin
    va:=Param2DNumArray[i].ParamValue;
    vstr:='[';
    for j:=0 to length(va)-1 do
    begin
      if j>0 then vstr:=vstr+',';
      vstr:=vstr+'[';
      for k:=0 to length(va[j])-1 do
      begin
        if k>0 then vstr:=vstr+',';
        vstr:=vstr+floattostr(va[j,k]);
      end;
      vstr:=vstr+']';
    end;
    vstr:=vstr+']';
    str:=str+'let '+Param2DNumArray[i].ParamName+' = '+vstr+';' +LineEnding;
  end;
  str:=str+LineEnding;
  result:=str;
end;

function TXGPUCanvas.JSPostMessageCode:String;
var
  str:String;
begin
  str:='function MyStringify(myarr) { ' + LineEnding
  +'    var arrayString = "["; ' + LineEnding
  +'    for (var z=0; z<myarr.length; z++) ' + LineEnding
  +'    {  ' + LineEnding
  +'      if (z>0) { arrayString = arrayString + ","; }' + LineEnding
  +'      arrayString = arrayString + "["; ' + LineEnding
  +'      for (var y=0; y<myarr[z].length; y++) ' + LineEnding
  +'      {   ' + LineEnding
  +'        if (y>0) { arrayString = arrayString + ","; }' + LineEnding
  +'        arrayString = arrayString + "["; ' + LineEnding
  +'        for (var x=0; x<myarr[z][y].length; x++) ' + LineEnding
  +'        {   ' + LineEnding
  +'          if (x>0) { arrayString = arrayString + ","; }' + LineEnding
  +'          arrayString = arrayString + myarr[z][y][x];' + LineEnding
  +'        }   ' + LineEnding
  +'        arrayString = arrayString + "]"; ' + LineEnding
  +'      }   ' + LineEnding
  +'      arrayString = arrayString + "]"; ' + LineEnding
  +'    }   ' + LineEnding
  +'    arrayString = arrayString + "]"; ' + LineEnding
  +'    return(arrayString); ' + LineEnding
  +'}'+ LineEnding;


  str:=str
  +'function PostMessageOutputArray(objid, cval) {'  + LineEnding
  // save the output array in a div element
  +'  var oa = document.getElementById("oarr");' + LineEnding
  +'  if (oa==null) {oa=document.createElement("DIV"); oa.id="oarr"; ' + LineEnding
  +'    oa.style.display="none";' + LineEnding
  +'    document.body.appendChild(oa);}' + LineEnding
  // save the animation counter value in a div element
  +'  var ac = document.getElementById("acdiv");' + LineEnding
  +'  if (ac==null) {ac=document.createElement("DIV"); ac.id="acdiv"; ' + LineEnding
  +'    ac.style.display="none";' + LineEnding
  +'    document.body.appendChild(ac);}' + LineEnding
  +'  if (running) {' + LineEnding
  +'    oa.innerHTML=outputArrayString;' + LineEnding
  +'    ac.innerHTML=cval;' + LineEnding;
  {$ifndef JScript}
  {$ifdef Chromium}
  str:=str
  // cef.  write to console log to trigger a cef event...
  +'    if (cval<='+IntToStr(self.StartIteration)+') {  ' + LineEnding
  +'      // console.log triggers a cef event - see HandleConsoleMessage... ' + LineEnding
  +'      console.log("'+myNode.NodeName+'O ");' + LineEnding
  +'    }' + LineEnding
  +'    else { ' + LineEnding
  +'       console.log("'+myNode.NodeName+'C "); ' + LineEnding
  +'    }'  + LineEnding;
  {$endif}
  {$else}
  str:=str
  +'    if (cval<='+IntToStr(self.StartIteration)+') {  ' + LineEnding
  +'       window.parent.postMessage({"objid":objid,"mtype":"FrameOutput","outputArray":oa.innerHTML,"animcount":ac.innerHTML},"*"); ' + LineEnding
  +'    }' + LineEnding
  +'    else { ' + LineEnding
  +'       window.parent.postMessage({"objid":objid,"mtype":"FrameOutput","outputArray":"","animcount":ac.innerHTML},"*"); ' + LineEnding
  +'    }'  + LineEnding;
  {$endif}
  str:=str+'} }'+LineEnding+LineEnding;

  str:=str
  +'function PostMessageStageArray(objid, cval) {'  + LineEnding
  +'  var sa = document.getElementById("sarr");' + LineEnding
  +'  if (sa==null) {sa=document.createElement("DIV"); sa.id="sarr"; ' + LineEnding
  +'    sa.style.display="none";' + LineEnding
  +'    document.body.appendChild(sa);}' + LineEnding
  +'  if (running) {  ' + LineEnding
  +'    sa.innerHTML=MyStringify(stageArray);' + LineEnding;
  {$ifndef JScript}
  {$ifdef Chromium}
  str:=str
  +'    if (cval<='+IntToStr(self.StartIteration)+') {  ' + LineEnding
  +'      // console.log triggers a cef event - see HandleConsoleMessage... ' + LineEnding
  +'      console.log("'+myNode.NodeName+'S ");' + LineEnding
  +'    }' + LineEnding;
  {$endif}
  {$else}
  if numKernels>0 then
  begin
    str:=str+'    if (cval<='+IntToStr(self.StartIteration)+') {  ' + LineEnding;
    str:=str+'      window.parent.postMessage({"objid":objid,"mtype":"StageOutput","stageArray":stageArray},"*"); ' + LineEnding;
    str:=str+'    }' + LineEnding;
  end;
  {$endif}
  str:=str+'} }'+LineEnding+LineEnding;
  result:=str;
end;

function TXGPUCanvas.JSGetPixelArray:String;
var
  str:string;
begin
  //  called from the FrameDone function.
  //  if self.FetchFrameOutput=true then
      str:=
      'function GetPixelArray(kernel) { ' + LineEnding
      //+'return(kernel.getPixels()); ' + LineEnding          // needs gpujs v2

      +'  const theImage = kernel.canvas;  ' + LineEnding
      +'  if (theImage==null) {console.log("'+self.MyNode.NodeName+'BrowserCanvas is null"); } ' + LineEnding
      +'  else { '  + LineEnding
      //   +'  console.log(''image height = ''+theImage.height); ' + LineEnding
      +'    tmpcanvas = document.createElement(''canvas'');  ' + LineEnding
      +'    tmpcanvas.width=theImage.width;  ' + LineEnding
      +'    tmpcanvas.height=theImage.height;  ' + LineEnding
      +'    var ctx = tmpcanvas.getContext("2d");  ' + LineEnding
      +'    ctx.drawImage(theImage, 0, 0); ' + LineEnding
      +'    if (ctx==null) {console.log("Context is null"); } ' + LineEnding
      +'    else { '  + LineEnding
      +'      var  theImageData = ctx.getImageData(0,0,theImage.width,theImage.height);' + LineEnding
      //   +'    console.log("theImageData="+theImageData.data);' + LineEnding
      //   +'    console.log("height="+theImage.height+" width="+theImage.width+" length="+theImageData.data.length);' + LineEnding
      +'      var arrayString = "["; ' + LineEnding
      +'      for (var h=0; h<theImage.height; h++) ' + LineEnding
      +'      {   ' + LineEnding
      +'        if (h>0) { arrayString = arrayString + ","; }' + LineEnding
      +'        arrayString = arrayString + "["; ' + LineEnding
      +'        for (var w=0; w<theImage.width; w++) ' + LineEnding
      +'        {   ' + LineEnding
      +'        // note the pixel colours cycle around in groups of four ' + LineEnding
      +'          var i=(h*theImage.width*4)+(w*4); ' + LineEnding
      +'          var red = theImageData.data[i];  ' + LineEnding
      +'          var green = theImageData.data[i+1]; ' + LineEnding
      +'          var blue = theImageData.data[i+2];  ' + LineEnding
      +'          var alpha = theImageData.data[i+3]; ' + LineEnding
      +'          if (w>0) { arrayString = arrayString + ","; }' + LineEnding
      +'          arrayString = arrayString + "[" + red + "," + green + "," + blue + "," + alpha + "]";' + LineEnding
      +'        }   ' + LineEnding
      +'        arrayString = arrayString + "]"; ' + LineEnding
      +'      }   ' + LineEnding
      +'      tmpcanvas.remove(); ' + LineEnding
      +'      arrayString = arrayString + "]"; ' + LineEnding
      +'      return(arrayString); ' + LineEnding
      +'    }' + LineEnding
      +'  }' + LineEnding
      +'}'+ LineEnding;
  result:=str;
end;

function TXGPUCanvas.JSFinalCode:String;
var
  str:string;
begin
  str:=
  '  function GetMessage(msg) {'  + LineEnding
  +'    alert("msg="+msg);  '  + LineEnding
  +'}'  + LineEnding ;

  str:=str
  +'  function ClearDown() {'  + LineEnding
  +'    stageArray=[[[0]]];  '  + LineEnding
  +'    outputArrayString="[]";  '  + LineEnding
  +'}'  + LineEnding ;

  str:=str
  +'  function RunCode(theCode) {'  + LineEnding
  +'    eval(theCode);  '  + LineEnding
  +'}'  + LineEnding ;

  {$ifdef JScript}
//    // handle an inbound message of format:{"objid":<id>, "mtype":"SetParam", "pName":<pName>, "pValue":<pValue>}
    str:=str
    +'window.addEventListener("message", function(ev) { '+Lineending
    + '  if (ev.data.objid!=undefined) { '  +LineEnding
//                + '  console.log("handle GPU inbound message "+ev.data.objid+"  "+ev.data.mtype); '+LineEnding
    + '  if (ev.data.mtype=="StartTheGPU") {    '+lineEnding
    + '    try {   '+lineEnding
    + '     stageArray=ev.data.pValue;  '+lineEnding
    + '     ev.data.pValue=[];  '+lineEnding
    + '     StartTheGPU();  '+lineEnding
    + '     }catch(err){alert(err.message);}  '+lineEnding
    + '  } '+LineEnding
    + '  if (ev.data.mtype=="SetNumParam") {    '+lineEnding
//                 + '  console.log(ev.data.pName+"=["+ev.data.pValue.toString()+"];"); '+LineEnding
    + '    try {   '+lineEnding
    + '      eval(ev.data.pName+"=["+ev.data.pValue.toString()+"];");  '+lineEnding
    + '     }catch(err){alert(err.message);}  '+lineEnding
    + '  } '+LineEnding
    + '  if (ev.data.mtype=="execCode") {    '+lineEnding
//                + '  console.log("handle execCode message "+ev.data.code); '+LineEnding
    + '    try {   '+lineEnding
    + '      eval(ev.data.code);  '+lineEnding
    + '     }catch(err){alert(err.message);}  '+lineEnding
    + '  } '+LineEnding
    + '} '+lineEnding
    + '} );'  +LineEnding;
  {$else}
  str:=str+LineEnding;
  {$endif}

  result:=str;
end;

function TXGPUCanvas.GPUJSCode(AnimCode:TStringList):String;
//function TXGPUCanvas.GPUJSCode(AnimCode:TStringList;dims:TDimsArray):String;
var
  str,vstr,plist, KName,tempstr:String;
  i,j,k,d:integer;
  h,w,n:integer;
  xdims,ydims,zdims:TStringList;
  ok:boolean;
begin
  result:='';
  ok := CheckForKernels(AnimCode);
  if not ok then EXIT;

  str:=JSInitialCode;
  str:=str + JSDeclareParameters;
  str:=str + JSPostMessageCode;
  str:=str + JSGetPixelArray;


  plist:=self.FullParamList;
  KName:=self.MyNode.NodeName+'CanvasRenderFn';

  // Build the Dimensions array
  ok:=self.BuildDimensionsArray(xdims,ydims,zdims);
  if not ok then EXIT;


  // Create the required set of non-graphical kernels.
  // All these kernels operate with the same set of parameters.
  for n:=0 to numKernels-1 do
  begin
    str:=str
    +'/*/------------ Create Kernel '+inttostr(n)+' -------/*/ ' + LineEnding
    +'const '+KName+inttostr(n)+' = '+self.MyNode.NodeName+'Matrix.createKernel(function(myArray,AnimationCounterValue'+plist
       +') { ' + LineEnding;
    str:=str + '  var myValue=0.0;' + LineEnding;
    str:=str + AnimCode[n+1];
    str:=str + '  return myValue;' + LineEnding;   // this goes into the relevant x,y,z position in outputArray
    str:= str
    +'},'+LineEnding
    +'{  output: [';
    for d:=0 to length(self.Dimensions[n+1])-1 do
    begin
      if d>0 then str:=str+',';
      str:= str + inttostr(self.Dimensions[n+1,d]);
    end;

    str:= str
    +'],' + LineEnding
    +'  graphical: false,' + LineEnding;

//    str:= str
//    +'])' + LineEnding
//    +'  .setGraphical(false)             ' + LineEnding
//    +'  .setUseLegacyEncoder(true)       ' + LineEnding;

    // integer parameters are loaded as constants
    if length(self.ConstIntArray)>0 then
    begin
      str:=str
      +'  constants:{';
      for i:=0 to length(self.ConstIntArray)-1 do
      begin
        if i>0 then str:=str+', ';
        str:=str
        +self.ConstIntArray[i].ConstName+': '+inttostr(self.ConstIntArray[i].ConstValue);
      end;
      str:=str
      +'},'+ LineEnding;
    end;
    str:=str + '  useLegacyEncoder: true' + LineEnding;
    str:= str + '});' + LineEnding;
    str:=str+LineEnding;
  end;

  //.......... Create a final Kernel (Graphical) ..............
  h:=self.ActualHeight;
  w:=self.ActualWidth;

  str:=str
  +'/*/------------ Create Graphical Kernel -------/*/ ' + LineEnding
  +'const '+KName+'G = '+self.MyNode.NodeName+'.createKernel(function(myArray,AnimationCounterValue'+plist
  +') { ' + LineEnding
  +'  var r = 0  ;      ' + LineEnding
  +'  var g = 0  ; /*/--initalise the default colour for the GPUCanvas pixel in r,g,b,a format --/*/  ' + LineEnding
  +'  var b = 0  ;   ' + LineEnding
  +'  var a = 1  ;  ' + LineEnding;
  str:=str + AnimCode[0];
  str:= str
  +'  this.color((r),(g),(b),(a));' + LineEnding
  +'})'+LineEnding
  +'  .setOutput(['+intToStr(w)+','+ intToStr(h)+'])              ' + LineEnding
  +'  .setLoopMaxIterations(['+IntToStr(self.MaxIterations)+'])   ' + LineEnding
  +'  .setGraphical(true)                                         ' + LineEnding;
  // integer parameters are loaded as constants
  if length(self.ConstIntArray)>0 then
  begin
    str:=str
    +'  .setConstants({';
    for i:=0 to length(self.ConstIntArray)-1 do
    begin
      if i>0 then str:=str+', ';
      str:=str
      +self.ConstIntArray[i].ConstName+': '+inttostr(self.ConstIntArray[i].ConstValue);
    end;
    str:=str
    +'       })' + LineEnding;
  end;
  str:= str + ';' + LineEnding;
  str:=str+LineEnding;


  str:=str
  +'/*/-------------------Build the nested Kernel codes ----------------------/*/' + LineEnding
  +'let AnimationCounterValue='+IntToStr(self.StartIteration)+'; '  +LineEnding
  +'let AnimationCounterMax='+IntToStr(self.NumFrames)+'; '  +LineEnding;

//combineKernels(k,k,...,lastKernel,combinedKernel)
  if numKernels>0 then
  begin
    str:=str
    +'const superKernel = '+self.MyNode.NodeName+'Matrix.combineKernels('+LineEnding;

    for n:=0 to numKernels-1 do
    begin
      str:=str
      +'    '+KName+inttostr(n)
      +', ';
      str:=str+LineEnding;
    end;
    str:=str
      +'    function(myArray,AnimationCounterValue'+plist+') {'+LineEnding
      +'      var rslt='+LineEnding;
    for n:=numKernels-1 downto 0 do
    begin
      str:=str
      +'        '+KName+inttostr(n)+'( '+LineEnding;
      if n=0 then
        str:=str+'        '+'myArray,'+LineEnding;
    end;
    for n:=0 to numKernels-1 do
    begin
      str:=str
      +'        '+'AnimationCounterValue'+plist+')';
      if n<numKernels-1 then
        str:=str+','
      else
        str:=str+';';
      str:=str+LineEnding;
    end;
    str:=str
    +'      return rslt;'+LineEnding
    +'    });'+LineEnding;
  end;



  // Initialise the stageArray...   [3D array of real]
  // Initial values come from the 'XGPU3DTable' component (data held in property InitStageData)
  {$ifndef JScript}
  str:=str
  +'let stageArray='+StringUtils.DelChars(self.InitStageData,'"')+';   '+LineEnding;
  {$else}
  // in the HTML environment, when the GPU canvas is within an iframe element, it appears that there is
  // a problem loading the page when the string is very long (fails). For this reason we have not initialised
  // the 3D stageArray here.  Instead, there is a message posted into the iframe document (StartTheGPU),
  // which is handled in the iframe, sets up the array, and then runs this function.
  str:=str
  +'let stageArray=[[[0]]];'  +LineEnding;
  {$endif}
  if numKernels>0 then
    str:=str+'let outputStageArray=Array('+inttoStr(self.Dimensions[1,2])+
                                   ').fill(Array('+inttoStr(self.Dimensions[1,1])+
                                   ').fill(Array('+inttoStr(self.Dimensions[1,0])+').fill(0.0)));' +LineEnding
  else
    str:=str+'let outputStageArray=[[[0]]];' +LineEnding;
  str:=str+LineEnding;
  str:=str+'function StartTheGPU() {'+LineEnding;



  // Run the combined non-graphical kernels...
  if numKernels>0 then
  begin
    // Call the build method for each kernel in turn (flushes out transpile errors)
    for n:=0 to numKernels-1 do
    begin
      str:=str
      +'    '+KName+inttostr(n)+'.build(stageArray,AnimationCounterValue'+plist+');  '+LineEnding
      +'    console.log("Kernel '+inttostr(n)+' built ok");  '+LineEnding;
    end;
    str:=str+LineEnding;

    str:=str
    +'    /*/-------------------Run the Nested Kernels ----------------------/*/    ' + LineEnding
    +'    outputStageArray = superKernel(stageArray,AnimationCounterValue'+plist+');  '+LineEnding
    +'    PostMessageStageArray("'+myNode.NodeName+'",AnimationCounterValue);' + LineEnding;
  end
  else
    str:=str
    +'    outputStageArray = stageArray;  '+LineEnding;

  str:=str
  +'    /*/-------------------Run the Graphical Kernel and place the result on the web page----------------------/*/    ' + LineEnding
  +'    '+KName+'G(outputStageArray,AnimationCounterValue'+plist+');               ' + LineEnding;
//  if self.FetchFrameOutput=true then
    str:=str
    +'    outputArrayString = GetPixelArray('+self.MyNode.NodeName+'CanvasRenderFnG); ' + LineEnding
    +'    PostMessageOutputArray("'+myNode.NodeName+'",AnimationCounterValue);' + LineEnding;

  // Put the GPU bitmap on the page...
  str:=str
  +'    var '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFnG.canvas;  ' + LineEnding        // GPUjs V2
//  +'    var '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFnG.getCanvas();  ' + LineEnding // GPUjs v1
  +'    document.getElementsByTagName("body")[0].appendChild('+self.MyNode.NodeName+'BrowserCanvas);     ' + LineEnding ;
  str:=str +
    '    '+self.MyNode.NodeName+'BrowserCanvas.addEventListener("webglcontextlost", (e) => { window.location.reload(); }); ' + LineEnding;

  if self.Animated then
    str:=str
    +'    /*/------------------- Animate the GPUCanvas ----------------------/*/    ' + LineEnding
    +'    AnimationFrameID=requestAnimationFrame(animate) ; '  + LineEnding;

  str:=str+'}'+LineEnding;  // end of function StartTheGPU()

  str:=str + JSFinalCode;

  result:=str;
end;

function TXGPUCanvas.GPUJSCodeForEmulationMode(AnimCode:TStringList):String;
var
  str,KName,tempstr:String;
  h,w,n:integer;
  ok:boolean;
begin
  // This builds a simple graphical kernel that will simply display the content of its
  // InitialStageArray.
  // The format of the expected 3D array is  [y...[x...[r,g,b,a]]]
  result:='';
  ok := CheckForKernels(AnimCode);
  if not ok then EXIT;

  str:=str + JSInitialCode;
  str:=str + JSPostMessageCode;
  str:=str + JSGetPixelArray;

  KName:=self.MyNode.NodeName+'CanvasRenderFn';

  //.......... Create a Graphical Kernel ..............
  h:=self.ActualHeight;
  w:=self.ActualWidth;

  str:=str
  +'/*/------------ Create Simple Graphical Kernel -------/*/ ' + LineEnding
  +'const '+KName+'G = '+self.MyNode.NodeName+'.createKernel(function(myArray) { ' + LineEnding
  +'  var r = 0  ;      ' + LineEnding
  +'  var g = 0  ; /*/--initalise the default colour for the GPUCanvas pixel in r,g,b,a format --/*/  ' + LineEnding
  +'  var b = 0  ;   ' + LineEnding
  +'  var a = 1  ;  ' + LineEnding;
  str:=str
  +'  r = myArray[this.thread.y][this.thread.x][0]; ' + LineEnding
  +'  g = myArray[this.thread.y][this.thread.x][1]; ' + LineEnding
  +'  b = myArray[this.thread.y][this.thread.x][2]; ' + LineEnding
  +'  //a = myArray[this.thread.y][this.thread.x][3]; ' + LineEnding;
  str:= str
  +'  this.color((r),(g),(b),(a));' + LineEnding
  +'})'+LineEnding
  +'  .setOutput(['+intToStr(w)+','+ intToStr(h)+'])              ' + LineEnding
  +'  .setLoopMaxIterations(['+IntToStr(self.MaxIterations)+'])   ' + LineEnding
  +'  .setGraphical(true)                                         ' + LineEnding;
  // integer parameters are loaded as constants
  str:= str + ';' + LineEnding;
  str:=str+LineEnding;

  // Initialise the stageArray...   [3D array of real]
  {$ifndef JScript}
  str:=str
  +'let stageArray='+StringUtils.DelChars(self.InitStageData,'"')+';   '+LineEnding;
  {$else}
  // in the HTML environment, when the GPU canvas is within an iframe element, it appears that there is
  // a problem loading the page when the string is very long (fails). For this reason we have not initialised
  // the 3D stageArray here.  Instead, there is a message posted into the iframe document (StartTheGPU),
  // which is handled in the iframe, sets up the array, and then runs this function.
  str:=str
  +'let stageArray=[[[0]]];'  +LineEnding;
  {$endif}

  str:=str+LineEnding;
  str:=str+'function StartTheGPU() {'+LineEnding;

  str:=str
  +'    /*/-------------------Run the Graphical Kernel and place the result on the web page----------------------/*/    ' + LineEnding
  +'    '+KName+'G(stageArray);               ' + LineEnding;

  // Put the GPU bitmap on the page...
  str:=str
  +'    var '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFnG.canvas;  ' + LineEnding        // GPUjs V2
//  +'    var '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFnG.getCanvas();  ' + LineEnding // GPUjs v1
  +'    document.getElementsByTagName("body")[0].appendChild('+self.MyNode.NodeName+'BrowserCanvas);     ' + LineEnding ;

  str:=str+'}'+LineEnding;  // end of function StartTheGPU()

  str:=str + JSFinalCode;

  result:=str;
end;

procedure SetOutputArrayValue(NodeName:String;const AValue,cval:String);
var
  myNode:TDataNode;
begin
  // set the property value for the relevant XGPUCanvas object
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    {$ifndef JScript}
    TXGPUCanvas(myNode.ScreenObject).GPUOutputString:=AValue;
    TXGPUCanvas(myNode.ScreenObject).animCounterString:=cval;
    {$else}
    TXGPUCanvas(myNode).GPUOutputString:=AValue;
    TXGPUCanvas(myNode).animCounterString:=cval;
    {$endif}
    myNode.SetAttributeValue('LastCounterValue',cval);
  end;
end;
procedure SetCounterValue(NodeName:String;const cval:String);
var
  myNode:TDataNode;
begin
  // set the property value for the relevant XGPUCanvas object
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    {$ifndef JScript}
    TXGPUCanvas(myNode.ScreenObject).animCounterString:=cval;
    {$else}
    TXGPUCanvas(myNode).animCounterString:=cval;
    {$endif}
  end;
  myNode.SetAttributeValue('LastCounterValue',cval);
end;

procedure SetStageArrayValue(NodeName:String;const AValue:String);
var
  myNode:TDataNode;
begin
  //{$ifdef JScript}
  //asm
  //console.log('SetStageArrayValue '+NodeName+' '+AValue);
  //console.log('SetStageArrayValue '+NodeName);
  //end;
  //{$endif}
  // set the property value for the relevant XGPUCanvas object
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
//    myNode.SetAttributeValue('GPUStageArray',AValue);
    {$ifndef JScript}
    TXGPUCanvas(myNode.ScreenObject).GPUStageString:=AValue;
    TXGPUCanvas(myNode.ScreenObject).GPUStageArray:=JsonStringTo3DNumArray(AValue);
    {$else}
    TXGPUCanvas(myNode).GPUStageString:=AValue;
    TXGPUCanvas(myNode).GPUStageArray:=JsonStringTo3DNumArray(AValue);
    {$endif}
  end;
end;

procedure SetStageArrayValue2(NodeName:String;AValue:T3DNumArray);
var
  myNode:TDataNode;
begin
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    {$ifndef JScript}
    TXGPUCanvas(myNode.ScreenObject).GPUStageArray:=AValue;
    TXGPUCanvas(myNode.ScreenObject).GPUStageString:='';
    {$else}
    TXGPUCanvas(myNode).GPUStageArray:=AValue;
    TXGPUCanvas(myNode).GPUStageString:='';
    {$endif}
  end;
end;


function GetOutputArrayValue(NodeName:String):T3DNumArray;
var
  myNode:TDataNode;
begin
  result:=nil;
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    {$ifndef JScript}
      {$ifdef Chromium}
      TXGPUCanvas(myNode.ScreenObject).GPUOutputArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUOutputString);
      result:=TXGPUCanvas(myNode.ScreenObject).GPUOutputArray;
      {$else}
      TXGPUCanvas(myNode.ScreenObject).GPUOutputArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUOutputString);
      result:=TXGPUCanvas(myNode.ScreenObject).GPUOutputArray;
      {$endif}
    {$else}
    TXGPUCanvas(myNode.ScreenObject).GPUOutputArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUOutputString);
    result:=TXGPUCanvas(myNode).GPUOutputArray;
    {$endif}
  end;
end;

function GetOutputArrayString(NodeName:String):String;
var
  myNode:TDataNode;
begin
  result:='';
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    {$ifndef JScript}
      result:=TXGPUCanvas(myNode.ScreenObject).GPUOutputString;
    {$else}
    result:=TXGPUCanvas(myNode).GPUOutputString;
    {$endif}
  end;
end;

function GetStageArrayValue(NodeName:String):T3DNumArray;
var
  myNode:TDataNode;
begin
  result:=nil;
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    {$ifndef JScript}
      if length( TXGPUCanvas(myNode.ScreenObject).GPUStageArray)=0 then
        TXGPUCanvas(myNode.ScreenObject).GPUStageArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUStageString);
      result:=TXGPUCanvas(myNode.ScreenObject).GPUStageArray;
    {$else}
    if length( TXGPUCanvas(myNode).GPUStageArray)=0 then
      TXGPUCanvas(myNode).GPUStageArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode).GPUStageString);
    result:=TXGPUCanvas(myNode).GPUStageArray;
    {$endif}
  end;
end;

function GetInitStageArrayValue(NodeName:String):T3DNumArray;
var
  myNode:TDataNode;
begin
  result:=nil;
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    result:=JSONStringTo3DNumArray(myNode.GetAttribute('InitStageData',false).AttribValue);
  end;
end;

function GetStageArrayString(NodeName:String):String;
var
  myNode:TDataNode;
begin
  result:='';
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
    {$ifndef JScript}
    if TXGPUCanvas(myNode.ScreenObject).GPUStageString<>'' then
      result:=TXGPUCanvas(myNode.ScreenObject).GPUStageString
    else
      result:=Num3DArrayToJsonString( TXGPUCanvas(myNode.ScreenObject).GPUStageArray);
    {$else}
    if TXGPUCanvas(myNode).GPUStageString<>'' then
      result:=TXGPUCanvas(myNode).GPUStageString
    else
      result:=Num3DArrayToJsonString(TXGPUCanvas(myNode).GPUStageArray);
    {$endif}
  end;
end;

//function GetParam(NodeName:String; pindex:integer):Float;
//begin
//  showMessage('GetParam');
//  result:=0;
//end;

function TXGPUCanvas.GPUJSAnimationFooter:String;
// This code is added only if the 'Animated' property is true...
var
  str,plist:String;
begin

  plist:=self.FullParamList;

  str:='/*/------------------ Graphics Animation Functions ---------------------------------/*/' + LineEnding;


// The FrameDone function is called after each animation frame is finished.
// It is an async function, so that inbound messages are picked up,
// and we can use postMessage, eg. for passing
// values back out from the IFrame into the project environment.
  str:=str
  +'function FrameDone(kernel) {'  + LineEnding;
//  if self.FetchFrameOutput=true then
//  begin
  str:=str;
  str:=str+'    if (running) {outputArrayString = GetPixelArray(kernel); ' + LineEnding
//          +'      console.log("FrameDone "+AnimationCounterValue);'  + LineEnding
          +'      PostMessageOutputArray("'+myNode.NodeName+'",AnimationCounterValue);}' + LineEnding;
//  end;
  str:=str+'    return new Promise(resolve => { '  + LineEnding
  +'  }); } '+ LineEnding;

    str:=str
   +'async function DoFrame() {  '  + LineEnding;
   if numKernels>0 then
     str:=str
     +'    outputStageArray=superKernel(stageArray,AnimationCounterValue'+plist+');  '+LineEnding
   else
     str:=str
     +'    outputStageArray=stageArray;  '+LineEnding;
   str:=str
   +'    '+self.MyNode.NodeName+'CanvasRenderFnG(outputStageArray,AnimationCounterValue'+plist+'); ' + LineEnding
   +'    AnimationCounterValue = AnimationCounterValue +1; '  + LineEnding
   +'    if (AnimationCounterValue > AnimationCounterMax) {' + LineEnding
   +'      AnimationCounterValue = '+IntToStr(self.StartIteration)+';};  '  + LineEnding
   +'    var xx = await FrameDone('+self.MyNode.NodeName+'CanvasRenderFnG) ; '  + LineEnding
   +'} '  + LineEnding;

   str:=str
   +'var AnimationFrameID;  '  + LineEnding
   +'function animate(timestamp){  '  + LineEnding
   +'    GPUIntervalRunner=setInterval(DoFrame, 1000/'+IntToStr(self.MaxFramesPerSec)+'); ' + LineEnding
   +'} '  + LineEnding;

  result:=str;
end;

function TXGPUCanvas.FetchAllAnimCode:TAnimCodeArray;
var
  allcode:TAnimCodeArray;
  bits,cdbits:TStringList;
  n:integer;
  RevisedAnimCode:String;
begin
  // The property 'AnimationCode' contains a string which is a concatenation of:
  //     Code block for the graphical output kernel
  //     Code block(s) for the non-graphical nested kernels (number of these is in property NumKernels).
  //     Delimiter between code blocks is eventListdelimiter
  //     Each code block is further subdivided into:
  //         Pascal Code block
  //         Dimensions of output array (eg.  x,y,z)
  //         Delimiter between code block and dimensions spec is EventAttributeDelimiter

  setlength(allcode,numKernels+1);
  bits:= stringsplit(self.AnimationCode,eventListdelimiter);

  // for each kernel...
  for n:=0 to bits.count-1 do
  begin
    if length(allcode)<=n then
      setlength(allcode,n+1);
    // get the code block for this kernel...
    allcode[n].CodeBlock:=TStringList.Create;

    cdbits:=StringSplit(bits[n],EventAttributeDelimiter);
    allcode[n].CodeBlock.Text:=cdbits[0];

  end;

  // Add Empty code blocks for missing stages (number of code blocks == numKernels+1)
  for n:=bits.Count to numKernels do
  begin
    allcode[n].CodeBlock:=TStringList.Create;
    allcode[n].CodeBlock.Text:='//Kernel '+inttostr(n)+LineEnding
      +'begin'+LineEnding
      +'  //eg. for 3D array stages, this will pass through the prior kernel result...'+LineEnding
      +'  //Note. Dimensions of myArray are as specified for the prior kernel output.'+LineEnding
      +'  myValue:=myArray[this.thread.z,this.thread.y,this.thread.x]; '+LineEnding
      +'end;'+LineEnding;

//    setlength(allcode[n].KDimensions,3);
//    allcode[n].KDimensionsStr:=inttostr(self.ActualWidth)+','+inttostr(self.ActualHeight)+','+inttostr(self.DfltZDepth);
//    allcode[n].KDimensions[0]:=self.ActualWidth;
//    allcode[n].KDimensions[1]:=self.ActualHeight;
//    allcode[n].KDimensions[2]:=self.DfltZDepth;
  end;

  // In case this has adjusted the data, save the concatenated code block again to the AnimationCode property
  RevisedAnimCode:='';
  for n:=0 to numKernels do
  begin
    if n>0 then
      RevisedAnimCode:=RevisedAnimCode + eventListdelimiter;
    RevisedAnimCode:=RevisedAnimCode + allcode[n].CodeBlock.Text;
  end;
  self.myNode.SetAttributeValue('AnimationCode',RevisedAnimCode);
  result:=allcode;
end;

function TXGPUCanvas.BuildPascalAnimationUnit(Compiler:TObject;RunMode:String):Boolean;
// Wrap the user-supplied code from the AnimationCode property in a unit, for compilation to JavaScript by pas2js.
// RunMode is 'GPU' if we are generating actual GPU kernels to run in the GPU canvas.
// Otherwise, RunMode is set for compilation of system event code, and the GPU kernels are included in this
// for generation of a GPU emulation unit (LazDll,LasJS,JSJS, etc).
var
  PascalHeader:TStringList;
  txt:String;
  AllAnimationCode:TAnimCodeArray;
  TheAnimationCode,xdims,ydims,zdims:TStringList;
  i,n:integer;
  ok:boolean;
begin
  ok:=true;
  PascalHeader:=TStringList.Create;

  if RunMode='GPU' then
  begin
    PascalHeader.Add(' unit GPUCode'+self.myNode.NodeName+'; ');
      PascalHeader.Add('{$ifdef Dll}{$mode objfpc}{$H+}{$R+}{$endif}');
    PascalHeader.Add('interface');
    PascalHeader.Add('uses Classes, SysUtils, Math;');
  end
  else
  begin
    // creating the emulation unit...
    PascalHeader.Add('unit GPUCode'+self.myNode.NodeName+';');
    PascalHeader.Add('{$ifndef JScript}');
    PascalHeader.Add('{$mode objfpc}{$H+}');
    PascalHeader.Add('{$endif}');
    PascalHeader.Add('interface');
    PascalHeader.Add('uses Classes, SysUtils, Math, contnrs, dateutils,');
    PascalHeader.Add('  rtlconsts, strutils, types, typinfo, EventsInterface');
    if RunMode='LazDll' then
      PascalHeader.Add('  ,InterfaceTypesDll;')
    else
      PascalHeader.Add('  ,InterfaceTypes;');
  end;

  PascalHeader.Add(' type ');
  PascalHeader.Add('     TNumArray = array of real;');
  PascalHeader.Add('     T2DNumArray = array of TNumArray;');
  PascalHeader.Add('     TConstantsRecord=record');
  PascalHeader.Add('       dummycons:integer;');
  for i:=0 to length(self.ConstIntArray)-1 do
  begin
    PascalHeader.Add('       '+self.ConstIntArray[i].ConstName+':integer;');
  end;
  PascalHeader.Add('     end;');
  PascalHeader.Add('     TXThread = record	');
  PascalHeader.Add('       x,y,z:integer;	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('     TGPUThread = class	');
  PascalHeader.Add('        thread:TXThread;	');
  if RunMode='GPU' then
  begin
    PascalHeader.Add('        const constants:TConstantsRecord = (   	');
    PascalHeader.Add('           dummycons:0;   	');
    for i:=0 to length(self.ConstIntArray)-1 do
    begin
      PascalHeader.Add('       '+self.ConstIntArray[i].ConstName+': '+inttostr(self.ConstIntArray[i].ConstValue)+';');
    end;
    PascalHeader.Add('     );');
  end
  else
  begin
    PascalHeader.Add('        constants:TConstantsRecord;');
  end;
  PascalHeader.Add('     procedure color(r,g,b,a:real); virtual; abstract;	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   TFuncNotSupported = record	');
  PascalHeader.Add('     dummy:string; 	');
  PascalHeader.Add('   end;	');

  if RunMode<>'GPU' then
  begin
    PascalHeader.Add('function Emulate_'+self.myNode.NodeName+'(animcounter,NumZPixels,NumYPixels,NumXPixels:integer):TGPUPixelArray; {$ifdef Dll}stdcall;{$endif}');
  end;

  PascalHeader.Add('implementation	');
  PascalHeader.Add('   function chr(b:TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function Ord(  X: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function pred( X: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function Succ( X: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function tan(v: real):real;	');
  PascalHeader.Add('   begin	');
  if RunMode='LazDll' then
    PascalHeader.Add('     result:=Math.tan(v);	')
  else if RunMode='LazJS' then
    PascalHeader.Add('     result:=System.sin(v)/System.cos(v);	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   function sqr(d: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
  PascalHeader.Add('   end;	');
  PascalHeader.Add('   	');

  PascalHeader.Add('// -------------- Declare Parameters List (for compilation) ---------- ');
  for i:=0 to length(self.ParamNumArray)-1 do
  begin
    PascalHeader.Add('   var '+ParamNumArray[i].ParamName+':TNumArray;');
  end;
  for i:=0 to length(self.Param2DNumArray)-1 do
  begin
    PascalHeader.Add('   var '+Param2DNumArray[i].ParamName+':T2DNumArray;');
  end;
  PascalHeader.Add('   var  this:TGPUThread;');
  PascalHeader.Add('   type  T2DArray=Array of Array of real;');
  PascalHeader.Add('   type  TmyArray=Array of T2DArray;');
  PascalHeader.Add('   var  myArray:TMyArray;');
  PascalHeader.Add('   var  r,g,b,a:real;');

  AllAnimationCode:=self.FetchAllAnimCode;
  if length(AllAnimationCode)>1 then
    PascalHeader.Add('   var  myValue:real;');


  txt :=  '   procedure PascalVersionOfGPUCode(AnimationCounterValue:integer);';
  PascalHeader.Add(txt);

  PascalHeader.Add(' var') ;
  PascalHeader.Add('   zzzzz1:integer;') ;          // used as position marker in resulting JS script
  //.............. user code block goes here .................
  TheAnimationCode:=TStringList.Create;


  for n:=0 to length(AllAnimationCode)-1 do
  begin
    if n=0 then
      PascalHeader.Add('var kkkkk'+inttostr(n)+':integer;')
    else
      PascalHeader.Add('procedure kkkkk'+inttostr(n)+'(AnimationCounterValue:integer);');
    TheAnimationCode.Text:=AllAnimationCode[n].CodeBlock.Text;
    WriteIncFile(Compiler,myNode.NodeName+'__'+inttostr(n),'','tempinc/',PascalHeader,TheAnimationCode);   // one per kernel
  end;
  //..........................................................
  PascalHeader.Add('var zzzzz2:integer;');              // used as position marker in resulting JS script

  if RunMode<>'GPU' then
  begin
    ok:=self.BuildDimensionsArray(xdims,ydims,zdims);
    if ok then
    begin
      PascalHeader.Add('function Emulate_'+self.myNode.NodeName+'(animcounter,NumZPixels,NumYPixels,NumXPixels:integer):TGPUPixelArray; {$ifdef Dll}stdcall;{$endif}');
      PascalHeader.Add('var ');
      PascalHeader.Add('  i,j,k:integer;');
      PascalHeader.Add('  EmulatedScreen:TGPUPixelArray;');
      PascalHeader.Add('  outArray:TMyArray;');

      PascalHeader.Add('begin');
      PascalHeader.Add('  this:=TGPUThread.Create();');

      PascalHeader.Add('  // Set all parameters to latest values from the GPU component');
      for i:=0 to length(self.ParamNumArray)-1 do
      begin
        PascalHeader.Add('   '+ParamNumArray[i].ParamName+':=GetGPUParamNumValue('''+self.myNode.NodeName+''','''+ParamNumArray[i].ParamName+''');');
      end;
      for i:=0 to length(self.Param2DNumArray)-1 do
      begin
        PascalHeader.Add('   '+Param2DNumArray[i].ParamName+':=GetGPUParam2DNumValue('''+self.myNode.NodeName+''','''+Param2DNumArray[i].ParamName+''');');
      end;
      for i:=0 to length(self.ConstIntArray)-1 do
      begin
        PascalHeader.Add('   this.constants.'+self.ConstIntArray[i].ConstName+':=GetGPUConstIntValue('''+self.myNode.NodeName+''','''+self.ConstIntArray[i].ConstName+''');');
      end;

      PascalHeader.Add('  myArray:=GetGPUInitStageArray('''+self.myNode.NodeName+''');');

      PascalHeader.Add('  // emulate each thread in turn ');
      PascalHeader.Add('  // ...separately for each kernel ..... ');
      for n:=1 to length(AllAnimationCode)-1 do
      begin
        PascalHeader.Add('  setlength(outArray,'+inttostr(self.Dimensions[n,2])+');');        //Z
        PascalHeader.Add('  for k:=0 to length(outArray)-1 do');
        PascalHeader.Add('  begin ');
        PascalHeader.Add('    setlength(outArray[k],'+inttostr(self.Dimensions[n,1])+');');   //Y
        PascalHeader.Add('    for j:=0 to length(outArray[k])-1 do');
        PascalHeader.Add('    begin ');
        PascalHeader.Add('      setlength(outArray[k,j],'+inttostr(self.Dimensions[n,0])+');'); //X
        PascalHeader.Add('      for i:=0 to length(outArray[k,j])-1 do ');
        PascalHeader.Add('      begin ');
        PascalHeader.Add('         this.thread.x := i;');
        PascalHeader.Add('         this.thread.y := j;');
        PascalHeader.Add('         this.thread.z := k;');
        PascalHeader.Add('         kkkkk'+inttostr(n)+'(animcounter); // sets myValue');
        PascalHeader.Add('         outArray[k,j,i] := myValue;');
        PascalHeader.Add('      end;');
        PascalHeader.Add('    end;');
        PascalHeader.Add('  end;');
        PascalHeader.Add('  myArray:=outArray;');
      end;

      PascalHeader.Add('  setlength(EmulatedScreen,NumZPixels);');
      PascalHeader.Add('  for k:=0 to NumZPixels-1 do');
      PascalHeader.Add('  begin ');
      PascalHeader.Add('    setlength(EmulatedScreen[k],NumYPixels); ');
      PascalHeader.Add('    for j:=0 to NumYPixels-1 do');
      PascalHeader.Add('    begin ');
      PascalHeader.Add('      setlength(EmulatedScreen[k,j],NumXPixels); ');
      PascalHeader.Add('      for i:=0 to NumXPixels-1 do ');
      PascalHeader.Add('      begin ');
      PascalHeader.Add('         r := 0;');
      PascalHeader.Add('         g := 0;');
      PascalHeader.Add('         b := 0;');
      PascalHeader.Add('         a := 1;');
      PascalHeader.Add('         this.thread.x := i;');
      PascalHeader.Add('         this.thread.y := j;');
      PascalHeader.Add('         this.thread.z := k;');

      PascalHeader.Add('         PascalVersionOfGPUCode(animcounter); // sets r,g,b,a');

      PascalHeader.Add('         // now save the pixel colour value ');
      PascalHeader.Add('         setlength(EmulatedScreen[k,j,i],4); ');
      PascalHeader.Add('         EmulatedScreen[k,j,i,0] := r;');
      PascalHeader.Add('         EmulatedScreen[k,j,i,1] := g;');
      PascalHeader.Add('         EmulatedScreen[k,j,i,2] := b;');
      PascalHeader.Add('         EmulatedScreen[k,j,i,3] := a;');
      PascalHeader.Add('      end;');
      PascalHeader.Add('    end;');
      PascalHeader.Add('  end;');
      PascalHeader.Add('  result := EmulatedScreen; ');
      PascalHeader.Add('end; ');
      PascalHeader.Add('{$ifdef Dll}exports Emulate_'+self.myNode.NodeName+';{$endif}');
    end;
  end;

  if ok then
  begin
    PascalHeader.Add('begin');
    PascalHeader.Add('end.');

    if RunMode<>'GPU' then
    begin
      {$ifndef JScript}
      // save the generated pas file
      SysUtils.DeleteFile('tempinc/GPUCode'+self.myNode.NodeName+'.pas');
      PascalHeader.SaveToFile('tempinc/GPUCode'+self.myNode.NodeName+'.pas');
      {$else}
      TPas2JSWebCompiler(Compiler).WebFS.SetFileContent('GPUCode'+self.NodeName+'.pas',PascalHeader.Text);
      {$endif}
    end;

    GeneratedPascalUnit:=PascalHeader.Text;
  end;

  result:=ok;                 //PascalHeader.Text;
  FreeAndNil(PascalHeader);
  FreeAndNil(TheAnimationCode);
end;


function TXGPUCanvas.CompileGPUToJS(var GPUJSOutput:String):Boolean;
var
  ProgPath, PasFileName,ObjectFileName,ExeFileName:String;
  UnitString:String;
  ok:Boolean;
  prog:TStringList;
  {$ifdef JScript}
  Res:Boolean;
  args:TStringList;
  lWebFS : TPas2JSWebFS;
  {$endif}
begin
  self.GeneratedHTML:='';
  result:=false;

  {$ifndef JScript}
  ok := BuildPascalAnimationUnit(nil,'GPU');
  if (ok = false) then
    EXIT;

  UnitString:=GeneratedPascalUnit;

  ProgPath:=ExtractFilePath(Application.ExeName)+'tempinc/';
  prog:=TStringList.Create;
  prog.Text:=UnitString;

  // clean up from previous runs
  PASFileName:= self.myNode.NodeName+'.pas';
  DeleteFile(ProgPath+PASFileName);
  ObjectFileName:= self.myNode.NodeName+'.o';
  DeleteFile(ProgPath+ObjectFileName);
  ExeFileName:= self.myNode.NodeName+'.js';
  DeleteFile(ProgPath+ExeFileName);

 // GPUCodeEditor.ItemValue:=PascalCode.Text;
  // save the text to be compiled to the .pas file
  prog.SaveToFile(ProgPath+PASFileName);
  prog.Free;
  ok:=false;

  GPUCodeEditor.MessageLines:='';

  TranspileMyProgram(self.myNode.NodeName,ExtractFilePath(Application.ExeName),'tempinc/',GPUCodeEditor,true,nil);
  // and do checking for errors...
  // if the JS file exists, run on browser...
  if FileExists(ExtractFilePath(Application.ExeName)+ExeFileName) then
  begin
    ok:=true;
    // and get the GPUJSOutput...
    GPUJSOutput:=ReadFile(ExtractFilePath(Application.ExeName)+ExeFileName);
//    EditAttributeValue('XMemo1','ItemValue',GPUJSOutput);        //!!!! temporary for debugging
  end
  else
  begin
    // GPU code failed to compile - show the editor with messages
    GPUCodeEditor.ItemValue:=UnitString;
    GPUCodeEditor.MessagesHeight:='30%';
    EditingGPUNode:=self.myNode;
    GPUEditorMode:='Unit';
    GPUCodeEditor.ReadOnly:=true;
    GPUComboBox.OptionList:=self.BuildKernelList;
    GPUComboBox.ItemIndex:=-1;
    GPUComboBox.PriorIndex:=-1;
    GPUEditorForm.Showing:='Modal';
  end;

  {$else}
  begin
    ok := BuildPascalAnimationUnit(MyWebCompiler.Compiler,'GPU');
    if (ok = false) then
      EXIT;

    UnitString:=GeneratedPascalUnit;

    MyWebCompiler.MyCodeEditor:=GPUCodeEditor;
    MyWebCompiler.Compiler.Log.OnLog:=@MyWebCompiler.DoLog;
    MyWebCompiler.Compiler.WebFS.LoadBaseURL:='';

    //showmessage('FirstUnitName='+FirstUnitName);
    if MyWebCompiler.MyCodeEditor<>nil then
      MyWebCompiler.myCodeEditor.ItemValue:=UnitString;

    Res:=False;

    // Load up the RTL sources that are required for the compilation...
    lWebFS:=MyWebCompiler.Compiler.WebFS;
    LoadRTLFilesForPas2JS(lWebFS);

    lWebFS.SetFileContent(self.NodeName+'.pas',UnitString);
//    showmessage('done main file save');
    args:=TStringList.Create;
    try
      Args.Add('-vwnhe');
      Args.Add('-O-');           //  Disable optimizations
      Args.Add('-Jc');           //  Write all JavaScript concatenated into the output file
      Args.Add('-Jirtl.js-');         //  Remove a JS file.
      Args.Add('-dJScript');
      Args.Add(self.NodeName+'.pas');

      //........................................................
      MyWebCompiler.Compiler.Run('','',Args,True);
      Res:=MyWebCompiler.Compiler.ExitCode=0;
      //........................................................

    finally
     Args.Free;
    end;

    //EditAttributeValue('XMemo2','','ItemValue',UnitString);        //!!!! temporary for debugging

    if res=true then
    begin
      //showmessage('compiler all done');
      GPUJSOutput:=MyWebCompiler.Compiler.WebFS.GetFileContent(self.NodeName+'.js');
    end
    else
    begin
      // GPU code failed to compile - show the editor with messages
      GPUCodeEditor.ItemValue:=UnitString;
      GPUCodeEditor.MessagesHeight:='30%';
      EditingGPUNode:=self.myNode;
      GPUCodeEditor.ReadOnly:=true;
      GPUEditorMode:='Unit';
      GPUComboBox.OptionList:=self.BuildKernelList;
      GPUComboBox.ItemIndex:=-1;
      GPUComboBox.PriorIndex:=-1;
      GPUEditorForm.Showing:='Modal';
    end;
  end;

  //EditAttributeValue('XMemo1','','ItemValue',GPUJSOutput);        //!!!! temporary for debugging

  ok:=res;
  if res=true then
  begin
  end;

//  showmessage('RunParser done.  Output='+GPUJSOutput);

  {$endif}

  //....decide if there are errors or not .......
  if ok=false then showmessage('GPU Compilation failed');
  //else showmessage('Compilation successful');
  result:=ok;
end;

function TXGPUCanvas.CompileAndTrimAnimCode:TStringList;
var
  Pas2jsOutput:String;
  PasString,Pas2JSRaw:String;
  Pas2JSTrimmed:TstringList;
  tmp:String;
  ok:Boolean;
  tmpList:TStringList;
  i,j:integer;
begin
  PasString:=myNode.getAttribute('AnimationCode',true).AttribValue;
  Pas2JSTrimmed:=TStringList.Create;
  if PasString<>'' then
  begin

    // Compile the Pascal code
    ok:=CompileGPUToJS(Pas2jsOutput);

   // EditAttributeValue('XMemo1','','ItemValue',Pas2jsOutput,false);        //!!!! temporary for debugging
    if ok then
    // Extract the bit of this that we need to keep
    begin
      tmpList:=TStringList.Create;
      tmpList.Text:=Pas2JSOutput;
      // delete up to the zzzzz1 line
      while (tmpList.Count>0) and (FoundString(tmpList[0],'zzzzz1')<1) do
         tmpList.Delete(0);
//      showmessage('after search for zzzzz1 '+inttostr(tmpList.Count)+' lines left');
      if tmpList.Count>0 then
      begin
        // delete the zzzzz1 line
        tmpList.Delete(0);
        // skip to the zzzzz2 line
        i:=0;
        while i<tmpList.Count do
        begin
          if FoundString(tmpList[i],'zzzzz2')>0 then
          begin
//            showmessage('found zzzzz2 at line '+inttostr(i));
            j:=i;
            tmpList[i-1]:='';     // replace preceding '};' with '}'
            i:=tmpList.Count;
          end
          else
          if (FoundString(tmpList[i],'kkkkk')>0)
          and (FoundString(tmpList[i],'function')>0) then
          begin
            tmpList[i-1]:='';    // remove the closing '};' - will be added back later.
           end;
          i:=i+1;
        end;
        // delete all after the zzzzz2 line
        while j<tmpList.Count do
        begin
           tmpList.Delete(j);
        end;
      end;
      // now, tmplist has the set of kernel procs, plus the graphical final kernel.
      // These need separating.

      Pas2JSRaw:=tmpList.Text;

      //showmessage('raw='+Pas2JSRaw);

      // Now filter out any qualifiers that may have been added by pas2js
      // eg. parameter variable P1 will have translated to $impl.P1
      // and also modify calls to some maths ffunctions...
      Pas2JSRaw := myStringReplace(Pas2JSRaw,'$impl.','',-1,-1);
      Pas2JSRaw := myStringReplace(Pas2JSRaw,'This.','this.',-1,-1);
      Pas2JSRaw := myStringReplace(Pas2JSRaw,'pas.System.Trunc','Math.floor',-1,-1);
      Pas2JSRaw := myStringReplace(Pas2JSRaw,'pas.Math.Ceil','Math.ceil',-1,-1);
      Pas2JSRaw := myStringReplace(Pas2JSRaw,'Math.log10','Math.log',-1,-1);


      // Look for 'for' loops.  Insert 'var' ahead of the loop variable.   eg. for (var i=0; i<something; i++)
      // NB. for loops MUST also be coded using a constant as the comparison element.
      Pas2JSRaw := myStringReplace(Pas2JSRaw,' for (',' for (var ',-1,-1);
      Pas2JSRaw := myStringReplace(Pas2JSRaw,' for (var var',' for (var ',-1,-1);

      //!!!! check for GPU invalid chars (eg $) here and throw error????
      // also refs to rtl.

      // now separate the kernel procs...
      //showmessage(Pas2JSRaw);
      Pas2JSTrimmed:=TStringList.Create;
      tmpList.Text:= Pas2JSRaw;
      j:=-1;
      tmp:='';
      for i:=0 to tmpList.Count-1 do
      begin
        if (FoundString(tmpList[i],'kkkkk')>0) then
        begin
          if j>-1 then
          begin
            Pas2JSTrimmed.Add(tmp);
          end;
          j:=j+1;
          tmp:='';
        end
        else
          tmp:=tmp+tmpList[i]+LineEnding;
        if i=tmpList.Count-1 then
          Pas2JSTrimmed.Add(tmp);
      end;

      tmpList.Free;
    end;
  end;

  result:=Pas2JSTrimmed;
  //FreeAndNil(Pas2JSTrimmed);
end;

function TXGPUCanvas.FullXMLString:String;
var
  Pas2JSTrimmed:TstringList;   // one string per kernel
  tmp:String;
  FullString,HTMLTop,HTMLBottom:String;
  {$ifdef JScript}
  GPURunnableHTML:String;
  {$endif}
begin
  {$ifndef JScript}
  if not (csDesigning in ComponentState) then
  {$endif}
  begin
    Pas2JSTrimmed:=self.CompileAndTrimAnimCode;
    if Pas2JSTrimmed.Count<1 then
    begin
      result:='';
      EXIT;
    end;

    if self.EmulationMode = false then
    begin
    // Get the set of kernel output dimensions
    // and wrap it with the GPU JS...
    tmp:= GPUJSCode(Pas2JSTrimmed);
    FullString:= tmp;
    if self.Animated then
    begin
      FullString:=FullString + GPUJSAnimationFooter;
      {$ifdef JScript}
      GPURunnableHTML:=GPURunnableHTML + GPUJSAnimationFooter;
      {$endif}
    end;
    end
    else
    begin
      FullString:= GPUJSCodeForEmulationMode(Pas2JSTrimmed);
    end;


    tmp:=UnSubstituteSpecials(gpujs);
    HTMLTop:=
    '<!DOCTYPE html>' +  LineEnding
    +'<html lang="en">' +  LineEnding
    +'<head>' +  LineEnding
    +'  <meta charset="utf-8"> ' +  LineEnding
    +'  <title></title>' +  LineEnding
    +'</head>' +  LineEnding
    +'<body  style="margin:0px; font:normal 12px Verdana, Arial, sans-serif;">' +  LineEnding
    +'<script>'+tmp+'</script>  ' +  LineEnding

 //   +'<script src="gpu-browser.min.js"></script>' +  LineEnding


    +'<div  id="GPUCanvas" > '+  LineEnding
    +'<script>'+  LineEnding;

    HTMLBottom:=
    '</script>' +  LineEnding
    +'</div> '+  LineEnding
    +'</body> ' +  LineEnding
    +'</html> '+  LineEnding;


    {$ifdef JScript}
    GPURunnableHTML:=
    HTMLTop
    +FullString +  LineEnding
      +'stageArray='+StringUtils.DelChars(self.InitStageData,'"')+';   '+LineEnding
      +'window.postMessage({"objid":"'+self.MyNode.NodeName+'", "mtype":"StartTheGPU", "pName":"", "pValue":stageArray},"*"); '+LineEnding
    +HTMLBottom;
    {$endif}

    FullString:=
      HTMLTop
      +FullString +  LineEnding
      {$ifndef JScript}
      + 'StartTheGPU();  '+lineEnding
      {$else}
      +  'window.parent.postMessage({"objid":"'+self.MyNode.NodeName+'","mtype":"GPUReady"},"*"); ' + LineEnding
      {$endif}
      +HTMLBottom;


//    EditAttributeValue('XMemo1','','ItemValue',FullString,false);        //!!!! temporary for debugging
    {$ifndef JScript}
    self.GeneratedHTML:=FullString;
    {$else}
    // this is the same HTML string, but with a statement included to start the GPU process (as this is done
    // via a message from outside the iframe, when embedded in the local page.
    self.GeneratedHTML:=GPURunnableHTML;
    {$endif}

  end;
  result:=FullString;
end;


procedure TXGPUCanvas.setupGPUPage;
var
  GPUString:String;
begin
  // called from StartMyGPU
  GPUString:=self.FullXMLString;
  self.HTMLSource:= '';
  self.HTMLSource:= GPUString;
end;

function TXGPUCanvas.GetParamNumValue(pName:String):TNumArray;
var
  i:integer;
  pval:TNumArray;
begin
  for i:=0 to length(ParamNumArray)-1 do
    if ParamNumArray[i].ParamName=pName then
    begin
      pval:=ParamNumArray[i].ParamValue;
    end;
  result:=pval;
end;

function TXGPUCanvas.GetParam2DNumValue(pName:String):T2DNumArray;
var
  i:integer;
  pval:T2DNumArray;
begin
  for i:=0 to length(Param2DNumArray)-1 do
    if Param2DNumArray[i].ParamName=pName then
    begin
      pval:=Param2DNumArray[i].ParamValue;
    end;
  result:=pval;
end;

function TXGPUCanvas.GetConstIntValue(pName:String):integer;
var
  i:integer;
  pval:integer;
begin
  for i:=0 to length(ConstIntArray)-1 do
    if ConstIntArray[i].ConstName=pName then
    begin
      pval:=ConstIntArray[i].ConstValue;
    end;
  result:=pval;
end;

procedure TXGPUCanvas.SetParamNumValue(pName:String;pValue:TNumArray;ForwardToWidget:Boolean);
var
  i,j:integer;
  tmp:String;
  myurl:string;
begin
  for i:=0 to length(ParamNumArray)-1 do
    if uppercase(ParamNumArray[i].ParamName)=uppercase(pName) then
    begin
      SetLength(ParamNumArray[i].ParamValue,length(pValue));
      for j:=0 to length(pValue)-1 do
        ParamNumArray[i].ParamValue[j]:=pValue[j];

      if (ForwardToWidget)
      and (self.Active)
      and (self.HTMLSource<>'')
      and (self.HTMLSource<>'about:blank') then
      begin
        {$ifndef JScript}
        {$ifdef Chromium}
        myurl:= myChromium.Browser.MainFrame.GetURL();
        if myurl<>'about:blank' then
        begin
          tmp:=NumArrayToJSONString(pValue);
          tmp:='RunCode("'+pName+'='+tmp+';")';
          myChromium.Browser.MainFrame.ExecuteJavaScript(tmp, myurl, 0);
        end;
        {$else}
        //!!!! need to refresh the GPU canvas display when it's on a separate browser page....  ??
        myurl:='';
        {$endif}
        {$else}
        asm
          var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
          if (ob!=null) {
            //alert('found iframe. posting param message');
            ob.contentWindow.postMessage({"objid":this.NodeName, "mtype":"SetNumParam", "pName":pName, "pValue":pValue},"*");
            }
        end;
        {$endif}
      end;

    end;
end;

procedure TXGPUCanvas.SetParam2DNumValue(pName:String;pValue:T2DNumArray;ForwardToWidget:Boolean);
var
  i,j,k:integer;
  tmp:String;
  myurl:string;
begin
  for i:=0 to length(Param2DNumArray)-1 do
    if uppercase(Param2DNumArray[i].ParamName)=uppercase(pName) then
    begin
      SetLength(Param2DNumArray[i].ParamValue,length(pValue));
      for j:=0 to length(pValue)-1 do
      begin
        setLength(Param2DNumArray[i].ParamValue[j],length(pValue[j]));
        for k:=0 to length(pValue[0])-1 do
          Param2DNumArray[i].ParamValue[j,k]:=pValue[j,k];
      end;

      if (ForwardToWidget)
      and (self.Active)
      and (self.HTMLSource<>'')
      and (self.HTMLSource<>'about:blank') then
      begin
        {$ifndef JScript}
        {$ifdef Chromium}
        myurl:= myChromium.Browser.MainFrame.GetURL();
        if myurl<>'about:blank' then
        begin
          tmp:=Num2dArrayToString(pValue);
          tmp:='RunCode("'+pName+'='+tmp+';")';
          myChromium.Browser.MainFrame.ExecuteJavaScript(tmp, myurl, 0);
        end;
        {$else}
        //!!!! need to refresh the GPU canvas display when it's on a separate browser page....  ??
        myurl:='';
        {$endif}
        {$else}
        asm
          var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
          if (ob!=null) {
            //alert('found iframe. posting param message');
            ob.contentWindow.postMessage({"objid":this.NodeName, "mtype":"SetNumParam", "pName":pName, "pValue":pValue},"*");
            }
        end;
        {$endif}
      end;

    end;
end;

procedure TXGPUCanvas.SetConstIntValue(pName:String;pValue:integer);
var
  i:integer;
begin
  for i:=0 to length(ConstIntArray)-1 do
    if uppercase(ConstIntArray[i].ConstName)=uppercase(pName) then
    begin
      ConstIntArray[i].ConstValue:=pValue;
    end;
end;

procedure TXGPUCanvas.SetAnimationCode(AValue:string);
var
  GPUString:string;
  FullString:String;
begin
  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    GPUString:=AValue;

    //showmessage('Frame setXMLString '+SVGString);
    myNode.SetAttributeValue('AnimationCode',GPUString);

    if (self.Active=true)
    and (not StartingUp) then
    begin
      FullString:=self.FullXMLString;
      myNode.SetAttributeValue('HTMLSource',FullString);
      //showmessage('SetAnimationCode RedisplayFrame');
      RedisplayFrame;

    end;
  end;
end;

procedure TXGPUCanvas.StartMyGPU;
var
  h,w:integer;
  arr:TGPUPixelArray;
  arr3d:string;
  animcounter,NumZPixels,NumYPixels,NumXPixels:integer;
begin
  // refresh the actual h/w attributes
  h:=self.ActualHeight;
  w:=self.ActualWidth;

  // clear the output data
  setlength(self.GPUOutputArray,0);
  setlength(self.GPUStageArray,0);
  self.GPUStageString:='';
  self.GPUOutputString:='';
  self.animCounterString:='';

  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    HandleEvent('OnStart',self.myNode.NodeName,self.myNode.NameSpace,'');

    if self.EmulationMode=true then
    begin
      animcounter:=self.EmulationFrame;
      NumZPixels:=1;
      NumYPixels:=h;
      NumXPixels:=w;
      arr:=self.ExecEmulatorFunc(animcounter,NumZPixels,NumYPixels,NumXPixels);
      arr3d:=Num3DArrayToJSONString(arr[0]);
      self.InitStageData:=arr3d;
    end;
    SetupGPUPage;
  end;
end;

procedure TXGPUCanvas.StopMyGPU(isdestroying:Boolean);
var
  tmp,doJS,myurl:String;
begin
  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    // stop the gpu loop
    doJS:='running=false;';
    if self.Animated then
      doJS:=doJS + ' clearInterval(GPUIntervalRunner);ClearDown();';
    if isdestroying then
      doJS:=doJS + LineEnding + myNode.NodeName+'.destroy();';
    {$ifndef JScript}
    {$ifdef Chromium}
    if myChromium<>nil then
      if myChromium.Browser<>nil then
      begin
        myurl:= myChromium.Browser.MainFrame.GetURL();
        if myurl<>'about:blank' then
        begin
          tmp:='RunCode("'+doJS+'")';
          myChromium.Browser.MainFrame.ExecuteJavaScript(tmp, myurl, 0);
        end;
      end;
      // make a change to the HTMLSource text, so that the GPU code will be fully rebuilt
      // when/if the GPU is re-started. (see SetupGPUPage and SetHTMLSource).
      myNode.SetAttributeValue('HTMLSource',myNode.GetAttribute('HTMLSource',true).AttribValue + '/**/');
    {$else}
    //!!!!  external browser ??
    {$endif}
    {$else}
    asm
    var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
    if (ob!=null) {
      ob.contentWindow.postMessage({"objid":this.NodeName, "mtype":"execCode", "code":doJS},"*");
      }
    end;
    self.HTMLSource:='';   //about:blank??  clear the canvas??
    {$endif}

  end;
end;

//------------------------------- SetActive ----------------------------
// Setting the Active flag will build and run the GPU code inside the IFrame
//----------------------------------------------------------------------
procedure TXGPUCanvas.SetActive(AValue:Boolean);
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('Active',myBoolToStr(AValue),'Boolean');
    if (StartingUp=false) and (AValue=true) then
    begin
      self.StartMyGPU;
    end
    else
      self.StopMyGPU(false);
  end;
end;


procedure TXGPUCanvas.SetAnimated(AValue:Boolean);
begin
  myNode.SetAttributeValue('Animated',myBoolToStr(AValue),'Boolean');
end;
procedure TXGPUCanvas.SetEmulationMode(AValue:Boolean);
begin
  myNode.SetAttributeValue('EmulationMode',myBoolToStr(AValue),'Boolean');
end;
procedure TXGPUCanvas.SetEmulationFrame(AValue:integer);
begin
  myNode.SetAttributeValue('EmulationFrame',intToStr(AValue),'Integer');
end;
procedure TXGPUCanvas.SetParamNumList(AValue:string);
var
  pNames:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('ParamNumList',AValue,'String');
  SetLength(ParamNumArray,0);

  //use this comma-delimited list to initialise ParamArray.
  if AValue<>'' then
  begin
    pNames:=TStringList.Create;
    pNames.StrictDelimiter:=true;
    pNames.LineBreak:=',';
    pNames.Text:=AValue;
    SetLength(ParamNumArray,pNames.Count);
    for i:=0 to pNames.Count-1 do
    begin
      ParamNumArray[i].ParamName:=trim(pNames[i]);
      SetLength(ParamNumArray[i].ParamValue,1);
      ParamNumArray[i].ParamValue[0]:=0;
    end;
    pNames.Free;
  end;

end;
procedure TXGPUCanvas.SetParam2DNumList(AValue:string);
var
  pNames:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('Param2DNumList',AValue,'String');
  SetLength(Param2DNumArray,0);

  //use this comma-delimited list to initialise Param2DNumArray.
  if AValue<>'' then
  begin
    pNames:=TStringList.Create;
    pNames.StrictDelimiter:=true;
    pNames.LineBreak:=',';
    pNames.Text:=AValue;
    SetLength(Param2DNumArray,pNames.Count);
    for i:=0 to pNames.Count-1 do
    begin
      Param2DNumArray[i].ParamName:=pNames[i];
      SetLength(Param2DNumArray[i].ParamValue,1);
      SetLength(Param2DNumArray[i].ParamValue[0],1);
      Param2DNumArray[i].ParamValue[0,0]:=0;
    end;
    pNames.Free;
  end;

end;
procedure TXGPUCanvas.SetConstIntList(AValue:string);
var
  pNames:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('ConstIntList',AValue,'String');
  SetLength(ConstIntArray,0);

  //use this comma-delimited list to initialise ParamArray.
  if AValue<>'' then
  begin
    pNames:=TStringList.Create;
    pNames.StrictDelimiter:=true;
    pNames.LineBreak:=',';
    pNames.Text:=AValue;
    SetLength(ConstIntArray,pNames.Count);
    for i:=0 to pNames.Count-1 do
    begin
      ConstIntArray[i].ConstName:=pNames[i];
      ConstIntArray[i].ConstValue:=0;
    end;
    pNames.Free;
  end;

end;

procedure TXGPUCanvas.SetInitStageData(AValue:string);
begin
  myNode.SetAttributeValue('InitStageData',AValue,'String');
end;
procedure TXGPUCanvas.SetKernelXDims(AValue:string);
begin
  myNode.SetAttributeValue('KernelXDims',AValue,'String');
end;
procedure TXGPUCanvas.SetKernelYDims(AValue:string);
begin
  myNode.SetAttributeValue('KernelYDims',AValue,'String');
end;
procedure TXGPUCanvas.SetKernelZDims(AValue:string);
begin
  myNode.SetAttributeValue('KernelZDims',AValue,'String');
end;
procedure TXGPUCanvas.SetMaxIterations(AValue:integer);
begin
  myNode.SetAttributeValue('MaxIterations',IntToStr(AValue),'Integer');
end;
procedure TXGPUCanvas.SetStartIteration(AValue:integer);
begin
  myNode.SetAttributeValue('StartIteration',IntToStr(AValue),'Integer');
end;
procedure TXGPUCanvas.SetNumFrames(AValue:integer);
begin
  myNode.SetAttributeValue('NumFrames',IntToStr(AValue),'Integer');
end;
procedure TXGPUCanvas.SetMaxFramesPerSec(AValue:integer);
begin
  myNode.SetAttributeValue('MaxFramesPerSec',IntToStr(AValue),'Integer');
end;
procedure TXGPUCanvas.SetNumKernels(AValue:integer);
begin
  myNode.SetAttributeValue('NumKernels',IntToStr(AValue),'Integer');
end;

function TXGPUCanvas.ExecEmulatorFunc(animcounter,NumZPixels,NumYPixels,NumXPixels:integer):TGPUPixelArray;
{$ifndef JScript}
type
   TMyFunc=function(animcounter,NumZPixels,NumYPixels,NumXPixels:integer):TGPUPixelArray; stdcall;
   // ie. profile for function Emulate_'+self.myNode.NodeName+'(animcounter,NumZPixels,NumYPixels,NumXPixels:integer):TGPUPixelArray; ');
var
   fn: TMyFunc;
{$endif}
var
   arr:TGPUPixelArray;
   tmp:string;
begin
  // called from StartMyGPU, when in emulation mode
  setlength(arr,0);
  {$ifndef JScript}
  if MyLibC<>dynlibs.NilHandle then
  begin
    fn:= TMyFunc(GetProcedureAddress(MyLibC,'Emulate_'+self.myNode.NodeName));
    if Assigned(fn) then
        try
          arr := fn (animcounter,NumZPixels,NumYPixels,NumXPixels);   //Executes the dll function
        except
          on ex:exception do
            showmessage('Error raised in function Emulate_'+self.myNode.NodeName+': '+ex.Message);
        end
    else
      showmessage('Function Emulate_'+self.myNode.NodeName+' not found in ExecEmulatorFunc')   ;
  end;
  {$else}
  tmp:='pas.GPUCode'+self.myNode.NodeName+'.Emulate_'+self.myNode.NodeName;
  tmp:=tmp+'(animcounter,NumZPixels,NumYPixels,NumXPixels);';
  asm
  arr = eval(tmp);
    //arr = pas.??????.Emulate_????(animcounter,NumZPixels,NumYPixels,NumXPixels);
  end;
  {$endif}
  result:=arr;
end;


begin
  // this is the set of node attributes that each GPUCanvas instance will have (added to the set inherited from TXIFrame).
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ActualHeight','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'ActualWidth','Integer','','',true,false);
  AddDefaultAttribute(myDefaultAttribs,'FrameWidth','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'FrameHeight','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelPos','String','Top','',false);
  AddDefaultAttribute(myDefaultAttribs,'LabelText','String','GPU Canvas','',false);
  AddDefaultAttribute(myDefaultAttribs,'HTMLSource','String','','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'Active','Boolean','False','',false,false);
  AddDefaultAttribute(myDefaultAttribs,'Animated','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'ParamNumList','String','','List of numeric parameters (1D arrays) to be passed in to kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'Param2DNumList','String','','List of numeric parameters (2D arrays) to be passed in to kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'ConstIntList','String','','List of integer constants to be passed in to kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxIterations','Integer','512','Any loops defined inside a kernel must have a maximum iteration count defined by MaxIterations',false);
  AddDefaultAttribute(myDefaultAttribs,'StartIteration','Integer','1','Initial value for AnimationCounterValue',false);
  AddDefaultAttribute(myDefaultAttribs,'NumFrames','Integer','100','Number of frames after which AnimationCounterValue resets to 0',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxFramesPerSec','Integer','10','Target frame rate (may be slower if kernel processing times are long)',false);
  AddDefaultAttribute(myDefaultAttribs,'AnimationCode','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'NumKernels','Integer','0','Number of nested non-graphical kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'LastCounterValue','Integer','0','For an animated GPU, latest value of the animation counter',true,false);
  AddDefaultAttribute(myDefaultAttribs,'InitStageData','String','[[["1"]]]','3D Array string for input to the first non-graphical kernel',false,false);
  AddDefaultAttribute(myDefaultAttribs,'KernelXDims','String','[]','x-dimensions of output from the non-graphical kernels eg. [100,150] for 2 kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'KernelYDims','String','[]','y-dimensions of output from the non-graphical kernels eg. [100,150] for 2 kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'KernelZDims','String','[]','z-dimensions of output from the non-graphical kernels eg. [1,2] for 2 kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'EmulationMode','Boolean','False','When true, GPU will activate using the pascal kernel code, and display the result',false);
  AddDefaultAttribute(myDefaultAttribs,'EmulationFrame','Integer','1','Animation frame to be run in emulation mode',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$IFndef JScript}
  RegisterClass(TXGPUCanvas);
  AddNodeFuncLookup(MyNodeType,@CreateGPUCanvasWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateInterfaceObjGPU,@CreateGPUCanvasWidget);
  {$endif}
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
  SuppressDesignerProperty(MyNodeType,'SuspendRefresh');
  SuppressDesignerProperty(MyNodeType,'BgColor');
  SuppressDesignerProperty(MyNodeType,'HTMLSource');


end.

///////// unused experiments ........ //////////////////////////

// +'     const '+self.MyNode.NodeName+' = new GPU({mode: ''webgl''});   '+LineEnding;
 //const gpu = new GPU({ mode: 'gpu' });
// +'     const '+self.MyNode.NodeName+' = new GPU({mode: ''cpu''});   '+LineEnding;

(*
str:=str
+'  ParamsAsString=function() { '+LineEnding
// numeric parameters
+'     var s="nums =["; '+LineEnding;
cma:='""';
for i:=0 to length(self.ParamNumArray)-1 do
begin
  str:=str+'s = s+'+cma+'+"["+'+ParamNumArray[i].ParamName+'.toString()+"]";'+LineEnding ;       //eg. s = s+","+"["+p1.toString()+"]";
  cma:='","';
end;
str:=str+'    s=s+"]";'+LineEnding
// image parameters
+'     s=s+" imgs =["; '+LineEnding;
cma:='""';
for i:=0 to length(self.ParamImgArray)-1 do
begin
  str:=str+'    s = s+'+cma+'+"["+'+ParamImgArray[i].ParamName+'.toString()+"]";'+LineEnding ;       //eg. s = s+","+"["+p1.toString()+"]";
  cma:='","';
end;
str:=str+'    s=s+"]";'+LineEnding;

str:=str+'    return(s);'+LineEnding;
str:=str+'}'+LineEnding;
*)

//  {$ifdef JScript}
//  str:=str
//  +'  function PostParamMessages(objid) {'  + LineEnding
//  +'     var pv=[0]; '  + LineEnding;
//  for i:=0 to length(self.ParamNumArray)-1 do
//  begin
//  str:=str
//  +'          pv='+ParamNumArray[i].ParamName+';'  + LineEnding
////   +'          alert("posting msg. pvalue="+pv);'  + LineEnding
//  +'          parent.postMessage({"objid":objid,"mtype":"FrameDone","pName":"'+ParamNumArray[i].ParamName+'","pValue":pv},"*"); ' + LineEnding;
//  end;
//  //!!!! Add integers and images.....
//  str:=str+'}'+LineEnding;
//  {$endif}

//  +'  function FetchParamValuesFromParent()  {' +LineEnding
 // +'    console.log("param 0 is "+parent.window.pas.XGPUCanvas.GetParam('''+myNode.NodeName+''',0)); '+LineEnding
 // +'    console.log("param 0 is "+parent.testnum); '+LineEnding
//  +'  }  '+LineEnding;

//   {$ifndef JScript}
//   // Desktop XIDE
//   +'        var msg = ParamsAsString(); ' + LineEnding
//  // +'        alert("changing title to:"+msg); ' + LineEnding
//  // +'        document.title = "params ="+msg; ' + LineEnding
//  {$ifdef Chromium}
//  // Desktop XIDE with CEF4
//  +'           var ob=getcomponent("paramString"); ' + LineEnding
//  +'           if (ob!=null) {ob.innerHTML=msg;} ' + LineEnding
//  // change the document title to trigger a cef titlechange event...
//  +'           document.title = "'+myNode.NodeName+' "+AnimationCounterValue;'  + LineEnding
//  {$else}
//  // Desktop XIDE - GPUCanvas running in external browser page - using polling to get title changes
//  {$endif}
//   {$else}
//   // XIDE in Browser Environment
//   //  +'      console.log("posting FrameDone message");'  + LineEnding
//   //+'          PostParamMessages("'+self.MyNode.NodeName+'");'  +LineEnding
//   {$endif}
//   +'    }); '  + LineEnding
//   +'  } ' + LineEnding;
//

//  .... in DoFrame.....
//   {$ifdef JScript}
//   +'            try {    '  + LineEnding
//   +'              FetchParamValuesFromParent();  '  + LineEnding
//   +'            }catch(err){alert(err.message); clearInterval(AnimationFrameID);}  '  + LineEnding
//   {$endif}

// ....... JS Mode.....
//procedure HandleGPUMessage(gpumsg:TGPUMessage);
//var
//  ItemNode:TdataNode;
//begin
//  if (gpumsg.objid<>'') then
//  begin
//    //showmessage('HandleMessage XGPUCanvas: '+gpumsg.objid+' '+gpumsg.mtype+' '+NumArrayToJSONString(gpumsg.pvalue));
//     //this is a notification sent out from within a GPU frame.
//     ItemNode:=findDataNodeById(systemnodetree,gpumsg.objid,false);
//     if ItemNode<>nil then
//     begin
//       // set the ParamArray values from the message
//       TXGPUCanvas(ItemNode).SetParamNumValue(gpumsg.pName, gpumsg.pValue,false);     //!!!! + int + img
////      showmessage(TXGPUCanvas(ItemNode).ParamNumArrayToString);
//       handleEvent('OnNewFrame',ItemNode.NodeName, '');
//     end;
//  end;
//
//end;

(*
function TXGPUCanvas.SetParamsFromMessage(msg:String):TNumArray;
// msg has format 'params =[[n,n,n],[n,n,n]....]
//                'ints =[[i,i,i],[i,i,i]....]
//                'strs =[[s,s,s],[s,s,s]....]'
var
  numstr,intstr,imgstr, str, lastbit:string;
  n,i,s,j,k:integer;
  sets:TStringList;
  bits:TStringList;
  bits2:TStringList;
begin
  str:=msg;
  n:=FoundString(msg,'nums =');
  i:=FoundString(msg,'ints =');
  s:=FoundString(msg,'strs =');
  if (n>0)
  and (i>0)
  and (s>0) then
  begin
    imgstr:=str;
    Delete(imgstr,1,s+5);

    Delete(str,s,length(str)-s);
    intstr:=str;
    Delete(intstr,1,i+5);

    Delete(str,i,length(str)-i);
    numstr:=str;
    Delete(numstr,1,i+5);

    i:=FoundString(numstr,'[');
    if i>0 then
    begin
      Delete(numstr,1,i+1);             //  'n,n,n],[n,n,n]....]'
      if numstr<>'' then
      begin
        bits:=stringsplit(numstr,'],[');  //  'n,n,n /   n,n,n  / ....   ]]'
        lastbit:=bits[bits.count-1];
        if length(lastbit)>1 then
        begin
          Delete(lastbit,length(lastbit)-1,2);
          bits[bits.count-1]:=lastbit;
          for j:=0 to bits.count-1 do
          begin
            bits2:=stringsplit(bits[j],',');
            for k:=0 to bits2.count-1 do
              self.ParamNumArray[j].ParamValue[k]:=StrToFloat(bits2[k]);
          end;
        end;
      end;
    end;

    i:=FoundString(imgstr,'[');
    if i>0 then
    begin
      Delete(imgstr,1,i+1);             //  'i,i,i],[i,i,i]....]'
      if imgstr<>'' then
      begin
        bits:=stringsplit(imgstr,'],[');  //  'i,i,i /   i,i,i  / ....   ]]'
        lastbit:=bits[bits.count-1];
        if length(lastbit)>1 then
        begin
          Delete(lastbit,length(lastbit)-1,2);
          bits[bits.count-1]:=lastbit;
          for j:=0 to bits.count-1 do
          begin
            bits2:=stringsplit(bits[j],',');
            for k:=0 to bits2.count-1 do
              self.ParamImgArray[j].ParamValue[k]:='';  //!!!! ??
          end;
        end;
      end;
    end;



  end;

end;
*)


//function TXGPUCanvas.ParamNumArrayToString:String;    // (used for debugging)
//var
//  i,j:integer;
//  str:String;
//begin
//  str:='[';
//  for i:=0 to length(ParamNumArray)-1 do
//  begin
//    if i>0 then str:=str+',';
//    str:=str+ParamNumArray[i].ParamName+':[';
//    for j:=0 to length(ParamNumArray[i].ParamValue)-1 do
//    begin
//      if j>0 then str:=str+',';
//      str:=str+FloatToStr(ParamNumArray[i].ParamValue[j]);
//    end;
//    str:=str+']';
//  end;
//  str:=str+']';
//  result:=str;
//end;






