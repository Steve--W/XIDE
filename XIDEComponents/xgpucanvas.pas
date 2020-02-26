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
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame, Math,
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XTabControl, XMemo, XComboBox, EventsInterface,
    WebTranspilerUtils,
  {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf,
    LCLType, gettext,
    {$ifdef Chromium}
    uCEFChromium, uCEFInterfaces, uCEFTypes, uCEFProcessMessage, uCEFMiscFunctions,
    cefXUtils,
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
type TGPUNumParams = Array of TGPUNumParam;
//type TGPUImgParam = record
//  ParamName:String;
//  ParamValue:TImgArray;
//  end;
//type TGPUImgParams = Array of TGPUImgParam;
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
//    ParamImgArray:TGPUImgParams;
    ConstIntArray:TGPUIntConsts;
//    {$ifndef JScript}
//    fHandleOnNewFrame:TEventHandler;
//    {$endif}

    function GetAnimationCode:string;
    function GetActive:Boolean;
    function GetAnimated:Boolean;
    function GetParamNumList:string;
    function GetConstIntList:string;
//    function GetParamImgList:string;
    function GetMaxIterations:integer;
    function GetStartIteration:integer;
    function GetNumFrames:integer;
    function GetMaxFramesPerSec:integer;
//    function GetFetchFrameOutput:Boolean;
    function GetNumKernels:integer;
    function GetDfltZDepth:integer;
    function GetInitStageData:string;

    procedure SetAnimationCode(AValue:string);
    procedure SetActive(AValue:Boolean);
    procedure SetAnimated(AValue:Boolean);
    procedure SetParamNumList(AValue:string);
    procedure SetConstIntList(AValue:string);
//    procedure SetParamImgList(AValue:string);
    procedure SetMaxIterations(AValue:integer);
    procedure SetStartIteration(AValue:integer);
    procedure SetNumFrames(AValue:integer);
    procedure SetMaxFramesPerSec(AValue:integer);
//    procedure SetFetchFrameOutput(AValue:Boolean);
    procedure SetNumKernels(AValue:integer);
    procedure SetDfltZDepth(AValue:integer);
    procedure SetInitStageData(AValue:string);

    procedure SetMyEventTypes;  override;
    procedure SetPropertyDefaults;
    procedure StartMyGPU;
    procedure StopMyGPU;
    function BuildPascalAnimationUnit(Compiler:TObject):String;
    function CompileGPUToJS(var GPUJSOutput:String):Boolean;
    function GPUJSCode(AnimCode:TStringList;dims:TDimsArray):String;
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
    GPUStageArray:T3DNumArray;            // output from main graphical kernel
    GPUOutputArray:T3DNumArray;            // output from non-graphical nested kernels
    GPUStageString:String;
    GPUOutputString:String;
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
    function GetConstIntValue(pName:String):integer;
//    function GetParamImgValue(pName:String):TImgArray;
    procedure SetParamNumValue(pName:String;pValue:TNumArray;ForwardToWidget:Boolean);
    procedure SetConstIntValue(pName:String;pValue:integer);
//    procedure SetParamImgValue(pName:String;pValue:TImgArray;ForwardToWidget:Boolean);
    function CompileAnimCode:TStringList;
    function FetchAllAnimCode:TAnimCodeArray;
    function BuildKernelList:String;

published
    { Published declarations }

    // Properties defined for this class...
    property Active: Boolean read GetActive write SetActive;
    property Animated: Boolean read GetAnimated write SetAnimated;
    property AnimationCode: String read GetAnimationCode write SetAnimationCode;
    property ParamNumList: String read GetParamNumList write SetParamNumList;
    property ConstIntList: String read GetConstIntList write SetConstIntList;
//    property ParamImgList: String read GetParamImgList write SetParamImgList;
    property MaxIterations: integer read GetMaxIterations write SetMaxIterations;
    property StartIteration: integer read GetStartIteration write SetStartIteration;
    property NumFrames: integer read GetNumFrames write SetNumFrames;
    property MaxFramesPerSec: integer read GetMaxFramesPerSec write SetMaxFramesPerSec;
//    property FetchFrameOutput: Boolean read GetFetchFrameOutput write SetFetchFrameOutput;
    property NumKernels:integer read GetNumKernels write SetNumKernels;
    property DfltZDepth:integer read GetDfltZDepth write SetDfltZDepth;
    property InitStageData: String read GetInitStageData write SetInitStageData;

  end;


procedure SetOutputArrayValue(NodeName:String;AValue:String);
procedure SetStageArrayValue(NodeName:String;AValue:String);
function GetOutputArrayValue(NodeName:String):T3DNumArray;
function GetOutputArrayString(NodeName:String):String;
function GetStageArrayValue(NodeName:String):T3DNumArray;
function GetStageArrayString(NodeName:String):String;

var
  gpujs:String;                 // contents of resource file gpu.js

implementation

const MyNodeType='TXGPUCanvas';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXGPUCanvas.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('OnStart');
 // MyEventTypes.Add('OnNewFrame');
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
      self.StopMyGPU;
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
    MType:=NewText[1];
    Delete(NewText,1,1);
    // converting the JSON string back to numeric array takes ages so just storing the
    // string for now.  Do the conversion when the array is wanted.
    // Send a cef message to fetch the new value of the frame output array
    // (Use the ArgumentList property if you need to pass some parameters.)
    TempMsg := TCefProcessMessageRef.New('getGPUData');
    TempMsg.ArgumentList.SetString(0,self.myNode.NodeName);

    myChromium.SendProcessMessage(PID_RENDERER, TempMsg);
  end;
end;
procedure TXGPUCanvas.GPUProcessMessageReceived(
  Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
var
  oText,sText:String;
  oarr,sarr:T3DNumArray;
begin
  case message.Name of
    'sendGPUarrays':
    begin
      //CefDebugLog('TXGPUCanvas.GPUProcessMessageReceived.');
      oText := message.ArgumentList.GetString(0);
      sText := message.ArgumentList.GetString(1);
      //just set attribute here
      //self.myNode.SetAttributeValue('SourceText',NewText);
      // convert the array string to 3d numeric array
     // EditAttributeValue('XMemo1','','ItemValue',NewText,false);
     self.GPUOutputString:=oText;
     self.GPUStageString:=sText;
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
    function GPUOutputHandler(ev) {
    if (ev.data.objid!=undefined) {
      //console.log("handle GPU outbound message "+ev.data.objid+"  "+ev.data.mtype);
      if (ev.data.mtype=="FrameOutput") {
        //console.log("handle FrameOutput message "+ev.data.outputArray);
        //console.log("handle FrameOutput message ");
        try {
          pas.XGPUCanvas.SetOutputArrayValue(ev.data.objid,ev.data.outputArray);
            }catch(err){alert(err.message);
        }
      }
      else if (ev.data.mtype=="StageOutput") {
         //console.log("handle StageOutput message "+ev.data.stageArray);
         //console.log("handle StageOutput message ");
         try {
           pas.XGPUCanvas.SetStageArrayValue(ev.data.objid,ev.data.stageArray);
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
//function TXGPUCanvas.GetFetchFrameOutput:Boolean;
//begin
//  result:=myStrToBool(myNode.getAttribute('FetchFrameOutput',true).AttribValue);
//end;
function TXGPUCanvas.GetParamNumList:string;
begin
  result:=myNode.getAttribute('ParamNumList',true).AttribValue;
end;
function TXGPUCanvas.GetConstIntList:string;
begin
  result:=myNode.getAttribute('ConstIntList',true).AttribValue;
end;
//function TXGPUCanvas.GetParamImgList:string;
//begin
//  result:=myNode.getAttribute('ParamImgList',true).AttribValue;
//end;
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
function TXGPUCanvas.GetDfltZDepth:integer;
begin
  result:=StrToInt(myNode.getAttribute('DfltZDepth',true).AttribValue);
end;
function TXGPUCanvas.GetInitStageData:string;
begin
  result:=myNode.getAttribute('InitStageData',true).AttribValue;
end;

function TXGPUCanvas.FullParamList:String;
var
  plist:String;
begin
  plist:='';
  if self.ParamNumList<>'' then plist:=','+self.ParamNumList;
 // if self.ParamIntList<>'' then plist:=plist+','+self.ParamIntList;
//  if self.ParamImgList<>'' then plist:=plist+','+self.ParamImgList;
  result:=plist;
end;

function TXGPUCanvas.GPUJSCode(AnimCode:TStringList;dims:TDimsArray):String;
var
  str,vstr,cma, plist, KName:String;
  i,j,d:integer;
  vn:TNumArray;
  vi:TIntArray;
//  vb:TImgArray;
  h,w,n:integer;
begin
  //numKernels:=2;
  if AnimCode.Count<1 then
  begin
    showmessage('Error: Unable to find any animation code block(s)');
    result:='';
    EXIT;
  end;
  if AnimCode.Count<numKernels+1 then
  begin
    showmessage('Error: Unable to find '+inttostr(numKernels+1)+' animation code blocks');
    result:='';
    EXIT;
  end;

  str:=
//  'document.domain = "/abc"; ' + LineEnding
  'document.title = "'+myNode.NodeName+' '+myNode.NodeType+'"; ' + LineEnding
  +'/*/ ------------------------------------ Initialise the GPU ---------------------------------/*/ ' + LineEnding
  +'     const '+self.MyNode.NodeName+' = new GPU({mode: ''gpu''});   '+LineEnding
  +'     let running=true; '+LineEnding;
  str:= str + 'let outputArrayString = ''[]'';'+LineEnding;

  str:= str + '/*/ -------------------------------- Initialise Parameters List -------------------------/*/ ' + LineEnding;
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
    str:=str+'   let '+ParamNumArray[i].ParamName+' = '+vstr+';' +LineEnding;
  end;

  //!! cef
  // cef.  write to console log to trigger a cef event...
  str:=str
  +'function PostMessageOutputArray(objid, cval) {'  + LineEnding
  +'   var oa = document.getElementById("oarr");' + LineEnding
  +'   if (oa==null) {oa=document.createElement("DIV"); oa.id="oarr"; ' + LineEnding
  +'     oa.style.display="none";' + LineEnding
  +'     document.body.appendChild(oa);}' + LineEnding
  +'   var sa = document.getElementById("sarr");' + LineEnding
  +'   if (sa==null) {sa=document.createElement("DIV"); sa.id="sarr"; ' + LineEnding
  +'     sa.style.display="none";' + LineEnding
  +'     document.body.appendChild(sa);}' + LineEnding
  +'   if (running) {oa.innerHTML=outputArrayString;' + LineEnding
  +'                 sa.innerHTML=JSON.stringify(stageArray);' + LineEnding
  {$ifndef JScript}
  {$ifdef Chromium}
  +'                 console.log("'+myNode.NodeName+'O ");' + LineEnding
  {$endif}
  {$else}
  +'    window.parent.postMessage({"objid":objid,"mtype":"FrameOutput","outputArray":oa.innerHTML},"*"); ' + LineEnding;
  if numKernels>0 then
    str:=str+'    window.parent.postMessage({"objid":objid,"mtype":"StageOutput","stageArray":sa.innerHTML},"*"); ' + LineEnding
  {$endif}
  ;
  str:=str+'} }'+LineEnding;


//     called from the FrameDone function.
//  if self.FetchFrameOutput=true then
    str:=str+'  function GetPixelArray(kernel) { ' + LineEnding
    //+'return(kernel.getPixels()); ' + LineEnding          // needs gpujs v2

    +'const theImage = kernel.getCanvas();  ' + LineEnding
    +'if (theImage==null) {console.log("'+self.MyNode.NodeName+'BrowserCanvas is null"); } ' + LineEnding
    +'else { '  + LineEnding
    //   +'  console.log(''image height = ''+theImage.height); ' + LineEnding
    +'  tmpcanvas = document.createElement(''canvas'');  ' + LineEnding
    +'  tmpcanvas.width=theImage.width;  ' + LineEnding
    +'  tmpcanvas.height=theImage.height;  ' + LineEnding
    +'  var ctx = tmpcanvas.getContext("2d");  ' + LineEnding
    +'  ctx.drawImage(theImage, 0, 0); ' + LineEnding
    +'  if (ctx==null) {console.log("Context is null"); } ' + LineEnding
    +'  else { '  + LineEnding
    +'    var  theImageData = ctx.getImageData(0,0,theImage.width,theImage.height);' + LineEnding
    //   +'    console.log("theImageData="+theImageData.data);' + LineEnding
    //   +'    console.log("height="+theImage.height+" width="+theImage.width+" length="+theImageData.data.length);' + LineEnding
    +'    var arrayString = "["; ' + LineEnding
    +'    for (var h=0; h<theImage.height; h++) ' + LineEnding
    +'    {   ' + LineEnding
    +'      if (h>0) { arrayString = arrayString + ","; }' + LineEnding
    +'      var arrayString = arrayString + "["; ' + LineEnding
    +'      for (var w=0; w<theImage.width; w++) ' + LineEnding
    +'      {   ' + LineEnding
    +'        // note the pixel colours cycle around in groups of four ' + LineEnding
    +'          var i=(h*theImage.width*4)+(w*4); ' + LineEnding
    +'          var red = theImageData.data[i];  ' + LineEnding
    +'          var green = theImageData.data[i+1]; ' + LineEnding
    +'          var blue = theImageData.data[i+2];  ' + LineEnding
    +'          var alpha = theImageData.data[i+3]; ' + LineEnding
    +'          if (w>0) { arrayString = arrayString + ","; }' + LineEnding
    +'          arrayString = arrayString + "[" + red + "," + green + "," + blue + "," + alpha + "]";' + LineEnding
    +'      }   ' + LineEnding
    +'      arrayString = arrayString + "]"; ' + LineEnding
    +'    }   ' + LineEnding
    +'    tmpcanvas.remove(); ' + LineEnding
    +'    arrayString = arrayString + "]"; ' + LineEnding
    +'    return(arrayString); ' + LineEnding
    +'  }' + LineEnding
    +'  }' + LineEnding
    +'}';


  plist:=self.FullParamList;
  KName:=self.MyNode.NodeName+'CanvasRenderFn';

  // Create the required set of non-graphical kernels.
  // All these kernels operate with the same set of parameters.
  for n:=0 to numKernels-1 do
  begin
    str:=str
    +'     /*/------------ start of create Kernel routine -------/*/ ' + LineEnding
    +'     const '+KName+inttostr(n)+' = '+self.MyNode.NodeName+'.createKernel(function(myArray,AnimationCounterValue'+plist
    +') { ' + LineEnding;
    str:=str + 'var myValue=0.0;' + LineEnding;
    str:=str + AnimCode[n+1];
    str:=str + 'return myValue;' + LineEnding;         // this goes into the relevant x,y,z position in outputArray
    str:= str
    +'     })  '+LineEnding
    +'  /*/------- end of create Kernel routine ---------------/*/ ' + LineEnding
//    +'       .setOutput(['+intToStr(w)+','+ intToStr(h)+','+inttostr(ZDim)+']) ' + LineEnding   // 3-D output
    +'       .setOutput([';
    for d:=0 to length(self.Dimensions[n+1])-1 do
    begin
      if d>0 then str:=str+',';
      str:= str + inttostr(self.Dimensions[n+1,d]);
    end;
    str:= str
    +']) ' + LineEnding
    +'       .setGraphical(false)             ' + LineEnding;
    // integer parameters are loaded as constants
    if length(self.ConstIntArray)>0 then
    begin
      str:=str
      +'       .setConstants({';
      for i:=0 to length(self.ConstIntArray)-1 do
      begin
        if i>0 then str:=str+', ';
        str:=str
        +self.ConstIntArray[i].ConstName+': '+inttostr(self.ConstIntArray[i].ConstValue);
      end;
      str:=str
      +'       }); ' + LineEnding;
    end;

  end;

//.......... Create a final Kernel (Graphical) ..............
h:=self.ActualHeight;
w:=self.ActualWidth;

str:=str
+'     /*/------------ start of create Kernel routine -------/*/ ' + LineEnding
+'     const '+KName+'G = '+self.MyNode.NodeName+'.createKernel(function(myArray,AnimationCounterValue'+plist
+') { ' + LineEnding
+'       var r = 0  ;      ' + LineEnding
+'       var g = 0  ; /*/--initalise the default colour for the GPUCanvas pixel in r,g,b,a format --/*/  ' + LineEnding
+'       var b = 0  ;   ' + LineEnding
+'       var a = 1  ;  ' + LineEnding;
str:=str + AnimCode[0];
str:= str
+'             this.color((r),(g),(b),(a));        ' + LineEnding
+'     })  '+LineEnding
+'  /*/------- end of create Kernel routine ---------------/*/ ' + LineEnding
+'       .setOutput(['+intToStr(w)+','+ intToStr(h)+'])              ' + LineEnding
+'       .setLoopMaxIterations(['+IntToStr(self.MaxIterations)+'])   ' + LineEnding
+'       .setGraphical(true)                                         ' + LineEnding;
// integer parameters are loaded as constants
if length(self.ConstIntArray)>0 then
begin
  str:=str
  +'       .setConstants({';
  for i:=0 to length(self.ConstIntArray)-1 do
  begin
    if i>0 then str:=str+', ';
    str:=str
    +self.ConstIntArray[i].ConstName+': '+inttostr(self.ConstIntArray[i].ConstValue);
  end;
  str:=str
  +'       }); ' + LineEnding;
end;


  str:=str
  +'     /*/-------------------Run the nested Kernel codes ----------------------/*/    ' + LineEnding
  +'     let AnimationCounterValue='+IntToStr(self.StartIteration)+'; '                                         +LineEnding
  +'     let AnimationCounterMax='+IntToStr(self.NumFrames)+'; '                                         +LineEnding;

//combineKernels(k,k,...,lastKernel,combinedKernel)
  if numKernels>0 then
  begin
    str:=str
    +'     const superKernel = '+self.MyNode.NodeName+'.combineKernels('+LineEnding;

    for n:=0 to numKernels-1 do
    begin
      str:=str
      +'                                     '+KName+inttostr(n)
      +', ';
      str:=str+LineEnding;
    end;
    str:=str
      +'                                     function(myArray,AnimationCounterValue'+plist+') {'+LineEnding
      +'                                      var rslt='+LineEnding;
    for n:=numKernels-1 downto 0 do
    begin
      str:=str
      +KName+inttostr(n)+'( '+LineEnding;
      if n=0 then
        str:=str+'myArray,';
    end;
    for n:=0 to numKernels-1 do
    begin
      str:=str
      +'AnimationCounterValue'+plist+')'+LineEnding;
      if n<numKernels-1 then
        str:=str+','
      else
        str:=str+';';
    end;
    str:=str
//    +'                                                         AnimationCounterValue'+plist+');'+LineEnding
    +'      return rslt;                                 });'+LineEnding;
  end;

  // Initialise the stageArray...   [3D array of real]
  // Initial values come from the 'XGPU3DTable' component (data held in property InitStageData)
  str:=str
  +'     let stageArray='+StringUtils.DelChars(self.InitStageData,'"')+';   '+LineEnding;

  // Run the combined non-graphical kernels...
  if numKernels>0 then
    str:=str
    +'     stageArray = superKernel(stageArray,AnimationCounterValue'+plist+');  '+LineEnding;

  str:=str
  +'     /*/-------------------Run the Graphical Kernel code and place it on the web page----------------------/*/    ' + LineEnding
  +'     '+KName+'G(stageArray,AnimationCounterValue'+plist+');               ' + LineEnding;
//  if self.FetchFrameOutput=true then
    str:=str
    +'    outputArrayString = GetPixelArray('+self.MyNode.NodeName+'CanvasRenderFnG); ' + LineEnding
    +'    PostMessageOutputArray("'+myNode.NodeName+'",AnimationCounterValue);' + LineEnding;

  // Put the GPU bitmap on the page...
  str:=str
  +'     var '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFnG.getCanvas();  ' + LineEnding
//  +'     let '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFnG.canvas;  ' + LineEnding     // works in gpujs v2
  +'     '+self.MyNode.NodeName+'BrowserCanvas.height = '+intToStr(h)+';  ' + LineEnding
  +'     document.getElementsByTagName("body")[0].appendChild('+self.MyNode.NodeName+'BrowserCanvas);     ' + LineEnding ;

  str:=str
  +'  function GetMessage(msg) {'  + LineEnding
  +'    alert("msg="+msg);  '  + LineEnding
  +'}'  + LineEnding ;

  str:=str
  +'  function RunCode(theCode) {'  + LineEnding
  +'    eval(theCode);  '  + LineEnding
  +'}'  + LineEnding ;

  {$ifdef JScript}
//    // handle an inbound message of format:{"objid":<id>, "mtype":"SetParam", "pName":<pName>, "pValue":<pValue>}
    str:=str
    +'  window.addEventListener("message", function(ev) { '+Lineending
                + 'if (ev.data.objid!=undefined) { '  +LineEnding
//                + '  console.log("handle GPU inbound message "+ev.data.objid+"  "+ev.data.mtype); '+LineEnding
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
  {$endif}

//EditAttributeValue('XMemo1','','ItemValue',str);        //!!!! temporary for debugging

result:=str;



end;

procedure SetOutputArrayValue(NodeName:String;AValue:String);
var
  myNode:TDataNode;
begin
//  {$ifdef JScript}
//  asm
//  console.log('SetOutputArrayValue '+NodeName+' '+AValue);
//  //console.log('SetOutputArrayValue '+NodeName);
//  end;
//  {$endif}
  // set the property value for the relevant XGPUCanvas object
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,'',true);
  if myNode<>nil then
  begin
//    myNode.SetAttributeValue('OutputArray',AValue);
    {$ifndef JScript}
    TXGPUCanvas(myNode.ScreenObject).GPUOutputString:=AValue;
    {$else}
    TXGPUCanvas(myNode).GPUOutputString:=AValue;
    {$endif}
  end;
end;

procedure SetStageArrayValue(NodeName:String;AValue:String);
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
    {$else}
    TXGPUCanvas(myNode).GPUStageString:=AValue;
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
    {$ifdef Chromium}
    result:=TXGPUCanvas(myNode.ScreenObject).GPUOutputString;
    {$else}
    result:=TXGPUCanvas(myNode.ScreenObject).GPUOutputString;
    {$endif}
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
    {$ifdef Chromium}
    TXGPUCanvas(myNode.ScreenObject).GPUStageArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUStageString);
    result:=TXGPUCanvas(myNode.ScreenObject).GPUStageArray;
    {$else}
    TXGPUCanvas(myNode.ScreenObject).GPUStageArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUStageString);
    result:=TXGPUCanvas(myNode.ScreenObject).GPUStageArray;
    {$endif}
    {$else}
    TXGPUCanvas(myNode.ScreenObject).GPUStageArray:=JSONStringTo3DNumArray(TXGPUCanvas(myNode.ScreenObject).GPUStageString);
    result:=TXGPUCanvas(myNode).GPUStageArray;
    {$endif}
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
    {$ifdef Chromium}
    result:=TXGPUCanvas(myNode.ScreenObject).GPUStageString;
    {$else}
    result:=TXGPUCanvas(myNode.ScreenObject).GPUStageString;
    {$endif}
    {$else}
    result:=TXGPUCanvas(myNode).GPUStageString;
    {$endif}
  end;
end;

function GetParam(NodeName:String; pindex:integer):Float;
begin
  showMessage('GetParam');
  result:=0;
end;

function TXGPUCanvas.GPUJSAnimationFooter:String;
// This code is added only if the 'Animated' property is true...
var
  str,cma,plist:String;
begin

  plist:=self.FullParamList;

  str:='/*/------------------ Now Animate the Graphics ------------------------------------------------/*/' + LineEnding;


// The FrameDone function is called after each animation frame is finished.
// It is an async function, so that inbound messages are picked up,
// and we can use postMessage, eg. for passing
// values back out from the IFrame into the project environment.
  str:=str
  +'  function FrameDone(kernel) {'  + LineEnding;
//  if self.FetchFrameOutput=true then
//  begin
    str:=str+'    if (running) {outputArrayString = GetPixelArray(kernel); ' + LineEnding
            +'    PostMessageOutputArray("'+myNode.NodeName+'",AnimationCounterValue);}' + LineEnding;
//  end;
  str:=str+'    return new Promise(resolve => { '  + LineEnding
  +'  }); } '+ LineEnding;

    str:=str
   +'  async function DoFrame() {  '  + LineEnding;
   if numKernels>0 then
   str:=str
   +'            stageArray=superKernel(stageArray,AnimationCounterValue'+plist+');  '+LineEnding
//   +'            PostMessageStageArray("'+myNode.NodeName+'",AnimationCounterValue,JSON.stringify(stageArray));' + LineEnding
   ;
   str:=str
   +'           '+self.MyNode.NodeName+'CanvasRenderFnG(stageArray,AnimationCounterValue'+plist+'); ' + LineEnding
//   +'updateTheCanvas();  '+LineEnding
   +'            AnimationCounterValue = AnimationCounterValue +1; '  + LineEnding
   +'            if (AnimationCounterValue > AnimationCounterMax) {AnimationCounterValue = 0};  '  + LineEnding
   +'            var xx = await FrameDone('+self.MyNode.NodeName+'CanvasRenderFnG) ; '  + LineEnding
   +'  } '  + LineEnding;

   str:=str
   +'  var AnimationFrameID;  '  + LineEnding
   +'  var GPUIntervalRunner;  '  + LineEnding
   +'  function animate(timestamp){  '  + LineEnding
   +'    GPUIntervalRunner=setInterval(DoFrame, 1000/'+IntToStr(self.MaxFramesPerSec)+'); ' + LineEnding
   +'    } '  + LineEnding
   +'  AnimationFrameID=requestAnimationFrame(animate) ; '  + LineEnding;

  result:=str;
end;

function TXGPUCanvas.FetchAllAnimCode:TAnimCodeArray;
var
  allcode:TAnimCodeArray;
  bits,cdbits,dimbits:TStringList;
  n,d:integer;
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

  for n:=0 to bits.count-1 do
  begin
    if length(allcode)<=n then
      setlength(allcode,n+1);
    // get the code block for this kernel...
    allcode[n].CodeBlock:=TStringList.Create;
    cdbits:=StringSplit(bits[n],EventAttributeDelimiter);
    allcode[n].CodeBlock.Text:=cdbits[0];
    if cdbits.Count<2 then
    begin
      cdbits.Add('');
    end;
    if trim(cdbits[1])='' then
      if n>0 then
        cdbits[1]:=inttostr(self.ActualWidth)+','+inttostr(self.ActualHeight)+','+inttostr(self.DfltZDepth)
      else
        // output for the graphical kernel is current pixel size
        cdbits[1]:=inttostr(self.ActualWidth)+','+inttostr(self.ActualHeight);
    // get the output dimensions for this kernel...
    allcode[n].KDimensionsStr:=cdbits[1];

    dimbits:=StringSplit(cdbits[1],',');
    if dimbits.count>3 then
      showmessage('Warning: Kernel outputs must be max 3 dimensions');
    setlength(allcode[n].KDimensions,dimbits.count);
    try
    for d:=0 to min(dimbits.count,3)-1 do
      allcode[n].KDimensions[d]:=strtoint(dimbits[d]);
    except
      showmessage('Warning: Found non-integer Kernel output dimensions in GPUCanvas '+self.myNode.NodeName);
    end;
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

    setlength(allcode[n].KDimensions,3);
    allcode[n].KDimensionsStr:=inttostr(self.ActualWidth)+','+inttostr(self.ActualHeight)+','+inttostr(self.DfltZDepth);
    allcode[n].KDimensions[0]:=self.ActualWidth;
    allcode[n].KDimensions[1]:=self.ActualHeight;
    allcode[n].KDimensions[2]:=self.DfltZDepth;
  end;

  // In case this has adjusted the data, save the concatenated code block again to the AnimationCode property
  RevisedAnimCode:='';
  setlength(self.Dimensions,numKernels+1);
  for n:=0 to numKernels do
  begin
    if n>0 then
      RevisedAnimCode:=RevisedAnimCode + eventListdelimiter;
    RevisedAnimCode:=RevisedAnimCode + allcode[n].CodeBlock.Text;
    RevisedAnimCode:=RevisedAnimCode + EventAttributeDelimiter;
    RevisedAnimCode:=RevisedAnimCode + allcode[n].KDimensionsStr;
    // store numeric dimensions data with the GPUCanvas component
    setlength(self.Dimensions[n],length(allcode[n].KDimensions));
    for d:=0 to length(allcode[n].KDimensions)-1 do
    begin
      self.Dimensions[n,d]:=allcode[n].KDimensions[d];
    end;
  end;
  self.myNode.SetAttributeValue('AnimationCode',RevisedAnimCode);
  result:=allcode;
end;

function TXGPUCanvas.BuildPascalAnimationUnit(Compiler:TObject):String;
// Wrap the user-supplied code from the AnimationCode property in a unit, for compilation to JavaScript by pas2js.
var
  PascalHeader:TStringList;
  UserCodeParameterList:String;
  AllAnimationCode:TAnimCodeArray;
  TheAnimationCode:TStringList;
  vstr:String;
  v:TNumArray;
  i,j,n:integer;
begin
  PascalHeader:=TStringList.Create;
  PascalHeader.Add(' unit GPUCode; ');
  PascalHeader.Add('{$ifdef Dll}{$mode objfpc}{$H+}{$R+}{$endif}');
  PascalHeader.Add('interface');
  PascalHeader.Add('uses Classes, SysUtils, Math;');
  PascalHeader.Add(' type ');
  PascalHeader.Add('     TNumArray = array of real;');
 // PascalHeader.Add('     T3DNumArray = array of array of array of real;');
  PascalHeader.Add('     TImgArray = array of string;');
  PascalHeader.Add('     TConstantsRecord=record');
  for i:=0 to length(self.ConstIntArray)-1 do
  begin
   PascalHeader.Add('       const '+self.ConstIntArray[i].ConstName+':integer='+inttostr(self.ConstIntArray[i].ConstValue)+';');
  end;
  PascalHeader.Add('     end;');
  PascalHeader.Add('     TXThread = record	');
  PascalHeader.Add('       x,y,z:integer;	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('     TGPUThread = class	');
  PascalHeader.Add('        thread:TXThread;	');
  PascalHeader.Add('        constants:TConstantsRecord;     	');
  PascalHeader.Add('        procedure color(r,g,b,a:real); virtual; abstract;	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('     TFuncNotSupported = record	');
  PascalHeader.Add('          dummy:string; 	');
  PascalHeader.Add('     end;	');
  PascalHeader.Add('   implementation	');
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
  PascalHeader.Add('   function tan(d: TFuncNotSupported):TFuncNotSupported;	');
  PascalHeader.Add('   begin	');
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
//  for i:=0 to length(self.ParamImgArray)-1 do
//  begin
//    PascalHeader.Add('   var '+ParamImgArray[i].ParamName+':TImgArray;');
//  end;
  PascalHeader.Add('   var  this:TGPUThread;');
  PascalHeader.Add('   type  T2DArray=Array of Array of real;');
  PascalHeader.Add('   type  TmyArray=Array of T2DArray;');
  PascalHeader.Add('   var  myArray:TMyArray;');
  PascalHeader.Add('   var  myValue:real;');

  PascalHeader.Add('   procedure PascalVersionOfGPUCode(AnimationCounterValue:integer');
  if UserCodeParameterList<>'' then
    PascalHeader.Add(';'+UserCodeParameterList);
  PascalHeader.Add(');	');

  PascalHeader.Add(' var       r,g,b,a:real;');
//  PascalHeader.Add('        kkkkk:integer;');
  PascalHeader.Add('        zzzzz1:integer;') ;          // used as position marker in resulting JS script
  //.............. user code block goes here .................
  TheAnimationCode:=TStringList.Create;

  AllAnimationCode:=self.FetchAllAnimCode;

 // TheAnimationCode.Text:=self.AnimationCode;
  for n:=0 to length(AllAnimationCode)-1 do
  begin
    if n=0 then
      PascalHeader.Add('var kkkkk'+inttostr(n)+':integer;')
    else
      PascalHeader.Add('procedure kkkkk'+inttostr(n)+';');
    TheAnimationCode.Text:=AllAnimationCode[n].CodeBlock.Text;
    WriteIncFile(Compiler,myNode.NodeName+'.'+inttostr(n),'','tempinc/',PascalHeader,TheAnimationCode);
  end;
  //..........................................................
  PascalHeader.Add('var zzzzz2:integer;');              // used as position marker in resulting JS script


  PascalHeader.Add('begin');
  PascalHeader.Add('end.');

  //showmessage(PascalHeader.Text);

  result:=PascalHeader.Text;
end;


function TXGPUCanvas.CompileGPUToJS(var GPUJSOutput:String):Boolean;
var
  ProgPath, PasFileName,ObjectFileName,ExeFileName:String;
  UnitString:String;
  Res,ok:Boolean;
  args:TStringList;
  prog:TStringList;
  {$ifdef JScript}
  lWebFS : TPas2JSWebFS;
  {$endif}
begin


  {$ifndef JScript}
  UnitString:=BuildPascalAnimationUnit(nil);

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

  TranspileMyProgram(self.myNode.NodeName,ExtractFilePath(Application.ExeName),'tempinc/',GPUCodeEditor,true);
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
  //showmessage('starting compile section...');
  //with MyWebCompiler do
  begin
    UnitString:=BuildPascalAnimationUnit(MyWebCompiler.Compiler);

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

  prog.free;

  //....decide if there are errors or not .......
  //if ok=false then showmessage('Compilation failed')
  //else showmessage('Compilation successful');
  result:=ok;
end;

function TXGPUCanvas.CompileAnimCode:TStringList;
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
end;

function TXGPUCanvas.FullXMLString:String;
var
  Pas2JSTrimmed:TstringList;   // one string per kernel
  tmp:String;
  FullString:String;
//  Dims:TDimsArray;
begin
  {$ifndef JScript}
  if not (csDesigning in ComponentState) then
  {$endif}
  begin
    Pas2JSTrimmed:=self.CompileAnimCode;
    if Pas2JSTrimmed.Count<1 then
    begin
      result:='';
      EXIT;
    end;

    // Get the set of kernel output dimensions
//    Dims:=self.Dimensions;

    // and wrap it with the GPU JS...
    tmp:= GPUJSCode(Pas2JSTrimmed,self.Dimensions);
    FullString:= tmp;
    if self.Animated then
      FullString:=FullString + GPUJSAnimationFooter;


    tmp:=UnSubstituteSpecials(gpujs);

    FullString:=
      '<!DOCTYPE html>' +  LineEnding
      +'<html>' +  LineEnding
      +'<body  style="margin:0px; font:normal 12px Verdana, Arial, sans-serif;">' +  LineEnding
      +'<script>'+tmp+'</script>  ' +  LineEnding
      +'<div  id="GPUCanvas" > '+  LineEnding
      +'<script>'+  LineEnding
      +FullString +  LineEnding
      +'</script>' +  LineEnding
//      +'<div id=oarr hidden> </textarea>' + LineEnding
      +'</div> '+  LineEnding
      +'</body> ' +  LineEnding
      +'</html> '+  LineEnding;

//    EditAttributeValue('XMemo1','','ItemValue',FullString,false);        //!!!! temporary for debugging
    self.GeneratedHTML:=FullString;

  end;
  result:=FullString;
end;


procedure TXGPUCanvas.setupGPUPage;
var
  GPUString:String;
begin
  // called from StartMyGPU
  GPUString:=self.FullXMLString;
  self.HTMLSource:= GPUString;
end;

function TXGPUCanvas.GetParamNumValue(pName:String):TNumArray;
var
  i:integer;
  tmp:String;
  pval:TNumArray;
begin
  for i:=0 to length(ParamNumArray)-1 do
    if ParamNumArray[i].ParamName=pName then
    begin
      pval:=ParamNumArray[i].ParamValue;
    end;
  result:=pval;
end;

function TXGPUCanvas.GetConstIntValue(pName:String):integer;
var
  i:integer;
  tmp:String;
  pval:integer;
begin
  for i:=0 to length(ConstIntArray)-1 do
    if ConstIntArray[i].ConstName=pName then
    begin
      pval:=ConstIntArray[i].ConstValue;
    end;
  result:=pval;
end;

//function TXGPUCanvas.GetParamImgValue(pName:String):TImgArray;
//var
//  i:integer;
//  tmp:String;
//  pval:TImgArray;
//begin
//  for i:=0 to length(ParamImgArray)-1 do
//    if ParamImgArray[i].ParamName=pName then
//    begin
//      pval:=ParamImgArray[i].ParamValue;
//    end;
//  result:=pval;
//end;

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

procedure TXGPUCanvas.SetConstIntValue(pName:String;pValue:integer);
var
  i:integer;
  tmp:String;
  myurl:string;
begin
  for i:=0 to length(ConstIntArray)-1 do
    if uppercase(ConstIntArray[i].ConstName)=uppercase(pName) then
    begin
      ConstIntArray[i].ConstValue:=pValue;

    end;
end;

(*procedure TXGPUCanvas.SetParamImgValue(pName:String;pValue:TImgArray;ForwardToWidget:Boolean);
var
  i,j:integer;
  tmp:String;
  myurl:string;
begin
  for i:=0 to length(ParamImgArray)-1 do
    if uppercase(ParamImgArray[i].ParamName)=uppercase(pName) then
    begin
      SetLength(ParamImgArray[i].ParamValue,length(pValue));
      for j:=0 to length(pValue)-1 do
        ParamImgArray[i].ParamValue[j]:=pValue[j];
      //ParamImgArray[i].ParamValue:=pValue;        // causing errors...?

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
          tmp:=ImgArrayToJSONString(pValue);
          tmp:='RunCode("'+pName+'='+tmp+';")';
          myChromium.Browser.MainFrame.ExecuteJavaScript(tmp, myurl, 0);
        end;
        {$else}
        //!!!!
        {$endif}
        //showmessage('update img param: '+tmp);
        {$else}
        asm
          var ob=document.getElementById(this.NameSpace+this.NodeName+'Contents');
          if (ob!=null) {
            //alert('found iframe. posting param message');
            ob.contentWindow.postMessage({"objid":this.NodeName, "mtype":"SetImgParam", "pName":pName, "pValue":pValue},"*");
            }
        end;
        {$endif}
      end;
    end;
end;
*)

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
      showmessage('SetAnimationCode RedisplayFrame');
      RedisplayFrame;

    end;
  end;
end;

procedure TXGPUCanvas.StartMyGPU;
var
  h,w:integer;
begin
  // refresh the actual h/w attributes
  h:=self.ActualHeight;
  w:=self.ActualWidth;

  setlength(self.GPUOutputArray,0);

  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    HandleEvent('OnStart',self.myNode.NodeName,self.myNode.NameSpace,'');
    SetupGPUPage;
  end;
end;

procedure TXGPUCanvas.StopMyGPU;
var
  tmp,doJS,myurl:String;
begin
  {$ifndef JScript}
  if not (csDesigning in componentState) then
  {$endif}
  begin
    // stop the gpu loop
    doJS:='running=false; clearInterval(GPUIntervalRunner);';

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
    //!!!!  external browser
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
      self.StopMyGPU;
  end;
end;


procedure TXGPUCanvas.SetAnimated(AValue:Boolean);
begin
  myNode.SetAttributeValue('Animated',myBoolToStr(AValue),'Boolean');
end;
//procedure TXGPUCanvas.SetFetchFrameOutput(AValue:Boolean);
//begin
//  myNode.SetAttributeValue('FetchFrameOutput',myBoolToStr(AValue),'Boolean');
//end;
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
      ParamNumArray[i].ParamName:=pNames[i];
      SetLength(ParamNumArray[i].ParamValue,1);
      ParamNumArray[i].ParamValue[0]:=0;
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

(*procedure TXGPUCanvas.SetParamImgList(AValue:string);
var
  pNames:TStringList;
  i:integer;
begin
  myNode.SetAttributeValue('ParamImgList',AValue,'String');
  SetLength(ParamImgArray,0);

  //use this comma-delimited list to initialise ParamArray.
  if AValue<>'' then
  begin
    pNames:=TStringList.Create;
    pNames.StrictDelimiter:=true;
    pNames.LineBreak:=',';
    pNames.Text:=AValue;

    SetLength(ParamImgArray,pNames.Count);
    for i:=0 to pNames.Count-1 do
    begin
      ParamImgArray[i].ParamName:=pNames[i];
      SetLength(ParamImgArray[i].ParamValue,1);
      //ParamImgArray[i].ParamValue[0]:=0;       //!!!!?
    end;
    pNames.Free;
  end;

end; *)

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
//procedure TXGPUCanvas.SetOutputArray(AValue:String);
//begin
//  myNode.SetAttributeValue('OutputArray',AValue,'String');
//end;
//procedure TXGPUCanvas.SetGPUStageArray(AValue:String);
//begin
//  myNode.SetAttributeValue('GPUStageArray',AValue,'String');
//end;
procedure TXGPUCanvas.SetNumKernels(AValue:integer);
begin
  myNode.SetAttributeValue('NumKernels',IntToStr(AValue),'Integer');
end;
procedure TXGPUCanvas.SetDfltZDepth(AValue:integer);
begin
  myNode.SetAttributeValue('DfltZDepth',IntToStr(AValue),'Integer');
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
//  AddDefaultAttribute(myDefaultAttribs,'ParamImgList','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'ConstIntList','String','','List of integer constants to be passed in to kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxIterations','Integer','512','Any loops defined inside a kernel must have a maximum iteration count defined by MaxIterations',false);
  AddDefaultAttribute(myDefaultAttribs,'StartIteration','Integer','1','Initial value for AnimationCounterValue',false);
  AddDefaultAttribute(myDefaultAttribs,'NumFrames','Integer','100','Number of frames after which AnimationCounterValue resets to 0',false);
  AddDefaultAttribute(myDefaultAttribs,'MaxFramesPerSec','Integer','10','Target frame rate (may be slower if kernel processing times are long)',false);
//  AddDefaultAttribute(myDefaultAttribs,'FetchFrameOutput','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'AnimationCode','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'NumKernels','Integer','0','Number of nested non-graphical kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'DfltZDepth','Integer','1','Default Number of z-planes in the stage arrays handled by non-graphical kernels',false);
  AddDefaultAttribute(myDefaultAttribs,'InitStageData','String','[[["1"]]]','3D Array string for input to the first non-graphical kernel',false);
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

//procedure TXGPUCanvas.ChromiumProcessMessageReceived(
//  Sender: TObject; const Browser: ICefBrowser;
//  sourceProcess: TCefProcessId;
//  const message: ICefProcessMessage; out Result: Boolean);
//var
//  ParamText:String;
//begin
//  case message.Name of
//    XGPUCANVAS_SEND_PARAMS:
//    begin
//      ParamText := message.ArgumentList.GetString(0);
//      SetParamsFromMessage(ParamText);
//    end
//  else
//    inherited;
//  end;
//end;
//
(*
procedure DOMVisitor_OnDocAvailable_TXGPUFrame(const browser: ICefBrowser; const frame: ICefFrame; const document: ICefDomDocument);
var
  msg: ICefProcessMessage;
  txt:String;
begin
  // CefDebugLog('DOMVisitor_OnDocAvailable_TXGPUFrame', CEF_LOG_SEVERITY_INFO );
  // Simple DOM searches
  //txt:=SimpleNodeSearch(document,'TXHTMLEditor','my_wysiwyg_editor');
  txt:='hello world';
  // Send back results to the browser process
  // Notice that the XHTMLEDITOR_SEND_TEXT message name needs to be recognized in
  // Chromium OnProcessMessageReceived method
  msg := TCefProcessMessageRef.New('sendoutputarray');
  msg.ArgumentList.SetString(0, txt);
  frame.SendProcessMessage(PID_BROWSER, msg);
end; *)
(*
procedure GPU_OnProcessMessageReceived(const browser       : ICefBrowser;
                                                const frame       : ICefFrame;
                                                      sourceProcess : TCefProcessId;
                                                const message       : ICefProcessMessage;
                                                var   aHandled      : boolean);
var
  TempFrame   : ICefFrame;
  TempVisitor : TCefFastDomVisitor2;
begin
  // Handle messages that are sent INTO this cef renderer
  aHandled := False;
  if (browser <> nil) then
    begin
      //CefLog('CEFXUtils OnProcessMessageReceived. ', 1, CEF_LOG_SEVERITY_ERROR, message.name);
      CefDebugLog('CEFXUtils OnProcessMessageReceived. ');
      if (message.name = 'getoutputarray')  then
      begin
      TempFrame:=frame;
      if TempFrame<>nil then
      begin
        cefDebugLog('VisitDom...');
        TempVisitor := TCefFastDomVisitor2.Create(browser, TempFrame, @DOMVisitor_OnDocAvailable_TXGPUFrame);
        TempFrame.VisitDom(TempVisitor);
      end;
        aHandled := True;
      end;
    end;
end;
*)

(*
// Handler for messages sent OUT of the Cef browser
procedure TXGPUCanvas.ChromiumProcessMessageReceived(
  Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage; out Result: Boolean);
var
  NewText:String;
begin
  case message.Name of
    'sendoutputarray':
    begin
      NewText := message.ArgumentList.GetString(0);
      cefDebugLog('message received');
      //EditAttributeValue('XMemo1','ItemValue',NewText);
      //just set attribute here
      self.myNode.SetAttributeValue('OutputArray',NewText);
      //event here (eg) to refresh ob inspector
      if StartingUp=false then
        CallHandleEventLater('Change',NewText,self.myControl);

    end
  else
    inherited;
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

(*
procedure TXGPUCanvas.TitleChange(Sender: TObject;const cefBrowser:ICefBrowser;const NewTitle:UString) ;
var
  params:TStringList;
  c:integer;
  NewText:String;
  TempMsg : ICefProcessMessage;

  TempFrame   : ICefFrame;
  TempVisitor : TCefFastDomVisitor2;
begin
  // A frame display has ended.
  if (not (csDesigning in componentState))
  and (not StartingUp)
  and (self.myNode<>nil)
  and (cefBrowser<>nil)
  and (myChromium<>nil)
  and (FoundString(NewTitle,self.myNode.NodeName)=1)
  then
  begin
    // Send a cef message to fetch the new value of the frame output array
    // (Use the ArgumentList property if you need to pass some parameters.)
    TempMsg := TCefProcessMessageRef.New('getoutputarray');
    //TempMsg.ArgumentList.SetStringValue(0,);
    myChromium.SendProcessMessage(PID_RENDERER, TempMsg);
    cefDebugLog('TXGPUCanvas TitleChange '+NewTitle);

  end;
end;
*)
(*
  // !! Massive fudge here...
  // Using the non-graphical kernels prior to running the graphical kernel always results in the
  // output canvas being square.  So, here we crop the returned image to the expected dimensions
  // before displaying it.
  str:=str
+'  function updateTheCanvas() {'  + LineEnding
+'  let '+self.MyNode.NodeName+'BrowserCanvas = '+self.MyNode.NodeName+'CanvasRenderFnG.getCanvas();' + LineEnding
+'  let displayCanvas = document.getElementById("displayCanvas"); ' + LineEnding
+'  let ctx = displayCanvas.getContext("2d"); ' + LineEnding
+'  let ww='+self.MyNode.NodeName+'BrowserCanvas.width; ' + LineEnding
+'  let hh='+self.MyNode.NodeName+'BrowserCanvas.height; ' + LineEnding
+'  if (hh>'+inttostr(h)+') { ' + LineEnding
+'    ctx.drawImage(DepthPlotBrowserCanvas, 0, (hh-'+inttostr(h)+'),'+inttostr(w)+','+inttostr(h)+',0,0,'+inttostr(w)+','+inttostr(h)+'); }' + LineEnding
+'  else if (ww>'+inttostr(w)+') { ' + LineEnding
+'    ctx.drawImage(DepthPlotBrowserCanvas, (ww-'+inttostr(w)+'), 0,'+inttostr(w)+','+inttostr(h)+',0,0,'+inttostr(w)+','+inttostr(h)+'); }' + LineEnding
+'  else  { ' + LineEnding
+'    ctx.drawImage(DepthPlotBrowserCanvas, 0, 0); }' + LineEnding
+'  }'  + LineEnding;

  str:=str
  +'  const displayCanvas = document.createElement(''canvas'');  ' + LineEnding
  +'  displayCanvas.id = "displayCanvas";  ' + LineEnding
  +'  displayCanvas.width='+inttostr(w)+'; ' + LineEnding
  +'  displayCanvas.height='+inttostr(h)+'; ' + LineEnding
  +'  document.getElementsByTagName("body")[0].appendChild(displayCanvas); ' + LineEnding
  +'  updateTheCanvas(); ' + LineEnding;
 *)

(*
str:=str
+'  function PostMessageOutputArray(objid, cval, outputArray) {'  + LineEnding
//    +'  var OutObj = new JSONObject();    '  + LineEnding
//    +'  var jZ = new JSONArray();          '  + LineEnding
//    +'  for (var z=0; z<outputArray.length; z++) { '  + LineEnding
//    +'    var jY = new JSONArray();          '  + LineEnding
//    +'    jZ.put(jY);                              '  + LineEnding
//    +'    for (var y=0; y<outputArray[z].length; y++) { '  + LineEnding
//    +'      var jX = new JSONArray();             '  + LineEnding
//    +'      jY.put(jX);                                 '  + LineEnding
//    +'      for (var x=0; x<outputArray[y].length; x++) { '  + LineEnding
//    +'        jX.put(outputArray[z][y][x]);              '  + LineEnding
//    +'      }   '  + LineEnding
//    +'    }   '  + LineEnding
//    +'  }   '  + LineEnding

//    +'   var mObj = new Object;    '  + LineEnding
//    +'   mObj.Data = outputArray;  '  + LineEnding
//    +'   if (running) {console.log("'+myNode.NodeName+'O "+JSON.stringify(mObj));}' + LineEnding ;

//    +'   if (running) {console.log("'+myNode.NodeName+'S "+outputArray);}' + LineEnding ;
*)



