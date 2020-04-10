unit PyXUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StringUtils,
  {$ifndef JScript}
  Dialogs, Forms, StdCtrls, variants,
  PythonEngine, VarPyth, PythonGUIInputOutput, DllInterface,
  {$endif}
  XMemo, EventsInterface;

{$ifndef Python}
implementation
end.
{$else}
var
  PythonLibDir,PythonVersion:String;

{$ifndef JScript}
function PyodideScript:TStringList;
procedure SetupPyEngine(PyLibDir,PyVersion:String);
procedure DoPy_InitEngine;
{$endif}
procedure PyExeString(cmds: string);
procedure RunInitialScript;

//const
//  cPyLibraryWindows = 'C:\Python-for-Lazarus-master\python4lazarus\Runtime\python38.dll';
//  cPyLibraryLinux = 'libpython3.8m.so.1.0';
//  cPyLibraryMac = '/Library/Frameworks/Python.framework/Versions/3.8/lib/libpython3.8.dylib';
//  cPyZipWindows = 'python38.zip';
{$ifndef JScript}
type
  TMyPyEvents = class(TObject)
    procedure PyVarExtGetData(Sender: TObject; var Data: PPyObject);
    procedure PyVarExtSetData(Sender: TObject; Data: PPyObject);
    procedure PyVarEExtGetData(Sender: TObject; var Data: PPyObject);
    procedure PyVarEExtSetData(Sender: TObject; Data: PPyObject);
    procedure PyIOSent(Sender: TObject; const Data:AnsiString);
    procedure PyIOSentUni(Sender: TObject; const Data: UnicodeString);
  end;
  TArgs=Array of Variant;
var
  PythonEngine1:TPythonEngine;
  PythonIO:TPythonGUIInputOutput;
  PyInterfaceVar:TPythonDelphiVar;
  PyInterfaceE:TPythonDelphiVar;
  PyEvents:TMyPyEvents;
{$endif}
var
  PyMemoComponent:TXmemo;


implementation

{$ifndef JScript}
var
  glbPyObj,glbPyObjE:PPyObject;
{$endif}

{$ifndef JScript}
function Arr2dstoVarArray(arr2ds:T2DStringArray):Variant;
var
  varr:Variant;
  i,j:integer;
begin
  varr:=VarArrayCreate([0, Length(arr2ds) - 1, 0, length(arr2ds[0])-1], varVariant);
  for i:=0 to length(arr2ds)-1 do
  begin
    for j:=0 to length(arr2ds[0])-1 do
    begin
      VarArrayPut(varr,arr2ds[i,j],[i,j]);
      //varr[i,j]:=arr2ds[i,j];
    end;
  end;
  result:=varr;
end;
function Arr3dntoVarArray(arr3dn:T3DNumArray):Variant;
var
  varr:Variant;
  i,j,k:integer;
begin
  varr:=VarArrayCreate([0, Length(arr3dn)-1, 0, length(arr3dn[0])-1, 0, length(arr3dn[0,0])-1], varVariant);
  for i:=0 to length(arr3dn)-1 do
  begin
    for j:=0 to length(arr3dn[0])-1 do
    begin
     for k:=0 to length(arr3dn[0,0])-1 do
     begin
      VarArrayPut(varr,arr3dn[i,j,k],[i,j,k]);
     end;
    end;
  end;
  result:=varr;
end;
function ConvertArrayOfVariantTo2DNumArray(varr:TArgs):T2dNumArray;
var
  arr:T2DNumArray;
  v:Variant;
  varri:TArgs;
  i,j,l,l0:integer;
begin
  l0:=length(varr);
  setlength(arr,length(varr));
  for i:=0 to length(arr)-1 do
  begin
    varri:=varr[i];
    l:=length(varri);
    setlength(arr[i],l);
    for j:=0 to l-1 do
    begin
      v:=varri[j];
      arr[i,j]:=Double(v);
    end;
  end;
  result:=arr;
end;

function RunInterfaceFunc(e:TEventStatus;fname:String;fnArgs:TArgs):Variant;
var
  v:Variant;
//  str:string;
  bool:Boolean;
  arr2ds:T2DStringArray;
  arr2dn:T2DNumArray;
  arr3dn:T3DNumArray;
  arr1dn:TNumArray;
  i:integer;
begin
  if fname='ShowMessage' then
    mmo.mmiShowMessage(fnArgs[0])
  else if fname='ShowXForm' then
    mmo.mmiShowXForm(fnArgs[0], myStrToBool(fnArgs[1]))
  else if fname='CloseXForm' then
    mmo.mmiCloseXForm(fnArgs[0])
  else if fname='GetPropertyValue' then
  begin
    v:=mmo.mmiGetPropertyValue(fnArgs[0], fnArgs[1]);
  end
  else if fname='SetPropertyValue' then
  begin
    mmo.mmiSetPropertyValue(fnArgs[0], fnArgs[1], fnArgs[2]);
  end
  else if fname='Confirm' then
  begin
    bool:=mmo.mmiconfirm(fnArgs[0]);
    v:=bool;
  end
  else if fname='Prompt' then
  begin
    v:=mmo.mmiPrompt(fnArgs[0], fnArgs[1]);
  end
  else if fname='CopyToClip' then
  begin
    mmo.mmiCopyToClip(fnArgs[0]);
  end
  else if fname='CopyFromClip' then
  begin
    v:=mmo.mmiCopyFromClip(e);
  end
  else if fname='LoadTableFromExcelCopy' then
  begin
    mmo.mmiLoadTableFromExcelCopy(fnArgs[0], fnArgs[1]);
  end
  else if fname='LoadTableFromNumArray' then
  begin
    //fnargs[1] is an array of variant, but needs to be a T2DNumArray???? !!!!
    arr2dn:=ConvertArrayOfVariantTo2DNumArray(fnargs[1]);
    mmo.mmiLoadTableFromNumArray(fnArgs[0], arr2dn);
  end
  else if fname='GetTableDataArray' then
  begin
    arr2ds:=mmo.mmiGetTableDataArray(fnArgs[0], fnargs[1]);
    v:=Arr2dstoVarArray(arr2ds);
  end
  else if fname='DoEvent' then
    mmo.mmiDoEvent(fnArgs[0], fnArgs[1], fnArgs[2])
  else if fname='MoveComponent' then
    mmo.mmiMoveComponent(fnArgs[0], fnArgs[1])
  else if fname='CopyComponent' then
    mmo.mmiCopyComponent(fnArgs[0], fnArgs[1], fnArgs[2])
  else if fname='DeleteComponent' then
  begin
    bool:=mmo.mmiDeleteComponent(fnArgs[0], fnargs[1]);
    v:=bool;
  end
  else if fname='GetGPUParamNumValue' then
  begin
    arr1dn:=mmo.mmiGetGPUParamNumValue(fnArgs[0], fnArgs[1]);
    v:=arr1dn;
  end
  else if fname='GetGPUConstIntValue' then
  begin
    i:=mmo.mmiGetGPUConstIntValue(fnArgs[0], fnArgs[1]);
    v:=i;
  end
  else if fname='SetGPUParamNumValue' then
    mmo.mmiSetGPUParamNumValue(fnArgs[0], fnArgs[1], fnargs[2])
  else if fname='SetGPUConstIntValue' then
    mmo.mmiSetGPUConstIntValue(fnArgs[0], fnArgs[1], fnargs[2])
  else if fname='StartMain' then
    mmo.mmiStartMain(e)
  else if fname='ShowBusy' then
    mmo.mmiShowBusy(e)
  else if fname='HideBusy' then
    mmo.mmiHideBusy
  else if fname='ProcessMessages' then
    mmo.mmiProcessMessages
  else if fname='MovePointerBetweenComponents' then
    mmo.mmiMovePointerBetweenComponents(fnArgs[0], fnArgs[1], fnArgs[2], fnArgs[3])
  else if fname='HidePointer' then
    mmo.mmiHidePointer
  else if fname='UserSystemAsString' then
    v:=mmo.mmiUserSystemAsString()
  else if fname='LoadUserSystemString' then
    mmo.mmiLoadUserSystemString(fnArgs[0])
  else if fname='ConsoleLog' then
    mmo.mmiConsoleLog(fnArgs[0])
  else if fname='Array2DToString' then
    v:=mmo.mmiArray2DToString(fnargs[0])
  else if fname='GetGPUPixelArray' then
  begin
    arr3dn:=mmo.mmiGetGPUPixelArray(fnArgs[0]);
    v:=Arr3dntoVarArray(arr3dn);
  end
  else if fname='GetGPUPixelArrayAsString' then
    v:=mmo.mmiGetGPUPixelArrayAsString(fnArgs[0])
  else if fname='GetGPUStageArray' then
  begin
    arr3dn:=mmo.mmiGetGPUStageArray(fnArgs[0]);
    v:=Arr3dntoVarArray(arr3dn);
  end
  else if fname='GetGPUStageArrayAsString' then
    v:=mmo.mmiGetGPUStageArrayAsString(fnArgs[0])
  else if fname='DebugStart' then
    mmo.mmiDebugStart;

  result:=v;
end;
{$endif}

{$ifndef JScript}
procedure TMyPyEvents.PyVarExtGetData(Sender: TObject; var Data: PPyObject);
begin
with PythonEngine1 do
 begin
   Data := glbPyObj;
   Py_XIncRef(Data); // This is very important
 end;
end;
procedure TMyPyEvents.PyVarExtSetData(Sender: TObject; Data: PPyObject);
var
  PKeyObj,PArgsObj,PRsltObj,POb:PPyObject;
  fname:string;
  fnArgs:Array of Variant;
 // myargs:TStringList;
  i:integer;
  v:Variant;
  t:TVarType;
  e:TeventStatus;
begin
with PythonEngine1 do
 begin
  Py_XDecRef(glbPyObj); // This is very important
  glbPyObj := Data;
  Py_XIncRef(glbPyObj); // This is very important
  //showmessage('ExtSetData done');

  // glbPyObj acts as a message from python....instruction to execute a pascal function
  // extract the fields from the message
  // IMPORTANTLY....the Python script has paused while this step is done....yay!
  PKeyObj := PythonEngine1.PyObject_GetAttrString(glbPyObj, 'fname');
  fname:=PythonEngine1.PyObjectAsString(PKeyObj);
  PRsltObj := PythonEngine1.PyObject_GetAttrString(glbPyObj, 'rslt');
  v:=PythonEngine1.PyObjectAsVariant(PRsltObj);
  //PArgsObj := PythonEngine1.PyObject_GetItem(glbPyObj, PythonEngine1.VariantAsPyObject('args'));

  PArgsObj := PythonEngine1.PyObject_GetAttrString(glbPyObj,'args');
  //PythonEngine1.PyTupleToStrings(PArgsObj,myargs);

  // args is a set of variants
  setlength(fnArgs,PythonEngine1.PyTuple_Size(PArgsObj));
  for i:=0 to length(fnArgs)-1 do
  begin
   POb:=PythonEngine1.PyTuple_GetItem(PArgsObj,i);
   fnArgs[i]:=PythonEngine1.PyObjectAsVariant(POb);
  end;

  t:=varType(v);
  e:=glbEvent;       // slightly dodgy.. this is reset at start of new event
  if (fname<>'xxx')
  and (varType(v) = 8)  // string
  and (v='xxx') then    // avoid re-triggering when return value has been set
  begin
    //showmessage('from python. fname='+fname);
    // run the requested function
    v:=RunInterfaceFunc(e,fname,fnArgs);
    // pass the current e values to Python
    PythonEngine1.PyObject_SetAttrString(glbPyObjE,'EventType',PythonEngine1.VariantAsPyObject(UTF8Decode(e.EventType)));
    PythonEngine1.PyObject_SetAttrString(glbPyObjE,'NodeId',PythonEngine1.VariantAsPyObject(UTF8Decode(e.NodeId)));
    PythonEngine1.PyObject_SetAttrString(glbPyObjE,'NameSpace',PythonEngine1.VariantAsPyObject(UTF8Decode(e.NameSpace)));
    PythonEngine1.PyObject_SetAttrString(glbPyObjE,'ReturnString',PythonEngine1.VariantAsPyObject(UTF8Decode(e.ReturnString)));
    PyInterfaceE.ValueObject:=glbPyObjE;
    // send the function result back to Python
    //PRsltObj:=PythonEngine1.PyString_FromString(PChar(str));
    PRsltObj:=PythonEngine1.VariantAsPyObject(v);
    PythonEngine1.PyObject_SetAttrString(glbPyObj,'rslt',PRsltObj);
    PyInterfaceVar.ValueObject:=glbPyObj;
  end;
  //myargs.free;
 end;
end;

procedure TMyPyEvents.PyVarEExtGetData(Sender: TObject; var Data: PPyObject);
begin
with PythonEngine1 do
 begin
   Data := glbPyObjE;
   Py_XIncRef(Data); // This is very important
 end;
end;
procedure TMyPyEvents.PyVarEExtSetData(Sender: TObject; Data: PPyObject);
begin
with PythonEngine1 do
  begin
    Py_XDecRef(glbPyObjE); // This is very important
    glbPyObjE := Data;
    Py_XIncRef(glbPyObjE); // This is very important
  end;
end;

procedure UpdateMemo(Data:String);
var
  oldval:string;
begin
  oldval:=PyMemoComponent.myNode.GetAttribute('ItemValue',false).AttribValue;
//  PyMemoComponent.myNode.SetAttributeValue('ItemValue',oldval+Data);
  PyMemoComponent.ItemValue:=oldval+LineEnding+Data;
end;

procedure TMyPyEvents.PyIOSent(Sender: TObject; const Data:AnsiString);
begin
//  showmessage('sent '+Data);
  UpdateMemo(Data);
end;

procedure TMyPyEvents.PyIOSentUni(Sender: TObject;  const Data: UnicodeString);
begin
//  showmessage('senduni '+data);
  UpdateMemo(UTF8Decode(Data));
end;

procedure DoPy_InitEngine;
var
  pth:string;
begin
  if Assigned(PyInterfaceVar) then
    PyInterfaceVar.Destroy;
  if Assigned(PyInterfaceE) then
    PyInterfaceE.Destroy;
  if Assigned(PythonEngine1) then
  begin
    PythonEngine1.Finalize;
    PythonEngine1.UnloadDll;
    PythonEngine1.Destroy;
  end;

  PythonEngine1:=TPythonEngine.Create(nil);
  PythonEngine1.Name:='PythonEngine1';
  PythonEngine1.PyFlags:=[pfUseClassExceptionsFlag];
  PythonEngine1.RegVersion:=PythonVersion;
  PythonEngine1.UseLastKnownVersion:=false;
  PythonEngine1.RedirectIO:=true;      //!! trying to get the send/receive events to work
  PythonEngine1.IO:=PythonIO;
//  S:=
//    {$ifdef windows} cPyLibraryWindows {$endif}
//    {$ifdef linux} cPyLibraryLinux {$endif}
  pth:=ExtractFileDir(PythonLibDir);
  if pth<>'' then
    pth:=pth+'\';
  PythonEngine1.DllPath:= pth;
  PythonEngine1.DllName:= ExtractFileName(PythonLibDir);
  PythonEngine1.LoadDll;

  PyInterfaceVar:=TPythonDelphiVar.Create(nil);
  PyInterfaceVar.Name:='PyInterfaceVar';
  PyInterfaceVar.VarName:='PyInterfaceVar';
  PyInterfaceVar.Module:='__main__';
  PyInterfaceVar.Engine:=PythonEngine1;
  PyInterfaceVar.OnExtGetData:=@PyEvents.PyVarExtGetData;
  PyInterfaceVar.OnExtSetData:=@PyEvents.PyVarExtSetData;
  PyInterfaceVar.Initialize;

  PyInterfaceE:=TPythonDelphiVar.Create(nil);
  PyInterfaceE.Name:='PyInterfaceE';
  PyInterfaceE.VarName:='PyInterfaceE';
  PyInterfaceE.Module:='__main__';
  PyInterfaceE.Engine:=PythonEngine1;
  PyInterfaceE.OnExtGetData:=@PyEvents.PyVarEExtGetData;
  PyInterfaceE.OnExtSetData:=@PyEvents.PyVarEExtSetData;
  PyInterfaceE.Initialize;
end;

procedure InitPythonComponents;
begin
  PythonIO:=TPythonGUIInputOutput.Create(nil);
  PyEvents:=TMyPyEvents.Create;
  PythonIO.OnSendData:=@PyEvents.PyIOSent;
  PythonIO.OnSendUniData:=@PyEvents.PyIOSentUni;
end;

procedure SetupPyEngine(PyLibDir,PyVersion:String);
begin
  PythonLibDir:=PyLibDir;
  PythonVersion:=PyVersion;
  InitPythonComponents;
  // start the engine
  DoPy_InitEngine;
end;

function PyodideScript:TStringList;
var
  script:TStringList;
begin
  script:=TStringList.Create;
  script.add('<script type="application/javascript" src="./resources/pyodide_local/pyodide.js">');
  script.add('</script>  ');
  script.add('<script type="application/javascript" >');
  script.add('languagePluginLoader.then(() => {');
  script.add('  // pyodide is now ready to use...');
  script.add('  console.log(''python: ''+pyodide.runPython(''import sys\nsys.version''));');
  //script.add('  pyodide.loadPackage([''numpy'',''scipy'']);');
  script.add('  pyodide.loadPackage(''numpy'');');
  script.add('  pyodide.loadPackage(''scipy'');');
  script.add('  pas.XIDEMain.StartupPython();');
  script.add('});');
  script.add('</script>  ');
  result:=script;
end;

procedure RunInitialScript;
var
  InitScript:TStringList;
  txt:String;
begin
  InitScript:=TStringList.Create;
  // load the initialisation py script
  // Sets up an internal library of XIDE Interface functions, available to the user.
  InitScript.Clear;
  InitScript.add('class MyMessage:');
  InitScript.add('  fname = ''xxx''');
  InitScript.add('  args = (''1'',''2'',''3'')');
  InitScript.add('  rslt = ''xxx''');
  InitScript.add('Xmsg = MyMessage()');
  InitScript.add('class eClass:');
  InitScript.add('  EventType = ''''');
  InitScript.add('  NodeId = ''''');
  InitScript.add('  NameSpace = ''''');
  InitScript.add('  ReturnString = ''''');
  InitScript.add('e = eClass()');
  InitScript.add('PyInterfaceE.Value = e');
  InitScript.add('');
  InitScript.add('def RunXIDEFunc(fname,args):');
  InitScript.add('  Xmsg.fname = fname');
  InitScript.add('  Xmsg.args = args'); // args is Tuple type
  InitScript.add('  Xmsg.rslt = ''xxx''');
//  {$ifndef JScript}
  InitScript.add('  PyInterfaceVar.Value = Xmsg');
  InitScript.add('  return Xmsg.rslt');
//  {$else}
//  InitScript.add('  PyInterfaceVar.Value = Xmsg');
//  {$endif}
  InitScript.add('def GetPropertyValue(NodeName,PropName):');
  InitScript.add('  return RunXIDEFunc(''GetPropertyValue'',(NodeName,PropName))');
//  InitScript.add('  print(msg.args[0]+'' ''+msg.args[1]+'' = ''+msg.rslt)');
  InitScript.add('def SetPropertyValue(NodeName,PropName,NewValue):');
  InitScript.add('  RunXIDEFunc(''SetPropertyValue'',(NodeName,PropName,NewValue))');
  InitScript.add('def ShowMessage(Messg):');
  InitScript.add('  RunXIDEFunc(''ShowMessage'',(Messg,0))');
  InitScript.add('def ShowXForm(XFormID,Modal):');
  InitScript.add('  RunXIDEFunc(''ShowXForm'',(XFormID,Modal))');
  InitScript.add('def CloseXForm(XFormID):');
  InitScript.add('  RunXIDEFunc(''CloseXForm'',(XFormID,0))');
  InitScript.add('def CopyToClip(str):');
  InitScript.add('  RunXIDEFunc(''CopyToClip'',(str,0))');
  InitScript.add('def CopyFromClip(e):');
  InitScript.add('  return RunXIDEFunc(''CopyFromClip'',(e,0))');
  InitScript.add('def LoadTableFromExcelCopy(TableName,CopiedString):');
  InitScript.add('  RunXIDEFunc(''LoadTableFromExcelCopy'',(TableName,CopiedString))');
  InitScript.add('def LoadTableFromNumArray(TableName,NumArray):');
  InitScript.add('  RunXIDEFunc(''LoadTableFromNumArray'',(TableName,NumArray))');
  InitScript.add('def GetTableDataArray(TableName,SkipHeader):');
  InitScript.add('  return RunXIDEFunc(''GetTableDataArray'',(TableName,SkipHeader))');
  InitScript.add('def DoEvent(EventType,NodeId,myValue):');
  InitScript.add('  RunXIDEFunc(''DoEvent'',(EventType,NodeId,myValue))');
  InitScript.add('def MoveComponent(NodeId,NewParentId):');
  InitScript.add('  RunXIDEFunc(''MoveComponent'',(NodeId,NewParentId))');
  InitScript.add('def CopyComponent(NodeId,NewParentId,NewName):');
  InitScript.add('  RunXIDEFunc(''CopyComponent'',(NodeId,NewParentId,NewName))');
  InitScript.add('def DeleteComponent(NodeId,ShowNotFoundMsg):');
  InitScript.add('  return RunXIDEFunc(''DeleteComponent'',(NodeId,ShowNotFoundMsg))');
  InitScript.add('def GetGPUParamNumValue(GPUName,pName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUParamNumValue'',(GPUName,pName))');
  InitScript.add('def GetGPUConstIntValue(GPUName,pName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUConstIntValue'',(GPUName,pName))');
  InitScript.add('def SetGPUParamNumValue(GPUName,pName,pValue):');
  InitScript.add('  RunXIDEFunc(''SetGPUParamNumValue'',(GPUName,pName,pValue))');
  InitScript.add('def SetGPUConstIntValue(GPUName,pName,pValue):');
  InitScript.add('  RunXIDEFunc(''SetGPUConstIntValue'',(GPUName,pName,pValue))');
  InitScript.add('def StartMain(e):');
  InitScript.add('  RunXIDEFunc(''StartMain'',(e,0))');
  InitScript.add('def ShowBusy(e):');
  InitScript.add('  RunXIDEFunc(''ShowBusy'',(e,0))');
  InitScript.add('def HideBusy():');
  InitScript.add('  RunXIDEFunc(''HideBusy'',(0,0))');
  InitScript.add('def ProcessMessages():');
  InitScript.add('  RunXIDEFunc(''ProcessMessages'',(0,0))');
  InitScript.add('def MovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2):');
  InitScript.add('  RunXIDEFunc(''MovePointerBetweenComponents'',(NodeName1,NodeName2,Sub1,Sub2))');
  InitScript.add('def HidePointer():');
  InitScript.add('  RunXIDEFunc(''HidePointer'',(0,0))');
  InitScript.add('def UserSystemAsString():');
  InitScript.add('  return RunXIDEFunc(''UserSystemAsString'',(0,0))');
  InitScript.add('def LoadUserSystemString(SystemString):');
  InitScript.add('  RunXIDEFunc(''LoadUserSystemString'',(SystemString,0))');
  InitScript.add('def ConsoleLog(txt):');
  InitScript.add('  RunXIDEFunc(''ConsoleLog'',(txt,0))');
  InitScript.add('def Array2DToString(arr):');
  InitScript.add('  return RunXIDEFunc(''Array2DToString'',(arr,0))');
  InitScript.add('def GetGPUPixelArray(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUPixelArray'',(GPUName,0))');
  InitScript.add('def GetGPUPixelArrayAsString(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUPixelArrayAsString'',(GPUName,0))');
  InitScript.add('def GetGPUStageArray(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUStageArray'',(GPUName,0))');
  InitScript.add('def GetGPUStageArrayAsString(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUStageArrayAsString'',(GPUName,0))');
  InitScript.add('print(''Python Engine Initialised'')');

  // execute the initialisation py script  (creates MyMessage python class and object)
  PythonEngine1.ExecStrings( InitScript );
  InitScript.Free;
end;
{$else}
procedure RunInitialScript;
var
  InitScript:TStringList;
  txt:String;
begin
  InitScript:=TStringList.Create;
  // load the initialisation py script
  // Sets up an internal library of XIDE Interface functions, available to the user.
  InitScript.Clear;
  InitScript.add('class eClass:');
  InitScript.add('  EventType = ''''');
  InitScript.add('  NodeId = ''''');
  InitScript.add('  NameSpace = ''''');
  InitScript.add('  ReturnString = ''''');
  InitScript.add('e = eClass()');
  InitScript.add('');
  InitScript.add('from js import pas');    //!!!!  ??????
  InitScript.add('def GetPropertyValue(NodeName,PropName):');
  InitScript.add('  return pas.InterfaceTypes.GetPropertyValue(NodeName,PropName)');
//  InitScript.add('def SetPropertyValue(NodeName,PropName,NewValue):');
//  InitScript.add('  RunXIDEFunc(''SetPropertyValue'',(NodeName,PropName,NewValue))');
  InitScript.add('def ShowMessage(Messg):');
  InitScript.add('  pas.InterfaceTypes.ShowMessage(Messg)');
  InitScript.add('def ShowXForm(XFormID,Modal):');
  InitScript.add('  pas.InterfaceTypes.ShowXForm(XFormID,Modal)');
  InitScript.add('def CloseXForm(XFormID):');
  InitScript.add('  pas.InterfaceTypes.CloseXForm(XFormID)');
  InitScript.add('def CopyToClip(str):');
  InitScript.add('  pas.InterfaceTypes.CopyToClip(str)');
  InitScript.add('def CopyFromClip(e):');
  InitScript.add('  return pas.InterfaceTypes.CopyFromClip(e)');
  InitScript.add('def LoadTableFromExcelCopy(TableName,CopiedString):');
  InitScript.add('  pas.InterfaceTypes.LoadTableFromExcelCopy(TableName,CopiedString)');
  InitScript.add('def LoadTableFromNumArray(TableName,NumArray):');
  InitScript.add('  pas.InterfaceTypes.LoadTableFromNumArray(TableName,NumArray)');
  InitScript.add('def GetTableDataArray(TableName,SkipHeader):');
  InitScript.add('  return pas.InterfaceTypes.GetTableDataArray(TableName,SkipHeader)');
  InitScript.add('def DoEvent(EventType,NodeId,myValue):');
  InitScript.add('  pas.InterfaceTypes.DoEvent(EventType,NodeId,myValue)');
  InitScript.add('def MoveComponent(NodeId,NewParentId):');
  InitScript.add('  pas.InterfaceTypes.MoveComponent(NodeId,NewParentId)');
  InitScript.add('def CopyComponent(NodeId,NewParentId,NewName):');
  InitScript.add('  pas.InterfaceTypes.CopyComponent(NodeId,NewParentId,NewName)');
  InitScript.add('def DeleteComponent(NodeId,ShowNotFoundMsg):');
  InitScript.add('  return pas.InterfaceTypes.DeleteComponent(NodeId,ShowNotFoundMsg)');
  InitScript.add('def GetGPUParamNumValue(GPUName,pName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUParamNumValue(GPUName,pName)');
  InitScript.add('def GetGPUConstIntValue(GPUName,pName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUConstIntValue(GPUName,pName)');
  InitScript.add('def SetGPUParamNumValue(GPUName,pName,pValue):');
  InitScript.add('  pas.InterfaceTypes.SetGPUParamNumValue(GPUName,pName,pValue)');
  InitScript.add('def SetGPUConstIntValue(GPUName,pName,pValue):');
  InitScript.add('  pas.InterfaceTypes.SetGPUConstIntValue(GPUName,pName,pValue)');
  InitScript.add('def StartMain(e):');
  InitScript.add('  pas.InterfaceTypes.StartMain(e)');
  InitScript.add('def ShowBusy(e):');
  InitScript.add('  pas.InterfaceTypes.ShowBusy(e)');
  InitScript.add('def HideBusy():');
  InitScript.add('  pas.InterfaceTypes.HideBusy()');
  InitScript.add('def ProcessMessages():');
  InitScript.add('  pas.InterfaceTypes.ProcessMessages()');
  InitScript.add('def MovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2):');
  InitScript.add('  pas.InterfaceTypes.MovePointerBetweenComponents(NodeName1,NodeName2,Sub1,Sub2)');
  InitScript.add('def HidePointer():');
  InitScript.add('  pas.InterfaceTypes.HidePointer()');
  InitScript.add('def UserSystemAsString():');
  InitScript.add('  return pas.InterfaceTypes.UserSystemAsString()');
  InitScript.add('def LoadUserSystemString(SystemString):');
  InitScript.add('  pas.InterfaceTypes.LoadUserSystemString(SystemString)');
  InitScript.add('def ConsoleLog(txt):');
  InitScript.add('  pas.InterfaceTypes.ConsoleLog(txt)');
  InitScript.add('def Array2DToString(arr):');
  InitScript.add('  return pas.InterfaceTypes.Array2DToString(arr)');
  InitScript.add('def GetGPUPixelArray(GPUName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUPixelArray(GPUName)');
  InitScript.add('def GetGPUPixelArrayAsString(GPUName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUPixelArrayAsString(GPUName)');
  InitScript.add('def GetGPUStageArray(GPUName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUStageArray(GPUName)');
  InitScript.add('def GetGPUStageArrayAsString(GPUName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUStageArrayAsString(GPUName)');

  // execute the initialisation py script
  txt:=InitScript.Text;
  asm
  pyodide.runPython(txt);
  end;
  InitScript.Free;
end;
{$endif}

procedure PyExeString(cmds: string);
var
  s: TStringList;
begin
  s := TStringList.create;
  try
    s.text := cmds;
    {$ifndef JScript}
    PythonEngine1.ExecStrings( s );
    {$else}
    asm
    pyodide.runPython(cmds);
    end;
    {$endif}
  finally
    s.free;
  end;
end;


//var.decode('utf-8') ... might be of use

end.
{$endif}

