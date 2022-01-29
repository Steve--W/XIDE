unit PyXUtils;

{$mode objfpc}{$H+}

interface

{$ifndef Python}
var
  PythonCodeExists:Boolean;
implementation
end.
{$else}
uses
  Classes, SysUtils, StringUtils,
  {$ifndef JScript}
  Math, Dialogs, Forms, StdCtrls, variants,
  PythonEngine, PythonGUIInputOutput,
  DllInterface,
  {$endif}
  NodeUtils, XMemo, EventsInterface;

var
  //PythonLibDir,
  PythonVersion:String;
  PythonCodeExists:Boolean;

{$ifndef JScript}
function PyodideScript:TStringList;
procedure SetupPyEngine(PyLibDir,PyVersion:String);
procedure DoPy_InitEngine;
{$endif}
procedure BuildXarray(XArrName:String;dims,mults:TStringArray;dflt:String);
procedure PyExeString(cmds: string);
procedure RunInitialScript;
procedure RedirectPyLog(MemoName:String);
function BuildPackageList:TStringArray;

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
{$else}
var
  PyPkTest:integer;
{$endif}
var
  PyMemoComponent:TXmemo;


implementation
uses XIDEMain,XDataModel,XObjectInsp;

function BuildPackageList:TStringArray;
var
  pypaks:TStringList;
  pknames:TStringArray;
  numpaks,i,j:integer;
  pypkstr:String;
begin
  pypkstr:=UIRootNode.GetAttribute('PythonPackages',true).AttribValue;
  pypaks := TStringList.Create;
  pypaks.LineBreak:=';';
  pypaks.SkipLastLineBreak:=true;
  pypaks.Text:=pypkstr;
  // de-duplicate the list
  for i:=pypaks.count-1 downto 0 do
    for j:=pypaks.count-1 downto 0 do
      if (j<>i) and (pypaks[j]=pypaks[i]) then
        pypaks[j]:='';
  for i:=pypaks.count-1 downto 0 do
    if pypaks[i]='' then
      pypaks.Delete(i);
  numpaks:=pypaks.Count;
  setlength(pknames,numpaks);
  for i:=0 to numpaks-1 do
    pknames[i]:=pypaks[i];
  result:=pknames;
end;

{$ifndef JScript}
var
  glbPyObj,glbPyObjE:PPyObject;


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
  i,j,l:integer;
begin
  setlength(arr,length(varr));
  for i:=0 to length(arr)-1 do
  begin
    varri:=varr[i];
    l:=length(varri);
    setlength(arr[i],l);
    for j:=0 to l-1 do
    begin
      v:=varri[j];
      if v<>null then
      try
      arr[i,j]:=Double(v);
      except
        on exception do
          arr[i,j]:=-999;
      end
      else
        arr[i,j]:=-999;
    end;
  end;
  result:=arr;
end;
function ConvertArrayOfVariantTo1DNumArray(varr:TArgs):TNumArray;
var
  arr:TNumArray;
  v:Variant;
  varri:TArgs;
  i,l:integer;
begin
  setlength(arr,length(varr));
  for i:=0 to length(varr)-1 do
  begin
    v:=varr[i];
    if v<>null then
      try
       arr[i]:=Double(v);
      except
        on exception do
          arr[i]:=-999;
      end
    else
      arr[i]:=-999;
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
  aov:Array of Variant;
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
  else if fname='GetTableDataForExcel' then
  begin
    mmo.mmiGetTableDataForExcel(fnArgs[0]);
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
    bool:=mmo.mmiDeleteComponent(fnArgs[0], fnargs[1], fnargs[2]);
    v:=bool;
  end
  else if fname='GetGPUParamNumValue' then
  begin
    arr1dn:=mmo.mmiGetGPUParamNumValue(fnArgs[0], fnArgs[1]);
    v:=arr1dn;
  end
  else if fname='GetGPUParam2DNumValue' then
  begin
    arr2dn:=mmo.mmiGetGPUParam2DNumValue(fnArgs[0], fnArgs[1]);
    v:=arr2dn;
  end
  else if fname='GetGPUConstIntValue' then
  begin
    i:=mmo.mmiGetGPUConstIntValue(fnArgs[0], fnArgs[1]);
    v:=i;
  end
  else if fname='SetGPUParamNumValue' then
  begin
    arr1dn:=ConvertArrayOfVariantTo1DNumArray(fnargs[2]);

    mmo.mmiSetGPUParamNumValue(fnArgs[0], fnArgs[1], arr1dn)
  end
  else if fname='SetGPUParam2DNumValue' then
  begin
    arr2dn:=ConvertArrayOfVariantTo2DNumArray(fnargs[2]);
    mmo.mmiSetGPUParam2DNumValue(fnArgs[0], fnArgs[1], arr2dn)
  end
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
  else if fname='GetGPUInitStageArray' then
  begin
    arr3dn:=mmo.mmiGetGPUInitStageArray(fnArgs[0]);
    v:=Arr3dntoVarArray(arr3dn);
  end
  else if fname='GetGPUStageArrayAsString' then
    v:=mmo.mmiGetGPUStageArrayAsString(fnArgs[0])
  else if fname='RedirectPyLog' then
    RedirectPyLog(fnArgs[0])
  else if fname='BuildXArrays' then
    BuildXArrays(fnArgs[0])
  else if fname='DebugStart' then
    mmo.mmiDebugStart;

  result:=v;
end;

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
    if (e<>nil) then
    begin
      PythonEngine1.PyObject_SetAttrString(glbPyObjE,'EventType',PythonEngine1.VariantAsPyObject(UTF8Decode(e.EventType)));
      PythonEngine1.PyObject_SetAttrString(glbPyObjE,'NodeId',PythonEngine1.VariantAsPyObject(UTF8Decode(e.NodeId)));
      PythonEngine1.PyObject_SetAttrString(glbPyObjE,'NameSpace',PythonEngine1.VariantAsPyObject(UTF8Decode(e.NameSpace)));
      PythonEngine1.PyObject_SetAttrString(glbPyObjE,'ReturnString',PythonEngine1.VariantAsPyObject(UTF8Decode(e.ReturnString)));
      PyInterfaceE.ValueObject:=glbPyObjE;
    end;
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
  if (PyMemoComponent<>nil)
  and (assigned(PyMemoComponent)) then
  begin
    oldval:=PyMemoComponent.myNode.GetAttribute('ItemValue',false).AttribValue;
    PyMemoComponent.ItemValue:=oldval+LineEnding+Data;
  end;
end;

procedure TMyPyEvents.PyIOSent(Sender: TObject; const Data:AnsiString);
begin
  UpdateMemo(Data);
end;

procedure TMyPyEvents.PyIOSentUni(Sender: TObject;  const Data: UnicodeString);
begin
  UpdateMemo(UTF8Decode(Data));
end;

procedure DoPy_InitEngine;
var
  ok:boolean;
begin
  if Assigned(PythonEngine1) then
  begin
    // Engine already exists,
    // clear out any previously-declared python vars, functions etc...
    PythonEngine1.ExecString('for name in dir(): '+LineEnding+
                             '  if not name.startswith(''_'')'+
                                ' and not name==''PyInterfaceVar'''+
                                ' and not name==''PyInterfaceE'':'+LineEnding+
                             '    del globals()[name]'+LineEnding);
  end
  else
  begin
    ok:=true;
    // First time in, Create python engine and interfaces...

    PythonEngine1:=TPythonEngine.Create(nil);
    PythonEngine1.Name:='PythonEngine1';
    PythonEngine1.PyFlags:=[pfUseClassExceptionsFlag];
    PythonEngine1.RedirectIO:=true;
    PythonEngine1.IO:=PythonIO;

    PythonEngine1.UseLastKnownVersion:=true;     // use the Python version installed on the machine
    PythonEngine1.AutoLoad:=false;
    try
    PythonEngine1.LoadDll;
    except
      on E: Exception do
      begin
        showmessage('Please ensure Python is installed, or remove the -dPython compiler directive to run without Python');
        ok:=false;
      end;
    end;

    if ok then
    begin
      PythonVersion:=PythonEngine1.RegVersion;

      // create interface objects for communication to/from Pascal code
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
  end;
end;

procedure SetupPyEngine(PyLibDir,PyVersion:String);
begin
  //PythonLibDir:=PyLibDir;
  //PythonVersion:=PyVersion;
  PythonIO:=TPythonGUIInputOutput.Create(nil);
  PyEvents:=TMyPyEvents.Create;
  PythonIO.OnSendData:=@PyEvents.PyIOSent;
  PythonIO.OnSendUniData:=@PyEvents.PyIOSentUni;
  // create and start the python engine
  DoPy_InitEngine;
end;


function PyodideScript:TStringList;
var
  script:TStringList;
  pknames:TStringArray;
  i,j,numpakstoload:integer;
  mystr:String;
begin
  CheckForPythonCode;

  pknames:=BuildPackageList;
  numpakstoload:=2+length(pknames);

  script:=TStringList.Create;
  script.add('<script type="application/javascript" >');
  script.add('var realalert=window.alert;');
  script.add('window.alert=function(msg){');
  script.add('  console.log(msg);');
  script.add('  if (msg.toLowerCase().includes("memory access out of bounds")) {window.alert=realalert;}');
  script.add('  else {realalert(msg);}');
  script.add('  return true;');
  script.add('};');
  script.add('</script> ');

  // Load the pyodide script from the web; if unavailable try loading from pyodide_local...
  script.add('<script src="https://cdn.jsdelivr.net/pyodide/v0.18.1/full/pyodide.js"></script>');
  script.add('<script type="application/javascript" >');
  script.add('var pyodideReady = 0;');
  script.add('var PyodideOffline = false;');
  script.add('var readyForRunMode = false;');
  //script.add('    window.languagePluginUrl = "https://pyodide-cdn2.iodide.io/v0.15.0/full/";');
  if not PythonCodeExists then
  begin
    // no need for user to wait for Pyodide to finish loading on startup if there's no Python in the system...
    script.add('console.log("No Python scripts found in this system")');
    script.add('readyForRunMode = true;');
  end;
  script.add('function CreatePystatdiv() {');
  // create a div to display Python load status
  script.add('  var pystatdiv=document.createElement("div")');
  script.add('  pystatdiv.id="PyLoadStatus"');
  script.add('  pystatdiv.innerHTML="Python Loading..."');
  script.add('  pystatdiv.style.height="40px"');
  script.add('  pystatdiv.style.width="250px"');
  script.add('  pystatdiv.style.right=0');
  script.add('  pystatdiv.style.top=0');
  script.add('  pystatdiv.style.zIndex=999');
  script.add('  pystatdiv.style.position="absolute"');
  script.add('  document.getElementsByTagName("body")[0].prepend(pystatdiv);');
  //+'  <div  id = "PyLoadStatus" style="height:50px; width:200px;top:0;right:0; position:absolute; z-index:999;"> HELLO WORLD'  +LineEnding
  //+'  </div> '  +LineEnding
  script.add('}');
  script.add('function HidePystatdiv() {');
  script.add('  var pystatdiv = document.getElementById("PyLoadStatus");');
  script.add('  pystatdiv.style.display="none";');
  script.add('}');
  script.add('function ShowPystatdiv() {');
  script.add('  var pystatdiv = document.getElementById("PyLoadStatus");');
  script.add('  pystatdiv.style.display="block";');
  script.add('}');


  script.add('var localErrDone = false;');
  script.add('var pysrc2=document.createElement("script")');
  script.add('var pysrc3=document.createElement("script")');
  script.add('var pyodide');
  script.add('pysrc2.setAttribute("type","application/javascript")');
  script.add('pysrc3.setAttribute("type","application/javascript")');

  script.add('let otherloadedPackages = new Array();');
  //script.add('document.getElementsByTagName("head")[0].prepend(pywebsrc)');
  //script.add('pysrc1.setAttribute("src","https://pyodide-cdn2.iodide.io/v0.15.0/full/pyodide.js");');
  //script.add('pywebsrc.setAttribute("src","https://cdn.jsdelivr.net/pyodide/v0.18.1/full/pyodide.js");');
  script.add('</script> ');

  script.add('<script type="application/javascript" >');
  script.add('function pysrcLoaded() { ');
  script.add('    // pyodide is now ready to use...load some packages...  ' );
  script.add('    console.log("python: "+pyodide.runPython("import sys\nsys.version"));' );
  script.add('    CreatePystatdiv() ');
  script.add('    loadPyPkg0("setuptools",'+inttostr(numpakstoload)+',doAllFinalInits);');
  script.add('    loadPyPkg0("micropip",'+inttostr(numpakstoload)+',doAllFinalInits);');
  script.add('    pyodide.runPython(''from js import pas'');');
  script.add(' } ');

  script.add('async function loadpyodidefromweb(){');
  script.add('    console.log("try web pyodide load...");');
  script.add('    try {');
  script.add('      pyodide = await loadPyodide({ indexURL : "https://cdn.jsdelivr.net/pyodide/v0.18.1/full/" })');
  script.add('      if (pyodide) {');
  script.add('        pysrcLoaded();}');
  script.add('      else {');
  script.add('        await loadpyodidelocal();');
  script.add('        console.log("done loadpyodidelocal. calling pysrcLoaded");');
  script.add('        pysrcLoaded();}');
  script.add('    } catch(err) {');
  script.add('      console.log(err.message);');
  script.add('      await loadpyodidelocal();');
  script.add('}}');

  //  full example of offline pyodide at....
  //https://github.com/basvandertol/pyodide/releases/download/localpyodide-v0.1/localpyodide.zip
  script.add('function noLocalPyodide(){');
  script.add('  if (localErrDone==false) {');
  script.add('    console.log("cannot load local pyodide - Python will be unavailable"); ');
  script.add('    console.log("To work with Pyodide offline, create a folder ./pyodide_local"); ');
  script.add('    console.log("   This folder must contain pyodide files, such as provided from:"); ');
  script.add('    console.log("      https://github.com/basvandertol/pyodide/releases/download/localpyodide-v0.1/localpyodide.zip");');
//  script.add('    console.log("      https://github.com/iodide-project/pyodide/releases/download/0.14.3/pyodide-build-0.14.3.tar.bz2");');
//  script.add('    console.log("      https://github.com/iodide-project/pyodide/releases/download/0.18.1/pyodide-build-0.18.1.tar.bz2");');
//  script.add('    console.log("   and also include the file loadlocal.js, which can be found at:");');
//  script.add('    console.log("      https://github.com/iodide-project/pyodide/tree/6a2dd522f1eb4143f2630deae0a1fa9555546dfe/runlocal");');
//  script.add('    console.log("   Alternatively there is a pyodide_local folder containing minimum required files provided at:");');
//  script.add('    console.log("      https://github.com/Steve--W/XIDE");');
  script.add('    localErrDone = true;');
  script.add('    alert("cannot load pyodide - Python will be unavailable. See console for messages."); ');
  script.add('    HidePystatdiv()');
  script.add('}} ');

  script.add('pysrc2.onerror = function (){');
  script.add('              noLocalPyodide(); ');
  script.add('            }');
  script.add('pysrc3.onerror = function (){');
  script.add('              noLocalPyodide(); ');
  script.add('            }');

  script.add('async function loadpyodidelocal(){ ');
  script.add('  console.log("try local pyodide load..."); ');
  script.add('  console.log("do languagePluginUrl...."); ');
  script.add('  window.languagePluginUrl = "./pyodide_local/";');
  script.add('  pysrc2.setAttribute("src", "pyodide_local/loadlocal.js");');
  script.add('  pysrc2.async = false;');
  script.add('  document.getElementsByTagName("head")[0].prepend(pysrc2);');
  script.add('  pysrc3.setAttribute("src", "pyodide_local/pyodide.js");');
  script.add('  pysrc3.async = false;');
  script.add('  pysrc2.after(pysrc3); ');
  script.add('  pysrc3.addEventListener("load", async function() { ');
  script.add('  console.log("do await thing...."); ');
  script.add('  await (async () => {  ');
  script.add('    console.log("language plugin thing...."); ');
  script.add('    await languagePluginLoader; ');
//  script.add('    await pyodide.loadPackage(['numpy', 'matplotlib']);  } ');
  script.add('      if (pyodide) {');
  script.add('        console.log("done loadpyodidelocal. calling pysrcLoaded");');
  script.add('        PyodideOffline=true; ');
  script.add('        pysrcLoaded();}');
  script.add('      else {noLocalPyodide();}');
  script.add('})();');
script.add('  }); ');
script.add('}');


  script.add('loadpyodidefromweb(); ');
  script.add('</script>');

  script.add('<script type="text/javascript" > ');
  //script.add('pywebsrc.onerror = function (){');
  //script.add('              console.log("web load failed");');
  //script.add('              loadpyodidelocal();');
  //script.add('            } ');

  script.add('</script>');

  script.add('<script type="text/javascript" > ');
  script.add('function DoFinalInits(){');
  script.add('  readyForRunMode = true;');
  script.add('  if (myDeployedMode!="FromLaz") { ');
  // script.add('  alert("python DoFinalInits - checking for saved system now"); ');
  script.add('    var ok=pas.XObjectInsp.CheckForSavedSystemOnLoad();');
  script.add('  } ');
  script.add('  if (pas.XObjectInsp.RunningDeployedRuntime==true) {');
  script.add('    pas.XObjectInsp.ContinueToggleToRunMode();  }');
  script.add('  }');

  script.add('function doContinueFunc(continueFunc) { ');
  script.add('    continueFunc();');
  script.add(' } ');
  script.add('function doAllFinalInits() { ');
  script.add('      DoFinalInits();');
  script.add('      pas.XIDEMain.StartupPython();');
  script.add(' } ');
  script.add('function checkReady(pkgName,numpaks,continueFunc) { ');
  script.add('  console.log("pyodideReady="+pyodideReady+" numpaks="+numpaks);' );
  script.add('  if (pkgName=="micropip") {' );
  script.add('    loadPyPkgs(doAllFinalInits);');
  script.add('  }' );
  script.add('  if (pyodideReady==numpaks) {');               // required packages all loaded
  script.add('      console.log(''######### required packages loaded ##################'');');
  script.add('      doContinueFunc(continueFunc);');
  script.add('      HidePystatdiv()');
  script.add('    };');
  script.add(' } ');

  script.add('function testPyPkLoaded(pkgName) {');
  script.add('var scrip=''pas.PyXUtils.PyPkTest=1;\n''+');
  script.add('''try:\n''+');
  script.add('''  import ''+pkgName+''\n''+');
  script.add('''  print("no exceptions")\n''+');
  script.add('''except ImportError as error:\n''+');
  script.add('''  pas.PyXUtils.PyPkTest=0;\n''+');
  script.add('''except Exception as exception:\n''+');
  script.add('''  pas.PyXUtils.PyPkTest=0;\n'';');
  //script.add('  console.log(scrip);');
  script.add('  pyodide.runPython(scrip);');
  //script.add('  console.log(''pas.PyXUtils.PyPkTest=''+pas.PyXUtils.PyPkTest);');
  script.add(' } ');
  //????'import pkgutil; print(1 if pkgutil.find_loader("module") else 0)'
  //???? or... try:
  //  import cow
  //  print('\nModule was installed')
  //except ImportError:
  //  print('\nThere was no such module installed')"
  script.add('function loadPyPkg0(pkgName,numpaks,continueFunc) { ');
  script.add('  if ((!(pkgName in pyodide.loadedPackages)) && (!(otherloadedPackages.includes(pkgName)))) {');
  script.add('    try {');
  script.add('      pyodide.loadPackage(pkgName).then(() => {');
  script.add('      if (pkgName in pyodide.loadedPackages) {');     // sadly, the pkg name is now in this list, even if the load failed.
  script.add('        testPyPkLoaded(pkgName);');
  script.add('        if (pas.PyXUtils.PyPkTest==1) {');
  script.add('          console.log(pkgName+" is now available");');
  script.add('          pyodideReady = pyodideReady+1;' );
  script.add('          checkReady(pkgName,numpaks,continueFunc);');
  script.add('        } ');
  script.add('        else {alert("Pyodide failed to load package " +pkgName+ " - please check console for details");} ' );
  script.add('      } ');
  script.add('      else {' );
  script.add('        console.log("Pyodide failed to load package " +pkgName+ " - attempting with micropip..."); ' );
  script.add('        if (PyodideOffline==false) { ' );
  script.add('          pyodide.runPython("import micropip"); ' );
  script.add('          pyodide.runPythonAsync("await micropip.install(''"+pkgName+"'')").then(() => {');
  script.add('            if (1==1) {');
  script.add('              PkgLoaded(pkgName); ');
  script.add('              checkReady(pkgName,numpaks,continueFunc);');
  script.add('            }');
  script.add('          },() => {alert("Pyodide/micropip failed to load package " +pkgName+ " - please check console for details");});');
  script.add('        }');
  script.add('        else {alert("Pyodide failed to load package " +pkgName+ " - please check console for details");} ' );
  script.add('      }  ');
  script.add('    },() => {alert("Pyodide failed to load package " +pkgName+ " - please check console for details");} );');
  script.add('  }  ');
  script.add('  catch(err) {alert("Pyodide failed to load package " +pkgName+ " - please check console for details");}  ');
  script.add(' }} ');

  script.add('function PkgLoaded(pkgName) { ');
  script.add('  pyodideReady = pyodideReady+1;' );
  script.add('  console.log("pyodideReady="+pyodideReady);' );
  script.add('  console.log(pkgName+" is now available");');
  script.add('  otherloadedPackages.push(pkgName); ');
  script.add('  }');

  script.add('async function LoadNextPkg(pkgNames,i,continueFunc) { ');
  script.add('  console.log("loadNextPackage ",i); ');
  script.add('  var mystr=pkgNames[i]; ');
  script.add('  console.log("Load Package "+mystr); ');
  script.add('  if (mystr=="xarray") {mystr="xarray==0.19.0";} ');
  script.add('  if ((!(pkgNames[i] in pyodide.loadedPackages)) && (!(otherloadedPackages.includes(pkgNames[i])))) {');
  script.add('    if (PyodideOffline==false) { ');
  script.add('      console.log("Need to load "+mystr); ');
  script.add('      pyodide.runPython("micropip.install(''"+mystr+"'')").then(() => {');
  script.add('        console.log(mystr+" micropip returned ok");');
  script.add('        testPyPkLoaded(pkgNames[i]);');
  script.add('        if (pas.PyXUtils.PyPkTest==1) {');
  script.add('          PkgLoaded(pkgNames[i])');
  script.add('          checkReady(pkgNames[i],pkgNames.length+2,continueFunc);');
  script.add('        }');
  script.add('        else {alert("Pyodide/micropip failed to load package " +pkgNames[i]+ " - please check console for details");} ' );
  script.add('        if (i<pkgNames.length-1) {');
  script.add('          i=i+1;');
  script.add('          LoadNextPkg(pkgNames,i,continueFunc);');
  script.add('        }');
  script.add('      },() => {alert("Pyodide/micropip failed to load package " +pkgNames[i]+ " - please check console for details");})');
  script.add('    } ');
  script.add('    else {');      // offline Pyodide.  micropip.install unavailable
  script.add('      loadPyPkg0(pkgNames[i],pkgNames.length+2,continueFunc)');
  script.add('      if (i<pkgNames.length-1) {');
  script.add('        i=i+1;');
  script.add('        LoadNextPkg(pkgNames,i,continueFunc); }');
  script.add('    } ');
  script.add('  } ');
  script.add('  else {');
  script.add('    console.log(mystr + " already loaded. "); ');
  script.add('    if (i<pkgNames.length-1) {');
  script.add('      i=i+1;');
  script.add('      await LoadNextPkg(pkgNames,i,continueFunc);');
  script.add('      checkReady(pkgNames[i],pkgNames.length+2,continueFunc);');
  script.add('    }');
  script.add('    else { ');
  script.add('      checkReady("",pkgNames.length+2,continueFunc);');
  script.add('    }');
  script.add('  }}');

  script.add('async function loadPyPkgs(continueFunc) { ');
  script.add('  console.log("loadPyPkgs"); ' );
  script.add('  pyodide.runPython("import micropip"); ' );
    //???           //https://pypi.org/project/numpy/#files
  script.add('  ShowPystatdiv()');
  script.add('  var pkNames=pas.PyXUtils.BuildPackageList();' );
  script.add('  if (pkNames.length>0) {await LoadNextPkg(pkNames,0,continueFunc);} ' );
  script.add('  else {checkReady("",2,continueFunc);} ' );
  script.add('} ');

  script.add('</script> ');


  script.add('<script> ');
  //script.add('        window.addEventListener(''DOMContentLoaded'', function() { ');
  script.add('        window.addEventListener(''load'', function() { ');
  //script.add('             languagePluginLoader.then(() => { pysrcLoaded();}); ');
  script.add('	    });');
  script.add('</script> ');

  result:=script;
end;

procedure RunInitialScript;
var
  InitScript:TStringList;
//  FPUExceptionMask:TFPUExceptionMask;
begin
  if PyMemoComponent=nil then
    PyMemoComponent:=XIDEMain.XIDEForm.XMemo1;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  InitScript:=TStringList.Create;
  // load the initialisation py script
  // Sets up an internal library of XIDE Interface functions, available to the user.
  InitScript.Clear;
  InitScript.add('import sys');
  InitScript.add('import json');
  InitScript.add('print(sys.version)');
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
  InitScript.add('  PyInterfaceVar.Value = Xmsg');
  InitScript.add('  return Xmsg.rslt');
  InitScript.add('def GetPropertyValue(NodeName,PropName):');
  //InitScript.add('  print(''GetPropertyValue(''+NodeName+'',''+PropName+'')'')');
  InitScript.add('  return RunXIDEFunc(''GetPropertyValue'',(NodeName,PropName)).decode(''utf-8'')');
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
  InitScript.add('def GetTableDataForExcel(TableName):');
  InitScript.add('  RunXIDEFunc(''GetTableDataForExcel'',(TableName))');
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
  InitScript.add('def DeleteComponent(NodeId,ShowNotFoundMsg,ShowConfirm):');
  InitScript.add('  return RunXIDEFunc(''DeleteComponent'',(NodeId,ShowNotFoundMsg,ShowConfirm))');
  InitScript.add('def GetGPUParamNumValue(GPUName,pName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUParamNumValue'',(GPUName,pName))');
  InitScript.add('def GetGPUParam2DNumValue(GPUName,pName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUParam2DNumValue'',(GPUName,pName))');
  InitScript.add('def GetGPUConstIntValue(GPUName,pName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUConstIntValue'',(GPUName,pName))');
  InitScript.add('def SetGPUParamNumValue(GPUName,pName,pValue):');
  InitScript.add('  RunXIDEFunc(''SetGPUParamNumValue'',(GPUName,pName,pValue))');
  InitScript.add('def SetGPUParam2DNumValue(GPUName,pName,pValue):');
  InitScript.add('  RunXIDEFunc(''SetGPUParam2DNumValue'',(GPUName,pName,pValue))');
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
  InitScript.add('def UserSystemAsString():');
  InitScript.add('  return RunXIDEFunc(''UserSystemAsString'',(0,0)).decode(''utf-8'')');
  InitScript.add('def LoadUserSystemString(SystemString):');
  InitScript.add('  RunXIDEFunc(''LoadUserSystemString'',(SystemString,0))');
  InitScript.add('def ConsoleLog(txt):');
  InitScript.add('  RunXIDEFunc(''ConsoleLog'',(txt,0))');
  InitScript.add('def Array2DToString(arr):');
  InitScript.add('  return RunXIDEFunc(''Array2DToString'',(arr,0)).decode(''utf-8'')');
  InitScript.add('def GetGPUPixelArray(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUPixelArray'',(GPUName,0))');
  InitScript.add('def GetGPUPixelArrayAsString(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUPixelArrayAsString'',(GPUName,0))');
  InitScript.add('def GetGPUStageArray(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUStageArray'',(GPUName,0))');
  InitScript.add('def GetGPUStageArrayAsString(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUStageArrayAsString'',(GPUName,0))');
  InitScript.add('def GetGPUInitStageArray(GPUName):');
  InitScript.add('  return RunXIDEFunc(''GetGPUInitStageArray'',(GPUName,0))');
  InitScript.add('def ShowPythonPlot(ImgName,fig):');            //!!!! do this with string var instead of file ????
  InitScript.add('  fig.savefig(ImgName+''.png'')');
  InitScript.add('  SetPropertyValue(ImgName,''Source'',ImgName+''.png'')');
  InitScript.add('def ConvertNumpyArrayToJSON(npArray):');
  InitScript.add('  return json.dumps(npArray.tolist())');
  InitScript.add('def SetPyConsole(nm):');
  InitScript.add('  RunXIDEFunc(''RedirectPyLog'',(nm,))');
  InitScript.add('def ResetXArrays(DefaultDims):');
  InitScript.add('  RunXIDEFunc(''BuildXArrays'',(DefaultDims,))');

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
asm
console.log('......................RunInitialScript');
end;
  // remove any previously-declared python vars, functions etc
  txt:='for name in dir(): '+LineEnding+
       '  if not name.startswith(''_''):'+LineEnding+
       '    del globals()[name]'+LineEnding;
  asm
  pyodide.runPython(txt);
  end;

  InitScript:=TStringList.Create;
  // load the initialisation py script
  // Sets up an internal library of XIDE Interface functions, available to the user.
  InitScript.Clear;

  InitScript.add('print("Initialising Pyodide Python environment...")');

  InitScript.add('class eClass:');
  InitScript.add('  EventType = ''''');
  InitScript.add('  NodeId = ''''');
  InitScript.add('  NameSpace = ''''');
  InitScript.add('  ReturnString = ''''');
  InitScript.add('e = eClass()');
  InitScript.add('');
  InitScript.add('from js import pas');
  InitScript.add('import io');
  InitScript.add('import base64');
  InitScript.add('import json');
  InitScript.add('def GetPropertyValue(NodeName,PropName):');
  InitScript.add('  return pas.InterfaceTypes.GetPropertyValue(NodeName,PropName)');
  InitScript.add('def SetPropertyValue(NodeName,PropName,NewValue):');
  InitScript.add('  pas.InterfaceTypes.SetPropertyValue(NodeName,PropName,NewValue)');
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
  InitScript.add('def GetTableDataForExcel(TableName):');
  InitScript.add('  pas.InterfaceTypes.GetTableDataForExcel(TableName)');
  InitScript.add('def LoadTableFromNumArray(TableName,NumArray):');
  InitScript.add('  pas.InterfaceTypes.LoadTableFromNumArray(TableName,NumArray)');
  InitScript.add('def GetTableDataArray(TableName,SkipHeader):');
  InitScript.add('  arr = eval(pas.InterfaceTypes.GetPropertyValue(TableName,''TableData''))');
  InitScript.add('  if ((len(arr)>0) and (SkipHeader==True)):');
  InitScript.add('    hdr = arr.pop(0)');
  InitScript.add('  return arr');
  InitScript.add('def DoEvent(EventType,NodeId,myValue):');
  InitScript.add('  pas.InterfaceTypes.DoEvent(EventType,NodeId,myValue)');
  InitScript.add('def MoveComponent(NodeId,NewParentId):');
  InitScript.add('  pas.InterfaceTypes.MoveComponent(NodeId,NewParentId)');
  InitScript.add('def CopyComponent(NodeId,NewParentId,NewName):');
  InitScript.add('  pas.InterfaceTypes.CopyComponent(NodeId,NewParentId,NewName)');
  InitScript.add('def DeleteComponent(NodeId,ShowNotFoundMsg,ShowConfirm):');
  InitScript.add('  return pas.InterfaceTypes.DeleteComponent(NodeId,ShowNotFoundMsg,ShowConfirm)');
  InitScript.add('def GetGPUParamNumValue(GPUName,pName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUParamNumValue(GPUName,pName)');
  InitScript.add('def GetGPUParam2DNumValue(GPUName,pName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUParam2DNumValue(GPUName,pName)');
  InitScript.add('def GetGPUConstIntValue(GPUName,pName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUConstIntValue(GPUName,pName)');
  InitScript.add('def SetGPUParamNumValue(GPUName,pName,pValue):');
  InitScript.add('  pas.InterfaceTypes.SetGPUParamNumValue(GPUName,pName,pValue)');
  InitScript.add('def SetGPUParam2DNumValue(GPUName,pName,pValue):');
  InitScript.add('  pas.InterfaceTypes.SetGPUParam2DNumValue(GPUName,pName,pValue)');
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
  InitScript.add('def GetGPUInitStageArray(GPUName):');
  InitScript.add('  return pas.InterfaceTypes.GetGPUSInittageArray(GPUName)');
  InitScript.add('def ShowPythonPlot(ImgName,fig):');
  InitScript.add('  buf = io.BytesIO()');
  InitScript.add('  fig.savefig(buf, format=''png'')');
  InitScript.add('  buf.seek(0)');
  InitScript.add('  img_str = ''data:image/png;base64,'' + base64.b64encode(buf.read()).decode(''UTF-8'')');
  InitScript.add('  pas.InterfaceTypes.SetImageSource(ImgName,img_str)');
  InitScript.add('def ConvertNumpyArrayToJSON(npArray):');
  InitScript.add('  return json.dumps(npArray.tolist())');
  InitScript.add('def PyodideLoadPackage(nm):');
  InitScript.add('  pas.InterfaceTypes.PyodideLoadPackage(nm)');
  InitScript.add('def PyodidePackageLoaded(nm):');
  InitScript.add('  return pas.InterfaceTypes.PyodidePackageLoaded(nm)');
  InitScript.add('def SetPyConsole(nm):');
  InitScript.add('  pas.PyXUtils.RedirectPyLog(nm)');
  InitScript.add('def ResetXArrays(DefaultDims):');
  InitScript.add('  pas.XDataModel.BuildXArrays(DefaultDims)');

  InitScript.add('print("Initialising Python done.")');


  // execute the initialisation py script
  txt:=InitScript.Text;
  asm
  console.log('RUN InitScript..............');
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

procedure RedirectPyLog(MemoName:String);
var
  MemoNode:TdataNode;
begin
  MemoNode:=NodeUtils.FindDataNodeById(UIRootNode,MemoName,'',false);
  {$ifndef JScript}
  if (MemoNode<>nil)
  and (MemoNode.ScreenObject is TXMemo) then
    PyMemoComponent:=TXMemo(MemoNode.ScreenObject);
  {$else}
  if (MemoNode<>nil)
  and (MemoNode.NodeType='TXMemo') then
    PyMemoComponent:=TXMemo(MemoNode);
  {$endif}
end;

procedure BuildXarray(XArrName:String;dims,mults:TStringArray;dflt:String);
var
  s: TStringList;
  i,m:integer;
  smults:String;
begin
//#Example = xr.DataArray(np.empty((X1_Num,X2_Num,Y_Num,1,NumXVars,)),  dims={"X1_Num":X1_Num,"X2_Num":X2_Num,"Y_Num":Y_Num,"one":1,"NumXVars":NumXVars})

  s := TStringList.create;
  s.Add('import numpy as np');
  s.Add('import xarray as xr');
  s.Add('dimsdict={}');

  // build the dimensions dictionary for this XArray
  smults:='';
  for i:=0 to length(dims)-1 do
  begin
    //dimsdict[dimname] =  nnn
    s.Add('dimsdict["'+dims[i]+'"]='+mults[i]);
    smults:=smults+mults[i]+',';
  end;
  //  build the XArray
  s.Add('print('''+XArrName+' = xr.DataArray(np.empty(('+smults+')), dims='',dimsdict,'')'+''')');
  s.add(XArrName+' = xr.DataArray(np.empty(('+smults+')), dims=dimsdict)');
  if dflt<>'' then
    s.add(XArrName+' = xr.full_like('+XArrName+','+dflt+')');
  s.Add('print('+XArrName+')');

  PyExeString( s.text);
  s.free;
end;

//var.decode('utf-8') ... might be of use

end.
{$endif}

