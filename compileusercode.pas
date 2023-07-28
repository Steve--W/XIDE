(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit CompileUserCode;

(*
This unit manages the Free Pascal compilation of user-created event handler code
into a dll, with functions callable in run mode.

Equivalent compilation to JS, for use in the HTML runtime, is done using pas2js compiler.

*)

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, StringUtils, StrUtils, NodeUtils,  UtilsJSCompile,
  webTranspilerUtils,
  XComposite,XGPUCanvas,
{$ifndef JScript}
  {$if defined ( windows)}
  Windows,                           // for CurrentThreadID
  {$endif}
  //CompilerLogUnit,
  LazsUtils, Controls, URIParser,
  PropEdits,TypInfo, Dialogs, Dynlibs,Process,
  XCode, Forms, Events, xpparser;
{$else}
  HTMLUtils,XCode,XIFrame,Events,
  webfilecache, pas2jswebcompiler, pparser,
  InterfaceTypes;
{$endif}

{$ifdef Python}
{$ifndef JScript}
type
  // record type for queued async functions
  TPyXQueueRec = record
    QDummy: string;
  end;
  PPyXQueueRec = ^TPyXQueueRec;
{$endif}
type
  TPyProcs = class(TObject)
    procedure GatherAndRunPythonScripts(parms:PtrInt);
  end;
var
    PyProcs:TPyProcs;
{$endif}

function CompileEventCode(MyCodeEditor:TXCode;RunMode:String):Boolean;
function   DfltUnitCode(UnitName,UnitType:String):string;
function   DfltEventCode:string;
function   DfltOpCode:string;
function   DfltThreadEventCode(NodeName:String):string;
function   DfltTreeNodeEventCode:string;
function   DfltPythonCode:string;
procedure GatherSourcedAttributes(StartNode:TDataNode);
{$ifdef Python}
{$ifdef JScript}
procedure GatherAndRunPythonScriptsFromJS;
{$endif}
function RunPyScript(PyScriptCode:TStringList;nm:String):Boolean;
procedure GatherAndRunPythonScriptsLater;
{$endif}

//type
//  TUserPyFile = record
//    ModuleName:String;
//    FuncName:String;
//  end;
//
Var
//  PyFuncsList:array of TUserPyFile;
  PyFuncsString:String;

var
    ConfigfpcPath:String;
    AllUserPyCode:String;

{$ifndef JScript}
{$else}
var
    JSOutput:String;


    eventsinterfacepas:String;
    interfacetypespas:String;

    classespas:String;
    contnrspas:String;
    dateutilspas:String;
    jspas:String;
    mathpas:String;
    rtlconstspas:String;
    strutilspas:String;
    systempas:String;
    sysutilspas:String;
    typespas:String;
    typinfopas:String;

{$endif}



implementation
uses  XObjectInsp, PyXUtils, XDataModel;

{$ifndef JScript}
type
   TMyDLL = function (var instring:string): string; stdcall ;

var
    AProcess: TProcess;
{$endif}
var
    PascalCode,ExportsList,NamespaceUnits,PyCodeFromComposites:TStringList;
    AllGPUNodes:TNodesArray;



{$ifndef JScript}
procedure DeleteDynamicIncFiles;
var
  lSearchRec:TSearchRec;
  lPath:string;
  procedure DeleteThem(aMask:String);
  begin
    if FindFirst(lPath+aMask,faAnyFile,lSearchRec) = 0 then
    begin
      try
        repeat
          SysUtils.DeleteFile(lPath+lSearchRec.Name);    //!! does not delete any files it thinks are open...
        until SysUtils.FindNext(lSearchRec) <> 0;
      finally
        SysUtils.FindClose(lSearchRec);  // Free resources on successful find
      end;
    end;
  end;

begin
  if not DirectoryExists('tempinc') then
    CreateDir('tempinc');

  lPath := 'tempinc/';
  DeleteThem('*.*');

  lPath := 'resources/xcomponents/';
  DeleteThem('*.ppu');

end;

procedure CloseMyFile(FileName:String);
var
   tf: TextFile;
begin
  if FileExists(FileName) then
  begin
    AssignFile(tf,FileName);
   {$I-}
    Reset(tf);
   {$I+}
   if IOResult <> 0 then   // file is open
   begin
    try
      CloseFile(tf);
    finally
    end;
   end;
 end;
end;

procedure WriteRTLIncFile(filepath,filename,suffix:String);
var
  TheStream:TFileStream;
  Incname:string;
  TxtLines:TStringList;
  i:integer;
begin
  // Load up the required file
  TxtLines:=TStringList.Create;
  TxtLines.LoadFromFile(filepath+filename+'.'+suffix);

  // Set up a file stream for the target .inc file
  IncName:='tempinc/'+filename+suffix+'.inc';
  try
    TheStream:=TFileStream.Create(IncName,fmCreate or fmOpenRead or fmOpenWrite or fmShareDenyNone);
  except
    showmessage('Unable to create file '+IncName);
  end;


  if suffix='js' then
  begin
    TxtLines.Text:=''''+SubstituteSpecials(TxtLines.Text)+'''';
  end
  else
  begin
      // Substitute single-quotes in the text to be copied
      TxtLines.Text:=ReplaceStr(TxtLines.Text,'''','&myapos;');
      for i:=0 to TxtLines.Count-1 do
      begin
        TxtLines[i]:='+ '''+TxtLines[i]+'\n'' ' ;
      end;
  end;


  // Wrap the text with pascal code that will load it into a string variable.
  if filename+suffix='gpu-browserjs' then
  begin
    // gpu-browser.js
    TxtLines.Insert(0,'pas.XGPUCanvas.gpujs = ');
  end
  else if suffix='js' then
    TxtLines.Insert(0,'pas.CompileUserCode.'+filename+suffix+' = ')
  else
    TxtLines.Insert(0,'pas.CompileUserCode.'+filename+suffix+' = '''' ');

  TxtLines.Insert(0,'asm');
  TxtLines.Add(';');
  TxtLines.Add('end;');
  //... and will put back the original quote chars
  if suffix='js' then
  begin
  end
  else
  begin
    //TxtLines.Add(filename+suffix+':=MyStringReplace('+filename+suffix+',''&myapos;'','''''''',-1,-1);');
    TxtLines.Add(filename+suffix+':=ReplaceStr('+filename+suffix+',''&myapos;'','''''''');');
  end;

  // Save the new .inc file
  TxtLines.SaveToStream(TheStream);

  TheStream.Free;
  TxtLines.Free;
end;

procedure WriteRTLIncFile(filepath,filename:String);  overload;
begin
  WriteRTLIncFile(filepath,filename,'pas');
end;

procedure WriteRTLIncFiles;
// Write files required by the pas2js compiler
begin

  // minimal required rtl set....
 // WriteRTLIncFile('resources/rtl/','system');
 // WriteRTLIncFile('resources/rtl/','rtlconsts');
 // WriteRTLIncFile('resources/rtl/','js');
 // WriteRTLIncFile('resources/rtl/','sysutils');
 // WriteRTLIncFile('resources/rtl/','types');
 // WriteRTLIncFile('resources/rtl/','classes');

  // common rtl set.....
  WriteRTLIncFile('resources/rtl/','classes');
  WriteRTLIncFile('resources/rtl/','contnrs');
  WriteRTLIncFile('resources/rtl/','dateutils');
  WriteRTLIncFile('resources/rtl/','js');
  WriteRTLIncFile('resources/rtl/','math');
  WriteRTLIncFile('resources/rtl/','rtlconsts');
  WriteRTLIncFile('resources/rtl/','strutils');
  WriteRTLIncFile('resources/rtl/','system');
  WriteRTLIncFile('resources/rtl/','sysutils');
  WriteRTLIncFile('resources/rtl/','types');
  WriteRTLIncFile('resources/rtl/','typinfo');

  //  WriteRTLIncFile('resources/rtl/','rtti');
  //  WriteRTLIncFile('resources/rtl/','timer');
  //  WriteRTLIncFile('resources/rtl/','nodejs');
  //  WriteRTLIncFile('resources/rtl/','objpas');
  //  WriteRTLIncFile('resources/rtl/','libjquery');
  //  WriteRTLIncFile('resources/rtl/','hotreloadclient');
  //  WriteRTLIncFile('resources/rtl/','class2pas');
  //  WriteRTLIncFile('resources/rtl/','browserconsole');
  //  WriteRTLIncFile('resources/rtl/','web');
  //  WriteRTLIncFile('resources/rtl/','webaudio');
  //  WriteRTLIncFile('resources/rtl/','webbluetooth');
  //  WriteRTLIncFile('resources/rtl/','webgl');
  //  WriteRTLIncFile('resources/rtl/','webrouter');

  WriteRTLIncFile('resources/project/','gpu-browser','js');
end;


procedure LoadFPCConfig;
var
  TheStream : TFileStream;
  TheLines:TStringList;
begin
  // pick up the last-used config file XIDERunSettings.dta
  TheLines:=TStringList.Create;
  if FileExists(ProjectDirectory+'XIDERunSettings.dta') then
  begin
    try
      TheStream:=TFileStream.Create(ProjectDirectory +'XIDERunSettings.dta',fmOpenRead or fmShareDenyNone);
      TheLines.LoadFromStream(TheStream);
      TheStream.Free;
    except
      TheLines.AddStrings(ConfigFPCPath,true);
    end;
    if TheLines.Count>0 then
    begin
      ConfigfpcPath:=TheLines[0];
    end;
  end;
  FreeAndNil(TheLines);
end;

{$endif}


function BuildEventheader(NameSpace,NodeName,EventType,RunMode,Phase:String):String;
var
  procName:String;
begin
  procName:= 'procedure '+NameSpace+NodeName + 'Handle' + EventType + Phase;
  if RunMode<>'JSJS' then
  begin
    result:=procName + '(e:TEventStatus;NodeId: AnsiString; myValue: AnsiString); ';
    if RunMode='LazDll' then
      result:=result + '  stdcall; ';
  end
  else
  begin
    // JSJS
    result:=procName + '(e:TEventStatus;NodeId,myValue:String); ';
  end;
end;

procedure GatherEventHeaders(RunMode,NameSpace:String;StartNode:TDataNode;UnitCode:TStringList;var n:integer);
var
    i:integer;
    tmp,Dflt:string;
begin
  if StartNode.NameSpace= NameSpace then
  begin
    // user-created event code is held in the data nodes (node.myEventHandlers[i].TheCode)
    if (StartNode.IsDynamic) or (StartNode=UIRootNode) then
      for i:=0 to length(StartNode.myEventHandlers)-1 do
      begin
        // (exclude events for worker threads...)
        if (StartNode.NodeType<>'TXThreads')
        or (FoundString(StartNode.myEventTypes[i],'Thread')<>1) then
        begin
          tmp:=StartNode.myEventHandlers[i].TheCode;
          dflt:=DfltEventCode;
          if (trim(StartNode.myEventHandlers[i].TheCode)<>'')
          and (tmp<>Dflt) then
          begin
            n:=n+1;
            tmp:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],RunMode,'');
            UnitCode.Add(tmp);
          end;
        end;
      end;
  end;

    for i:=0 to length(StartNode.ChildNodes)-1 do
      GatherEventHeaders(RunMode,NameSpace,StartNode.ChildNodes[i],UnitCode,n);

end;

procedure GatherEventHeadersForWorkerThreads(RunMode:String;StartNode:TDataNode;CodeBlock:TStringList);
var
    i:integer;
    tmp,Dflt:string;
begin

  if (StartNode.IsDynamic)
  and (StartNode.NodeType='TXThreads')  then
  begin
    for i:=0 to length(StartNode.myEventHandlers)-1 do
    begin
      if (FoundString(StartNode.myEventTypes[i],'Thread')=1)
      and (FoundString(StartNode.myEventTypes[i],'ThreadVars')<>1) then
      begin
        tmp:=StartNode.myEventHandlers[i].TheCode;
        dflt:=DfltThreadEventCode(StartNode.NodeName);
        if (trim(StartNode.myEventHandlers[i].TheCode)<>'')
        and (tmp<>Dflt) then
        begin
          tmp:=BuildEventHeader('',StartNode.NodeName,StartNode.myEventTypes[i],RunMode,'');  //!!!!namespace
          CodeBlock.Add(tmp);
        end;
      end;
    end;
  end;

  for i:=0 to length(StartNode.ChildNodes)-1 do
    GatherEventHeadersForWorkerThreads(RunMode,StartNode.ChildNodes[i],CodeBlock);
end;

procedure GatherVarsForWorkerThreads1(RunMode:String;StartNode:TDataNode;CodeBlock:TStringList);
var
    i:integer;
    VarNames:string;
    VarNamesList:TStringList;
begin

  if (StartNode.IsDynamic)
  and (StartNode.NodeType='TXThreads')  then
  begin
    VarNames := StartNode.GetAttribute('ThreadVarNums',true).AttribValue;
    if VarNames<>'' then
    begin
      CodeBlock.Add('type TXVars'+StartNode.NodeName+' = class(TObject)');
      CodeBlock.Add('private');
      VarNamesList:=TStringList.Create;
      VarNamesList.StrictDelimiter:=true;
      VarNamesList.Delimiter:=',';
      VarNamesList.LineBreak:=',';
      VarNamesList.Text:=VarNames;
      for i:=0 to VarNamesList.Count-1 do
      begin
          CodeBlock.Add('  f'+VarNamesList[i]+':float;');
      end;
      for i:=0 to VarNamesList.Count-1 do
      begin
          CodeBlock.Add('  procedure set'+VarNamesList[i]+'(AValue:float);');
      end;
      CodeBlock.Add('public');
      for i:=0 to VarNamesList.Count-1 do
      begin
          CodeBlock.Add('  property '+VarNamesList[i]+':float read f'+VarNamesList[i]+' write set'+VarNamesList[i]+';');
      end;
      CodeBlock.Add('end;');
      CodeBlock.Add('var XVars'+StartNode.NodeName+':TXVars'+StartNode.NodeName+';');

      VarNamesList.Free;
    end;
  end
  else
    for i:=0 to length(StartNode.ChildNodes)-1 do
      GatherVarsForWorkerThreads1(RunMode,StartNode.ChildNodes[i],CodeBlock);
end;
procedure GatherVarsForWorkerThreads2(RunMode:String;StartNode:TDataNode;CodeBlock:TStringList);
var
    i:integer;
    VarNames:string;
    VarNamesList:TStringList;
begin

  if (StartNode.IsDynamic)
  and (StartNode.NodeType='TXThreads')  then
  begin
    VarNames := StartNode.GetAttribute('ThreadVarNums',true).AttribValue;
    if VarNames<>'' then
    begin
      VarNamesList:=TStringList.Create;
      VarNamesList.StrictDelimiter:=true;
      VarNamesList.Delimiter:=',';
      VarNamesList.LineBreak:=',';
      VarNamesList.Text:=VarNames;
      for i:=0 to VarNamesList.Count-1 do
      begin
          CodeBlock.Add('procedure TXVars'+StartNode.NodeName+'.set'+VarNamesList[i]+'(AValue:float);');
          CodeBlock.Add('begin');
          CodeBlock.Add('{$if defined ( windows)}');
          CodeBlock.Add('  if GetCurrentThreadID = MainThreadID  then ');         //!!!! and browser code????
          CodeBlock.Add('{$endif}');
          CodeBlock.Add('    f'+VarNamesList[i]+' := AValue;');                   //!!!! else... raise error?
          CodeBlock.Add('end;');
      end;

      VarNamesList.Free;
    end;
  end
  else
    for i:=0 to length(StartNode.ChildNodes)-1 do
      GatherVarsForWorkerThreads2(RunMode,StartNode.ChildNodes[i],CodeBlock);
end;
procedure GatherVarsForWorkerThreads3(RunMode:String;StartNode:TDataNode;CodeBlock:TStringList);
var
    i:integer;
    VarNames:string;
begin

  if (StartNode.IsDynamic)
  and (StartNode.NodeType='TXThreads')  then
  begin
    VarNames := StartNode.GetAttribute('ThreadVarNums',true).AttribValue;
    if VarNames<>'' then
    begin
      CodeBlock.Add('  XVars'+StartNode.NodeName+':=TXVars'+StartNode.NodeName+'.Create;');
    end;
  end
  else
    for i:=0 to length(StartNode.ChildNodes)-1 do
      GatherVarsForWorkerThreads3(RunMode,StartNode.ChildNodes[i],CodeBlock);
end;

procedure GatherEventCode(RunMode,NameSpace:String;Compiler:TObject;StartNode:TDataNode;UnitCode:TStringList);
var
    i:integer;
    hdr,tmp,Dflt,ns:string;
    IncCode,InitCode:TStringList;
begin
  if StartNode.NameSpace=NameSpace then
  begin
    // user-created event code is held in the data nodes (node.myEventHandlers[i].TheCode)
    IncCode:=TStringList.Create;
    if (StartNode.IsDynamic) or (StartNode=UIRootNode) then
      for i:=0 to length(StartNode.myEventHandlers)-1 do
      // (exclude events for worker threads...)
      if (StartNode.NodeType<>'TXThreads')
      or (FoundString(StartNode.myEventTypes[i],'Thread')<>1) then
      begin
        tmp:=StartNode.myEventHandlers[i].TheCode;
        Dflt:=DfltEventCode;
        if (trim(tmp)<>'')
        and (tmp<>Dflt) then
        begin
           // Insert a procedure containing the code for the event initialisation
           hdr:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],'','Init');
           tmp:=trim(StartNode.myEventHandlers[i].InitCode);
           if not PythonCodeExists then
             PythonCodeExists := (FoundStringCI(tmp,'RunPython(')>0);
           InitCode:=StringSplit(tmp,LineEnding);
           InitCode.Insert(0,hdr);
           if InitCode.Count=1 then
             InitCode.Add('begin end;');
           if Namespace='' then
             ns:=''
           else
             ns:=Namespace+'__';
           WriteIncFile(Compiler,ns+StartNode.NodeName, StartNode.myEventTypes[i]+'__Init','tempinc/', UnitCode, InitCode);

           // Insert a procedure containing the main code for the event
           hdr:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],'','Main');
           tmp:=StartNode.myEventHandlers[i].TheCode;
           if not PythonCodeExists then
             PythonCodeExists := (FoundStringCI(tmp,'RunPython(')>0);
           IncCode:=StringSplit(tmp,LineEnding);
           IncCode.Insert(0,hdr);
           WriteIncFile(Compiler,ns+StartNode.NodeName, StartNode.myEventTypes[i],'tempinc/', UnitCode, IncCode);

           // Insert a control procedure to run the init and main code for the event
           tmp:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],RunMode,'');
           UnitCode.Add(tmp);

           UnitCode.Add('begin');
           UnitCode.Add('  ExecuteEventHandler(e,nodeID,myValue,'
                        +'@'+NameSpace+StartNode.NodeName + 'Handle' + StartNode.myEventTypes[i] + 'Init'+','
                        +'@'+NameSpace+StartNode.NodeName + 'Handle' + StartNode.myEventTypes[i] + 'Main'+');');
           UnitCode.Add('end;');


           {$ifndef JScript}
           if RunMode='LazDll' then
             ExportsList.Add('exports '+NameSpace+StartNode.NodeName + 'Handle' + StartNode.myEventTypes[i]+';');
           {$endif}

        end;
      end;
    FreeAndNil(IncCode);

  end;

  for i:=0 to length(StartNode.ChildNodes)-1 do
      GatherEventCode(RunMode,NameSpace,Compiler,StartNode.ChildNodes[i],UnitCode);

end;

procedure GatherEventCodeForWorkerThreads(RunMode,NameSpace:String;Compiler:TObject;StartNode:TDataNode;CodeBlock:TStringList);
var
    i:integer;
    hdr,tmp,Dflt,et:string;
    IncCode:TStringList;
begin
  // user-created event code is held in the data nodes (node.myEventHandlers[i].TheCode)
  IncCode:=TStringList.Create;
  if (StartNode.NodeType='TXThreads')
  and (StartNode.IsDynamic) then
    for i:=0 to length(StartNode.myEventHandlers)-1 do
    if FoundString(StartNode.myEventTypes[i],'Thread')=1 then          // just the Thread<n> events
    begin
      et:=StartNode.myEventTypes[i];
      tmp:=StartNode.myEventHandlers[i].TheCode;
      Dflt:=DfltThreadEventCode(StartNode.NodeName);
      if (trim(tmp)<>'')
      and (tmp<>Dflt) then
      begin

         // Insert a procedure containing the main code for the event
         hdr:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],RunMode,'');
         IncCode:=StringSplit(StartNode.myEventHandlers[i].TheCode,LineEnding);
         IncCode.Insert(0,hdr);
         WriteIncFile(Compiler,StartNode.NodeName, StartNode.myEventTypes[i],'tempinc/', CodeBlock, IncCode);

         {$ifndef JScript}
         if RunMode='LazDll' then
           CodeBlock.Add('exports '+NameSpace+StartNode.NodeName + 'Handle' + StartNode.myEventTypes[i]+';');
         {$endif}

      end;
    end;
  FreeAndNil(IncCode);

  for i:=0 to length(StartNode.ChildNodes)-1 do
    GatherEventCodeForWorkerThreads(RunMode,NameSpace,Compiler,StartNode.ChildNodes[i],CodeBlock);
end;


procedure BuildThreadEventsUnit(Compiler:TObject;RunMode:String);
var
  UnitCode:TStringList;
    procedure AddUnitCodeLine(str:String);
    begin
      UnitCode.add(str);
    end;
begin
  UnitCode:=TStringList.Create;
  // Note: this unit does not contain the user-interface functions provided for main-thread events
  // (units InterfaceTypes, or dllInterface).

  AddUnitCodeLine('unit '+DllName+'Threads;');
  AddUnitCodeLine('{$ifndef JScript}');
  AddUnitCodeLine('{$mode objfpc}{$H+}');
  AddUnitCodeLine('{$endif}');
  AddUnitCodeLine('interface');
  AddUnitCodeLine('uses Classes, SysUtils, Math, contnrs, dateutils,');
  AddUnitCodeLine('{$if defined ( windows)}');
  AddUnitCodeLine('  windows, ');
  AddUnitCodeLine('{$endif}');
  AddUnitCodeLine('  rtlconsts, strutils, types, typinfo, EventsInterface;');

  AddUnitCodeLine('');
  AddUnitCodeLine('type AnsiString=String;');

  AddUnitCodeLine('');
  GatherEventHeadersForWorkerThreads(RunMode,SystemNodeTree,UnitCode);

  AddUnitCodeLine('');
  GatherVarsForWorkerThreads1(RunMode,SystemNodeTree,UnitCode);

  AddUnitCodeLine('');
  AddUnitCodeLine('implementation' );
  AddUnitCodeLine('');

  GatherVarsForWorkerThreads2(RunMode,SystemNodeTree,UnitCode);
  GatherEventCodeForWorkerThreads(RunMode,'',Compiler,SystemNodeTree,UnitCode);

  AddUnitCodeLine('    ' );
  AddUnitCodeLine('begin');
  GatherVarsForWorkerThreads3(RunMode,SystemNodeTree,UnitCode);
  AddUnitCodeLine('end.');

  {$ifndef JScript}
  // save the generated pas file
  SysUtils.DeleteFile('tempinc/'+DllName+'Threads'+'.pas');
  UnitCode.SaveToFile('tempinc/'+DllName+'Threads'+'.pas');
  {$else}

  TPas2JSWebCompiler(Compiler).WebFS.SetFileContent(DllName+'Threads'+'.pas',UnitCode.Text);
  {$endif}

  // add this unit to the uses list in the main module
  PascalCode.Add(','+DllName+'Threads');

  FreeAndNil(UnitCode);
end;

function AddPasUnit(UnitNode:TDataNode; MainUnitCode:TStringList;Compiler:TObject):String;
var
    tmp,nm:string;
    UnitCode:TStringList;
begin
  UnitCode:=TStringList.Create;
  // Pascal Unit code is all in attribute attribute : Code
  UnitCode.Clear;
  nm:=UnitNode.NodeName;

  if UnitNode.NameSpace<>'' then
    nm:=UnitNode.NameSpace+'__'+nm;
  UnitCode.Add('unit '+nm+';');

  // add user-written unit code block
  tmp:=UnitNode.GetAttribute('Code',true).AttribValue;
  UnitCode.Append(tmp);

  {$ifndef JScript}
  // save the generated pas file
  SysUtils.DeleteFile('tempinc/'+nm+'.pas');
  UnitCode.SaveToFile('tempinc/'+nm+'.pas');
  {$else}
  TPas2JSWebCompiler(Compiler).WebFS.SetFileContent(nm+'.pas',UnitCode.Text);
  //WriteToLocalStore(nm+'.pas',UnitCode.Text);
  XIDEUserUnits.add(nm+'.pas');      // used within the pas2js compiler (see pparser.pp)
  {$endif}

  // add this unit to the uses list in the main module
  MainUnitCode.Add(','+nm);

  FreeAndNil(UnitCode);
  result:=nm;
end;

procedure AddPyScript(UnitNode:TDataNode; PythonCode:TStringList);
var
    tmp,nm:string;
begin
  // Python Script code is all in attribute : Code
  nm:=UnitNode.NodeName;
  if UnitNode.NameSpace<>'' then
    nm:=UnitNode.NameSpace+'__'+nm;

  PythonCode.add('print(''executing script '+nm+''')');

  // add user-written unit code block
  tmp:=UnitNode.GetAttribute('Code',true).AttribValue;
  PythonCode.Append(tmp);

end;


procedure AddExecFunc(Namespace:String;UnitCode:TStringList);
begin
  UnitCode.Add('type THandler = procedure(e:TEventStatus;NodeId:AnsiString;myValue:AnsiString); ');
  UnitCode.Add('procedure ExecuteEventHandler(e:TEventStatus;NodeId: AnsiString; myValue: AnsiString; initfunc,mainfunc:THandler); ' );
  UnitCode.Add('var  ' );
  UnitCode.Add('  asyncWaiting:boolean; ' );
  UnitCode.Add('begin' );
  UnitCode.Add('  asyncWaiting := false;' );
  UnitCode.Add('  if (e<>nil) then' );
  UnitCode.Add('    asyncWaiting := e.EventHasWaitingAsyncProcs;' );
  UnitCode.Add('  AppMethods.mmiSetEventsNameSpace('''+Namespace+''');' );
  UnitCode.Add('  if ((e=nil) or (e.InitDone=false)) ' );
  UnitCode.Add('  and (not asyncWaiting) then ' );
  UnitCode.Add('  begin ' );
  UnitCode.Add('    if (e=nil) then' );
  UnitCode.Add('    begin ' );
  UnitCode.Add('      e:=TEventStatus.Create(e.eventType,NodeId);' );
  UnitCode.Add('      e.NameSpace:='''+Namespace+''';' );
  UnitCode.Add('    end;' );
  UnitCode.Add('    e.InitRunning:=true; ' );
  UnitCode.Add('    e.InitDone:=true;' );
  UnitCode.Add('    initfunc(e,NodeId,myValue);' );
  UnitCode.Add('    e.InitRunning:=false;' );
  UnitCode.Add('  end;' );
  UnitCode.Add('  if e.AsyncProcsRunning.Count = 1 then' );
  UnitCode.Add('    e.ClearAsync(''ShowBusy'');' );
  UnitCode.Add('  if e.EventHasWaitingAsyncProcs = false then ' );
  UnitCode.Add('  begin ' );
  {$ifndef JScript}
  UnitCode.Add('    mainfunc(e,NodeId,myValue);' );
  {$else}
  UnitCode.Add('    asm' );     /// timeout/job-queue so that any changes made in the 'init' secton will be refreshed on screen
  UnitCode.Add('    myTimeout(mainfunc,5,''Event Main'',0,e,NodeId,myValue); ');
  UnitCode.Add('    end;' );
  {$endif}
  UnitCode.Add('  end;' );
  UnitCode.Add('end;' );
end;

function GatherUserUnits(RunMode:String; Compiler:TObject):String;
var
    i:integer;
    FirstUnitName,nm:string;

begin

  // user-created units are held as data nodes (attribute 'Code')
  // create a pas file on disk for each unit, and insert the unit name for the dll 'uses' clause

  {$ifdef JScript}
  XIDEUserUnits.clear;
  {$endif}

  for i:=0 to length(CodeRootNode.ChildNodes)-1 do
  begin
    if CodeRootNode.ChildNodes[i].NodeType='PasUnit' then
    begin
      nm:=AddPasUnit(CodeRootNode.ChildNodes[i],PascalCode,Compiler);
      if i=0 then FirstUnitName:=nm;
    end;
  end;

  // Build a separate unit to hold 'event' code for worker threads (within TXThreads components)
  // This code is in a separate unit from the main event code so that the scope of worker threads
  // can be limited (they are self-contained, with no access to data/functions in the main thread).
  BuildThreadEventsUnit(Compiler,RunMode);

  result:=FirstUnitName;

end;


procedure GatherUserUnitsInComposites(RunMode,NameSpace:String;StartNode:TDataNode;MainUnitCode:TStringList;Compiler:TObject);
var
    i:integer;
    tmp:string;
begin
  if (StartNode.NodeType='TXComposite') then
  begin
    for i:=0 to length(StartNode.ChildNodes)-1 do
      if StartNode.ChildNodes[i].NodeType='PasUnit' then
      begin
        tmp:=AddPasUnit(StartNode.ChildNodes[i],MainUnitCode,Compiler);
      end
      else if StartNode.ChildNodes[i].NodeType='PythonScript' then
      begin
        AddPyScript(StartNode.ChildNodes[i],PyCodeFromComposites);
      end;
  end;

end;

procedure BuildNamespaceUnit(RunMode,NameSpace:String; ThisNode:TDataNode;Compiler:TObject);
var
    UnitCode:TStringList;
    n:integer;
    procedure AddUnitCodeLine(str:String);
    begin
      UnitCode.add(str);
    end;

begin

  // create a pas file on disk for each unit, and insert the unit name for the dll 'uses' clause
  {$ifdef JScript}
  NamespaceUnits.Add(NameSpace);
  {$endif}

  n:=0;
  UnitCode:=TStringList.Create;

  // Unit code is built from separate attributes : Code, Init.
  UnitCode.Clear;
  AddUnitCodeLine('unit '+NameSpace+';');
  AddUnitCodeLine('{$ifndef JScript}');
  AddUnitCodeLine('{$mode objfpc}{$H+}');
  AddUnitCodeLine('{$endif}');
  AddUnitCodeLine('interface');
  AddUnitCodeLine('uses Classes, SysUtils, Math, contnrs, dateutils,');
  AddUnitCodeLine('  rtlconsts, strutils, types, typinfo, EventsInterface');
  GatherUserUnitsInComposites(RunMode,NameSpace,ThisNode,UnitCode,Compiler);
  if RunMode='LazDll' then
   AddUnitCodeLine('  ,InterfaceTypesDll;')
  else
  begin
   AddUnitCodeLine('  ,InterfaceTypes;');
  end;

  GatherEventHeaders(RunMode,NameSpace,SystemNodeTree,UnitCode,n);

  AddUnitCodeLine('implementation');

  //GatherUserFuncs(RunMode,NameSpace,Compiler,CodeRootNode,UnitCode,n);

  AddExecFunc(NameSpace,UnitCode);
  GatherEventCode(RunMode,NameSpace,Compiler,SystemNodeTree,UnitCode);

  AddUnitCodeLine('    ' );
  AddUnitCodeLine('begin');
  AddUnitCodeLine('end.');

  if n>0 then
  begin
   {$ifndef JScript}
   // save the generated pas file
   //!!!! namespaces....255 char limit on file names !!!!!!!!!
   SysUtils.DeleteFile('tempinc/'+NameSpace+'.pas');
   UnitCode.SaveToFile('tempinc/'+NameSpace+'.pas');
   {$else}
   TPas2JSWebCompiler(Compiler).WebFS.SetFileContent(NameSpace+'.pas',UnitCode.Text);
   {$endif}

   // add this unit to the uses list in the main module
   PascalCode.Add(','+NameSpace);

  end;

  FreeAndNil(UnitCode);

end;

procedure ConstructNamespaceUnits(RunMode,NameSpace:String; Compiler:TObject; StartNode:TDataNode);
var
    i:integer;
    CompositeNameSpace:String;
begin
  // Where a system node is an encapsulated sub-system, the child nodes will have a non-blank NamesSpace value.
  // For all 'namespace' nodes, construct a separate unit to contain user functions and event code local to the namespace group.
  CompositeNameSpace:=NameSpace;
  if StartNode.NodeType='TXComposite' then
  begin
    CompositeNameSpace:=CompositeNameSpace+StartNode.NodeName;
    if length(StartNode.ChildNodes)>0 then
      BuildNamespaceUnit(RunMode,CompositeNameSpace,StartNode,Compiler);
  end;

  // Walk the system tree looking for namespace components
  for i:=0 to length(StartNode.ChildNodes)-1 do
    if (StartNode.ChildNodes[i].NodeClass='UI')
    or (StartNode.ChildNodes[i].NodeClass='Root') then
      ConstructNamespaceUnits(RunMode,CompositeNameSpace,Compiler,StartNode.ChildNodes[i]);

end;

procedure ConstructDataModelUnits(RunMode:String);
begin
  ConstructPascalDM(RunMode,PascalCode);
end;

function ConstructGPUUnits(RunMode:String; Compiler:TObject):Boolean;
// Each XGPUCanvas widget contains a set of 'animation code' kernels, written in pascal.
// Build a separate pascal unit for each GPU widget, which can be called at runtime to
// provide a GPU emulation mode.
var
  i:integer;
  txt:String;
  ok:Boolean;
begin
  result:=true;
  AllGPUNodes := NodeUtils.FindNodesOfType(UIRootNode,'TXGPUCanvas');
  for i:=0 to length(AllGPUNodes)-1 do
  begin
    {$ifdef JScript} asm console.log('RunMode',RunMode); end; {$endif}
    ok:=TXGPUCanvas(AllGPUNodes[i].ScreenObject).BuildPascalAnimationUnit(Compiler,RunMode);
    if ok then
    begin
      txt:=TXGPUCanvas(AllGPUNodes[i].ScreenObject).GeneratedPascalUnit;
      // add this unit to the uses list in the main module
      PascalCode.Add(',GPUCode'+AllGPUNodes[i].NodeName);
    end
    else
    begin
      result:=false;
      EXIT;
    end;
  end;
end;

{$ifdef Python}
function RunPyScript(PyScriptCode:TStringList;nm:String):Boolean;
var
    ok:Boolean;
    tmp:string;
begin
  ok:=true;
  if PyScriptCode.Count > 0 then
  begin
  {$ifndef JScript}
    try
    PythonEngine1.ExecStrings( PyScriptCode );
    except
      on E: Exception do
      begin
        showmessage('Python error in '+nm+' : '+e.Message);
        ok:=false;
      end;
    end;
    {$else}
    tmp:=PyScriptCode.Text;
    asm
    try {
    pyodide.runPython(tmp);
    } catch(err) { alert(err.message+'  in '+nm);
         ok=false;
         }
    end;
    {$endif}
  end;
  result:=ok;
end;

procedure TPyProcs.GatherAndRunPythonScripts(parms:PtrInt);
var
  ok:Boolean;
  i,j:integer;
  tmp,nm:string;
  lines:TStringList;
  UnitNode:TdataNode;
  PyCode,PyList:TStringList;
  txt:String;
  {$ifndef JScript}
  ReceivedQueueRec: TPyXQueueRec;
  {$endif}
  function RunPythonCodeAndClear:Boolean;
  var
    ok:Boolean;
  begin
    ok:=RunPyScript(PyCode,'');
    PyCode.Clear;            // this is to reset Python line numbering back to 0
    result:=ok;
  end;

begin
  {$ifndef JScript}
  Screen.Cursor := crHourglass;
  {$else}
  asm console.log('GatherAndRunPythonScripts'); end;
  {$endif}

  ok:=true;
  // user-created scripts are held as data nodes (class 'Code', type 'PythonScript')
  Lines:=TStringList.Create;
  PyCode:=TStringList.Create;
  //PyExeString('import os');
  //PyExeString('orig = os.getcwd()');
  //PyExeString('print("orig = ",os.getcwd())');
  //{$ifndef JScript}
  //PyExeString('os.chdir("tempinc")');
  //{$else}
  //{$endif}
  //SetLength(PyFuncsList,0);
  PyCode.Clear;

  {$ifndef JScript}
  PyCode.add('def ListFunctions(TempDict):');
  PyCode.add('  functions = []');
  PyCode.add('  for key, value in TempDict.items():');
  PyCode.add('     if callable(value) : functions.append(key)');
  PyCode.add('  return(functions)');
  PyCode.add('TempDict = locals().copy()');
  PyCode.add('InitialFunctions =  ListFunctions(TempDict)');
  {$else}
  // in the browser/Pyodide environment, we have not re-initialised Python on entry to run mode,
  // so it is necessary to clear previous declarations of functions from the dictionary first
  PyCode.add('TempDict = locals().copy()');
  PyCode.add('FinalFunctions =  ListFunctions(TempDict)');
  PyCode.add('AddedFunctions = [item for item in FinalFunctions if item not in InitialFunctions]');
  PyCode.add('for item in AddedFunctions:');
  PyCode.add('  exec(str(item) + " = None")');
  {$endif}
  PyCode.add('WorkingFunctions = [item for item in InitialFunctions]');
  PyCode.add('AllFuncsStr = ''[''');
  ok:=RunPythonCodeAndClear;

  for i:=0 to length(CodeRootNode.ChildNodes)-1 do
  begin
    if ok then
    begin
      if CodeRootNode.ChildNodes[i].NodeType='PythonScript' then
      begin
       PythonCodeExists:=true;
       UnitNode:=CodeRootNode.ChildNodes[i];
       // code is all in attribute : Code
       nm:=UnitNode.NodeName;
       PyCode.add('print(''running script '+nm+''')');
       ok:=RunPythonCodeAndClear;

       // add user-written unit code block
       tmp:=UnitNode.GetAttribute('Code',true).AttribValue;
       Lines:=StringSplit(tmp,LineEnding);
       for j:=0 to Lines.Count-1 do
         PyCode.add(Lines[j]);
       //At the bottom of each of our python units add...............
       PyCode.add('NewDict = locals().copy()');
       PyCode.add('FinalFunctions =  ListFunctions(NewDict)');
       PyCode.add('AddedFunctions = [item for item in FinalFunctions if item not in WorkingFunctions]');
       //PyCode.add('print(''Added '',AddedFunctions)');
       PyCode.add('if (len(AddedFunctions)>0) and (AllFuncsStr!=''[''):');
       PyCode.Add('  AllFuncsStr = AllFuncsStr + '',''');
       PyCode.add('for j in range(len(AddedFunctions)):');
       PyCode.add('  if j>0:');
       PyCode.add('    AllFuncsStr = AllFuncsStr + '',''');
       PyCode.add('  AllFuncsStr = AllFuncsStr + ''"'+nm+','' +str(AddedFunctions[j])+''"''');                // ("myname", "funcname")
       PyCode.add('WorkingFunctions = [item for item in FinalFunctions]');
       ok:=RunPythonCodeAndClear;
      end;
    end;
  end;
  PyCode.add('AllFuncsStr = AllFuncsStr + '']''');
  //PyCode.add('print("AllFuncsStr",AllFuncsStr)');
  PyCode.add('UpdatepyLoadedFuncs(AllFuncsStr)');

    if PythonCodeExists then
    begin
      AllUserPyCode:=PyCode.Text;
      // scripts for the main system are held under the root node CodeRootNode.
      // There may be other scripts held within composite components (already gathered into PyCodeFromComposites).
      ok:=RunPyScript(PyCodeFromComposites,'');
      ok:=RunPyScript(PyCode,'');
      {$ifdef JScript}
      asm console.log('done GatherAndRunPythonScripts'); end;
      {$endif}
    end;
    PyScriptsExecuted:=true;



  FreeAndNil(PyCode);
  FreeAndNil(Lines);
  {$ifndef JScript}
  Screen.Cursor := crDefault;
  {$endif}
end;

{$ifndef JScript}
procedure GatherAndRunPythonScriptsLater;
var
  QueueRecToSend: PPyXQueueRec;
begin
  New(QueueRecToSend);
  QueueRecToSend^.QDummy:='1';
  Application.QueueAsyncCall(@PyProcs.GatherAndRunPythonScripts,PtrInt(QueueRecToSend)); // put msg into queue that will be processed from the main thread after all other messages
end;
{$else}
procedure GatherAndRunPythonScriptsFromJS;
begin
  try
  PyProcs.GatherAndRunPythonScripts(0);
  except
    on E: Exception do
    begin
      showmessage('Error executing Python script: '+e.Message);
    end;
  end;
  DeleteGreyOverlay('Grey1');
end;
procedure GatherAndRunPythonScriptsLater;
begin
  UpdateWaitMessage('Grey1','Running Python Scripts...');
  // timeout here so the grey overlay gets updated
  asm
  myTimeout(pas.CompileUserCode.GatherAndRunPythonScriptsFromJS,5,'GatherAndRunPythonScriptsFromJS',0,0);
  end;
end;
{$endif}

{$endif}


{$ifndef JScript}

procedure WriteProjectIncFiles;
begin
  WriteRTLIncFile('resources/xcomponents/','eventsinterface');
  WriteRTLIncFile('resources/project/','interfacetypes');

end;

{$endif}

function initialiseCodeToBeCompiled(RunMode:String; Compiler:TObject; var FirstUnitName:String):Boolean;    //RunMode is LazJS, LazDll or JSJS
var
    i,n:integer;
    ok:Boolean;
begin
 // delete old .inc and .pas files
  {$ifndef JScript}
 DeleteDynamicIncFiles;
  WriteRTLIncFiles;
  WriteProjectIncFiles;
  {$endif}
 PascalCode.Clear;
 ExportsList.Clear;
 PyCodeFromComposites.Clear;
 n:=0;

 if (RunMode = 'LazJS') or (RunMode = 'JSJS') then
 begin
   setlength(XIDEProcsList,0);
   // Build pascal source code for events unit - to be compiled by the Pas2JS compiler
   PascalCode.Add('unit '+DllName+';' );
   PascalCode.Add('interface' );
   PascalCode.Add('    ' );
   PascalCode.Add('uses');
   PascalCode.Add('  Classes, SysUtils, Math, EventsInterface, InterfaceTypes, ');  //!!!! make uses clause editable ??
   PascalCode.Add('  contnrs, dateutils, rtlconsts, strutils, types, typinfo');

   FirstUnitName:=GatherUserUnits(RunMode,Compiler);
   ConstructNamespaceUnits(RunMode,'',Compiler,MainFormProjectRoot);
   ConstructDataModelUnits(RunMode);
   ok:=ConstructGPUUnits(RunMode,Compiler);
   if not ok then
   begin
     result:=ok;
     EXIT;
   end;
   PascalCode.Add(';');
   PascalCode.Add('');

   GatherEventHeaders(RunMode,'',SystemNodeTree,PascalCode,n);

   PascalCode.Add('');
   PascalCode.Add('implementation' );
   PascalCode.Add('');

   AddExecFunc('',PascalCode);
   GatherEventCode(RunMode,'',Compiler,SystemNodeTree,PascalCode);

   PascalCode.Add('    ' );
   PascalCode.Add('begin');
   PascalCode.Add('end.');
 end
 else if RunMode = 'LazDll' then
 begin
   // Build source code for events DLL - to be compiled by the Free Pascal compiler
   PascalCode.Add('library '+DllName+';' );
   PascalCode.Add('{$mode objfpc}{$H+}{$R+}');
   PascalCode.Add( '    ' );
   PascalCode.Add('uses');
   //windows, ??
   {$ifdef linux}
   PascalCode.Add('  cthreads,');
   {$endif}
   PascalCode.Add('  Classes, SysUtils, strutils, Math, EventsInterface, InterfaceTypesDll ');
   FirstUnitName:=GatherUserUnits(RunMode,nil);
   ConstructNamespaceUnits(RunMode,'',nil,SystemNodeTree);
   ConstructDataModelUnits(RunMode);
   ok:=ConstructGPUUnits(RunMode,Compiler);
   if not ok then
   begin
     result:=ok;
     EXIT;
   end;
   PascalCode.Add(';');
   PascalCode.Add('');
   PascalCode.Add('');

   AddExecFunc('',PascalCode);
   GatherEventCode(RunMode,'',nil,SystemNodeTree,PascalCode);

   PascalCode.Add( '    ' );
   PascalCode.Add( 'procedure SetDllContext(mmi:IMyMethodInterface); stdcall;' );
   PascalCode.Add( 'begin' );
   PascalCode.Add( '  InterfaceTypesDll.SetDllContext(mmi);  ' );
   PascalCode.Add( 'end;' );
   PascalCode.Add( 'exports SetDllContext;' );
   PascalCode.Add( '    ' );
   for i:=0 to ExportsList.Count-1 do
     PascalCode.Add(ExportsList[i]);
   PascalCode.Add( '    ' );
    PascalCode.Add( 'begin');
    PascalCode.Add( '  ' );
    PascalCode.Add( 'end.');
 end;
 {$ifndef Python}
 if PythonCodeExists then
   showmessage('Warning: The system contains Python code.  These cannot be executed unless the XIDE framework is built with the ''Python'' option');
 {$endif}
 result:=ok;
end;

procedure GatherSourcedAttributes(StartNode:TDataNode);
var
  i,n:integer;
  CompositeNode:TDataNode;
  NodesList:TNodesArray;
  procedure CollectAll(StartNode:TDataNode);
  var
    i:integer;
  begin
    if StartNode.IsDynamic then
    begin
      for i:=0 to length(StartNode.NodeAttributes)-1 do
      begin
        if StartNode.NodeAttributes[i].AttribSource.InputNodeName<>'' then
        begin
          setlength(SourcedAttribs,length(SourcedAttribs)+1);
          SourcedAttribs[length(SourcedAttribs)-1].TheAttribute:=StartNode.NodeAttributes[i];
          SourcedAttribs[length(SourcedAttribs)-1].TheNode:=StartNode;
          SourcedAttribs[length(SourcedAttribs)-1].InProgress:=false;
          //SourcedAttribs[length(SourcedAttribs)-1].SourceNode:=FindDataNodeById(SystemNodeTree,
          SourcedAttribs[length(SourcedAttribs)-1].SourceNode:=FindDataNodeById(UIRootNode,
                                                                               StartNode.NodeAttributes[i].AttribSource.InputNodeName,
                                                                               StartNode.NodeAttributes[i].AttribSource.InputNameSpace,
                                                                               true);
        end;
      end;
    end;
    for i:=0 to length(StartNode.ChildNodes)-1 do
    begin
      CollectAll(StartNode.ChildNodes[i]);
    end;
  end;
begin
  NodesList:=FindNodesOfType(StartNode,'TXCompositeIntf');
  for n:=0 to length(NodesList)-1 do
  begin
  if NodesList[n].IsDynamic then
  begin
    // For any TXCompositeIntf (composite interface element) nodes, each of its non-default "input" attributes
    // will need to be set up with values sourced from the element container (type TXComposite).
    // NB. an input property has attribreadonly set to True on the TXCompositeIntf component.
    // An output property has attribreadonly set to False on the TXCompositeIntf component.
    CompositeNode:=FindCompositeContainer(NodesList[n]);
    if CompositeNode<>nil then
      for i:=0 to length(NodesList[n].NodeAttributes)-1 do
        if (not IsADefaultAttrib('TXCompositeIntf',NodesList[n].NodeAttributes[i].AttribName))
        and (NodesList[n].NodeAttributes[i].AttribReadOnly=true) then  //ie. an "input" property
        begin
          NodesList[n].NodeAttributes[i].AttribSource.InputAttribName:=NodesList[n].NodeAttributes[i].AttribName;
          NodesList[n].NodeAttributes[i].AttribSource.InputNameSpace:=CompositeNode.NameSpace;
          NodesList[n].NodeAttributes[i].AttribSource.InputNodeName:=CompositeNode.NodeName;
        end;
  end;
  end;
  NodesList:=FindNodesOfType(StartNode,'TXComposite');
  for n:=0 to length(NodesList)-1 do
  begin
    // if this node is a TXComposite (composite element) then each of its non-default "output" attributes
    // will need to be set up with values sourced from an interface element internal to the composite (type TXCompositeIntf).
    for i:=0 to length(NodesList[n].NodeAttributes)-1 do
    begin
      if (not IsADefaultAttrib('TXComposite',NodesList[n].NodeAttributes[i].AttribName))
      and (NodesList[n].NodeAttributes[i].AttribReadOnly=true) then //ie. an "output" property
      begin
        CompositeNode:=FindInterfaceNode(NodesList[n],NodesList[n].NodeName,NodesList[n].NodeAttributes[i].AttribName);
        if CompositeNode<>nil then
        begin
          NodesList[n].NodeAttributes[i].AttribSource.InputAttribName:=NodesList[n].NodeAttributes[i].AttribName;
          NodesList[n].NodeAttributes[i].AttribSource.InputNameSpace:=CompositeNode.NameSpace;
          NodesList[n].NodeAttributes[i].AttribSource.InputNodeName:=CompositeNode.NodeName;
        end;
      end;
    end;
  end;
  CollectAll(StartNode);
end;

{$ifndef JScript}
function DefaultFPCConfig:String;
var
  cfg:String;
begin

  {$ifdef windows}
  cfg:='C:/fpcupdeluxe/fpc/bin/i386-win32';   // Location of Lazarus installation (needed for Free Pascal compilation of user events dll)
  {$endif}
  {$ifdef linux}
  cfg:='/home/pi/fpcupdeluxe/fpc/bin/arm-linux';
  {$endif}
  result:=cfg;

end;

procedure RunFPCCompiler(MyCodeEditor:TXCode; DLLFileName,PASFileName:String);
var
    Lines:TStringList;
begin
  Lines:=TStringList.Create;
  SysUtils.DeleteFile('resources/project/'+DLLFileName);
  SysUtils.DeleteFile('fpcerrors.txt');
  SysUtils.DeleteFile('fpcdebug.txt');

  //Unload the lib, if already loaded
  if MyLibC <>  DynLibs.NilHandle then
    if FreeLibrary(MyLibC) then
      MyLibC:= DynLibs.NilHandle;

  // create the TProcess object
  AProcess := TProcess.Create(nil);

  // Tell the new AProcess what the command to execute is.
  // Use the Free Pascal compiler
  AProcess.Executable:= ConfigfpcPath+'/fpc';

  // add parameters
  AProcess.Parameters.Add('@'+ConfigfpcPath+'/fpc.cfg');

  AProcess.Parameters.Add('resources/project/'+PASFileName);
  AProcess.Parameters.Add('-Fu./resources/xcomponents');      // for EventsInterface.pas
  AProcess.Parameters.Add('-Fu./tempinc');
  AProcess.Parameters.Add('-Fi./tempinc');
  AProcess.Parameters.Add('-Fefpcerrors.txt');
  AProcess.Parameters.Add('-dDll');
  AProcess.Parameters.Add('-gl');
  //AProcess.Parameters.Add('-debug-log=DllDebugLog.log');
  AProcess.Parameters.Add('-vewilv');              //verbose     //v: writes to fpcdebug.txt
  AProcess.Parameters.Add('-Xg');              //debugging    //compiler debugging
  AProcess.Parameters.Add('-B');                //build all units
  AProcess.Parameters.Add('-Cr');               //range checking
//    AProcess.Parameters.Add('-k -R ./');               //pass to linker
  //AProcess.Parameters.Add('-gw');

  {$ifdef linux}
  AProcess.Parameters.Add('-fPIC');              // Generate PIC code
  {$endif}

  // We will define an option for when the program
  // is run. This option will make sure that our program
  // does not continue until the program we will launch
  // has stopped running.
  // see for bug fix ..... http://wiki.freepascal.org/Executing_External_Programs#Reading_large_output
  //AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes, poNoConsole];
  //AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes, poNewConsole];
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];

  Lines.clear;
  // Now let AProcess run the program
  try
    try
    AProcess.Execute;
    except
      on E:Exception do
      begin
        showmessage('Unable to execute process fpc.exe.  Check Run Settings. '+e.Message);
        MyCodeEditor.MessageLines:='Error: Unable to execute process fpc.exe.  Check Run Settings. '+e.Message;
      end;
    end;
  finally
    // Now read the output of fpc into a memo
    if FileExists('fpcerrors.txt') then
    begin
      Lines.LoadFromFile('fpcerrors.txt');
      MyCodeEditor.MessageLines:=Lines.Text;
    end
    else
    begin
      MyCodeEditor.MessageLines:='Error: Cannot find fpc output file fpcerrors.txt.  Check Run Settings.';
    end;
  end;
  // This is not reached until process stops running.
  AProcess.Free;
  Lines.Free;

end;

function CheckForCompilationErrors(MyCodeEditor:TXCode;DllFileName,RunMode:String):Boolean;
var
  FoundError,FoundFatal:Boolean;
  i:integer;
begin
  // check messages for errors
  FoundError:=false;
  FoundFatal:=false;
  i:=0;
  while (i<MyCodeEditor.TheMessages.lines.Count)
  and (FoundError=false)
  and (FoundFatal=false) do
  begin
    MyCodeEditor.GetMessage(FoundError,MyCodeEditor.TheMessages.lines[i],'Error:');
    MyCodeEditor.GetMessage(FoundFatal,MyCodeEditor.TheMessages.lines[i],'Fatal:');
    i:=i+1;
  end;

  // does the compiled file exist?
  if (RunMode = 'LazDll')
  and (not FileExists('resources/project/'+DLLFileName)) then
    FoundError:=true;

  if (FoundError) or (FoundFatal) then
    result:=false
  else
    result:=true;

end;

function CompileEventCode(MyCodeEditor:TXCode; RunMode:String):Boolean;
var
   ExtraDirectives:TStringList;
   PASFileName,FirstUnitName:string;
   DLLFileName:string;
   i:integer;
   ok:Boolean;
begin
  Screen.Cursor := crHourglass;
  PythonCodeExists:=false;

  ExtraDirectives:=TStringList.Create;

  XIDEProjectDir:=ProjectDirectory;   // see xpparser

  DllName:=MainUnitName+'Events';

  // clean up from previous runs
  MyCodeEditor.MessageLines:='';

  PASFileName:= DllName+'.pas';
  SysUtils.DeleteFile('resources/project/'+PASFileName);
  SysUtils.DeleteFile('resources/project/'+DllName+'.o');

  ok:=InitialiseCodeToBeCompiled(RunMode,nil,FirstUnitName);
  if ok then
  begin
    MyCodeEditor.ItemValue:=PascalCode.Text;
    // save the text to be compiled to the .pas file
    MyCodeEditor.TheEditor.Lines.SaveToFile('resources/project/'+PASFileName);

    if RunMode = 'LazJS' then
    begin
      // run the pas2js compiler just for the events unit to check for errors
      {$ifdef Python}
      ExtraDirectives.add('-dPython');
      {$endif}
      TranspileMyProgram(DllName,ProjectDirectory,'resources/project/',MyCodeEditor,false,ExtraDirectives);
      // retrieve the list of functions to be shown under the code tree
      RebuildCodeTree;  //uses xpparser.XIDEProcsList;
    end

    else if RunMode = 'LazDll' then
    begin
      // run the free pascal compiler (FPC) to create the dll
      // (from http://wiki.freepascal.org/Executing_External_Programs#TProcess)
      DLLFileName:= DllName+'.'+SharedSuffix;
      {$ifdef linux}
      DLLFileName:='lib'+DLLFileName;
      {$endif}
      RunFPCCompiler(MyCodeEditor,DLLFileName,PASFileName);
    end;

    ok := CheckForCompilationErrors(MyCodeEditor,DllFileName,RunMode);

  end;

  ExtraDirectives.Free;

  Screen.Cursor := crDefault;

  result:=ok;

end;

{$else}

function ExtractModuleName(instr:String):String;
var
  k:integer;
  found:boolean;
  modname:String;
begin
  found:=false;
  k:=1;
  while k< length(instr) do
  begin
    if instr[k]='"' then
    begin
      if found then
        k:=length(inStr)
      else
        found:=true;
    end
    else if found then
      modname:=modname+instr[k];
    k:=k+1;
  end;
  //showmessage('found name '+modname);
  result:=modname;
end;

function LocateNextModule(var inLines:TStringList):integer;
var
  i,j,k:integer;
  modname,tmp:String;
begin
  result:=-1;
  tmp:=inLines.Text;
  i:=0;
  j:=0;
  while i < inLines.Count do
  begin
    j:=FoundString(inLines[i],'rtl.module("');
    if j > 0 then
    begin
      result:=i;
      i:=inLines.Count;
    end;
    i:=i+1;
  end;
  //showmessage('linenum='+inttostr(result)+' line is '+inLines[result]);
end;

function CompileEventCode(MyCodeEditor:TXCode; RunMode:String):Boolean;
var
  tmp,FirstUnitName,CurrentUnitName:String;
  ok:Boolean;
  i,j,l:integer;
  args,JSOutputLines,JSKeep : TStringList;
  Res,PasModuleExists : Boolean;
  lWebFS : TPas2JSWebFS;
  NSUnits,GPUUnits:array of String;
begin
  PythonCodeExists:=false;
  NamespaceUnits.Clear;

  MyWebCompiler.mycodeeditor:=MyCodeEditor;
  DllName:=MainUnitName+'Events';
  // clean up from previous runs
  MyCodeEditor.MessageLines:='';

  //.....Run the compiler .......

  MyWebCompiler.Compiler.Log.OnLog:=@MyWebCompiler.DoLog;

  MyWebCompiler.Compiler.WebFS.LoadBaseURL:='';

  ok:=InitialiseCodeToBeCompiled(RunMode,MyWebCompiler.Compiler,FirstUnitName); // populates PascalCode stringlist, and builds user units
  if ok then
  begin
    MyCodeEditor.ItemValue:=PascalCode.Text;
    //WriteToLocalStore('CompileMain',PascalCode.Text);

    Res:=False;

    lWebFS:=MyWebCompiler.Compiler.WebFS;
    // Load up the RTL sources that are required for the compilation...    //!!!! maybe do this only once??
    asm
    lWebFS.SetFileContent('eventsinterface.pas',pas.CompileUserCode.eventsinterfacepas);
    lWebFS.SetFileContent('interfacetypes.pas',pas.CompileUserCode.interfacetypespas);
    end;
    LoadRTLFilesForPas2JS(lWebFS);

    // Load up the main .pas file
    lWebFS.SetFileContent(DllName+'.pas',PascalCode.Text);

    args:=TStringList.Create;
    try
      //Args.Add(-Jeutf-8);      //  encode messages as:  Unicode UTF-8. Default when using -Fe.
      //-Jminclude               //  include Pascal sources in source map.
//      Args.Add('-Tbrowser');   //  Set target platform
    // -Tnodejs
      Args.Add('-vwnhe');
      Args.Add('-O-');           //  Disable optimizations
      Args.Add('-Jc');           //  Write all JavaScript concatenated into the output file
      //      Args.Add('-Jirtl.js');   //  Insert JS file <x> into main JS file.
      Args.Add('-Jirtl.js-');         //  Remove a JS file.
      Args.Add('-dJScript');
      Args.Add('-B');                //build all units
// -Pecmascript5     // Set target processor.
// -MobjFPC  // FPC's Object Pascal compatibility mode (default)
// -Sc      // Support operators like C (*=,+=,/= and -=)
// -Ju<x> : Add <x> to foreign unit paths. Foreign units are not compiled.
      Args.Add(DllName+'.pas');

      //........................................................
      MyWebCompiler.Compiler.Run('','',Args,True);
      Res:=MyWebCompiler.Compiler.ExitCode=0;
      //........................................................

    finally
     Args.Free;
    end;

    //EditAttributeValue('XMemo2','','ItemValue',PascalCode.Text);        //!!!! temporary for debugging

    if res=true then
    begin

      // First De-register the events unit, plus any other user units used
      SetLength(NSUnits,NamespaceUnits.Count);
      for i:=0 to NamespaceUnits.Count-1 do
        NSUnits[i]:=NamespaceUnits[i];
      SetLength(GPUUnits,length(AllGPUNodes));
      for i:=0 to length(AllGPUNodes)-1 do
      begin
        GPUUnits[i]:=AllGPUNodes[i].NodeName;
      end;
      asm
      try {
      var hd=document.head;
      var div=document.getElementById('UserEventCodeContainer');
      if (div==null) {
         div=document.createElement('div');
         div.id='UserEventCodeContainer';
         hd.appendChild(div);
         }
      div.innerHTML='';

      // remove the prior registration of this module...
      if (pas[pas.Events.DllName]) {
        pas[pas.Events.DllName]=null;
        // and any user units ....
        var codeRoot=pas.NodeUtils.CodeRootNode;
        for (var i=0; i<codeRoot.ChildNodes.length; i++) {
             if (codeRoot.ChildNodes[i].NodeType=='PasUnit')
                {
                 //alert('removing unit '+codeRoot.ChildNodes[i].NodeName);
                 pas[codeRoot.ChildNodes[i].NodeName]=null;
                 }
           }
        // and any TXComposite (namespace) units....
        for (i=0; i<NSUnits.length; i++) {
          pas[NSUnits[i]]=null;
        }
        // and any TXGPUCanvas (emulation) units....
        for (i=0; i<GPUUnits.length; i++) {
          pas[GPUUnits[i]]=null;
          pas['GPUCode'+GPUUnits[i]]=null;
        }
        // and the worker threads unit ....
        pas[pas.Events.DllName+'Threads']=null;

        // and the DataModel unit ....
        var DMRoot=pas.XDataModel.DMRoot;
        //console.log('removing unit DMRoot');
        pas['DMRoot']=null;

        }
      } catch(err) { alert(err.message + ' in CompileEventCode (units de-registration) ');     div.innerHTML=''; ok=false;}
      end;

      // Capture the output from the Pas2JS compiler
      JSOutput:=MyWebCompiler.Compiler.WebFS.GetFileContent(DllName+'.js');

      // Delete from the JS file all units that already exist in the main page...
      if FirstUnitName='' then FirstUnitName:=DllName;

      // pas2js has produced a JS output file including all referenced rtl units.
      // Chop up the output into separate modules, and retain only those rtl modules
      // that are not already registered...
      JSOutputLines:=TStringList.Create;
      JSOutputLines.Text:=JSOutput;
      JSKeep:=TStringList.Create;

      PasModuleExists:=false;

      CurrentUnitName:='.';
      while (CurrentUnitName<>'')
      and (CurrentUnitName<>FirstUnitName) do
      begin
        i:=LocateNexTModule(JSOutputLines);
        CurrentUnitName:=ExtractModuleName(JSOutputLines[i]);
        //asm console.log('i=',i,' checking module '+CurrentUnitName); end;
        if (i-1) > 0 then
          for j:=0 to i-1 do
          begin
            if PasModuleExists=false then
              JSKeep.Add(JSOutputLines[0]);
            JSOutputLines.Delete(0);
          end;
        //deal with this module...
        if (CurrentUnitName<>FirstUnitName) then
        begin
          PasModuleExists:=true;
          asm
          //console.log(pas[CurrentUnitName]);
          if (pas[CurrentUnitName]==undefined) {
             PasModuleExists = false;
          }
          end;
          if PasModuleExists = false then
          begin
            //asm console.log('keeping module '+CurrentUnitName); end;
            JSKeep.Add(JSOutputLines[0]);
          end;
          JSOutputLines.Delete(0);
        end;
      end;

      //tmp:=JSKeep.Text;
      //asm alert( tmp); end;

      JSOutput:=JSKeep.Text + JSOutputLines.Text;
      JSOutputLines.Free;
      JSKeep.Free;
    end;

    //EditAttributeValue('XMemo1','','ItemValue',JSOutput);        //!!!! temporary for debugging

    ok:=res;
    if res=true then
    begin

      //showmessage('Events compilation done.  Output='+JSOutput);

      // Inject the generated JS script
      asm
      try {
        //alert('inject script : '+ pas.CompileUserCode.JSOutput);
        var script = document.createElement('script');
        script.innerHTML = pas.CompileUserCode.JSOutput;
        div.appendChild(script);
      } catch(err) { alert(err.message + ' in CompileEventCode (injecting script) ');     div.innerHTML=''; ok=false;}

      try {

        //alert('try re-registration...');
        // must re-register the events unit and any user units in its uses clause....
        var module = pas[pas.Events.DllName];
        //alert('module '+pas.Events.DllName+'='+module);
        if (module!=null) {
          rtl.loadintf(module);      // to register the new module's properties
          rtl.loadimpl(module);      // to register the new module's properties
        }
        else {alert('Problem encountered with unit registration in rtl');
              ok=false;
              }
        } catch(err) { alert(err + ' in CompileEventCode (module registration) ');     div.innerHTML=''; ok=false;}
        //} catch(err) { alert(err.name+' '+err.at+' '+err.text+' '+err.message + ' in CompileEventCode 2 ');     div.innerHTML=''; ok=false;}

      end;
    end;

    // retrieve the list of functions to be shown under the code tree
    RebuildCodeTree;  //pparser.XIDEProcsList;

    //....decide if there are errors or not .......
    if ok=false then showmessage('Compilation failed')
    else showmessage('Compilation successful');
  end;
  result:=ok;
end;


{$endif}

function   DfltUnitCode(UnitName,UnitType:String):string;
begin
  if UnitType='PasUnit' then
    result:='{$ifdef Dll}'+ LineEnding                  //NB. unit name is generated at compile time
          +'{$mode objfpc}{$H+}{$R+}'+ LineEnding
          +'{$endif}'+ LineEnding
          + 'interface '+ LineEnding
          + 'uses Classes, Sysutils,'+ LineEnding
          + '   Math, contnrs, dateutils, rtlconsts, strutils, types, typinfo,EventsInterface,'+ LineEnding
          + '{$ifdef Dll} InterfaceTypesDll; {$else} InterfaceTypes; {$endif} ' + LineEnding
          + LineEnding
          + 'implementation ' + LineEnding
          + ' ' + LineEnding
          + 'begin' + LineEnding
          + '// IMPORTANT: do not use any of the XIDE interface functions ' + LineEnding
          + '//(eg. SetPropertyValue etc in unit InterfaceTypesDll) ' + LineEnding
          + '// inside the unit initialization section.....errors will occur.' + LineEnding
          + 'end. '
  else  if UnitType='PythonScript' then
    result:='#Python script';
end;

function   DfltEventCode:string;
begin
  result:= 'begin' + LineEnding +
            ' ' + LineEnding +
            'end;' + LineEnding;
end;
function   DfltOpCode:string;
begin
  result:= 'begin' + LineEnding +
            ' ' + LineEnding +
            'end;' + LineEnding;
end;
function   DfltThreadEventCode(NodeName:String):string;
begin
  result:= 'begin' + LineEnding +
           '  with XVars'+NodeName+' do ' + LineEnding +
           '  begin' + LineEnding +
           '  end;' + LineEnding +
           'end;' + LineEnding;
end;
function   DfltTreeNodeEventCode:string;
begin
  result:= 'var' + LineEnding +
          '  values:TNodeEventValue;' + LineEnding +
          '  SourceName,SrcText,DstText:String;'  + LineEnding +
          'begin' + LineEnding +
          '  values:=TNodeEventValue(e.ValueObject);' + LineEnding +
          '  SourceName:=values.SourceName;  // name of tree being dragged from' + LineEnding +
          '  SrcText:=values.SrcText;        // text of treenode being dragged' + LineEnding +
          '  DstText:=values.DstText;        // text of treenode being dragged over' + LineEnding +
          '  // set e.ReturnString to "True" or "False" ' + LineEnding +
          '  e.ReturnString:=''True'';' + LineEnding +
          'end;' + LineEnding;
end;
function   DfltPythonCode:string;
begin
  result:= '# Python script' + LineEnding;
end;

begin
  PascalCode:=TStringList.Create;
  ExportsList:=TStringList.Create;
  PyCodeFromComposites:=TStringList.Create;
  {$ifdef Python}
  PyProcs:=TPyProcs.Create;
  {$endif}
  {$ifndef JScript}
  MyLibC := dynlibs.NilHandle;
  ConfigFPCPath:=DefaultFPCConfig;
  LoadFPCConfig;
  {$else}
  NamespaceUnits:=TStringList.Create;
  {$endif}

end.


