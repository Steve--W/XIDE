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
  Classes, SysUtils, StringUtils, NodeUtils,  UtilsJSCompile,
  webTranspilerUtils,
  XComposite,
{$ifndef JScript}
  {$if defined ( windows)}
  Windows,                           // for CurrentThreadID
  {$endif}
  //CompilerLogUnit,
  LazsUtils, Controls, URIParser,
  PropEdits,TypInfo, Dialogs, Dynlibs,Process,
  XCode, Forms, Events;
{$else}
  HTMLUtils,XCode,XIFrame,Events,
  webfilecache, pas2jswebcompiler,
  InterfaceTypes;
{$endif}

function CompileEventCode(MyCodeEditor:TXCode;RunMode:String):Boolean;
function   DfltUnitCode(UnitName,UnitType:String):string;
function   DfltFunctionCode(FnName:String):string;
function   DfltEventCode:string;
function   DfltThreadEventCode(NodeName:String):string;
function   DfltTreeNodeEventCode:string;
procedure GatherSourcedAttributes(StartNode:TDataNode);

var
    ConfigfpcPath:String;

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
//    rttipas:String;
    strutilspas:String;
    systempas:String;
    sysutilspas:String;
    typespas:String;
    typinfopas:String;

//      browserconsolepas:String;
//      class2paspas:String;
//      hotreloadclientpas:String;
//      libjquerypas:String;
//      nodejspas:String;
//      objpaspas:String;
//      timerpas:String;
//      webpas:String;
//      webaudiopas:String;
//      webbluetoothpas:String;
//      webglpas:String;
//      webrouterpas:String;
{$endif}



implementation
uses  XObjectInsp;

{$ifndef JScript}
type
   TMyDLL = function (var instring:string): string; stdcall ;

var
    AProcess: TProcess;
{$endif}
var
    PascalCode,ExportsList,NamespaceUnits:TStringList;


{$ifndef JScript}
procedure DeleteDynamicIncFiles;
var
  lSearchRec:TSearchRec;
  lPath:string;
begin
  if not DirectoryExists('tempinc') then
    CreateDir('tempinc');

  lPath := 'tempinc/';

  if FindFirst(lPath+'*.*',faAnyFile,lSearchRec) = 0 then
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
  Incname,tempText,oneLine:string;
  TxtLines:TStringList;
  i,j:integer;
  URI: TURI;
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
      TxtLines.Text:=MyStringReplace(TxtLines.Text,'''','&myapos;',-1,-1);
      for i:=0 to TxtLines.Count-1 do
      begin
        TxtLines[i]:='+ '''+TxtLines[i]+'\n'' ' ;
      end;
  end;


  // Wrap the text with pascal code that will load it into a string variable.
  if filename+suffix='gpujs' then
  begin
    // gpu.js
    TxtLines.Insert(0,'pas.XGPUCanvas.'+filename+suffix+' = ');
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
    TxtLines.Add(filename+suffix+':=MyStringReplace('+filename+suffix+',''&myapos;'','''''''',-1,-1);');
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

  WriteRTLIncFile('resources/project/','gpu','js');
end;


procedure LoadFPCConfig;
var
  TheStream : TFileStream;
  TheLines:TStringList;
  ProgPath:String;
begin
  // pick up the last-used config file XIDERunSettings.dta
  ProgPath:=ExtractFilePath(Application.ExeName);
  TheLines:=TStringList.Create;
  if FileExists(ProgPath+'XIDERunSettings.dta') then
  begin
    try
      TheStream:=TFileStream.Create(ProgPath +'XIDERunSettings.dta',fmOpenRead or fmShareDenyNone);
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
    result:=procName + '(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString); ';
    if RunMode='LazDll' then
      result:=result + '  stdcall; ';
  end
  else
  begin
    result:=procName + '(e:TEventStatus;nodeID,myValue:String); ';
  end;
end;

procedure GatherEventHeaders(RunMode,NameSpace:String;StartNode:TDataNode;UnitCode:TStringList;var n:integer);
var
    i,j:integer;
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
    i,j:integer;
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
    i,j:integer;
    tmp,VarNames:string;
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
    i,j:integer;
    tmp,VarNames:string;
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
    i,j:integer;
    tmp,VarNames:string;
    VarNamesList:TStringList;
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
    i,j:integer;
    hdr,tmp,Dflt,nm,et:string;
    IncCode,InitCode:TStringList;
begin
  if StartNode.NameSpace=NameSpace then
  begin
    nm:=StartNode.NodeName;
    // user-created event code is held in the data nodes (node.myEventHandlers[i].TheCode)
    IncCode:=TStringList.Create;
    if (StartNode.IsDynamic) or (StartNode=UIRootNode) then
      for i:=0 to length(StartNode.myEventHandlers)-1 do
      // (exclude events for worker threads...)
      if (StartNode.NodeType<>'TXThreads')
      or (FoundString(StartNode.myEventTypes[i],'Thread')<>1) then
      begin
        et:=StartNode.myEventTypes[i];
        tmp:=StartNode.myEventHandlers[i].TheCode;
        Dflt:=DfltEventCode;
        if (trim(tmp)<>'')
        and (tmp<>Dflt) then
        begin
           // Insert a procedure containing the code for the event initialisation
           hdr:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],RunMode,'Init');
           tmp:=trim(StartNode.myEventHandlers[i].InitCode);
           InitCode:=StringSplit(tmp,LineEnding);
           InitCode.Insert(0,hdr);
           if InitCode.Count=1 then
             InitCode.Add('begin end;');
           WriteIncFile(Compiler,Namespace+StartNode.NodeName, StartNode.myEventTypes[i]+'__Init','tempinc/', UnitCode, InitCode);

           // Insert a procedure containing the main code for the event
           hdr:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],RunMode,'Main');
           IncCode:=StringSplit(StartNode.myEventHandlers[i].TheCode,LineEnding);
           IncCode.Insert(0,hdr);
           WriteIncFile(Compiler,NameSpace+StartNode.NodeName, StartNode.myEventTypes[i],'tempinc/', UnitCode, IncCode);

           // Insert a control procedure to run the init and main code for the event
           tmp:=BuildEventHeader(NameSpace,StartNode.NodeName,StartNode.myEventTypes[i],RunMode,'');
           UnitCode.Add(tmp);

           UnitCode.Add('begin');
           UnitCode.Add('  AppMethods.mmiSetEventsNameSpace('''+NameSpace+''');');
           UnitCode.Add('  if (e=nil) or (e.InitRunning=false) then');
           UnitCode.Add('  begin');
           UnitCode.Add('    if (e=nil) then');
           UnitCode.Add('    begin');
           UnitCode.Add('      e:=TEventStatus.Create('''+StartNode.myEventTypes[i]+''','''+StartNode.NodeName+''');');
           UnitCode.Add('      e.NameSpace:='''+NameSpace+''';');
           UnitCode.Add('    end;');
            // If the event has initialisation code, run this first...
           UnitCode.Add('    e.InitRunning:=true;');
           UnitCode.Add('    '+NameSpace+StartNode.NodeName + 'Handle' + StartNode.myEventTypes[i] + 'Init(e,nodeID,myValue);');

           UnitCode.Add('  end');
           UnitCode.Add('  else');
           UnitCode.Add('    e.InitRunning:=false;');

           // If the initialisation code has called any async functions, these will have been logged in the
           // event status.  Do not continue with the main event code unless all of the async functions
           // have recorded completion.
           UnitCode.Add('  if e.EventHasWaitingAsyncProcs = true then');
           UnitCode.Add('    EXIT;');

           // run the main event code
           UnitCode.Add('  '+NameSpace+StartNode.NodeName + 'Handle' + StartNode.myEventTypes[i] + 'Main(e,nodeID,myValue);');

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
    i,j:integer;
    hdr,tmp,Dflt,nm,et:string;
    IncCode,InitCode:TStringList;
begin
  nm:=StartNode.NodeName;
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


function BuildFuncHeaderWithParams(FuncNode:TDataNode;num:integer):String;
var
   k:integer;
   tmp:string;
   FuncInputs:TCodeInputs;
begin
  tmp:=FuncNode.GetAttribute('Inputs',true).AttribValue;
  FuncInputs:=StringToCodeInputs(tmp);
  if length(FuncInputs)>0 then
  begin
    tmp:='function '+FuncNode.NameSpace+FuncNode.NodeName+'(';
    for k:=0 to length(FuncInputs)-1 do
    begin
      if k>0 then tmp:=tmp+';';
      tmp:=tmp+FuncInputs[k].InputSynonym+':String';
    end;
    tmp:=tmp+'):String; ';
    if (num=1) then
      tmp:=tmp+' overload;';
    result:=tmp;
  end
  else
    result:='';
end;
function BuildFuncHeaderNoParams(FuncNode:TDataNode;num:integer):String;
var
   tmp:string;
   FuncInputs:TCodeInputs;
begin
  tmp:=FuncNode.GetAttribute('Inputs',true).AttribValue;
  FuncInputs:=StringToCodeInputs(tmp);

  tmp:='';
  tmp:='function '+FuncNode.NameSpace+FuncNode.NodeName+':String;';
  if (num=1)
  and (length(FuncInputs)>0) then
    tmp:=tmp+' overload;';
  result:=tmp;
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

function GatherUserUnits(RunMode:String; Compiler:TObject):String;
var
    i,j,k:integer;
    tmp,FirstUnitName,nm:string;
    lines, FuncCode:TStringList;
    UnitNode, FuncNode:TdataNode;
    UnitCode:TStringList;
    FuncInputs:TCodeInputs;
    procedure AddUnitCodeLine(str:String);
    begin
      UnitCode.add(str);
    end;

begin

  // user-created units are held as data nodes (attribute 'Code')
  // create a pas file on disk for each unit, and insert the unit name for the dll 'uses' clause
  UnitCode:=TStringList.Create;
  Lines:=TStringList.Create;

  for i:=0 to length(CodeRootNode.ChildNodes)-1 do
  begin
        if CodeRootNode.ChildNodes[i].NodeType='RawUnit' then
        begin
           UnitNode:=CodeRootNode.ChildNodes[i];
           if i=0 then FirstUnitName:=UnitNode.NodeName;
           // Raw Unit code is all in attribute attribute : Code
           UnitCode.Clear;
           nm:=UnitNode.NodeName;

           // add user-written unit code block
           tmp:=UnitNode.GetAttribute('Code',true).AttribValue;
           Lines:=StringSplit(tmp,LineEnding);
           for j:=0 to Lines.Count-1 do
             AddUnitCodeLine(Lines[j]);

           {$ifndef JScript}
           // save the generated pas file
           SysUtils.DeleteFile('tempinc/'+UnitNode.NodeName+'.pas');
           UnitCode.SaveToFile('tempinc/'+UnitNode.NodeName+'.pas');
           {$else}
           TPas2JSWebCompiler(Compiler).WebFS.SetFileContent(UnitNode.NodeName+'.pas',UnitCode.Text);
           {$endif}

           // add this unit to the uses list in the main module
           PascalCode.Add(','+UnitNode.NodeName);
        end;
  end;

  // Build a separate unit to hold 'event' code for worker threads (within TXThreads components)
  // This code is in a separate unit from the main event code so that the scope of worker threads
  // can be limited (they are self-contained, with no access to data/functions in the main thread).
  BuildThreadEventsUnit(Compiler,RunMode);

  FreeAndNil(UnitCode);

  result:=FirstUnitName;

end;

procedure GatherUserFuncs(RunMode,NameSpace:String; Compiler:TObject; StartNode:TDataNode;UnitCode:TStringList;var n:integer);
var
    i,j,k:integer;
    tmp,nm:string;
    lines, FuncCode:TStringList;
    UnitNode, FuncNode:TdataNode;
    FuncInputs:TCodeInputs;

begin

  // user-created units are held as data nodes (attribute 'Code')
  // find the user-defined functions, and add them to the main unit
  // in the implementation section (UnitCode)

  if (StartNode=CodeRootNode) then
  begin
    Lines:=TStringList.Create;
    FuncCode:=TStringList.Create;

    UnitNode:=StartNode;

    // Insert declared functions
    for j:=0 to length(UnitNode.ChildNodes)-1 do
      if (UnitNode.ChildNodes[j].NodeType='Function')
      and (UnitNode.ChildNodes[j].NameSpace=NameSpace) then
      begin
        n:=n+1;
        FuncNode:=UnitNode.ChildNodes[j];
        tmp:=FuncNode.GetAttribute('Inputs',true).AttribValue;
        FuncInputs:=StringToCodeInputs(tmp);
        if length(FuncInputs)>0 then
          tmp:=BuildFuncHeaderWithParams(FuncNode,2)
        else
          tmp:=BuildFuncHeaderNoParams(FuncNode,2);

        UnitCode.Add(tmp);

        FuncCode.Clear;
        tmp:=FuncNode.GetAttribute('Code',true).AttribValue;
        Lines:=StringSplit(tmp,LineEnding);
        for k:=0 to Lines.Count-1 do
          FuncCode.Add(Lines[k]);

        // each function is held in its own inc file so that we can display the relevant code section
        // when there are compiler errors
        WriteIncFile(Compiler,FuncNode.NameSpace+FuncNode.NodeName, '','tempinc/', UnitCode, FuncCode);

        // add the overloaded function to fetch values of all declared inputs
        if length(FuncInputs)>0 then
        begin
          tmp:=BuildFuncHeaderNoParams(FuncNode,2);
          Lines:=TStringList.Create;
          Lines.Add(tmp);
          Lines.Add('begin');
               tmp:='result:='+FuncNode.NameSpace+FuncNode.NodeName+'(';
          for k:=0 to length(FuncInputs)-1 do
          begin
            if k>0 then tmp:=tmp+',';
            // either fetch an attribute value, or execute a function
            if FuncInputs[k].InputAttribName<>'' then
              tmp:=tmp+'GetPropertyValue('''+FuncInputs[k].InputNodeName+''','''+FuncInputs[k].InputAttribName+''')'
            else
              tmp:=tmp+FuncInputs[k].InputNodeName;
          end;
          tmp:=tmp+');';
          Lines.Add(tmp);
          Lines.Add('end;');
          for k:=0 to Lines.Count-1 do
            UnitCode.Add(Lines[k]);
        end;
      end;
    FreeAndNil(FuncCode);
    FreeAndNil(Lines);
  end;


  if (StartNode=CodeRootNode) then
    for i:=0 to length(StartNode.ChildNodes)-1 do
      GatherUserFuncs(RunMode,NameSpace,Compiler,StartNode.ChildNodes[i],UnitCode,n);

end;

procedure BuildNamespaceUnit(RunMode,NameSpace:String; Compiler:TObject);
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
           AddUnitCodeLine('  rtlconsts, strutils, types, typinfo, EventsInterface,');
           if RunMode='LazDll' then
             AddUnitCodeLine('  InterfaceTypesDll;')
           else
           begin
             AddUnitCodeLine('  InterfaceTypes;');
           end;

           GatherEventHeaders(RunMode,NameSpace,SystemNodeTree,UnitCode,n);

           AddUnitCodeLine('implementation');

           GatherUserFuncs(RunMode,NameSpace,Compiler,CodeRootNode,UnitCode,n);

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

  // Build a separate unit to hold 'event' code for worker threads (within TXThreads components)
  // This code is in a separate unit from the main event code so that the scope of worker threads
  // can be limited (they are self-contained, with no access to data/functions in the main thread).
  //BuildThreadEventsUnit(Compiler,RunMode);

  FreeAndNil(UnitCode);

end;

function ConstructNamespaceUnits(RunMode,NameSpace:String; Compiler:TObject; StartNode:TDataNode):String;
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
      BuildNamespaceUnit(RunMode,CompositeNameSpace,Compiler);
  end;

  // Walk the system tree looking for namespace components
  for i:=0 to length(StartNode.ChildNodes)-1 do
    if (StartNode.ChildNodes[i].NodeClass='UI')
    or (StartNode.ChildNodes[i].NodeClass='Root') then
      ConstructNamespaceUnits(RunMode,CompositeNameSpace,Compiler,StartNode.ChildNodes[i]);

end;

{$ifndef JScript}

procedure WriteProjectIncFiles;
begin
  WriteRTLIncFile('resources/xcomponents/','eventsinterface');
  WriteRTLIncFile('resources/project/','interfacetypes');

end;

{$endif}

function initialiseCodeToBeCompiled(RunMode:String; Compiler:TObject):String;    //RunMode is LazJS, LazDll or JSJS
var
    tmp,FirstUnitName:String;
    i,n:integer;
begin
 // delete old .inc and .pas files
  {$ifndef JScript}
 DeleteDynamicIncFiles;
  WriteRTLIncFiles;
  WriteProjectIncFiles;
  {$endif}
 PascalCode.Clear;
 ExportsList.Clear;
 n:=0;

 if (RunMode = 'LazJS') or (RunMode = 'JSJS') then
 begin
   // Build pascal source code for events unit - to be compiled by the Pas2JS compiler
   PascalCode.Add('unit '+DllName+';' );
   PascalCode.Add('interface' );
   PascalCode.Add('    ' );
   PascalCode.Add('uses');
   PascalCode.Add('  Classes, SysUtils, Math, EventsInterface, InterfaceTypes, ');  //!!!! make uses clause editable ??
   PascalCode.Add('  contnrs, dateutils, rtlconsts, {rtti,} strutils, types, typinfo');

   FirstUnitName:=GatherUserUnits(RunMode,Compiler);
   ConstructNamespaceUnits(RunMode,'',Compiler,UIRootItem);
   PascalCode.Add(';');
   PascalCode.Add('');

   GatherEventHeaders(RunMode,'',SystemNodeTree,PascalCode,n);

   PascalCode.Add('');
   PascalCode.Add('implementation' );
   PascalCode.Add('');
   GatherUserFuncs(RunMode,'',Compiler,CodeRootNode,PascalCode,n);

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
   PascalCode.Add('  Classes, SysUtils, Math, {initc,} EventsInterface, InterfaceTypesDll ');
   FirstUnitName:=GatherUserUnits(RunMode,nil);
   ConstructNamespaceUnits(RunMode,'',nil,SystemNodeTree);
   PascalCode.Add(';');
   PascalCode.Add('');
   PascalCode.Add('');

   GatherUserFuncs(RunMode,'',nil,CodeRootNode,PascalCode,n);

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
 result:=FirstUnitName;
end;

procedure GatherSourcedAttributes(StartNode:TDataNode);
var
  i:integer;
  CompositeNode:TDataNode;
  //TCodeInputRec
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
        SourcedAttribs[length(SourcedAttribs)-1].SourceNode:=FindDataNodeById(SystemNodeTree,StartNode.NodeAttributes[i].AttribSource.InputNodeName,
                                                                              StartNode.NameSpace,true);
      end;
    end;
    // if this node is a TXCompositeIntf (composite interface element) then each of its non-default attributes
    // will need to be set up with values sourced from the element container (type TXComposite).
    if StartNode.NodeType='TXCompositeIntf' then
    begin
      CompositeNode:=FindCompositeContainer(StartNode);
      if CompositeNode<>nil then
      for i:=0 to length(StartNode.NodeAttributes)-1 do
        if (not IsADefaultAttrib('TXCompositeIntf',StartNode.NodeAttributes[i].AttribName)) then
        begin
          setlength(SourcedAttribs,length(SourcedAttribs)+1);
          SourcedAttribs[length(SourcedAttribs)-1].TheAttribute:=StartNode.NodeAttributes[i];
          SourcedAttribs[length(SourcedAttribs)-1].TheNode:=StartNode;
          SourcedAttribs[length(SourcedAttribs)-1].InProgress:=false;
          SourcedAttribs[length(SourcedAttribs)-1].SourceNode:=FindCompositeContainer(StartNode);
          SourcedAttribs[length(SourcedAttribs)-1].TheAttribute.AttribSource.InputAttribName:=StartNode.NodeAttributes[i].AttribName;
          SourcedAttribs[length(SourcedAttribs)-1].TheAttribute.AttribSource.InputNodeName:=CompositeNode.NodeName;
        end;
    end;
    // if this node is a TXComposite (composite element) then each of its non-default attributes
    // will need to be set up with values sourced from an interface element internal to the composite (type TXCompositeIntf).
    if StartNode.NodeType='TXComposite' then
    begin
      for i:=0 to length(StartNode.NodeAttributes)-1 do
      begin
        if (not IsADefaultAttrib('TXComposite',StartNode.NodeAttributes[i].AttribName))
        and (StartNode.NodeAttributes[i].AttribSource.InputAttribName='') then    // don't do this if there is already an explicit source
        begin
          CompositeNode:=FindInterfaceNode(StartNode,StartNode.NodeName,StartNode.NodeAttributes[i].AttribName);
          if CompositeNode<>nil then
          begin
            setlength(SourcedAttribs,length(SourcedAttribs)+1);
            SourcedAttribs[length(SourcedAttribs)-1].TheAttribute:=StartNode.NodeAttributes[i];
            SourcedAttribs[length(SourcedAttribs)-1].TheNode:=StartNode;
            SourcedAttribs[length(SourcedAttribs)-1].InProgress:=false;
            SourcedAttribs[length(SourcedAttribs)-1].SourceNode:=FindInterfaceNode(StartNode,StartNode.NodeName,StartNode.NodeAttributes[i].AttribName);
            SourcedAttribs[length(SourcedAttribs)-1].TheAttribute.AttribSource.InputAttribName:=StartNode.NodeAttributes[i].AttribName;
            SourcedAttribs[length(SourcedAttribs)-1].TheAttribute.AttribSource.InputNodeName:=CompositeNode.NodeName;
          end;
        end;
      end;
    end;
  end;
  for i:=0 to length(StartNode.ChildNodes)-1 do
  begin
    GatherSourcedAttributes(StartNode.ChildNodes[i]);
  end;
end;

{$ifndef JScript}
function DefaultFPCConfig:String;
var
  cfg:String;
begin

  //cfg.Add('resources/xcomponents');       // Location of the units of the XComponents package
  {$ifdef windows}
  cfg:='C:/fpcupdeluxe/fpc/bin/i386-win32';   // Location of Lazarus installation (needed for Free Pascal compilation of user events dll)
  {$endif}
  {$ifdef linux}
  cfg:='/home/pi/fpcupdeluxe/fpc/bin/arm-linux';
  {$endif}
  result:=cfg;

end;

function CompileEventCode(MyCodeEditor:TXCode; RunMode:String):Boolean;
var
   TheStream : TFileStream;
   Lines:TStringList;
   PASFileName:string;
   DLLFileName:string;
   ProgPath:String;
   i:integer;
   FoundError,FoundFatal:Boolean;
begin
  Screen.Cursor := crHourglass;

  ProgPath:=ExtractFilePath(Application.ExeName);
  Lines:=TStringList.Create;
  DllName:=MainUnitName+'Events';

  // clean up from previous runs
  MyCodeEditor.MessageLines:='';

  PASFileName:= DllName+'.pas';
  SysUtils.DeleteFile('resources/project/'+PASFileName);
  SysUtils.DeleteFile('resources/project/'+DllName+'.o');

  InitialiseCodeToBeCompiled(RunMode,nil);
  MyCodeEditor.ItemValue:=PascalCode.Text;
  // save the text to be compiled to the .pas file
  MyCodeEditor.TheEditor.Lines.SaveToFile('resources/project/'+PASFileName);

  if RunMode = 'LazJS' then
  begin
    // run the pas2js compiler just for the events unit to check for errors
    TranspileMyProgram(DllName,ProgPath,'resources/project/',MyCodeEditor,false);
  end

  else if RunMode = 'LazDll' then
  begin
    // run the free pascal compiler (FPC) to create the dll
    // (from http://wiki.freepascal.org/Executing_External_Programs#TProcess)

    DLLFileName:= DllName+'.'+SharedSuffix;
    {$ifdef linux}
    DLLFileName:='lib'+DLLFileName;
    {$endif}
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
  end;

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

  Lines.Free;
  Screen.Cursor := crDefault;

  if (FoundError) or (FoundFatal) then
    result:=false
  else
    result:=true;

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
  //asm alert('start of locate...'+tmp); end;
  i:=0;
  j:=0;
  while i < inLines.Count do
  begin
    j:=FoundString(inLines[i],'rtl.module("');
    if j > 0 then
    begin
      //showmessage('found '+inLines[i]);
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
  i,j:integer;
  args,JSOutputLines,JSKeep : TStringList;
  Res,PasModuleExists : Boolean;
  lWebFS : TPas2JSWebFS;
  NSUnits:array of String;
begin
  NamespaceUnits.Clear;

  MyWebCompiler.mycodeeditor:=MyCodeEditor;
  DllName:=MainUnitName+'Events';
  //showmessage('CompileEventCode '+RunMode);
  // clean up from previous runs
  MyCodeEditor.MessageLines:='';

  //.....Run the compiler .......

  //showmessage('CompileEventCode. starting compile section...');

    MyWebCompiler.Compiler.Log.OnLog:=@MyWebCompiler.DoLog;

    MyWebCompiler.Compiler.WebFS.LoadBaseURL:='';

    FirstUnitName:=InitialiseCodeToBeCompiled(RunMode,MyWebCompiler.Compiler); // populates PascalCode stringlist, and builds user units
    //showmessage('FirstUnitName='+FirstUnitName);
    MyCodeEditor.ItemValue:=PascalCode.Text;

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
      //showmessage('compiler all done - success');

    // First De-register the events unit, plus any other user units used
    SetLength(NSUnits,NamespaceUnits.Count);
    for i:=0 to NamespaceUnits.Count-1 do
    begin
      NSUnits[i]:=NamespaceUnits[i];
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
             if (codeRoot.ChildNodes[i].NodeType=='RawUnit')
                {
                 //alert('removing unit '+codeRoot.ChildNodes[i].NodeName);
                 pas[codeRoot.ChildNodes[i].NodeName]=null;
                 }
           }
        // and any TXComposite (namespace) units....
        for (i=0; i<NSUnits.length; i++) {
          pas[NSUnits[i]]=null;
        }
        // and the worker threads unit ....
        pas[pas.Events.DllName+'Threads']=null;
        }
      } catch(err) { alert(err.message + ' in CompileEventCode (units de-registration) ');     div.innerHTML=''; ok=false;}
     end;


    // Capture the output from the Pas2JS compiler
    JSOutput:=MyWebCompiler.Compiler.WebFS.GetFileContent(DllName+'.js');

    // Delete from the JS file all units that already exist in the main page...
    if FirstUnitName='' then FirstUnitName:=DllName;
    //showmessage('FirstUnitName='+DllName);

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
      //showmessage('i='+inttostr(i)+' checking module '+CurrentUnitName);
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
        //alert(pas[CurrentUnitName]);
          if (pas[CurrentUnitName]==undefined) {
             PasModuleExists = false;
          }
        end;
        if PasModuleExists = false then
        begin
          //showmessage('keeping module '+CurrentUnitName);
          JSKeep.Add(JSOutputLines[0]);
        end;
        JSOutputLines.Delete(0);
      end;
      //tmp:=CopyThisModule(JSOutputLines);
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

  //....decide if there are errors or not .......
  if ok=false then showmessage('Compilation failed')
  else showmessage('Compilation successful');
  result:=ok;
end;


{$endif}

function   DfltUnitCode(UnitName,UnitType:String):string;
begin
  if UnitType='RawUnit' then
    result:='unit '+UnitName+';' + LineEnding
          +'{$ifdef Dll}'+ LineEnding
          +'{$mode objfpc}{$H+}'+ LineEnding
          +'{$endif}'+ LineEnding
          + 'interface '+ LineEnding
          + 'uses Classes, Sysutils,'+ LineEnding
          + '   Math, contnrs, dateutils, rtlconsts, {rtti,} strutils, types, typinfo,'+ LineEnding
          + '{$ifdef Dll} InterfaceTypesDll; {$else} InterfaceTypes; {$endif} ' + LineEnding
          + LineEnding
          + 'implementation ' + LineEnding
          + ' ' + LineEnding
          + 'begin' + LineEnding
          + '// IMPORTANT: do not use any of the XIDE interface functions ' + LineEnding
          + '//(eg. SetPropertyValue etc in unit InterfaceTypesDll) ' + LineEnding
          + '// inside the unit initialization section.....errors will occur.' + LineEnding
          + 'end. '
  else
    result:='// Declare variables and functions local to this unit...(within implementation section)';
end;

function   DfltFunctionCode(FnName:String):string;
begin
  result:= 'begin ' + LineEnding +
            '  result := '''';' + LineEnding +
            'end;' + LineEnding;
end;
function   DfltEventCode:string;
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

begin
  PascalCode:=TStringList.Create;
  ExportsList:=TStringList.Create;
  {$ifndef JScript}
  MyLibC := dynlibs.NilHandle;
  ConfigFPCPath:=DefaultFPCConfig;
  LoadFPCConfig;
  {$else}
  NamespaceUnits:=TStringList.Create;
  {$endif}
end.

