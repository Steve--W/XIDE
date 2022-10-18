(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XObjectInsp;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, Stringutils, StrUtils, NodeUtils, Events, PopupMemo,
{$ifndef JScript}
  LazsUtils, Menus, DynLibs,
  Controls, ComCtrls, PropEdits, ExtCtrls, Dialogs,Forms,CompilerLogUnit, xpparser,
{$else}
  AboutUnit,pparser,HTMLUtils,
{$endif}
  UtilsJSCompile,XIFrame, XSVGContainer,
  WrapperPanel,  CompileUserCode,
  XHBox, XVBox, XCode,XColorPicker,
  XTree,  XButton, XScrollBox, XEditBox, XCheckBox, XComboBox, XTabControl,
  XForm, XTable, XMemo, XMenu, CodeEditor, PropertyEditUnit, EventsInterface,
  XGPUCanvas, XGPUEditor, XCompositeIntf, XDBTable, StylesUtils,IntfParamUnit,IntfEventUnit;

{$ifdef JScript}
function CheckForSavedSystemOnLoad:Boolean;
procedure ShowXFormForDesign(XFormName:String);
procedure ToggleToRunModeAfterCompile(ok:boolean);
procedure BrowserSaveData(TheData:String);
procedure ContinueToggleToRunMode;
procedure CompleteDeployFromBrowser(deployname:String);
procedure InitialiseComposites;
procedure CompleteToggleToRunMode(ok:Boolean);
{$endif}

{$ifndef JScript}
type TLoadTimerTag = class
       public
         systemstring:string;
         SysName:String;
         procedure DoXMLToNodeTree(sender:TObject);
       end;
{$endif}

type
  TOIEventWrapper = class
    procedure OIEditProperty(e:TEventStatus;nodeId:string;myValue:string);
    procedure OIEditEvent(e:TEventStatus;nodeId:string;myValue:string);
    procedure OIEditEventCode(e:TEventStatus;nodeId:string;myValue:string);
    procedure OIEditEventCodeFromCodeTree(NodeNameToEdit:string;EventToEdit:string);
    procedure OIEditIntfEvent(e:TEventStatus;nodeId:String;myValue:String);
    procedure TestButtonClick(e:TEventStatus;nodeId:string;myValue:string);
    procedure OIEditPropertyButtonClick(e:TEventStatus;nodeId:string;myValue:string);
    procedure OIDelPropertyButtonClick(e:TEventStatus;nodeId:string;myValue:string);
    procedure OIDelEventButtonClick(e:TEventStatus;nodeId:string;myValue:string);
    procedure CloseGPUEditor(e:TEventStatus;nodeId:string;myValue:string);
    {$ifdef JScript}
    procedure OIPasteTarget(e:TEventStatus;nodeId:string;myValue:string);
    {$endif}
  end;

procedure DumpFullNodeTree;
procedure InitialiseXIDE;
function XIDEConfirm(promptString:String):boolean;
function GetTreeTextHierarchy(StartNode,TopRootNode:TDataNode):String;
function GetTreeTextHierarchyWithIndices(StartNode,TopRootNode:TDataNode):String;
procedure SetupAvailableResources;
procedure RebuildNavigatorTree;
procedure RebuildCodeTree;
procedure RebuildResourcesTree;
procedure BuildSkeletonResourceTree;
procedure SaveSystemData;
procedure OINavTreeNodeChange(e:TEventStatus;nodeId,NameSpace:string;myValue:string);
procedure OICodeTreeNodeChange(nodeId:string;myValue:string);
procedure OIResourceTreeNodeChange(nodeId:string;myValue:string);
procedure OIDragItem(e:TEventStatus;nodeId:string;myValue:string);
procedure OIDropItem(e:TEventStatus;nodeId:string;myValue:string);
function OIPasteItem(nodeId:string;myValue:string):boolean;
procedure OICutItem(nodeId:string;myValue:string);
procedure CopyNavNode( NodeToCopy:TDataNode);
procedure OICopySelectedItem;
procedure OIDeleteSelectedItem;
function OIDeleteItem(NodeId,NameSpace:String;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;
procedure OIComponentCopy(nodeId:string;myValue:string);
procedure OISystemLoad(e:TEventStatus;nodeId:string);
procedure OIClearSystem;
procedure OIDeploySystem;
function OITreeNodeHint(TreeLabelStr:String):String;
function OIResTreeNodeHint(TreeLabelStr:String):String;
procedure PopulateObjectInspector(CurrentNode:TDataNode);
procedure RefreshObjectInspectorLater(targetNode:TDataNode);
procedure OIPropsEventsTabChange;
procedure PopulateResourceInspector(CurrentNode:TDataNode);
function TreeLabelToID(TreeLabelStr,TreeName:String;var FirstBit:String):String;
function GetNavigatorHint(InTree:TDataNode;SystemNodeName:String):String;
function CompositeResourcesString(QuotedString:Boolean):String;
function BuildSystemString(Encapsulate:Boolean):String;
Procedure SaveSystemToClip;
Procedure SaveSystemToFile;
procedure ClearResourceInspector;
procedure ClearInspectors;
procedure OILoadSavedSystem(SysName:String);
function DoSystemLoad(SystemDescription,SysName:string):Boolean;
procedure CodeEditorClosed(EditBoxNode:TdataNode);
procedure PropertyEditorClosed(EditBoxNode:TdataNode);
function isValidSystemData(SystemDescription:string):boolean;
procedure EditEventCode(NodeNameToEdit,EventToEdit,MainCode,InitCode:String);
procedure OIAddCodeUnitNode(UnitType:String);
procedure OIEditCodeUnit;
procedure OIDeleteCodeUnit;
procedure DoToggleDesignRunMode(Sender:TXMenuItem);
procedure DisplayDllCompileErrors;
function OINavTreeAllowDrop(DstNode:TDataNode):Boolean;
procedure SelectNavTreeNode(CurrentNode:TDataNode; refresh:boolean);
Function ConstructSystemTreeString(CurrentItem:TDataNode; level:Integer; IncludeTypes,IncludeProperties:Boolean;
                                   Exclusions:TStringList;PropType:String):String;
procedure OIMoveItem(nodeId,NameSpace:string;NewParentId:string);
procedure OICopyToNewParent(nodeId,NameSpace:string;NewParentId:string;NewName:String);  overload;
{$ifndef JScript}
procedure SaveCompositesToIncFile;
procedure OICopyToNewParent(nodeId,NameSpace:string;NewParentId:string;NewName:PChar);  overload;
{$endif}
procedure OIMoveNavSiblingUpDown(UpDown:String);
procedure CodeTreeMoveSiblingUpDown(UpDown:String);
procedure DoSelectNavTreeNode(CurrentNode:TDataNode; refresh:boolean);
procedure RefreshObjectInspector(CurrentNode:TDataNode);
procedure ShowHideObjectInspector(show:Boolean);
function XIDEPrompt(PromptString,DefaultString:string):string;
procedure RedisplayResourceTree;
procedure OIEncapsulate;
procedure OIDeleteResource;
procedure OILoadResource;
procedure OIAddInterfaceElement;
procedure DiscoverSavedSystems(var NamesList:TStringList);
procedure OICodeSearch;
function NameStringIsValid(nm:String):Boolean;
function CanAddChildToParent(ParentNode,SourceNode:TDataNode):Boolean;
function GetValidItemName(PromptString,DefaultString:String):String;
function ComponentNameIsUnique(ScreenObjectName,NameSpace:string):Boolean;
procedure SetScreenToDesignMode;
procedure CheckForPythonCode;


const
  ResourceDataRootName=  'ResourceRoot' ;
  CodeDataRootName=  'dllRoot' ;
  UIProjectRootName = 'UIRoot';                    //the scrollbox containing the user-designed (mainform) system
  PropertyEditorScrollboxName:string = 'PropertyEditorScrollbox';
  CompositePropsScrollboxName:string = 'CompositePropsScrollbox';
  CompositePropsTabName:string = 'OICompositePropsTab';
  EventsEditorScrollboxName:string = 'EventsEditorScrollbox';
//  ResourceEditorScrollboxName:string = 'ResourceEditorScrollbox';
  DeploymentModeOptions:Array[0..1] of string = ('Design','Run');
  ShowResourcesOptions:Array[0..1] of string = ('Left','Right');


var
  MainFormProjectRoot:TDataNode;
  DesignMode:Boolean;
  NavTreeComponent,DataTreeComponent,ResourcesNodeTree, ResourceTreeComponent, CodeTreeComponent:TDataNode;
  OIEventWrapper:TOIEventWrapper;
  LastLazUserInterfaceItemSelected,LastHTMLUserInterfaceItemSelected:String;
  LastHTMLUserInterfaceItemHadBorder:Boolean;
  TreeInFocus, ObjectInspectorSelectedNavTreeNode, ObjectInspectorSelectedCodeTreeNode, ObjectInspectorSourceNode:TDataNode;
  OISelectedCodeProcName:String;
  OISelectedCodeLineNum:integer;
  RunModeAttempts:integer;

  PropertiesNode, InterfacePropsNode, InterfaceTabNode, EventsNode,
    AddInterfacePropsButtonNode, OITabs, DMAttribsNode : TDataNode;

  {$ifndef JScript}
  UITopControl:TWinControl;
  {$else}
  RunningDeployedRuntime:Boolean;
  CompositesString:String;
  {$endif}

const
  AttributeEditorNameDelimiter:string = '__';

implementation
uses PasteDialogUnit, PyXUtils, XDataModel, XIDEMain;


  //SandraMode:Boolean = true;

var
  ObjectInspectorSelectedCodeNodeText:String;
  AvailableResourcesSelectedNode:TDataNode;
  ObjectInspectorSourceCut:Boolean;

//type
//  TCompositeResource = record
//    ResourceName:String;
//    SourceString:String;
//  end;
//var
//  ListOfComposites:Array of TCompositeResource;

function XIDEConfirm(promptString:String):boolean;
var
  ok:boolean;
begin
  ok:=confirm(promptString);

  result:=ok;
end;

function GetTreeTextHierarchy(StartNode,TopRootNode:TDataNode):String;
var
  ParentNode:TDataNode;
  treeText:String;
begin
  treeText:=StartNode.NodeType+'('+StartNode.NodeName+')';
  ParentNode:=StartNode.NodeParent;
  while (ParentNode<>nil) and (ParentNode<>TopRootNode.NodeParent) do
  begin
    treeText:=ParentNode.NodeType+'('+ParentNode.NodeName+')'+'/'+treeText;
    ParentNode:=ParentNode.NodeParent;
  end;
  result:=treeText;
end;
function GetTreeTextHierarchyWithIndices(StartNode,TopRootNode:TDataNode):String;
var
  ParentNode:TDataNode;
  treeText:String;
  thisindex:integer;
begin
  treeText:='';
  //{$ifdef JScript}asm console.log('GetTreeTextHierarchyWithIndices StartNode='+StartNode.NodeName); end; {$endif}
  if StartNode.NodeParent <> nil then
    thisindex:=StartNode.NodeParent.GetChildIndex(StartNode)
  else
    thisindex:=0;
  treeText:= intToStr(thisindex) + '~:~' + StartNode.NodeType+'('+StartNode.NodeName+')';
  ParentNode:=StartNode.NodeParent;
  //{$ifdef JScript}asm console.log('GetTreeTextHierarchyWithIndices ParentNode=',ParentNode); end; {$endif}
  while (ParentNode<>nil) and (ParentNode<>TopRootNode.NodeParent) do
  begin
    if ParentNode.NodeParent <> nil then
      thisindex:=ParentNode.NodeParent.GetChildIndex(ParentNode)
    else
      thisindex:=0;
    treeText:= intToStr(thisindex) + '~:~' + ParentNode.NodeType+'('+ParentNode.NodeName+')'+'~/~'+treeText;
    ParentNode:=ParentNode.NodeParent;
  end;
  result:=treeText;
end;

Function ConstructSystemTreeString(CurrentItem:TDataNode; level:Integer; IncludeTypes,IncludeProperties:boolean;
                                   Exclusions:TStringList;PropType:String):String;
// Recursive
var ArrayString:String;
    i,j,numchildren:integer;
    InsertingAttributes:Boolean;
begin
  // In the main form, start at UIRoot node

  if CurrentItem<>nil then
  begin
    if (CurrentItem.NameSpace='')
    and (
    ((CurrentItem.NodeClass='UI') and (CurrentItem.IsDynamic=true))
    or ((CurrentItem.NodeClass='SVG') and (CurrentItem.IsDynamic=true))
    or ((CurrentItem.NodeClass='UI') and (CurrentItem.NodeType='TXForm') and (CurrentItem.NodeName = MainForm.Name))
    or ((CurrentItem.NodeClass='UI') and (CurrentItem.NodeType='TXMainMenu'))
    or ((CurrentItem.NodeClass='NV') and (CurrentItem.IsDynamic=true))
    or (CurrentItem.NodeClass='DM')
    or (CurrentItem=SystemNodeTree)
    or (CurrentItem=ResourcesNodeTree)
    or (CurrentItem=CodeRootNode)
    or (CurrentItem=UIRootNode)
    or (CurrentItem=MainFormProjectRoot)
    or (CurrentItem.NodeClass='Code')
    or (CurrentItem.NodeClass[1]='R')
    )
    then
    begin
      if (Exclusions=nil)
      or (Exclusions.IndexOf(CurrentItem.NodeName)<0) then
      begin
        if CurrentItem.NodeClass[1]<>'R' then
         j:=0;
          //showmessage('Currentitem='+CurrentItem.NodeClass+' '+CurrentItem.NodeType+' '+CurrentItem.Nodename);
        numchildren:=length(CurrentItem.ChildNodes);

        if level = 0 then ArrayString:=' ';

        if (IncludeProperties)
        and ((CurrentItem.NodeClass='UI') or (CurrentItem.NodeClass='NV') or (CurrentItem.NodeClass='DM'))
        and (length(CurrentItem.NodeAttributes)>0) then
          InsertingAttributes:=true
        else
          InsertingAttributes:=false;

        if level=0
        then ArrayString:='['
        else
        begin
          if (NumChildren>0)
          or (InsertingAttributes) then
              ArrayString:=ArrayString+',['
          else
              ArrayString:=ArrayString+',';
        end;

        if IncludeTypes then
          ArrayString:=ArrayString+'"'+CurrentItem.NodeType+'('+CurrentItem.NodeName+')"'
        else
          ArrayString:=ArrayString+'"'+CurrentItem.NodeName+'"';

        if (InsertingAttributes) then
        begin
          for j:=0 to length(CurrentItem.NodeAttributes)-1 do
          begin
            if (Exclusions.IndexOf(CurrentItem.NodeName+'.'+CurrentItem.NodeAttributes[j].AttribName)<0)
            and ((CurrentItem.NodeAttributes[j].AttribType=PropType)
                or (PropType='')
                or (Currentitem.NodeType='TXCompositeIntf')         //!!!! improve this
                ) then
            begin
              ArrayString:=ArrayString+',';
              ArrayString:=ArrayString+'"'+CurrentItem.NodeName+'.'+CurrentItem.NodeAttributes[j].AttribName+'"';
            end;
          end;
        end;

        if (CurrentItem.NodeName <> MainForm.Name) then
          for i:=0 to numchildren-1 do
          begin
             ArrayString:=ArrayString+ConstructSystemTreeString(CurrentItem.ChildNodes[i],level+1,IncludeTypes,IncludeProperties,Exclusions,PropType);
          end
        else
        begin
          // Main Form only....
          // for main form, just want  UIProjectRootName, plus main menu
          for i:=0 to numchildren-1 do
          begin
            if (CurrentItem.ChildNodes[i].NodeType='TXMainMenu') then
               ArrayString:=ArrayString+ConstructSystemTreeString(CurrentItem.ChildNodes[i],level+1,IncludeTypes,IncludeProperties,Exclusions,PropType);
          end;

          ArrayString:=ArrayString+ConstructSystemTreeString(MainFormProjectRoot,level+1,IncludeTypes,IncludeProperties,Exclusions,PropType);

        end;

        if ( NumChildren>0)
        or (level=0)
        or (InsertingAttributes) then
          ArrayString:=ArrayString+']';

      end;
    end;
  end;

  result:= ArrayString;
end;

Function ConstructCodeTreeString(CurrentItem:TDataNode; level:Integer):String;
// Recursive
var ArrayString, ProcsString:String;
    i,p,numchildren:integer;
begin
  if CurrentItem<>nil then
  begin
    if (CurrentItem.NameSpace='')
    and (
    (CurrentItem=CodeRootNode)
    or (CurrentItem.NodeClass='Code')
    )
    then
    begin
      numchildren:=length(CurrentItem.ChildNodes);

      p:=0;
      ProcsString:='';
      if (CurrentItem.NodeClass='Code')
      and (NumChildren=0) then
      begin
        // This is a raw unit.
        // If the compiler has been run (pas2js), then there will be a list of defined procedures
        // available for display in this tree.
        for i:=0 to length(XIDEProcsList)-1 do
        begin
          if lowercase(XIDEProcsList[i].FileName) = lowercase(CurrentItem.NodeName)+'.pas' then
          begin
            p:=p+1;
            //ProcsString:=ProcsString+',"'+inttostr(XIDEProcsList[i].LineNum)+'('+XIDEProcsList[i].Name+')"';
            ProcsString:=ProcsString+',"'+XIDEProcsList[i].Name+'"';
          end;
        end;
      end;


      if level = 0 then ArrayString:=' ';

      if level=0
      then ArrayString:='['
      else
      begin
        if (NumChildren>0) or (p>0)
        then
              ArrayString:=ArrayString+',['
        else
              ArrayString:=ArrayString+',';
      end;

      ArrayString:=ArrayString+'"'+CurrentItem.NodeType+'('+CurrentItem.NodeName+')"';


      if (p>0) then
      begin
        // This is a raw unit.
        // If the compiler has been run (pas2js), then there will be a list of defined procedures
        // available for display in this tree.
        ArrayString:=ArrayString+ProcsString+']';
      end;

      for i:=0 to numchildren-1 do
      begin
         ArrayString:=ArrayString+ConstructCodeTreeString(CurrentItem.ChildNodes[i],level+1);
      end;

      if ( NumChildren>0)
      or (level=0)
      then
        ArrayString:=ArrayString+']';

    end;
  end;

  result:= ArrayString;
end;


function TreeLabelToID(TreeLabelStr,TreeName:String;var FirstBit:String):String;
//e.g. "EditBox(MySimpleID)"  =>  "MySimpleID"
// or  "EditBox(MySimpleID) Click"  =>  "MySimpleID"
var i,ln:integer;
  newstring,tempstr,TreeLabel2:String;
  StartOfIdFound:boolean;
begin
  TreeLabel2:=TreeLabelStr;
  StartOfIdFound:=false;
  newstring:='';
  FirstBit:='';
  ln:=length(TreeLabel2);
  i:=1;
  while i< ln+1 do
  begin
    tempstr:=TreeLabel2[i];
    if (StartOfIdFound=true) and  (tempstr<>')') then
      newstring:=newstring+tempstr;
    if (StartOfIdFound=false) and (tempstr<>'(') then
      FirstBit:=FirstBit+tempstr;
    if tempstr='(' then
      StartOfIdFound:=true;
    if tempstr=')' then
      i:=ln;
    i:=i+1;
  end;
  if StartOfIdFound=false then // no brackets
  begin
    newstring:=TreeLabelStr;
  end;
  //newstring:=myStringReplace( newstring,'Contents','',999,999);
  newstring:=ReplaceStr( newstring,'Contents','');
  result :=  trim(newstring);
end;


Function ConstructEventsTreeString(CurrentItem:TDataNode;var e:integer):String;
// Recursive
var ArrayString,dflt:String;
    i,numchildren:integer;
begin
  dflt:=DfltEventCode;
  if CurrentItem<>nil then
  if CurrentItem.NameSpace='' then
  begin

      numchildren:=length(CurrentItem.ChildNodes);

      if CurrentItem=MainFormProjectRoot then
        ArrayString:=',["Root(Events)"';

      for i:=0 to CurrentItem.myEventTypes.Count-1 do
      begin
        if (CurrentItem.HasUserEventCode(CurrentItem.myEventTypes[i]))
        and (CurrentItem.GetEventCode(CurrentItem.myEventTypes[i])<>dflt) then
        begin
           e:=e+1;
           ArrayString:=ArrayString+',';
           ArrayString:=ArrayString+'"'+CurrentItem.NodeType+'('+CurrentItem.NodeName+') '+CurrentItem.myEventTypes[i]+'"'
        end;
      end;

 //     {$ifdef JScript} showmessage(CurrentItem.Nodename+' has '+inttostr(numchildren)+' children'); {$endif}
      for i:=0 to numchildren-1 do
      begin
 //       {$ifdef JScript} showmessage('Child '+inttostr(i)+' '+CurrentItem.ChildNodes[i].NodeName); {$endif}
        ArrayString:=ArrayString+ConstructEventsTreeString(CurrentItem.ChildNodes[i],e);
      end;

      if CurrentItem=MainFormProjectRoot then
        ArrayString:=ArrayString+']';
  end;

  result:= ArrayString;
end;

Function ConstructGPUCodeTreeString(CurrentItem:TDataNode):String;
// Recursive
var ArrayString:String;
    i,numchildren:integer;
begin
  if CurrentItem<>nil then
  if CurrentItem.NameSpace='' then
  begin

      numchildren:=length(CurrentItem.ChildNodes);

      if CurrentItem=MainFormProjectRoot then
        ArrayString:=',["Root(GPUCode)"';

      if (CurrentItem.NodeType='TXGPUCanvas')
      and (CurrentItem.NodeClass='UI')
      and (CurrentItem.IsDynamic=true) then
      begin
        ArrayString:=ArrayString+',';
        ArrayString:=ArrayString+'"'+CurrentItem.NodeType+'('+CurrentItem.NodeName+') "'
      end;

      for i:=0 to numchildren-1 do
      begin
        ArrayString:=ArrayString+ConstructGPUCodeTreeString(CurrentItem.ChildNodes[i]);
      end;

      if CurrentItem=MainFormProjectRoot then
        ArrayString:=ArrayString+']';
  end;

  result:= ArrayString;
end;

function GetNavigatorHint(InTree:TDataNode;SystemNodeName:String):String;
var
SystemNode:TDataNode;
foundattrib:TNodeAttribute;
begin
  result:='';
  SystemNode:=FindDataNodeById(InTree,SystemNodeName,'',false);
  //showmessage('GetNavigatorHint '+SystemNodeName);
  if SystemNode<>nil then
  begin
    if SystemNode.HasAttribute('Hint') then
    begin
      foundattrib:=SystemNode.GetAttribute('Hint',false);
      if foundattrib<>nil then
        result:=foundattrib.AttribValue;
    end;
  end;
  //showmessage('GetNavigatorHint result '+result);
end;

procedure InitResourcesNodeTree;
var
  emptyAttribs:TNodeAttributesArray;
  RRoot:TDataNode;
begin
  setlength(emptyAttribs,0);
  RRoot:=FindDataNodeById(SystemNodeTree,'ResourceRoot','',false);
  if RRoot=nil then
  begin
    ResourcesNodeTree:=AddChildToDataNode(SystemNodeTree,'ResourceRoot',ResourceDataRootName,'','',emptyAttribs,-1);
    //ResourcesNodeTree.AddAttribute('ParentName','String', SystemRootName,true);
    // This represents the set of library 'resource' elements that are available in the resources tree
  end;
end;

function RegisterResource(ClassName,ScreenObjectType,ScreenObjectName,ParentName:string; Hint:String):TDataNode;  overload;
 var
      myAttribs:TNodeAttributesArray;
      myparent,myself:TDataNode;
      n,i:integer;
      TempChildNodes:TChildNodesArray;
  begin

    myparent:= FindDataNodeById(ResourcesNodeTree,ParentName,'',true);
    if myparent<>nil
    then
    begin
      setlength(myAttribs,1);
      myAttribs[0]:=TNodeAttribute.Create(nil,'Hint');
      myAttribs[0].AttribType:='String';
      myAttribs[0].AttribReadOnly:=True;
      myAttribs[0].AttribValue:=Hint;

      myself:=AddChildToDataNode(myparent,ClassName,ScreenObjectName,ScreenObjectType,'',myAttribs,-1);

      if myself<>nil then
      begin
        //mySelf.AddAttribute('ParentName', ParentName,'String',true);
        //mySelf.SetAttributeValue('ParentName', ParentName,'String');

        // myself is a local variable.  Re-assign it to the childnodes array.
        TempChildNodes:= myparent.ChildNodes;
        n:= Length(TempChildNodes);
        myparent.ChildNodes[n-1]:=myself;
      end;

      result:=myself;
    end;
  end;

procedure BuildSkeletonResourceTree;
var
  emptyAttribs:TNodeAttributesArray;
begin
  setLength(emptyAttribs,0);
  ResourcesNodeTree:=AddChildToDataNode(SystemNodeTree,'ResourceRoot',ResourceDataRootName,'','',emptyAttribs,-1);
  //ResourcesNodeTree.AddAttribute('ParentName','String', SystemRootName,true);
  RegisterResource('RUI','','Composites','ResourceRoot','');
end;

procedure SaveSystemData;
var
  TheData:String;
  NavSelected,CodeSelected:TDataNode;
begin
  if not StartingUp then
  begin
    NavSelected:=ObjectInspectorSelectedNavTreeNode;
    CodeSelected:=ObjectInspectorSelectedNavTreeNode;

    TheData:=BuildSystemString(false);
    {$ifndef JScript}
    WriteToLocalStore('XIDESavedData.txt',TheData);
    {$else}
    BrowserSaveData(TheData);
    {$endif}
    SaveLocalDB;

    ObjectInspectorSelectedNavTreeNode:=NavSelected;
    ObjectInspectorSelectedNavTreeNode:=CodeSelected;
  end;
end;

{$ifdef JScript}
procedure BrowserSaveData(TheData:String);
var
  sysname:string;
begin
  sysname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  asm
   //console.log('saving local XIDESavedData'+sysname);
   pas.HTMLUtils.WriteToLocalStore("XIDESavedData"+sysname,TheData);
  end;
end;
{$endif}

procedure InitialiseXIDE;
begin
  //OITabs:=FindDataNodeById(SystemNodeTree,'OITabs','',true);
  OITabs:=FindDataNodeById(UIRootNode,'OITabs','',true);
  PropertiesNode:=FindDataNodeById(OITabs,PropertyEditorScrollboxName,'',true);
  DMAttribsNode:=FindDataNodeById(UIRootNode,DMAttribsScrollboxName,'',true);
  InterfacePropsNode:=FindDataNodeById(OITabs,CompositePropsScrollboxName,'',true);
  EventsNode:=FindDataNodeById(OITabs,EventsEditorScrollboxName,'',true);
  InterfaceTabNode:=FindDataNodeById(OITabs,CompositePropsTabName,'',true);
  AddInterfacePropsButtonNode:=FindDataNodeById(UIRootNode,'OIAddPropertyButton','',true);

  InitResourcesNodeTree;
  RebuildResourcesTree;
  // Populate navigator and code tree from SystemNodeTree
  RebuildNavigatorTree;
  RebuildCodeTree;
  RebuildDMTree;

  {$ifndef JScript}
  ConsoleNode:=FindDataNodeById(UIRootNode,'XMemo1','',true);
  {$else}
  AboutXIDEForm.BuildText;
  PasteDialogUnit.SetupPasteDialogForm;
  PasteDialogUnit.PasteTarget.myNode.registerEvent('MemoPaste',@OIEventWrapper.OIPasteTarget);
  XGPUEditor.CreateGPUEditForm;
  {$endif}
end;


{$ifdef JScript}
procedure InitialiseComposites;
var
  composites:TDataNode;
  i:integer;
  itemname,src:String;
begin
  //showmessage('InitialiseComposites '+CompositesString);
  // The string CompositesString contains the XML list of the stored composites defined in the desktop system.
  // These are now being transferred into the browser environment.
  composites:=FindDataNodeById(ResourcesNodeTree,'Composites','',false);
  if (composites<>nil)
  and (CompositesString<>'</>') then
    XMLToNodeTree(CompositesString,composites,false);
  for i:=0 to length(composites.ChildNodes)-1 do
  begin
    itemName:=composites.ChildNodes[i].NodeName;
    // these names all have a _xcmp suffix.
    composites.ChildNodes[i].NodeName:=itemName;
    src:=composites.ChildNodes[i].GetAttribute('SourceString',false).AttribValue;
    itemName:=Copy(itemName,1,length(itemName)-5);
    WriteToLocalStore(itemName+'.xcmp',src);
  end;
  RebuildResourcesTree;
end;
{$endif}

procedure ClearAllComposites;
var
  composites:TDataNode;
begin
  composites:=FindDataNodeById(ResourcesNodeTree,'Composites','',false);
  DeleteNodeChildren(composites);
end;

procedure DiscoverSavedFiles(suffix:String;var NamesList:TStringList);
{$ifndef JScript}
var
  Info : TSearchRec;
  Count : Longint;
  bits:TStringList;
  tmp:String;
  dt: TDateTime;
  y, d, m, h, min, sec, msec: word;
begin

  bits:=TStringList.Create;
    Count:=0;
    If FindFirst ('SavedSystems/*.'+suffix,faAnyFile,Info)=0 then
    begin
      Repeat
        Inc(Count);
        With Info do
          begin
            bits:=stringsplit(Name,'.');
            tmp:=bits[0];
            if suffix<>'xcmp' then
            begin
              dt := Info.TimeStamp;
              decodeDate( dt, y, m, d );
              decodeTime( dt, h, min, sec, msec );
              tmp:=tmp+'      '+format('%.2d',[d])+'/'+format('%.2d',[m])+'/'+inttostr(y)+
                       ' '+format('%.2d',[h])+':'+format('%.2d',[min])+':'+format('%.2d',[sec]);
            end;
            if bits[0]<>'' then
              NamesList.Add(tmp);
          end;
      Until FindNext(info)<>0;
    end;
    FindClose(Info);
{$else}
var
  namesArray:TStringArray;
  n:integer;
  bits:TStringList;
  nm,ts:String;
begin
  bits:=TStringList.Create;
  setlength(namesArray,0);
  asm
  namesArray = Object.keys(localStorage);
  end;
  for n:= 0 to length(namesArray)-1 do
  begin
    bits:=stringsplit(namesArray[n],'.');
    if (bits.count=2)
    and (bits[1]=suffix) then
    begin
      nm:=bits[0];
      asm
      if (suffix!='xcmp') {
      // find the timestamp...
      try {
      var object = JSON.parse(localStorage.getItem(namesArray[n]));
      ts = '';
      if ((object!=null)&&(suffix!='xcmp')) {
        if ( object.hasOwnProperty('timestamp') ) {
        var tsd = new Date(object.timestamp);
        ts = '      '+tsd.toLocaleDateString()+' '+tsd.toLocaleTimeString();
        }
      }
      } catch(err) {ts='';  // skip the un-parseable item.
                   }
      }
      end;
      NamesList.Add(nm+ts);
    end;
  end;
{$endif}
  bits.Free;
end;

procedure DiscoverSavedSystems(var NamesList:TStringList);
begin
  DiscoverSavedFiles('xide',NamesList);
end;
procedure DiscoverSavedComposites(var NamesList:TStringList);
begin
  DiscoverSavedFiles('xcmp',NamesList);
end;

procedure SetupAvailableResources;
var
  NamesList:TStringList;
  i:integer;
  str:String;
  RNode:TDataNode;
begin

  RegisterResource('RUI','','UIComponents',ResourceDataRootName,'');
  // Set up the tree of available components

  RegisterResource('RUI','','Layout','UIComponents','');
  RegisterResource('RUI','TXVBox','TXVBox','Layout','Panel for vertically aligned items');
  RegisterResource('RUI','TXHBox','TXHBox','Layout','Panel for horizontally aligned items');
  RegisterResource('RUI','TXGroupBox','TXGroupBox','Layout','');
  RegisterResource('RUI','TXScrollBox','TXScrollBox','Layout','Scrollable panel (with vertical alignment of items)');
  RegisterResource('RUI','TXTabControl','TXTabControl','Layout','Container for a set of Tab Pages');
  RegisterResource('RUI','TXTabSheet','TXTabSheet','Layout','Tab Page');
  RegisterResource('RUI','TXForm','TXForm','Layout','Pop-up Form');

  RegisterResource('RUI','','Numeric','UIComponents','');
  RegisterResource('RUI','TXProgressBar','TXProgressBar','Numeric','');
  RegisterResource('RUI','TXNumericSlider','TXNumericSlider','Numeric','');
  //AddAttrib(AttrParams,'StepSize','Real','1',false);
  RegisterResource('RUI','TXNumberSpinner','TXNumberSpinner','Numeric','');

  RegisterResource('RUI','','Text','UIComponents','');
  RegisterResource('RUI','TXLabel','TXLabel','Text','Simple text label');
  RegisterResource('RUI','TXHyperlink','TXHyperlink','Text','URL Link item');
  //AddAttrib(AttrParams,'ContainerWidth','','',false);
  RegisterResource('RUI','TXEditBox','TXEditBox','Text','Editable text box');
  RegisterResource('RUI','TXMemo','TXMemo','Text','Editable multi-line text box');
  RegisterResource('RUI','TXTable','TXTable','Text','2D Tabular data display');
  RegisterResource('RUI','TXDBTable','TXDBTable','Text','2D Tabular dataset display');

  //{$ifdef Python}
  ////AddAttrib(AttrParams,'Language','String','Python',false);
  //RegisterResource('RUI','TXCode','TXCode','Text','');
  //{$endif}

  RegisterResource('RUI','','Selectors','UIComponents','');
  RegisterResource('RUI','TXButton','TXButton','Selectors','Button');
  //AddAttrib(AttrParams,'Enabled','Boolean','True',false);
  RegisterResource('RUI','TXCheckBox','TXCheckBox','Selectors','Checkbox');
  RegisterResource('RUI','TXRadioBtns','TXRadioBtns','Selectors','Radio buttons group');
  //AddAttrib(AttrParams,'ItemIndex','Integer','0',false);
  RegisterResource('RUI','TXComboBox','TXComboBox','Selectors','Drop-down selector');
  //AddAttrib(AttrParams,'ItemText','String','Tree',false);
  RegisterResource('RUI','TXTree','TXTree','Selectors','Hierarchical tree structure');
  RegisterResource('RUI','TXDatePicker','TXDatePicker','Selectors','Date picker');
  RegisterResource('RUI','TXColorPicker','TXColorPicker','Selectors','Colour picker');
  RegisterResource('RUI','TXMainMenu','TXMainMenu','Selectors','Main menu, container for menu items');
  RegisterResource('RUI','TXMenuItem','TXMenuItem','Selectors','Menu Item');

  //   RegisterResource('RUI','FilePicker','FilePicker','Selectors',-1,AttrParams);

  RegisterResource('RUI','','Media','UIComponents','');
  RegisterResource('RUI','TXImage','TXImage','Media','Image');
  RegisterResource('RUI','TXBitMap','TXBitMap','Media','Bitmap');

     // missing.....Audio
     // missing.....Video
     // missing.....Chat

  RegisterResource('RUI','','IFrame','UIComponents','');
  RegisterResource('RUI','TXIFrame','TXIFrame','IFrame','IFrame (embedded HTML) component');
  RegisterResource('RUI','TXGPUCanvas','TXGPUCanvas','IFrame','GPU Canvas');
  RegisterResource('RUI','TXHTMLEditor','TXHTMLEditor','IFrame','Editable HTML text frame');
  RegisterResource('RUI','TXHTMLText','TXHTMLText','IFrame','Non-editable HTML Text frame');
  RegisterResource('RUI','TXSVGContainer','TXSVGContainer','IFrame','SVG Container');
  RegisterResource('RUI','','SVGComponents','IFrame','');
  RegisterResource('RSVG','TXSVGText','TXSVGText','SVGComponents','SVG Text element');
  RegisterResource('RSVG','TXSVGRect','TXSVGRect','SVGComponents','SVG Rectangle element');
  RegisterResource('RSVG','TXSVGRoundedRect','TXSVGRoundedRect','SVGComponents','SVG Rounded Rectangle element');
  RegisterResource('RSVG','TXSVGCircle','TXSVGCircle','SVGComponents','SVG Circle element');
  RegisterResource('RSVG','TXSVGEllipse','TXSVGEllipse','SVGComponents','SVG Ellipse element');
  RegisterResource('RSVG','TXSVGLine','TXSVGLine','SVGComponents','SVG Line element');
  RegisterResource('RSVG','TXSVGPolyLine','TXSVGPolyLine','SVGComponents','SVG Poly-line element');
  RegisterResource('RSVG','TXSVGPolyGon','TXSVGPolyGon','SVGComponents','SVG Polygon element');

  RegisterResource('RUI','','Composites','UIComponents','');

      // Set up the list of previously saved composite items
      NamesList:=TStringList.Create;
      DiscoverSavedComposites(NamesList);
      for i:=0 to NamesList.count-1 do
      begin
        //ClearAttribs(AttrParams);
        //AddAttrib(AttrParams,'CompositeType','String',NamesList[i],true);
        {$ifndef JScript}
        str:=ReadFromLocalStore('SavedSystems/'+NamesList[i]+'.xcmp');
        {$else}
        str:=ReadFromLocalStore(NamesList[i]+'.xcmp');
        {$endif}
        //AddAttrib(AttrParams,'SourceString','String',str,true);
        RNode := RegisterResource('RUI','TXComposite',NamesList[i]+'_xcmp','Composites','Composite (encapsulated) component');   //suffix to protect nodename-uniqueness
        RNode.SetAttributeValue('CompositeType',NamesList[i],'String',true);
        RNode.SetAttributeValue('SourceString',str,'String',true);
      end;

  RegisterResource('RNV','','Non-Visual Components',ResourceDataRootName,'');
  RegisterResource('RNV','TXStore','TXStore','Non-Visual Components','Data Store item (keyname, value pair)');
  RegisterResource('RNV','TXTrapEvents','TXTrapEvents','Non-Visual Components','Event handler to intercept system events');
  RegisterResource('RNV','TXThreads','TXThreads','Non-Visual Components','To provide multi-thread process');
  RegisterResource('RNV','TXCompositeIntf','TXCompositeIntf','Non-Visual Components','Interface element for Composites');

  NamesList.Free;

end;


function XIDEPrompt(PromptString,DefaultString:string):string;
var
  str:string;
begin
    {$ifndef JScript}
    str:=DefaultString;
    if not InputQuery('XIDE', PromptString, str) then
      str:=''   //user cancelled the dialog
    else
    begin
      //Following event handler being called (while probably in design mode) due to the requirement for
      //event logging during design.
      // Not available for capture by user-written code.
      HandleEvent(nil,'UserInput',SystemRootName,'',str);
    end;
    {$else}
    asm
     var res = prompt(PromptString, DefaultString);
     if (res==null) {str=''}
      else {
        str=res;
        //Following event handler being called (while probably in design mode) due to the requirement for
        //event logging during design.
        // Not available for capture by user-written code.
        pas.Events.handleEvent(null,'UserInput',pas.NodeUtils.SystemRootName,'',str);
      }
    end;
    {$endif}

    result:=str;
end;

function NameStringIsValid(nm:String):Boolean;
var
  ok:Boolean;
begin
  ok:=false;
  if FoundString(nm,'.')>0 then
    showmessage('Enter the name without dot characters ''.''')
  else if FoundString(nm,' ')>0 then
    showmessage('Enter the name without space characters')
  else if not SysUtils.IsValidIdent(nm) then
    showmessage('Name is not valid - Use Alphanumeric characters plus ''_''; first char non-numeric.')
  else
    ok:=true;
  result:=ok;
end;

function GetValidItemName(PromptString,DefaultString:String):String;
var
  resultstring:String;
  ok:Boolean;
begin
  ok:=false;
  while not ok do
  begin
    resultstring:= trim(XIDEPrompt(PromptString,DefaultString));
    if resultstring<>'' then
    begin
      DefaultString:=resultstring;
      ok:=NameStringIsValid(resultstring);
    end
    else
      ok:=true; // user cancelled the action
  end;
  result:=resultstring;
end;

function GetComponentName(PromptString:string):string;
var DefaultString:string;
begin
   DefaultString := 'Created_'+DateTimeToStr(Now);
   DefaultString :=ReplaceStr( DefaultString,' ','_');
   DefaultString :=ReplaceStr( DefaultString,':','_');
   DefaultString :=ReplaceStr( DefaultString,'-','_');
   DefaultString :=ReplaceStr( DefaultString,'/','_');
   result:=GetValidItemName(PromptString,DefaultString);
end;

function ComponentNameIsUnique(ScreenObjectName,NameSpace:string):Boolean;
var
  myresult:Boolean;
  founditem:TDataNode;
begin
  myresult:=true;
  founditem:=FindDataNodeById(SystemNodeTree,ScreenObjectName,NameSpace,false);    //UIRootNode??
  if (founditem<>nil)
  then
  begin
    ShowMessage('Error. Name >'+ScreenObjectName+'< is not unique when creating a new system component' );
    myresult:=false;
  end;
  result:=myresult;
end;

{$ifdef JScript}

procedure ShowXFormForDesign(XFormName:String);
var
  XFormNode:TDataNode;
begin
  //showmessage('ShowXFormForDesign '+XFormName);
  // Swap the centre section of the screen for the requested XForm, so it can be displayed while
  // still accessing the nav tree and resources tree
  asm
    var ob = document.getElementById(XFormName);
    var UIScreen = document.getElementById('UIRoot');
    var UIContent = document.getElementById('UIRootContents');
    UIContent.style.display = "none";
    UIScreen.appendChild(ob);
    //pas.XForm.ShowXForm(XFormName);
    ob.style.position='relative';
  end;
  XFormNode:=FindDataNodeById(UIRootNode,XFormName,'',true);
  TXForm(XFormNode.ScreenObject).Showing:='Modal';
end;

procedure CloseXFormForDesign(XFormName:String);
var
  UIRootNodeName:string;
begin
  //showmessage('CloseXFormForDesign '+XFormName);
  UIRootNodeName:=UIRootNode.Nodename;
  asm
    //alert('UIRootNodeName='+UIRootNodeName);
    var ob = document.getElementById(XFormName);
    if (ob!=null) {
      ob.style.position='fixed';
      ob.style.display = "none";
      var root =  document.getElementById(UIRootNodeName);
      root.appendChild(ob);
      }
    var UIContent = document.getElementById('UIRootContents');
    UIContent.style.display = "flex";
  end;
  //if OpenXForms.IndexOf(XFormName)>-1 then
  DeleteOpenXForm(XFormName,'');
end;

{$endif}

procedure DeHighlightObject(OldNode:TDataNode);
// de-highlight the previous selected component.
begin
  {$ifndef JScript}
  if OldNode<>nil then
    begin
      SetBooleanProperty(TComponent(OldNode.ScreenObject),'IsSelected',false);
    end;
  {$else}
  asm
    //alert('LastHTMLUserInterfaceItemSelected='+pas.XObjectInsp.LastHTMLUserInterfaceItemSelected+' hadBorder='+pas.XObjectInsp.LastHTMLUserInterfaceItemHadBorder);
    pas.HTMLUtils.UnHighlight(pas.XObjectInsp.LastHTMLUserInterfaceItemSelected,pas.XObjectInsp.LastHTMLUserInterfaceItemHadBorder);
  end;
  {$endif}

end;

procedure HighlightNavigatorObject(CurrentNode:TDataNode);
var
  {$ifdef JScript}
  mf:TForm;
  mfn,cfn:string;
  {$endif}
  tmp:string;
  Border:Boolean;
  i:integer;
  ParentNode:TDataNode;
  TabPage:TXTabSheet;
begin
  tmp:='';
  if CurrentNode.HasAttribute('Border') then
    tmp:=CurrentNode.GetAttribute('Border',false).AttribValue;
  if tmp='' then
    Border:=false
  else
    Border:=myStrToBool(tmp);

  // if the component is in a XForm, show the form
  {$ifndef JScript}
  for i:=length(OpenXForms)-1 downto 0 do
  begin
    if (OpenXForms[i].NodeName<>MainForm.Name) or (OpenXForms[i].NameSpace<>'') then
      CloseXForm(OpenXForms[i].NodeName,OpenXForms[i].NameSpace);
  end;
  if CurrentNode.MyForm<>nil  then
    ShowXForm(CurrentNode.MyForm.Name,false,CurrentNode.NameSpace);
  {$else}
  for i:=length(OpenXForms)-1 downto 0 do
  begin
    if OpenXForms[i].NameSpace='' then
      CloseXFormForDesign(OpenXForms[i].NodeName);
  end;
  {$endif}


  {$ifndef JScript}
  // highlight component on screen
  if CurrentNode.NodeClass='UI' then
    SetBooleanProperty(TComponent(CurrentNode.ScreenObject),'IsSelected',true)
  else
    SetBooleanProperty(CurrentNode,'IsSelected',true);
  if LastLazUserInterfaceItemSelected <> '' then
  begin
    LastLazUserInterfaceItemSelected:='';
  end;
  if (CurrentNode.ScreenObject<>nil) then
  begin
    LastLazUserInterfaceItemSelected:=CurrentNode.NodeName;
  end;
 {$Else}
    // HTML - highlight component on screen
    mf:=MainForm;
    mfn:=MainForm.Name;
    if CurrentNode.MyForm<>nil then
      cfn:=CurrentNode.MyForm.Name
    else
    begin
      //showmessage('CurrentNode.MyForm is nil');
      cfn:=mf.Name;
    end;
    asm
     try{
     //alert('highlight component on screen...'+CurrentNode.NodeName);
     //alert('cfn='+cfn+' mfn='+mfn);
      if  (cfn!=mfn) {
         $mod.ShowXFormForDesign(cfn)  }
      else {
        var UIContent = document.getElementById('UIRootContents');
        UIContent.style.display = "flex";
      }

     var myobj = document.getElementById(CurrentNode.NameSpace+CurrentNode.NodeName);
     if ((myobj!=null)&&(myobj!=cfn)) {

       pas.HTMLUtils.Highlight(myobj.id);
       pas.XObjectInsp.LastHTMLUserInterfaceItemSelected = myobj.id;
       pas.XObjectInsp.LastHTMLUserInterfaceItemHadBorder = Border;

       }
     //alert('highlight component done.');
     }catch(err) { alert(err.message+'  in XObjectInsp.HighlightNavigatorObject'); }
   end;

  {$endif}

  // ok we have raised the relevant form, and highlighted the object.
  // Now check up the parent list in case it's on a closed tab page...
  //ParentNode:=FindParentOfNode(SystemNodeTree,CurrentNode);
  ParentNode:=CurrentNode.NodeParent;
  while (ParentNode<>nil)
  and (ParentNode.NodeType<>'TXForm')
  and (ParentNode.NodeName<>'ApplicationRoot')
  and (ParentNode<>UIRootNode) do
  begin
    if ParentNode.NodeType='TXTabSheet' then
    begin
      TabPage:=TXTabSheet(ParentNode.ScreenObject);
      //ParentNode:=FindParentOfNode(SystemNodeTree,ParentNode);
      ParentNode:=ParentNode.NodeParent;
      {$ifndef JScript}
      TXTabControl(ParentNode.ScreenObject).ActivePage:=TabPage;
      {$else}
      ChangeTabPage(TabPage.NodeName,ParentNode.NodeName,'',TabPage);
      {$endif}
    end;
    //ParentNode:=FindParentOfNode(SystemNodeTree,ParentNode);
    ParentNode:=ParentNode.NodeParent;
  end;

end;

procedure RefreshObjectInspector(CurrentNode:TDataNode);
var Prefix1,AttributePrefix:string;
  i:integer;
  WidgetNode:TDataNode;
  myAttribs:TNodeAttributesArray;
  s:boolean;
  BoxName,AttribValue:String;
  MyWidget:TObject;
begin
  s:=SuppressEvents;
  SuppressEvents:=true;
  //showmessage('RefreshObjectInspector. Node='+CurrentNode.NodeName);

  if (CurrentNode<>nil)  then
  begin
      if CurrentNode.NodeClass='DM' then
        Prefix1:='DM'
      else
        Prefix1:='OI';
      AttributePrefix:=Prefix1+AttributeEditorNameDelimiter+CurrentNode.NodeName;
      myAttribs:=CurrentNode.NodeAttributes;

      for i:=0 to length(myAttribs)-1 do
      begin
        //exclude Suppressed properties that user shouldn't see
        if (FindSuppressedProperty(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName)<0)
        and (CurrentNode.NodeAttributes[i].AttribName<>'ParentName') then
        begin
          BoxName:=AttributePrefix+AttributeEditorNameDelimiter
                    +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                    +IntToStr(i+2);
          AttribValue:=CurrentNode.NodeAttributes[i].AttribValue;
          WidgetNode:=FindDataNodeById(PropertiesNode,BoxName,'',false);
          if WidgetNode<>nil then
          begin
            MyWidget:=WidgetNode.ScreenObject;

            if (CurrentNode.NodeAttributes[i].AttribType = 'Boolean')
            and (myWidget is TXCheckBox) then
            begin
               TXCheckBox(myWidget).Checked:=myStrtoBool(AttribValue);
            end
            else if (myWidget is TXComboBox) then
            begin
              TXComboBox(myWidget).ItemValue:=AttribValue;
            end
            else if (myWidget is TXEditBox) then
            begin
              TXEditBox(myWidget).ItemValue:=AttribValue;
            end
            else if (myWidget is TXColorPicker) then
            begin
              TXColorPicker(myWidget).ItemValue:=AttribValue;
            end;
          end;
       end;

     end;

    //------------------- Refresh the registered Events Tabpage -----------------------

    if CurrentNode.IsDynamic then
    begin
        for i:=0 to CurrentNode.MyEventTypes.count-1 do
        begin
          BoxName:=AttributePrefix+AttributeEditorNameDelimiter
                    +CurrentNode.MyEventTypes[i]+AttributeEditorNameDelimiter
                    +IntToStr(i+2);
          AttribValue:=CurrentNode.myEventHandlers[i].TheCode;

          WidgetNode:=FindDataNodeById(EventsNode,BoxName,'',false);
          if WidgetNode<>nil then
          begin
            MyWidget:=WidgetNode.ScreenObject;
            if MyWidget<>nil then
              TXEditBox(myWidget).ItemValue:=AttribValue;
          end;

        end;
    end;
  end;

  SuppressEvents:=s;
end;

procedure RefreshObjectInspectorLater(targetNode:TDataNode);
begin
  {$ifndef JScript}
  // do after delay because this process deletes the button whose click event we are handling
  ObjectInspectorSelectedNavTreeNode:=nil;
  HandleEventLater(nil,'TreeNodeClick','NavTree','','');
  {$else}
  PopulateObjectInspector(targetNode);
  {$endif}
end;

procedure DoSelectNavTreeNode(CurrentNode:TDataNode; refresh:boolean);
var
  mynodeText:string;
  IFrameNode:TDataNode;
  okToContinue:Boolean;
begin
  if CurrentNode.NameSpace<>'' then
    EXIT;

  okToContinue:=true;
  {$ifdef JScript}
  asm
    var ob=document.getElementById('Grey1');
    if (ob!=null) {
      if (ob.style.display!='none') {
        okToContinue=false;
      }
    }
  end;
  if length(OpenXForms)>0 then
    okToContinue:=false;
  {$endif}

   //!! Should operate this by node id, not text  :: HOWEVER, in the nav tree, node texts are all unique.
  //showmessage('DoSelectNavTreeNode '+CurrentNode.NodeClass+' '+CurrentNode.NodeType+' '+CurrentNode.NodeName+' ');
  if (DesignMode) and (okToContinue) then
  begin
    XDBTable.SetDSNameOptions(DMRoot);

    {$ifdef Chromium}
     {$ifndef JScript}
     // For an SVG container, inspect the Title, and switch to a child SVG node, if appropriate.
     if CurrentNode.NodeType='TXSVGContainer' then
     begin
       IFrameNode:=SVGItemFromTitle(TXSVGContainer(CurrentNode.ScreenObject).FrameTitle);
       if IFrameNode<>nil then
         CurrentNode:=IFrameNode;
     end;
     {$endif}
    {$endif}

     mynodeText := CurrentNode.NodeType+'('+CurrentNode.Nodename+')';
     //showmessage('mynodeText='+mynodeText);

     TreeInFocus := UIRootNode;

     {$ifndef JScript}
     TXTree(NavTreeComponent.ScreenObject).SelectedNodeText:=mynodeText;  // (Windows) this selects the node in the navtree component, if changed.
     {$endif}

     if (ObjectInspectorSelectedNavTreeNode=nil)
     or (ObjectInspectorSelectedNavTreeNode<>CurrentNode) then
     begin
       DeHighlightObject(ObjectInspectorSelectedNavTreeNode);

       //display the attributes of the selected node in the object inspector
       ObjectInspectorSelectedNavTreeNode:=CurrentNode;
       PopulateObjectInspector(CurrentNode);
       {$ifdef JScript}
       TXTree(NavTreeComponent).SelectedNodeText:=mynodeText;   // (Browser) this selects the node in the navtree component, if changed.
       {$endif}

       // Highlight the selected object (dotted border)
       HighlightNavigatorObject(ObjectInspectorSelectedNavTreeNode);
     end
     else  if (refresh) then
     begin
       // just refresh all the displayed property values
       RefreshObjectInspector(CurrentNode);
     end;

  end;
end;

procedure  SelectNavTreeNode(CurrentNode:TDataNode; refresh:boolean);
begin
  if CurrentNode.NameSpace='' then
  begin
    {$ifndef JScript}
    DoSelectNavTreeNode(CurrentNode,refresh);
    {$else}
    asm
    myTimeout(pas.XObjectInsp.DoSelectNavTreeNode,5,'DoSelectNavTreeNode',0,CurrentNode,refresh);
    end;
    {$endif}
  end;
end;


procedure  SelectCodeTreeNode(CurrentNode:TDataNode; refresh:boolean; TreeNodeText:String);
begin
     //!!!! Should to operate this by node id, not text  :: HOWEVER, in the code tree, node texts are all unique.

     //showmessage('SelectCodeTreeNode '+CurrentNode.Nodename+' >'+TreeNodeText+'<');
     //mynodeText := CurrentNode.NodeType+'('+CurrentNode.Nodename+')';
     TreeInFocus := CodeRootNode;

     {$ifndef JScript}
     TXTree(CodeTreeComponent.ScreenObject).SelectedNodeText:=TreeNodeText;  // this selects the node in the codetree component, if changed.
     {$endif}

     if CurrentNode<>nil then
     begin
       if (refresh)
       or (ObjectInspectorSelectedCodeTreeNode=nil)
       or (ObjectInspectorSelectedCodeTreeNode<>CurrentNode) then
       begin

         if DesignMode then
         begin
           ObjectInspectorSelectedCodeTreeNode:=CurrentNode;
           {$ifdef JScript}
           TXTree(CodeTreeComponent).SelectedNodeText:=TreeNodeText;  // this selects the node in the codetree component, if changed.
           {$endif}

         end;
       end;
       ObjectInspectorSelectedCodeNodeText:=TreeNodeText;
     end
     else
     begin
       ObjectInspectorSelectedCodeTreeNode:=CurrentNode;
     end;
end;

procedure RebuildResourcesTree;
 var
     newtreestring:string;
     ResTree:TXTree;
 begin
   //showmessage('ConstructSystemTreeString for resources');
   ClearResourceInspector;
   DeleteNodeChildren(ResourcesNodeTree);

   SetupAvailableResources;

   newtreestring:= ConstructSystemTreeString(ResourcesNodeTree,0,false,false,nil,'');
   ResTree:=TXTree(ResourceTreeComponent.ScreenObject);
   {$ifndef JScript}
   TXTree(ResourceTreeComponent.ScreenObject).TreeData:=newtreestring;
   TmyTreeView(ResTree.myControl).ExpandTreeNodes(ResTree.OpenToLevel);
   {$else}
   TXTree(ResourceTreeComponent).TreeData:=newtreestring;
   {$endif}
end;

procedure RebuildNavigatorTree;
var
    newtreestring:string;
    tempNode:TDataNode;
begin
(*
actual node tree structure...
(SystemNodeTree)
 |
UIRootNode
   |
   MainForm
       |
       Whole of main form
   XForms

what we want to see on the navigator is....
UIRootNode
   |
   MainForm
       UIRoot (centre section only)
       MainMenu
       Non-visual components
   XForms (dynamic only)

*)

  // construct string for the 'UI' section of the main form
  //showmessage('ConstructSystemTreeString for nav');
  newtreestring:= ConstructSystemTreeString(UIRootNode,0,true,false,nil,'');
  {$ifndef JScript}
  // WriteToFile('navtree.txt',newtreestring);
  TXTree(NavTreeComponent.ScreenObject).TreeData:=newtreestring;
  {$else}
  TXTree(NavTreeComponent).TreeData:=newtreestring;
  {$endif}

  // set hints for the special attributes on UIRootNode
  UIRootNode.GetAttribute('DeploymentMode',false).AttribHint:='When the designed system is deployed to html (menu System>Deploy) it is set up to start in design mode or run mode ';
  UIRootNode.GetAttribute('DBVersion',false).AttribHint:='Latest iteration number of the local database built from data description on entry to run mode ';
  UIRootNode.GetAttribute('PythonPackages',false).AttribHint:='Python packages to be loaded in the browser (pyodide) environment (; delimited, eg numpy;matplotlib). ';


  if ObjectInspectorSelectedNavTreeNode<>nil then
  begin
    //showmessage('selecting node '+ObjectInspectorSelectedNavTreeNode.nodename);
    tempNode:=ObjectInspectorSelectedNavTreeNode;
    ObjectInspectorSelectedNavTreeNode:=nil;   // so it will be selected/green bordered
    SelectNavTreeNode(tempNode,true);
  end;
end;

procedure RebuildCodeTree;
var
    newtreestring:string;
    e:integer;
begin
(*
CodeRootNode
   |
   Unit1
   ...
   Event Handlers
      |
      Event 1
      ...
   GPU Canvas widgets
      |
      Widget 1 Animation Code
*)

  // construct string for the 'Code' section of the main form
  //showmessage('ConstructSystemTreeString for code');
  newtreestring:= ConstructCodeTreeString(CodeRootNode,0);

  // For improved visibility, also add nodes to this tree for all event handlers
  // defined within the UIRootNode
  e:=0;
  newtreestring := '["Root",'+newtreestring + ConstructEventsTreeString(SystemNodeTree,e);

  // AND any GPUCanvas widgets, for access to the AnimationCode property
  newtreestring := newtreestring + ConstructGPUCodeTreeString(SystemNodeTree);

  newtreestring := newtreestring + ']';


  {$ifndef JScript}
  TXTree(CodeTreeComponent.ScreenObject).TreeData:=newtreestring;
  {$else}
  TXTree(CodeTreeComponent).TreeData:=newtreestring;
  {$endif}
  if ObjectInspectorSelectedCodeTreeNode<>nil then
  begin
    SelectCodeTreeNode(ObjectInspectorSelectedCodeTreeNode,true,ObjectInspectorSelectedCodeNodeText);
  end;

end;

procedure HandleNavTreeClickEvent(DataNodeId:String; TreeNodeText:String);
var CurrentNode :TDataNode;
begin
//ShowMessage('ntc event. node='+DataNodeId+' Text='+TreeNodeText);
  if TreeNodeText<>'Root' then
  begin
    CurrentNode:=FindDataNodeById(UIRootNode,DataNodeId,'',true);
    if CurrentNode=nil then
      showmessage('Cannot find node '+DataNodeId+' in XObjectInsp.HandleNavTreeClickEvent')
    else
    begin
      SelectNavTreeNode(CurrentNode,false);
    end;

  end;
end;

function FindLineNumForProc(ProcName:String):integer;
var
  i,ln:integer;
begin
  ln:=1;
  i:=0;
  while i<length(XIDEProcsList) do
  begin
    if lowercase(XIDEProcsList[i].Name) = lowercase(ProcName) then
    begin
      ln:=XIDEProcsList[i].LineNum;
    end;
    i:=i+1;
  end;
  result:=ln;
end;

procedure HandleCodeTreeClickEvent(TreeNodeId,TreeNodeText,FirstBit:String);
var
  CurrentNode :TDataNode;
  ParentText, ParentId, p1:String;
  {$ifndef JScript}
  myTreeNode:TTreeNode;
  {$else}
  selectedNodeId:String;
  {$endif}
begin
//ShowMessage('HandleCodeTreeClickEvent. node='+TreeNodeId+' text='+TreeNodeText);
  OISelectedCodeProcName:='';
  if (TreeNodeText<>CodeRootName)
  and (TreeNodeText<>'Root')
  and (TreeNodeText<>'Root(Events)')
  and (TreeNodeText<>'Root(GPUCode)') then
  begin
    CurrentNode:=FindDataNodeById(SystemNodeTree,TreeNodeId,'',false);
    if CurrentNode<>nil then
    begin
      SelectCodeTreeNode(CurrentNode,false,TreeNodeText);
    end
    else
    begin
      // TreeNodeId might be the name of a function within a pascal unit (if compiler has been run),
      // in which case the relevant data node is the parent pasunit.
      {$ifndef JScript}
      myTreeNode:=TMyTreeView(TXTree(CodeTreeComponent.ScreenObject).myControl).Selected;
      ParentText:=myTreeNode.Parent.Text;
      {$else}
      selectedNodeId:=TXTree(CodeTreeComponent).SelectedNodeId;
      ParentText:=TXTree(CodeTreeComponent).getParentOfNode(selectedNodeId);
      //showmessage('ParentText1='+ParentText);
      ParentText:=TXTree(CodeTreeComponent).TextOfNode(ParentText);
      //showmessage('ParentText2='+ParentText);
      {$endif}
      ParentId:=TreeLabelToID(ParentText,'CodeTree',p1);
      CurrentNode:=FindDataNodeById(SystemNodeTree,ParentId,'',true);
      //CurrentNode:=FindDataNodeById(UIRootNode,ParentId,'',true);
      if (CurrentNode<>nil)
      and (CurrentNode.NodeType='PasUnit') then
      begin
         SelectCodeTreeNode(CurrentNode,false,TreeNodeText);
         OISelectedCodeProcName:=TreeNodeId;
         //OISelectedCodeLineNum:=strtoint(FirstBit);
         OISelectedCodeLineNum:=FindLineNumForProc(OISelectedCodeProcName);
      end
      else
      begin
        showmessage('Cannot find system node '+TreeNodeId);
        ObjectInspectorSelectedCodeTreeNode:=nil;
      end;
    end;
  end
  else
    ObjectInspectorSelectedCodeTreeNode:=nil;
end;

procedure CopyNavNode( NodeToCopy:TDataNode);
begin
   // Copy an item from the Navigator tree.  Populates ObjectInspectorSourceNode.
   if (NodeToCopy<>nil)
   and (NodeToCopy.NodeName<>'') and (NodeToCopy.NodeType<>'') then
   begin
     ObjectInspectorSourceNode:=CopyNode(NodeToCopy,true);
     ObjectInspectorSourceCut:=false;
     //ShowMessage('CopyNavNode.  Source node is '+NodeToCopy.NodeName);
     XIDEForm.OIPaste.Hint:='Ready to paste UI node '+ObjectInspectorSourceNode.NodeName;
   end
   else ShowMessage('Select an item (from the Navigation Tree) to copy before calling this action');
end;

procedure PickItem( SelectedResourcesTreeNode:TDataNode);
begin
   // Copy an item from the Resources tree, ready for paste elsewhere
   if (SelectedResourcesTreeNode<>nil)
   and (SelectedResourcesTreeNode.NodeName<>'') and (SelectedResourcesTreeNode.NodeType<>'')
   and (SelectedResourcesTreeNode.NodeType<>'Demo') and (SelectedResourcesTreeNode.NodeType<>'Test') then
   begin
      ObjectInspectorSourceNode:=CopyNode(SelectedResourcesTreeNode,true);
      ObjectInspectorSourceCut:=false;
      XIDEForm.OIPaste.Hint:='Ready to paste Resource node '+ObjectInspectorSourceNode.NodeName;
   end;
//   else
//     ShowMessage('Select an item (from the Resource Tree) to copy before calling this action');
end;

procedure  SelectResourceTreeNode(TreeNodeText:string;CurrentNode:TDataNode);
begin
  //!!!! Should to operate this by node id, not text  :: HOWEVER, in the resource tree, node texts are all unique.
  //showmessage('SelectResourceTreeNode '+TreeNodeText);
  AvailableResourcesSelectedNode:=CurrentNode;
  PopulateResourceInspector(CurrentNode);
end;

procedure HandleResourcesTreeClickEvent(nodeId,TreeNodeId:String);
var CurrentNode :TDataNode;
begin
  CurrentNode:=FindDataNodeById(ResourcesNodeTree,TreeNodeId,'',true);
  //ShowMessage('HandleResourcesTreeClickEvent treenodeId='+TreeNodeId+' currentnode='+currentnode.NodeName);
  if CurrentNode=nil then
    showmessage('Cannot find node '+TreeNodeId+' in XObjectInsp.HandleResourcesTreeClickEvent')
  else
  begin
    SelectResourceTreeNode(TreeNodeId,CurrentNode);
    if AvailableResourcesSelectedNode.NodeClass<>'RSS' then
      PickItem(CurrentNode);
  end;

end;

procedure DeleteItemQuietly(InTree,SelectedNode:TDataNode);
var
   ParentNode:TDataNode;
begin
  ParentNode:=FindParentOfNode(InTree,SelectedNode);
     DeleteNode(ParentNode,SelectedNode);

     if InTree.NodeName=SystemRootName then
     begin
      // showmessage('reset selected node to '+ParentNode.NodeName);
       ObjectInspectorSelectedNavTreeNode:=ParentNode;
     end
     else if InTree.NodeName=CodeRootName then
     begin
       ObjectInspectorSelectedCodeTreeNode:=ParentNode;
       ObjectInspectorSelectedCodeNodeText:='';  //!!!! ??
       OISelectedCodeProcName:='';
     end;

end;


function DeleteItem(InTree,SelectedNode:TDataNode):Boolean;
var
  Deleted:Boolean;
begin
  Deleted:=false;
  if  (SelectedNode=nil) or (SelectedNode.NodeName='') then
    ShowMessage('Select an item on the Navigation Tree before calling this action')
  else
  begin
    if SelectedNode.IsDynamic then
    begin
      if (SelectedNode.NodeName=UIProjectRootName)
      or (SelectedNode.NodeName=CodeRootName)
      or (SelectedNode.NodeName=ResourceDataRootName)
      then
        ShowMessage('Cannot remove the root node')
      else
      begin
         DeleteItemQuietly(InTree,SelectedNode);

         if DesignMode then
         begin
           {$ifdef JScript}
           if not RunningDeployedRuntime then
           {$endif}
           SaveSystemData;

           if InTree.NodeName=SystemRootName then
           begin
              RebuildNavigatorTree;
              RebuildCodeTree;  // (to remove displayed event code items)
           end
           else if InTree.NodeName=CodeRootName then
           begin
              RebuildCodeTree;
           end;
         end;

         Deleted:=true;
      end;
    end
    else
      showmessage('Lazarus-designed components cannot be deleted here.  Only dynamically created components may be deleted at runtime');
  end;
  result:=Deleted;
end;

function CutItemQuietly(InTree,SelectedNode:TDataNode):TDataNode;
begin
  ObjectInspectorSourceNode:=CopyNode(SelectedNode,true);
  //ShowMessage('cutting '+ObjectInspectorSourceNode.NodeName);
  ObjectInspectorSourceCut:=true;
  DeleteItemQuietly(InTree,SelectedNode);
  //ShowMessage('finished cutting.  oi node is '+ObjectInspectorSourceNode.NodeName);
  XIDEForm.OIPaste.Hint:='Ready to paste UI node '+ObjectInspectorSourceNode.NodeName;
  result:=ObjectInspectorSourceNode;
end;

procedure CutItem(InTree,SelectedNode:TDataNode);
var
   myName:string;
   cutNode:TDataNode;
begin
  //ShowMessage('cutitem.  selectednode='+SelectedNode.NodeName);
  if  (SelectedNode=nil) or (SelectedNode.NodeName='') then
    ShowMessage('Select an item on the Navigation Tree before calling this action')
  else
  begin
    if SelectedNode.IsDynamic then
    begin
      if (SelectedNode.NodeName=UIProjectRootName)
      or (SelectedNode.NodeName=ResourceDataRootName)
      then
        ShowMessage('Cannot remove the root node')
      else
      begin
        myName:=SelectedNode.NodeName;
        cutNode:=CutItemQuietly(InTree,SelectedNode);

        if designMode then
        begin
          SaveSystemData;  // this clears ObjectInspectorSourceNode

          if InTree.NodeName=SystemRootName then
          begin
            RebuildNavigatorTree;
            RebuildCodeTree;
          end;
          ObjectInspectorSourceNode:=cutNode;
        end;
        ShowMessage('Node '+ myName +' ready to paste');

      end;
    end
    else
      showmessage('Lazarus-designed components cannot be deleted here.  Only dynamically created components may be deleted at runtime');

  end;
end;

procedure RenameChildNodes(MyNode:TDataNode;Prefix:string);
var
   i:integer;
begin
  for i:=0 to length(MyNode.ChildNodes)-1 do
  begin
    MyNode.ChildNodes[i].NodeName:=Prefix + MyNode.ChildNodes[i].NodeName;
    RenameChildNodes(MyNode.ChildNodes[i],Prefix);
  end;
end;

procedure PasteItemQuietly(InTree:TDataNode;pos:integer;ParentNode,SourceNode:TDataNode);
var
   TreePos:integer;
   glb:Boolean;
   SaveSelectedCodeTreeNode:TDataNode;
   SaveNodeText:String;
   procedure UnHighlight(TreeNode:TDataNode);
   begin
     {$ifndef JScript}
     if TreeNode.NodeClass='UI' then
       SetBooleanProperty(TreeNode.ScreenObject,'IsSelected',false)
     else
       SetBooleanProperty(TreeNode,'IsSelected',false);
     {$else}
     if IsPublishedProp(TreeNode,'IsSelected') then
       SetBoolProp(TreeNode,'IsSelected',false);
     {$endif}
   end;
begin
  SourceNode.ScreenObject:=nil;
  TreePos:=pos;
  {$ifndef JScript}
  glb:=  GlobalSuppressFrameDisplay;
  {$endif}

  if ObjectInspectorSourceCut=false then
    // this is a copy - have to rename all child nodes
    if SourceNode.NodeType <> 'TXComposite' then
      RenameChildNodes(SourceNode,SourceNode.NodeName);    //!!!! this will break any references in event code to these nodes

  if (InTree.NodeName=SystemRootName) then
  begin
    // un-highlight selected item in nav tree
    if ObjectInspectorSelectedNavTreeNode<>nil then
      UnHighlight(ObjectInspectorSelectedNavTreeNode);

    //.........Create the new node and associated screen object........................
    {$ifndef JScript}
    GlobalSuppressFrameDisplay:=false;
    {$endif}
    ObjectInspectorSelectedNavTreeNode:=InsertSystemNode(ParentNode,SourceNode,TreePos);
    {$ifndef JScript}
    GlobalSuppressFrameDisplay:=glb;
    {$endif}

    if DesignMode then
    begin
      SaveSystemData;
      // rebuild the system description tree
      RebuildNavigatorTree;
      RebuildCodeTree;
    end;
  end
  else if (InTree.NodeName=CodeRootName) then
  begin
    // un-highlight selected item in code tree
    if ObjectInspectorSelectedCodeTreeNode<>nil then
      UnHighlight(ObjectInspectorSelectedCodeTreeNode);
    //.........Create the new node ........................
    SaveNodeText:=ObjectInspectorSelectedCodeNodeText;
    SaveSelectedCodeTreeNode:=InsertSystemNode(ParentNode,SourceNode,TreePos);

    if DesignMode then
    begin
      SaveSystemData;
      ObjectInspectorSelectedCodeTreeNode:=SaveSelectedCodeTreeNode;
      ObjectInspectorSelectedCodeNodeText:=SaveNodeText;
      // rebuild the system description tree
      RebuildCodeTree;
    end;
  end;
end;

function CanAddChildToParent(ParentNode,SourceNode:TDataNode):Boolean;
begin
  result:=false;
 // showmessage('add '+SourceNode.NodeClass+' to '+ParentNode.NodeType);
  if (ParentNode.NodeType<>'TXSVGContainer') and (SourceNode.NodeClass = 'SVG') then
     ShowMessage('Can only paste SVG items to an SVGContainer.')
  else if (ParentNode.NodeType='TXSVGContainer') and (SourceNode.NodeClass <> 'SVG') then
     ShowMessage('Can only paste SVG items to an SVGContainer.')
  else if (ParentNode.NodeType='TXTabControl') and (SourceNode.NodeType<>'TXTabSheet') then
    ShowMessage('Only a TabSheet can be inserted under a TabControl element')
  else if (ParentNode.NodeType<>'TXTabControl') and (SourceNode.NodeType='TXTabSheet') then
    ShowMessage('A TabSheet can only be inserted under a TabControl element')
  else if (ParentNode.NodeType='TXMainMenu') and (SourceNode.NodeType<>'TXMenuItem') then
    ShowMessage('Only a MenuItem can be inserted under a MainMenu element')
  else if (ParentNode.NodeType<>'TXMainMenu') and (ParentNode.NodeType<>'TXMenuItem') and(SourceNode.NodeType='TXMenuItem') then
    ShowMessage('A MenuItem can only be inserted under a MainMenu or MenuItem element')
  else if (ParentNode.NodeType='TXForm') and (SourceNode.NodeType='TXMainMenu') and (ParentNode.NodeName=MainForm.Name) then
    ShowMessage('The Main Form already has a Main Menu')
  else if (ParentNode.NodeType<>'TXForm') and (SourceNode.NodeType='TXMainMenu') then
    ShowMessage('A MainMenu can only be inserted under a Form element')
  else if (SourceNode.NodeClass='NV') and (ParentNode<>UIRootNode) then
    ShowMessage(ParentNode.NodeType+' '+SourceNode.NodeClass+' '+'A non-visual component can only be inserted under the UI Root Node')
  else if (ParentNode.NodeClass='NV') then
    ShowMessage('A non-visual component cannot contain child nodes')
  else if (SourceNode.NodeType='TXForm') and (ParentNode<>UIRootNode) then
    ShowMessage('XForm can only be added to the UI Root Node')
  else if (SourceNode.NodeClass<>'NV') and (SourceNode.NodeType<>'TXForm') and (ParentNode=UIRootNode) then
    ShowMessage('Only XForm and Non-Visual items can be added to the UI Root Node - please select another container')
  else if (SourceNode.NodeClass='UI') and (ParentNode.NodeType='TXForm') and (ParentNode.NodeName=MainForm.Name) then
    ShowMessage('In the main form, UI items can only be added within the UI Root Node')
  else if (ParentNode.NodeClass='DM') and (ParentNode.NodeType='DMRoot') and (SourceNode.NodeType<>'DMClass') then
    ShowMessage('Only a DMClass can be inserted under a DMRoot element')
  else if (ParentNode.NodeClass='DM') and (ParentNode.NodeType<>'DMClass') and (SourceNode.NodeType='DMContains') then
    ShowMessage('A DMContains can only be inserted under a DMClass element')
  else if (ParentNode.NodeClass='DM') and (ParentNode.NodeType<>'DMClass') and (SourceNode.NodeType='DMOp') then
    ShowMessage('A DMOp can only be inserted under a DMClass element')
  else if (ParentNode.NodeClass='DM') and (ParentNode.NodeType<>'DMClass') and (SourceNode.NodeType='DMRef') then
    ShowMessage('A DMRef can only be inserted under a DMClass element')
  else
    // all ok - go ahead and paste
    result:=true;

end;

function OINavTreeAllowDrop(DstNode:TDataNode):Boolean;
var
  SrcNode:TDataNode;
  ok:Boolean;
begin
  ok:=true;
  SrcNode:=ObjectInspectorSourceNode;
      // cannot drop to Root Node, except for forms and non-visuals.
      if (DstNode.NodeClass='Root')
      and (SrcNode.NodeType <> 'TXForm')
      and (SrcNode.NodeClass <> 'NV')
      and (SrcNode.NodeClass <> 'RNV') then
        ok:=false;
      // on main form, cannot drop to Form node
      if (DstNode.NodeType='TXForm')
      and (DstNode.NodeName=MainForm.Name)
      and (SrcNode.NodeType <> 'TXMainMenu')
      and (SrcNode.NodeClass <> 'NV')
      and (SrcNode.NodeClass <> 'RNV') then
        ok:=false;
      // have to drop txform onto the UI Root node
      if (DstNode<>UIRootNode)
      and (SrcNode.NodeType='TXForm') then
        ok:=false;
      // cannot drop non-menu items into a menu
      if ((DstNode.NodeType = 'TXMainMenu')
      or (DstNode.NodeType = 'TXMenuItem'))
      and (SrcNode.NodeType <> 'TXMenuItem') then
        ok:=false;
      // cannot drop menu items into a non-menu
      if (DstNode.NodeType <> 'TXMainMenu')
      and (DstNode.NodeType <> 'TXMenuItem')
      and (SrcNode.NodeType = 'TXMenuItem') then
        ok:=false;
      // main menu can only drop to a form item
      if (DstNode.NodeType <> 'TXForm')
      and (SrcNode.NodeType = 'TXMainMenu') then
        ok:=false;
      // main form already has a main menu
      if (DstNode.NodeType = 'TXForm')
      and (SrcNode.NodeType = 'TXMainMenu')
      and (DstNode.NodeName=MainForm.Name) then
        ok:=false;
      // cannot drop tab page except into a tab control
      if (DstNode.NodeType<>'TXTabControl')
      and (SrcNode.NodeType='TXTabSheet') then
        ok:=false;
      if (DstNode.NodeType='TXTabControl')
      and (SrcNode.NodeType<>'TXTabSheet') then
        ok:=false;
      // can only drop non-visuals onto the UI Root node
      if (DstNode<>UIRootNode)
      and ((SrcNode.NodeClass = 'NV')
        or (SrcNode.NodeClass = 'RNV')) then
        ok:=false;
      // can only drop SVG items into an SVG Container
      if (DstNode.NodeType<>'TXSVGContainer') and (DstNode.NodeClass<>'SVG')
      and ((SrcNode.NodeClass = 'SVG') or (SrcNode.NodeClass = 'RSVG')) then
         ok:=false;
      if ((DstNode.NodeType='TXSVGContainer') or (DstNode.NodeClass='SVG'))
      and (SrcNode.NodeClass <> 'SVG') and (SrcNode.NodeClass <> 'RSVG') then
         ok:=false;
  result:=ok;
end;

procedure SetSystemName(NewName:String);
begin
  UIRootNode.SetAttributeValue('SystemName',NewName);
  {$ifdef JScript}
  asm
    document.title=NewName;
  end;
  {$endif}
end;

procedure LoadNamedSystem(sysname:string);
var
  SystemDescription,filename:String;
begin
//   basename:=myStringReplace(sysname,'_xide','',1,-1);
   filename:=sysname+'.xide';
   {$ifndef JScript}
   // on Desktop, saved systems are in this sub-folder...
   filename:='SavedSystems/'+filename;
   {$endif}
   SystemDescription:=ReadFromLocalStore(filename);

   DoSystemLoad(SystemDescription,sysname);
   if UIRootNode.GetAttribute('SystemName',true).AttribValue='XIDESystem' then
     SetSystemName(sysname);
end;

procedure LoadCompositeResource(CompNode:TdataNode);
var
  SystemDescription:String;
begin
   SystemDescription:=CompNode.GetAttribute('SourceString',false).AttribValue;

   DoSystemLoad(SystemDescription,CompNode.NodeName);

end;

function PasteItem(NavTreeDestinationNode:TDataNode;OrigSourceNode:TDataNode;NewName:String):boolean;
var
   SourceNode,ParentNode, NewNode:TDataNode;
   TreePos:integer;
   ok,TargetIsContainer:Boolean;
   CompositeResource:Boolean;
   InterfaceNodes:TNodesArray;
   i,j:integer;
   evtyp:String;
   evh:TeventHandlerRec;
begin
  ok:=false;
  if  OrigSourceNode.NodeClass='RSS' then
    EXIT;
  // make a copy of the source node (as we may make changes here)
  SourceNode:=CopyNode(OrigSourceNode,true);

  CompositeResource:=false;
  //showmessage('PasteItem...source='+SourceNode.Nodename+' dest='+NavTreeDestinationNode.NodeName);
  //showmessage('PasteItem...dest='+NavTreeDestinationNode.Nodename+' type='+NavTreeDestinationNode.NodeType+' class='+NavTreeDestinationNode.NodeClass);
  if (NavTreeDestinationNode=nil) or (SourceNode=nil)
  or (NavTreeDestinationNode.NodeName='') or (SourceNode.NodeName='') then
  begin
      if (NavTreeDestinationNode=nil) or (NavTreeDestinationNode.NodeName='') then
        ShowMessage('Select a destination to paste to before calling this action') ;
      if (SourceNode=nil) or (SourceNode.NodeName='') then
        ShowMessage('Select an item to copy before calling this action');
  end
  else
  begin
    //ShowMessage('PasteItem.  Source='+SourceNode.NodeName+' Dest='+NavTreeDestinationNode.NodeName);
    if (SourceNode.NodeClass='RUI') then
      if (SourceNode.NodeType='TXComposite') then
        CompositeResource:=true
      else
        setlength(SourceNode.NodeAttributes,0);  // removes the Hint attribute (from the resource tree node)

    if SourceNode.NodeClass='RUI' then SourceNode.NodeClass:='UI';
    if SourceNode.NodeClass='RNV' then SourceNode.NodeClass:='NV';
    if SourceNode.NodeClass='RSVG' then SourceNode.NodeClass:='SVG';

    if (SourceNode.NodeClass<>'UI')
    and (SourceNode.NodeClass<>'NV')
    and (SourceNode.NodeClass<>'SVG')
    then
    begin
       ShowMessage('Can only add UI, NV and SVG items to the Navigator tree');
       EXIT;
    end;

    // Is the destination node a container (paste new child) or not (paste sibling)
    if (NavTreeDestinationNode = UIRootNode)
    or (NavTreeDestinationNode.NodeType='TXForm') then
      TargetIsContainer:=true
    else if (NavTreeDestinationNode.NodeType='TXTabSheet')
    and (SourceNode.NodeType='TXTabSheet') then
      TargetIsContainer:=false
    else
    begin
      if (NavTreeDestinationNode.ScreenObject = nil)                    //eg. is nil for SVG internal widgets
      {$ifndef JScript}
      or (NavTreeDestinationNode.ScreenObject = NavTreeDestinationNode)
      {$endif}
      then
        TargetIsContainer:=false
      else
      begin
        {$ifndef JScript}
        TargetIsContainer:=GetBooleanProperty(TComponent(NavTreeDestinationNode.ScreenObject),'isContainer');
        {$else}
        TargetIsContainer:=GetBoolProp(NavTreeDestinationNode.ScreenObject,'isContainer');
      {$endif}
      end;
    end;

    if TargetIsContainer = true then
    begin
      //ShowMessage('paste under container');
      ParentNode:=NavTreeDestinationNode;
      TreePos:=-1;
    end
    else
    begin
      // ShowMessage('paste as sibling');
      //ParentNode:=FindParentOfNode(SystemNodeTree,NavTreeDestinationNode);
      ParentNode:=NavTreeDestinationNode.NodeParent;
      TreePos:=ParentNode.GetChildIndex(NavTreeDestinationNode);
    end;

   //showmessage('PasteItem. parent='+ParentNode.Nodename+' type='+ParentNode.NodeType+' class='+ParentNode.NodeClass);

    if CanAddChildToParent(ParentNode,SourceNode) then
    begin
       if NewName='' then
       begin
         if ObjectInspectorSourceCut=false then
         begin
             // Dialog for name entry
             NewName:=GetComponentName('Enter Component Name:');
         end
         else
         begin
            NewName:=SourceNode.NodeName;
         end;
       end;

       // Is the source node named uniquely?
       //showmessage('check unique name '+NewName);
       if (NewName<>'') and (ComponentNameIsUnique(NewName,OrigSourceNode.NameSpace)) then
       begin
         SourceNode.NodeName:=NewName;
         PasteItemQuietly(UIRootNode,TreePos,ParentNode,SourceNode);
         ok:=true;

          if CompositeResource then
          begin
            NewNode:=FindDataNodeById(ParentNode,NewName,OrigSourceNode.NameSpace,true);
            // next, expand the composite node according to its SourceString
            XMLToNodeTree(SourceNode.GetAttribute('SourceString',true).AttribValue, NewNode, true);
            // and reset the instantiated SourceString to blank
            NewNode.SetAttributeValue('SourceString','','String',true);
            // From within the expanded composite, find interface node(s?) and add the defined properties and events
            InterfaceNodes:=FindNodesOfType(NewNode,'TXCompositeIntf',true,NewNode.NodeName);
            for i:=0 to length(InterfaceNodes)-1 do     //!! only allow one of these
            begin
              for j:=0 to length(InterfaceNodes[i].NodeAttributes)-1 do
                // copying interface attribute here so that it is accessible from main system,
                // but if the property is not "Input" to the composite, then the value will not be editable in design mode.
                NewNode.AddAttribute(InterfaceNodes[i].NodeAttributes[j].AttribName,'String',
                                     InterfaceNodes[i].NodeAttributes[j].AttribValue,
                                     (not InterfaceNodes[i].NodeAttributes[j].AttribReadOnly));
              for j:=0 to InterfaceNodes[i].myEventTypes.Count-1 do
              begin
                evtyp := InterfaceNodes[i].myEventTypes[j];
                // copying event type and code here so that it is accessible from main system,
                // (ie. using a DoEvent call), but if the interface event is 'readonly', then
                // the code in the event will not be editable
                // in design mode, as it is defined within the encapsulated composite element.
                evh := InterfaceNodes[i].GetEvent(evtyp);
                NewNode.AddEvent(evtyp,
                                 evh.TheCode,
                                 evh.InitCode,
                                 evh.ReadOnlyInterface,
                                 evh.EventHint
                                 );
              end;
            end;
          end;
       end;
    end;

  end;
  result:=ok;
end;

procedure ShowHideNode(aNode:TDataNode;show:Boolean);
begin
   if aNode<>nil then
     if aNode.NodeClass='UI' then
     begin
         {$ifndef JScript}
         SetBooleanProperty(aNode.ScreenObject,'IsVisible',show);
         {$else}
         if IsPublishedProp(aNode,'IsVisible') then
         begin
           SetBoolProp(aNode,'IsVisible',show);
         end;
         {$endif}
     end;
end;

procedure ClearResourceInspector;
var
  btnNode:TDataNode;
begin
  AvailableResourcesSelectedNode:=nil;
  PopulateResourceInspector(nil);
  btnNode:=FindDataNodeById(systemnodetree,'ResourceTreeDelBtn','',true);
  TXButton(btnNode.ScreenObject).Enabled:=false;
  btnNode:=FindDataNodeById(systemnodetree,'ResourceTreeLoadBtn','',true);
  TXButton(btnNode.ScreenObject).Enabled:=false;
end;


procedure ClearInspectors;
begin
  if NavTreeComponent.ScreenObject <>nil then
  begin
    ObjectInspectorSourceNode:=nil;
    XIDEForm.OIPaste.Hint:='Nothing to paste. Select a Resource, or Copy/Cut a Designer node first.';

    TXTree(NavTreeComponent.ScreenObject).DeSelectNode;
    DeHighlightObject(ObjectInspectorSelectedNavTreeNode);
    PopulateObjectInspector(nil);
    ClearResourceInspector;
    ObjectInspectorSelectedNavTreeNode:=nil;
    DMClearSelection;

    TXTree(CodeTreeComponent.ScreenObject).DeSelectNode;
    ObjectInspectorSelectedCodeTreeNode:=nil;
    ObjectInspectorSelectedCodeNodeText:='';
    OISelectedCodeProcName:='';

    if CodeEditform<>nil then
    begin
      CodeEditForm.CodeEdit.ItemValue:='';
      CodeEditForm.CodeEditInit.ItemValue:='';
    end;
  end;
end;

function CompositeResourcesString(QuotedString:Boolean):String;
var
  StartNode:TdataNode;
  systemstring:String;
begin
  StartNode:=FindDataNodeById(ResourcesNodeTree,'Composites','',true);
  systemstring:=NodeTreeToXML(StartNode,MainFormProjectRoot,false,QuotedString);
  result:= systemstring;

end;
{$ifndef JScript}
procedure SaveCompositesToIncFile;
var
  systemstring:string;

begin
  systemstring:= CompositeResourcesString(true);

  WriteToFile(ProjectDirectory+'tempinc/systemcomposites.inc','XObjectInsp.CompositesString := '''+systemstring+''';');
end;
{$endif}
procedure DebugWriteNodeTree(StartNode,ParentNode:TdataNode;var txt:String;lvl:integer);
var
  i:integer;
begin
  txt:=txt+LineEnding;
  for i:=0 to lvl-1 do
    txt:=txt+'  ';
  if ParentNode<>nil then
    txt:=txt+StartNode.NodeClass+' '+ StartNode.NodeType+' '+StartNode.NodeName+' '+myBoolToStr(StartNode.IsDynamic)+' '+StartNode.NodeParent.NodeName+' '+ParentNode.NodeName
  else
    txt:=txt+StartNode.NodeClass+' '+ StartNode.NodeType+' '+StartNode.NodeName+' '+myBoolToStr(StartNode.IsDynamic);
  for i:=0 to length(StartNode.ChildNodes)-1 do
    DebugWriteNodeTree(StartNode.ChildNodes[i],StartNode,txt,lvl+1);
end;
procedure DumpFullNodeTree;
var
  txt:String;
begin
  DebugwriteNodetree(SystemnodeTree,nil,txt,0);
  {$ifndef JScript}
  WriteToLocalStore('debugtree',txt);
  {$else}
  myCopyToClip('debugtree',txt );
  {$endif}
end;

function BuildSystemString(Encapsulate:Boolean):String;
var
  systemstring,fullstring:string;
  StartNode,UINode, MenuNode,StyleTreeParent:TDataNode;
  i:integer;
  TopType,TopClass:String;
begin
  //showmessage('BuildSystemString');
  ClearInspectors;
  // Save just the user-design portions of the system node tree.
  // Mainform Menu items, Mainform centre section, PLUS dynamic XForms added.

  UINode:=MainFormProjectRoot;
  TopType:=UINode.NodeType;
  TopClass:=UINode.NodeClass;
  UINode.NodeType:='Root';    // so that this item will be skipped on load
  UINode.NodeClass:='Root';    // so that this item will be skipped on load
  UINode.IsDynamic:=true;

  MenuNode:=FindDataNodeById(UIRootNode,'XIDEMainMenu','',true);

  // Create a temporary root node to enclose the project nodes (copy from UIRootNode)
  StartNode:= CopyNode(UIRootNode,false);
  StartNode.NodeType:='Root';    // so that the top item will be skipped on load
  StartNode.NodeClass:='Root';    // so that the top item will be skipped on load
  StartNode.IsDynamic:=true;
  if not Encapsulate then
  begin
    // normal system...
    setlength(StartNode.ChildNodes,2);
    StartNode.ChildNodes[0]:=MenuNode;        // give it one child node for the Mainform menu items
    StartNode.ChildNodes[1]:=UINode;        // give it one child node for the user interface section
  end
  else
  begin
    // encapsulation...
    setlength(StartNode.ChildNodes,1);
    StartNode.ChildNodes[0]:=UINode;        // give it one child node for the user interface section
  end;

  systemstring:= NodeTreeToXML(StartNode,nil,true,false);

  UINode.NodeType:=TopType;
  UINode.NodeClass:=TopClass;
  UINode.IsDynamic:=false;

  // add non-visual components
  StartNode:=FindDataNodeById(SystemNodeTree,SystemRootName,'',true);
  for i:=0 to length(StartNode.ChildNodes)-1 do
  begin
    if (StartNode.ChildNodes[i].NodeClass='NV')
    and (StartNode.ChildNodes[i].IsDynamic) then
    begin
      systemstring:=systemstring+NodeTreeToXML(StartNode.ChildNodes[i],StartNode,true,false);
    end;
  end;

//  showmessage('6');
  // now look for other XForms
  StartNode:=FindDataNodeById(SystemNodeTree,SystemRootName,'',true);
  for i:=0 to length(StartNode.ChildNodes)-1 do
  begin
    if (StartNode.ChildNodes[i].NodeType='TXForm')
    and (StartNode.NodeName<>'CodeEditForm')
    and (StartNode.NodeName<>'PasteDialog')
    and (StartNode.ChildNodes[i].IsDynamic)
    then
    begin
      systemstring:=systemstring+NodeTreeToXML(StartNode.ChildNodes[i],nil,true,false);
    end;
  end;

//  showmessage('7');
  // and add any functions and raw code units
  StartNode:=FindDataNodeById(SystemNodeTree,CodeRootName,'',true);
  if StartNode=nil then
  begin
    showmessage('oops. cannot find node CodeUnits in BuildSystemString');
    EXIT;
  end;
  for i:=0 to length(StartNode.ChildNodes)-1 do
  begin
    if (StartNode.ChildNodes[i].NodeType='PasUnit')
    or (StartNode.ChildNodes[i].NodeType='PythonScript')
    and (StartNode.ChildNodes[i].IsDynamic) then
    begin
      systemstring:=systemstring+NodeTreeToXML(StartNode.ChildNodes[i],CodeRootNode,true,false);
    end;
  end;

  // and add any data model elements
  StartNode:=DMRoot;
  if StartNode=nil then
  begin
    showmessage('oops. cannot find node DMRoot in BuildSystemString');
    EXIT;
  end;
  for i:=0 to length(StartNode.ChildNodes)-1 do
  begin
    systemstring:=systemstring+NodeTreeToXML(StartNode.ChildNodes[i],DMRoot,true,false);
  end;

  if not Encapsulate then
  begin
    // add the tree 'StyleSheet' so that its data is preserved with the user's project
    StyleTreeParent:=FindDataNodeById(systemnodetree,'StyleDesigner','',true);
    systemstring:=systemstring+NodeTreeToXML(StylesNode,StyleTreeParent,false,false);
  end;

  fullstring:= systemstring;

  result:=fullstring;
end;

Procedure SaveSystemToClip;
var
  fullstring:String;
begin
  fullstring:=BuildSystemString(false);
  myCopyToClip('System',fullstring );
end;

Procedure SaveSystemToFile;
var
  fullstring,oldname,sysname,storename:String;
begin
  oldname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  sysname:=GetValidItemName('Enter System Name',oldname);
  if sysname='' then
    EXIT;

  SetSystemName(sysname);
  storename:=sysname+'.xide';
  {$ifndef JScript}
  storename:='SavedSystems/'+storename;
  {$endif}
  fullstring:=BuildSystemString(false);
  WriteToLocalStore(storename,fullstring);
  RebuildResourcesTree;

  if sysname<>oldname then
  begin
    // copy any existing stored datasets
    // (desktop)...create new directory, and copy contents
    // (browser)...copy local database to new name (NB. Async function)
    DBSaveAs(oldname,sysname);
  end;
end;

function isValidSystemData(SystemDescription:string):boolean;
var teststring,sys:string;
   i:integer;
   MatchFound:boolean;
begin
  MatchFound:=true;
  sys:=trim(SystemDescription);
  teststring:='<Root|; Class |=Root|; Name |=UIRoot';

  if length(SystemDescription)>=length(teststring) then
  begin
    for i :=1 to Length(teststring) do
    begin
       if (sys[i]<> teststring[i])
       then  MatchFound:=false;
    end;
  end;
  result:=MatchFound;
end;

procedure CheckEventCode(StartNode:TDataNode);
var
  i:integer;
  tmp:string;
begin
  if (StartNode.IsDynamic) or (StartNode=UIRootNode) then
    for i:=0 to length(StartNode.myEventHandlers)-1 do
    begin
      tmp:=StartNode.myEventHandlers[i].TheCode;
      if (trim(tmp)<>'')  then
      begin
         tmp:=trim(StartNode.myEventHandlers[i].InitCode);
         if not PythonCodeExists then
           PythonCodeExists := (FoundStringCI(tmp,'RunPython(')>0);

         tmp:=StartNode.myEventHandlers[i].TheCode;
         if not PythonCodeExists then
           PythonCodeExists := (FoundStringCI(tmp,'RunPython(')>0);

      end;
    end;
  if not PythonCodeExists then
    for i:=0 to length(StartNode.ChildNodes)-1 do
      CheckEventCode(StartNode.ChildNodes[i]);

end;
procedure CheckForPythonCode;
var
  i:integer;
begin
  PythonCodeExists:=false;
  for i:=0 to length(CodeRootNode.ChildNodes)-1 do
  begin
    if CodeRootNode.ChildNodes[i].NodeType='PythonScript' then
    begin
       PythonCodeExists:=true;
    end;
  end;
  if not PythonCodeExists then
  begin
    // also check ALL Pascal event code for 'RunPython' calls...
    PythonCodeExists:=false;
    CheckEventCode(SystemNodeTree);
  end;

end;

{$ifndef Python}

procedure CheckForPythonCode2;
begin
  CheckForPythonCode;
  if PythonCodeExists then
    showmessage('Warning: The loaded system contains Python code.  These cannot be executed unless the XIDE framework is built with the ''Python'' option');
end;
{$endif}

{$ifndef JScript}
procedure TLoadTimerTag.DoXMLToNodeTree(sender:TObject);
var
   myTimer:tTimer;
   myTag:TLoadTimerTag;
   glb:Boolean;
   i:integer;
begin
  glb:=GlobalSuppressFrameDisplay;
  GlobalSuppressFrameDisplay:=true;

  OIClearSystem;
  ClearAllComposites;

  myTimer:=TTimer(sender);
  myTimer.Enabled:=false;
  myTag:=TLoadTimerTag(myTimer.Tag);
  XMLToNodeTree(myTag.systemstring,UIRootNode);

  {$ifdef JScript}
  i:=length(DMRoot.ChildNodes);
  asm console.log('DoXMLToNodeTree 1. DMRoot numchildnodes=',i); end;
  {$endif}

  if myTag.SysName<>'' then
  begin
    SetSystemName(myTag.SysName);
  end;

  RebuildResourcesTree;
  RedisplayResourceTree;
{$ifdef JScript}
i:=length(DMRoot.ChildNodes);
asm console.log('DoXMLToNodeTree 2. DMRoot numchildnodes=',i); end;
{$endif}

  RebuildNavigatorTree;
{$ifdef JScript}
i:=length(DMRoot.ChildNodes);
asm console.log('DoXMLToNodeTree 3. DMRoot numchildnodes=',i); end;
{$endif}
  RebuildCodeTree;
{$ifdef JScript}
i:=length(DMRoot.ChildNodes);
asm console.log('DoXMLToNodeTree 4. DMRoot numchildnodes=',i); end;
{$endif}
  RebuildDMTree;
  SelectNavTreeNode(MainFormProjectRoot,true);

  //make sure UIRoot width attribute is still at 60% (design mode)
  //if ShowResourceTree<>'Hide' then
    MainFormProjectRoot.SetAttributeValue('ContainerWidth','60%');
  //else
  //  MainFormProjectRoot.SetAttributeValue('ContainerWidth','80%');

  {$ifndef Python}
  CheckForPythonCode2;
  {$endif}

  GlobalSuppressFrameDisplay:=glb;

  sender.Destroy;
end;

function DoSystemLoad(SystemDescription,SysName:string):Boolean;
var
  tempTimer:TTimer;
  myTag:TLoadTimerTag;
  ok:boolean;
begin
   if (isValidSystemData(SystemDescription)=true)
   then
   begin
     //
     // throw a timer to avoid event contention while we are deleting objects
     tempTimer:=TTimer.Create(MainForm);
     tempTimer.Enabled:=false;
     tempTimer.Interval:=100;

     myTag:=TLoadTimerTag.Create;
     myTag.systemstring:=SystemDescription;
     myTag.SysName:=SysName;
     tempTimer.Tag:=WinSizeDependentInt(myTag);

     tempTimer.OnTimer:=@mytag.DoXMLToNodeTree;
     tempTimer.Enabled:=true;
   end
   else
   begin
     ok:=false;
     ShowMessage('Error.....Put a valid system description string on the clipboard before calling this option ');
   end;
   result:=ok;
end;
{$else}
function DoSystemLoad(SystemDescription,SysName:string):boolean;
var
  ok:Boolean;
begin
   ok:=true;
   if (isValidSystemData(SystemDescription)=true)
   then
   begin
     StartingUp:=true;    //suppress event handling
     OIClearSystem;
     ClearAllComposites;
    asm
    try {
    //alert('default parent is '+pas.NodeUtils.UIRootNode);
    pas.NodeUtils.XMLToNodeTree(SystemDescription,pas.NodeUtils.UIRootNode);
    } catch(err) { ok=false; alert(err.message+'  in XObjectInsp.DoSystemLoad ');}
    end;

    if SysName<>'' then
      SetSystemName(SysName);

    UnSuspendFrames(SystemNodeTree);

    RebuildResourcesTree;
    RedisplayResourceTree;

    if not RunningDeployedRuntime then
      SaveSystemData;

    RebuildNavigatorTree;
    RebuildCodeTree;

    GenerateStyleSheets;
    //InitAutomatedCursor;

    SelectNavTreeNode(MainFormProjectRoot,true);
    {$ifndef Python}
    CheckForPythonCode2;
    {$endif}

   end
   else
   begin
     ok:=false;
     ShowMessage('Error.....system description string is not valid ');
   end;
   StartingUp:=false;
   result:=ok;
end;
{$endif}

procedure OINavTreeNodeChange(e:TEventStatus;nodeId,NameSpace:string;myValue:string);
var
  NodeText,TreeNodeId,p1:String;
  myTree:TXTree;
begin
  {$ifndef JScript}
  myTree:=TXTree(NavTreeComponent.ScreenObject);
  {$else}
  myTree:=TXTree(NavTreeComponent);
  {$endif}
  NodeText:=myTree.SelectedNodeText;
  TreeNodeId:=TreeLabelToID(NodeText,'NavTree',p1);

  //NodeID is the Navigator Ttree object clicked on    eg. NavigationTreeContents   or in html, the name of the node...
  //myValue is  the label text of the clicked node (desktop) OR the treenode id (Browser)
  //TreeNodeId is  the associated datanode name (from the treenode text)
  if TreeNodeId<>'' then
  begin
    //showmessage('OINavTreeNodeChange.  Id='+TreeNodeId);
     HandleNavTreeClickEvent(TreeNodeId,nodeText);
  end;
end;

procedure OIResourceTreeNodeChange(nodeId:string;myValue:string);
var
  NodeText,TreeNodeId,p1:String;
  myTree:TXTree;
begin
  {$ifndef JScript}
  myTree:=TXTree(ResourceTreeComponent.ScreenObject);
  NodeText:=myTree.SelectedNodeText;
  {$else}
  myTree:=TXTree(ResourceTreeComponent);
  NodeText:=myTree.SelectedNodeText;
  {$endif}
  TreeNodeId:=TreeLabelToID(NodeText,'ResourceTree',p1);

  //NodeID is the Navigator Ttree object clicked on    eg. NavigationTreeContents   or in html, the name of the node...
  //myValue is  the label text of the clicked node (desktop) OR the treenode id (Browser)
  //TreeNodeId is  the screen object name (from the name within myvalue)
  if TreeNodeId<>'' then
  begin
     // ShowMessage('OIResourceTreeNodeChange nodeId='+nodeId);
      HandleResourcesTreeClickEvent(nodeId,NodeText);
  end;
end;

procedure OICodeTreeNodeChange(nodeId:string;myValue:string);
var
  NodeText,TreeNodeId,FirstBit:String;
  myTree:TXTree;
begin
  {$ifndef JScript}
  myTree:=TXTree(CodeTreeComponent.ScreenObject);
  NodeText:=myTree.SelectedNodeText;
  {$else}
  myTree:=TXTree(CodeTreeComponent);
  NodeText:=myTree.SelectedNodeText;
  {$endif}
  TreeNodeId:=TreeLabelToID(NodeText,'CodeTree',FirstBit);

  if TreeNodeId<>'' then
  begin
    //showmessage('OICodeTreeNodeChange. nodeId='+nodeId+' TreeNodeId='+TreeNodeId);
     HandleCodeTreeClickEvent(TreeNodeId,NodeText,FirstBit);
  end;
end;

procedure OICutItem(nodeId:string;myValue:string);
begin
  if TreeInFocus<>nil then
  begin
    // if we are looking at the nav tree, cut the selected nav node;
    // if we are looking at the code tree, cut the selected code node;
    if TreeInFocus.NodeName=SystemRootName then
      CutItem(UIRootNode,ObjectInspectorSelectedNavTreeNode);
  end;
end;

function OIPasteItem(nodeId:string;myValue:string):boolean;
// if called via drag/drop, myvalue is the text of the destination node
// otherwise, from Paste button, destination node is ObjectInspectorSelectedNavTreeNode.
var dst,p1:string;
  DestNode:TDataNode;
  ok:boolean;
begin
  ok:=false;
//showmessage('OIPasteItem. '+nodeId);
  if (TreeInFocus<>nil) and (ObjectInspectorSourceNode<>nil) then
  begin
    if myValue='OIPaste' then
        DestNode:=ObjectInspectorSelectedNavTreeNode
    else
    begin
        dst:=TreeLabelToID( myValue, TreeInFocus.NodeName,p1);  // destination node
        DestNode:=FindDataNodeById(UIRootNode,dst,'',true);      //!!namespace - assuming top design level only
    end;
//showmessage('dest node '+DestNode.NodeName);
    if TreeInFocus.NodeName=SystemRootName then
    begin
      ok:=PasteItem(DestNode,ObjectInspectorSourceNode,'');
    end;
  end
  else
    if (TreeInFocus=nil) then
       ShowMessage('Select Paste destination first')
    else if ObjectInspectorSourceNode=nil then
    begin
      ShowMessage('Copy an item first');
      XIDEForm.OIPaste.Hint:='Nothing to paste. Select a Resource, or Copy/Cut a Designer node first.';
    end;
  result:=ok;
end;

procedure OIDropItem(e:TEventStatus;nodeId:string;myValue:string);
var
  TreeNodeId,p1:string;
  OriginalSource, OriginalParent:TDataNode;
  values:TNodeEventValue;
  OriginalPos:integer;
  ok,ItemWasCut:boolean;
begin
  // Drop an item on the Navigator tree
  //showmessage('OIDropItem.  myValue='+myValue);

  TreeNodeId:=TreeLabelToID( myValue, 'NavTree',p1);  // destination node

  TreeInFocus:=UIRootNode;
  DeHighlightObject(ObjectInspectorSelectedNavTreeNode);
  ObjectInspectorSelectedNavTreeNode:=FindDataNodeById(UIRootNode,TreeNodeId,'',true);
  //ShowMessage('drop '+ObjectInspectorSourceNode.NodeName+' at '+ObjectInspectorSelectedNavTreeNode.NodeName);

  // if an intra-tree drag/drop, then cut the source node first
  // find the original source node (still in the nav tree)
  OriginalSource:=FindDataNodeById(SystemNodeTree,ObjectInspectorSourceNode.NodeName,'',false);
  if (OriginalSource<>nil) then
  begin
    OriginalParent:=OriginalSource.NodeParent;
    OriginalPos:=OriginalParent.GetChildIndex(OriginalSource);
  end;
  if (OriginalSource<>nil)
  and (OriginalSource <> ObjectInspectorSelectedNavTreeNode)
  and (OriginalSource.IsDynamic=true)
  and (NodeIsDescendantOf(OriginalSource,UIRootNode.NodeName) > -1)   // is in Nav tree
  then
  begin
    CutItemQuietly(UIRootNode,OriginalSource);
    ItemWasCut:=true;
  end;

  // paste the source node into the Nav tree
  if (OriginalSource <> ObjectInspectorSelectedNavTreeNode) then
    ok:=OIPasteItem(nodeId,myValue);
  if not ok then
    if ItemWasCut then
      // reverse the cut
      PasteItemQuietly(UIRootNode,OriginalPos,OriginalParent,OriginalSource);
end;

procedure OIMoveNavSiblingUpDown(UpDown:String);
var
  thisNode, myParent:TdataNode;
  c,n,i:integer;
begin
  thisNode:=ObjectInspectorSelectedNavTreeNode;
  if thisNode<>nil then
  begin
    //myParent:=FindParentOfNode(SystemNodeTree,thisNode);
    myParent:=thisNode.NodeParent;
    if myParent<>nil then
    begin
      for i:=0 to length(myParent.ChildNodes)-1 do
      begin
        if myParent.ChildNodes[i]=thisNode then
          c:=i;
      end;
      if UpDown='Up' then
        if c>0 then n:=c-1
        else n:=-1;
      if UpDown='Down' then
        if c<length(myParent.ChildNodes)-1 then
          n:=c+1
        else
          n:=-1;
      if n>-1 then
      begin
        CopyNavNode( thisNode);    // populates  ObjectInspectorSourceNode
        CutItemQuietly(UIRootNode,thisNode);
        // paste the source node back under the parent at new position
        PasteItemQuietly(UIRootNode,n,myParent,ObjectInspectorSourceNode);
      end;
    end;
    RebuildNavigatorTree;
  end;
end;

procedure CodeTreeMoveSiblingUpDown(UpDown:String);
var
  thisNode, myParent:TdataNode;
  c,n,i:integer;
  NodeText:String;
begin
  thisNode:=ObjectInspectorSelectedCodeTreeNode;
  NodeText:=ObjectInspectorSelectedCodeNodeText;
  if thisNode<>nil then
  begin
    myParent:=thisNode.NodeParent;
    if (myParent<>nil)
    and (myParent.NodeName = 'CodeUnits') then
    begin
      for i:=0 to length(myParent.ChildNodes)-1 do
      begin
        if myParent.ChildNodes[i]=thisNode then
          c:=i;
      end;
      if UpDown='Up' then
        if c>0 then n:=c-1
        else n:=-1;
      if UpDown='Down' then
        if c<length(myParent.ChildNodes)-1 then
          n:=c+1
        else
          n:=-1;
      if n>-1 then
      begin
        CopyNavNode( thisNode);    // populates  ObjectInspectorSourceNode
        CutItemQuietly(CodeRootNode,thisNode);
        // paste the source node back under the parent at new position
        PasteItemQuietly(CodeRootNode,n,myParent,ObjectInspectorSourceNode);
      end;
    end;
    ObjectInspectorSelectedCodeNodeText:=NodeText;
    RebuildCodeTree;
  end;
end;


procedure OIMoveItem(nodeId,NameSpace:string;NewParentId:string);
// Interface function (available to user code block) to re-parent a node in the nav tree.
var
  OriginalSource, DestNode:TDataNode;
begin
  // find the source node
  OriginalSource:=FindDataNodeById(UIRootNode,NodeId,NameSpace,false);
  if (OriginalSource<>nil) then
  begin

    TreeInFocus:=UIRootNode;
    // find the new parent node
    DestNode:=FindDataNodeById(UIRootNode,NewParentId,NameSpace,true);
    if DestNode<>nil then
    begin
      //ShowMessage('OIMoveItem '+ObjectInspectorSourceNode.NodeName+' at '+ObjectInspectorSelectedNavTreeNode.NodeName);

      if (OriginalSource <> DestNode)
      and (DestNode.IsDynamic=true)
      and (CanAddChildToParent(DestNode,OriginalSource)) then
      begin
         CopyNavNode( OriginalSource);    // populates  ObjectInspectorSourceNode
         CutItemQuietly(UIRootNode,OriginalSource);
         // paste the source node under the new parent
         PasteItem(DestNode,ObjectInspectorSourceNode,'');
      end
      else
        showmessage('MoveComponent:  Cannot move node '+nodeId+' to new parent '+NewParentId);
    end
    else
      showmessage('MoveComponent:  Cannot find destination node '+NewParentId);
  end
  else
    showmessage('MoveComponent:  Cannot find source node '+nodeId);
end;

procedure OICopyToNewParent(nodeId,NameSpace:string;NewParentId:string;NewName:String);
// Interface function (available to user code block) to copy a node in the nav tree to the given parent.
var
  OriginalSource, DestNode:TDataNode;
  s:boolean;
begin
  // find the source node
  OriginalSource:=FindDataNodeById(UIRootNode,NodeId,NameSpace,false);
  if (OriginalSource<>nil) then
  begin
    s:=SuppressEvents;
    SuppressEvents:=true;
    TreeInFocus:=UIRootNode;
    // find the new parent node
    DestNode:=FindDataNodeById(UIRootNode,NewParentId,NameSpace,true);
    if (DestNode<>nil) then
    begin
      //ShowMessage('OIMoveItem '+ObjectInspectorSourceNode.NodeName+' at '+ObjectInspectorSelectedNavTreeNode.NodeName);

        if (OriginalSource <> DestNode)
        and (DestNode.IsDynamic=true)
        and (CanAddChildToParent(DestNode,OriginalSource)) then
        begin
           CopyNavNode( OriginalSource);    // populates  ObjectInspectorSourceNode
           // paste the source node under the new parent
           PasteItem(DestNode,ObjectInspectorSourceNode,NewName);
        end
        else
          showmessage('MoveComponent:  Cannot move node '+nodeId+' to new parent '+NewParentId);
    end
    else
      showmessage('MoveComponent:  Cannot find destination node '+NewParentId);
    SuppressEvents:=s;
  end
  else
    showmessage('MoveComponent:  Cannot find source node '+nodeId);
end;
{$ifndef JScript}
procedure OICopyToNewParent(nodeId,NameSpace:string;NewParentId:string;NewName:PChar);
begin
   OICopyToNewParent(nodeId,NameSpace,NewParentId,strPas(NewName));
end;
{$endif}

procedure OICopySelectedItem;
begin
  if TreeInFocus<>nil then
  begin
    if TreeInFocus.NodeName=SystemRootName then
    begin
      CopyNavNode( ObjectInspectorSelectedNavTreeNode);
    end;

  end
  else
    ShowMessage('OICopySelectedItem.  TreeInFocus is nil');

end;

procedure OIDeleteComposite;
var
  filename:string;
begin
  if (AvailableResourcesSelectedNode<>nil) and (AvailableResourcesSelectedNode.NodeClass='RUI') then
  begin
    filename:=myStringReplace(AvailableResourcesSelectedNode.NodeName,'_xcmp','.xcmp',1,-1);
    if XIDEConfirm('OK to delete composite resource '+filename+'?') then
    begin
      {$ifndef JScript}
      filename:='SavedSystems/'+filename;
      {$endif}
      ClearLocalStore( filename);
      RebuildResourcesTree;
    end;
  end
  else
    ShowMessage('Select a composite resource first');
end;
procedure OIDeleteResource;
begin
  if (AvailableResourcesSelectedNode<>nil)
    and (AvailableResourcesSelectedNode.NodeClass='RUI')
    and (AvailableResourcesSelectedNode.NodeType='TXComposite') then
    if XIDEConfirm('OK to delete '+AvailableResourcesSelectedNode.NodeName+'?') then
      OIDeleteComposite;
end;

procedure OILoadComposite;
begin
  if (AvailableResourcesSelectedNode<>nil)
    and (AvailableResourcesSelectedNode.NodeClass='RUI')
    and (AvailableResourcesSelectedNode.NodeType='TXComposite') then
  begin
    if XIDEConfirm('OK to load composite '+AvailableResourcesSelectedNode.NodeName+' for editing?') then
      LoadCompositeResource(AvailableResourcesSelectedNode);
  end
  else
    ShowMessage('Select a composite resource first');
end;


procedure OILoadSavedSystem(SysName:String);
begin
    if XIDEConfirm('OK to load system '+SysName+'?') then
      LoadNamedSystem(SysName);
end;

procedure OILoadResource;
begin

  if (AvailableResourcesSelectedNode<>nil)
    and (AvailableResourcesSelectedNode.NodeClass='RUI')
    and (AvailableResourcesSelectedNode.NodeType='TXComposite') then
    OILoadComposite;
end;

procedure OIDragItem(e:TEventStatus;nodeId:string;myValue:string);
var
  TreeNodeId,p1:string;
begin
  // Nav Tree, and Resource Tree.

  TreeNodeId:=TreeLabelToID( myValue, nodeId,p1);
  //ShowMessage('OIDragItem nodeid '+nodeId+' node='+myNode.NodeName+' treenodeid='+TreeNodeId);
  if nodeId = 'ResourceTree' then
  begin
    AvailableResourcesSelectedNode:=FindDataNodeById(ResourcesNodeTree,TreeNodeId,'',true);
    //ShowMessage('OIDragItem node '+AvailableResourcesSelectedNode.NodeName);
    if (AvailableResourcesSelectedNode<>nil)
    and (AvailableResourcesSelectedNode.NodeName<>'')
    and (AvailableResourcesSelectedNode.NodeType<>'')
    and (AvailableResourcesSelectedNode.NodeType<>'Demo')
    and (AvailableResourcesSelectedNode.NodeType<>'Test') then
      PickItem( AvailableResourcesSelectedNode);
  end
  else
  if nodeId = 'NavTree' then
    begin
      ObjectInspectorSelectedNavTreeNode:=FindDataNodeById(UIRootNode,TreeNodeId,'',true);
      TreeInFocus:=UIRootNode;
      OICopySelectedItem;
    end;
end;

procedure OIDeleteSelectedItem;
begin
  if TreeInFocus<>nil then
  begin
    if (TreeInFocus.NodeName=SystemRootName)
    and (ObjectInspectorSelectedNavTreeNode<>nil) then
    begin
      if XIDEConfirm('OK to delete component '+ObjectInspectorSelectedNavTreeNode.NodeName+'?') then
        DeleteItem(UIRootNode,ObjectInspectorSelectedNavTreeNode)
    end;
  end;
end;

function OIDeleteItem(NodeId,NameSpace:String;ShowNotFoundMsg:Boolean=true;ShowConfirm:Boolean=true):Boolean;
var
  NodeToDelete:TdataNode;
  ok,Deleted:Boolean;
begin
  Deleted:=false;
  ok:=false;
  NodeToDelete:=FindDataNodeById(UIRootNode,NodeId,NameSpace,ShowNotFoundMsg);
  if NodeToDelete<>nil then
  begin
    if ShowConfirm=false then
    begin
      ok:=true ;
    end
    else
    begin
      if XIDEConfirm('OK to delete component '+NodeToDelete.NodeName+'?') then
        ok:=true;
    end;
    if ok then
      Deleted:=DeleteItem(UIRootNode,NodeToDelete);
  end;
  result:=Deleted;
end;

procedure OISystemLoad(e:TEventStatus;nodeId:string);
var
  SystemDescription:String;
begin
  //{$ifdef JScript}
  //asm console.log('OISystemLoad'); end;
  //{$endif}

   if (e=nil)  or (e.InitRunning=false) then
   begin
     if (e=nil) then
     begin
       e:=TEventStatus.Create('Click',nodeId);
     end;
     e.initRunning:=true;
     PasteDialogUnit.CompletionEvent:=e;
     {$ifdef JScript}
     e.AsyncProcsRunning.Add('CopyFromClip');
     {$endif}

   end
   else
   begin
     e.InitRunning:=false;
   end;

   if e.InitRunning then
   begin
     PasteDoneBtn.IsVisible:=false;
     SystemDescription:=mygetClipboardData('System'); // (html) opens the popup for paste action
   end;
   e.InitDone:=true;

   if e.EventHasWaitingAsyncProcs = false then
   // this is lazarus and a confirm dialog is not needed
   // otherwise this is HTML and we have waited for a ctrl-V event from the PasteDialog form
   begin
     {$ifndef JScript}
     // Lazarus only
     DoSystemLoad(SystemDescription,'');
     {$else}
     asm
       //console.log('call DoSystemLoad ');
       pas.NodeUtils.StartingUp=false;
       var pasteTarget = document.getElementById('PasteTargetContents');
       var PasteString = pasteTarget.value;
       //alert('Paste string = >'+PasteString+'<' );
       pas.XObjectInsp.DoSystemLoad(PasteString,'');
     end;
     {$endif}
   end;

end;

procedure OIClearSystem;
var
  i:integer;
begin
    // close XForm windows
    for i:=length(OpenXForms)-1 downto 0 do
    begin
      CloseXForm(OpenXForms[i].NodeName,OpenXForms[i].NameSpace);
    end;
    SetSystemName('XIDESystem');

    PyMemoComponent:=XIDEMain.XIDEForm.XMemo1;

    ClearInspectors;
    ClearAllDynamicNodes(SystemNodeTree); // clear any existing dynamic screen components under Root

    // open the properties tab in the object inspector
    XIDEForm.ObjectInspectorTabs.TabIndex:=0;
    XIDEForm.OITabs.TabIndex:=0;
    XIDEForm.ResourceInspectorTabs.TabIndex:=0;

    RebuildNavigatorTree;
    RebuildCodeTree;
    RebuildResourcesTree;
    InitialiseStyleDesigner;

    InitDMTree;
    DMChanged:=false;
    RebuildDMTree;
 end;


function SetMyDeployedMode(wholesystem,dm2:String):String;
var
  dp1,dp2:integer;
begin
  // look for the string /*Deployment*/
  dp1 := FoundString(wholesystem,'/*Deployment1*/');
  if dp1>0 then
  begin
    dp2:=FoundString(wholesystem,'/*Deployment2*/');
    if dp2>0 then
    begin
      dp2:=dp2+15;
      Delete(wholesystem,dp1,(dp2-dp1));
      Insert('/*Deployment1*/var myDeployedMode = '''+dm2+''';/*Deployment2*/',wholesystem,dp1);
    end;
  end;
  result:=wholesystem;
end;

{$ifdef JScript}
procedure CompleteDeployFromBrowser(deployname:String);
var
  htmlHead,htmlBody,wholesystem,projectJS,currentNodeTree,dm2,sysname,dpstr:String;
  ok:boolean;
  dp1,dp2:integer;
begin
  // make temporary changes to root node attributes
  sysname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  dm2:=UIRootNode.GetAttribute('DeploymentMode',false).AttribValue;
  UIRootNode.SetAttributeValue('SystemName',deployname);

  ok:=CompileEventCode(CodeEditForm.CodeEdit,'JSJS');
  DeleteGreyOverlay('Grey1');
  if ok then
  begin
    wholesystem:='<!DOCTYPE HTML>'  +LineEnding
        + '<html  lang="en">' +LineEnding;
    projectJS:='';
    asm
      //console.log('finding core js code');
      // find the block of JS code that defines the XIDE framework
      //!! this needs to be the JS for running the framework, is NOT the compiled user code
      var corecode = document.getElementById("ProjectCodeContainer");
      if (corecode != null) {
        projectJS=corecode.innerHTML;
        }
    end;
    htmlHead:=BuildHTMLHead('XIDEMain',sysname,dm2,projectJS);
    wholesystem:=wholesystem + htmlHead;
    htmlBody:=BuildHTMLBody;
    wholesystem:=wholesystem + htmlBody;


    // set LoadedSystemString...
    currentNodeTree := NodeTreeToXML(SystemNodeTree,nil,false,true);
    // look for the string 'pas.NodeUtils.LoadedSystemString ='
    dp1 := 0;
    dp1 := FoundString(wholesystem,chr(112)+'as.NodeUtils.LoadedSystemString = "*";');  // length 39
    if dp1>0 then
    begin
      dp1 := dp1 + 39;
      dp2 := FoundString(wholesystem,'<\/Root>";');   // length 10
      if dp2>0 then
      begin
        dp2 := dp2 + 10;
        Delete(wholesystem,dp1,(dp2-dp1));
        Insert('pas.NodeUtils.LoadedSystemString = '''+currentNodeTree+''';',wholesystem,dp1);
      end;
    end;

    PasteDialogUnit.PasteTarget.ItemValue:=wholesystem;
    PasteDialogUnit.PasteDoneBtn.IsVisible:=true;
    PasteDialogUnit.PasteLabel.LabelCaption:='Press Done to complete the copy to clipboard action';
    PasteDialogUnit.PasteTarget.IsVisible:=false;

    OpenModal('PasteDialog');

  end;
  UIRootNode.SetAttributeValue('SystemName',sysname);
end;

{$endif}

procedure OIDeploySystem;
// Save a browser-ready html copy of the whole system (including XIDE framework) to the clipboard
var
  wholesystem,jsText,sysname,deployname:String;
  lines,ExtraLines,ExtraDirectives:TStringList;
  ok:boolean;
  dm:String;
begin
  // First delete object inspector dynamic property editor fields
  ClearInspectors;

  sysname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  deployname:=XIDEPrompt('Name of Deployed System',sysname);
  if deployname='' then  // user has cancelled
    EXIT;

  ExtraDirectives:=TStringList.Create;

  {$ifndef JScript}
  lines:=TStringList.Create;
  // Compile the user-created event code into a unit.
  ok:=CompileEventCode(CodeEditForm.CodeEdit,'LazJS');
  if ok then
  begin
     Screen.Cursor := crHourglass;

     UIRootNode.SetAttributeValue('SystemName',deployname);
     // Save the system definition data for the compiler
     SaveSystemToIncFile;
     UIRootNode.SetAttributeValue('SystemName',sysname);

     // additional inc file for composite resources
     SaveCompositesToIncFile;

     {$ifdef Python}
     ExtraDirectives.Add('-dPython');
     {$endif}
     // now cross compile from a saved copy of this source with the conditional define
     // switch (JScript) set to compile the JS version instead of the Lazarus version
     TranspileMyProgram('XIDEMain',ProjectDirectory,'resources/project/',CodeEditForm.CodeEdit,false,ExtraDirectives);

     if FileExists('XIDEMain.js') then
       lines.LoadFromFile('XIDEMain.js');

     Screen.Cursor := crDefault;

  end
  else
  begin
    DisplayDllCompileErrors;
  end;

  {$ifdef Python}
  ExtraLines:=PyodideScript;
  ExtraLines.Add('<script>');
  ExtraLines.AddStrings(lines);
  lines.Text:=ExtraLines.Text;
  {$else}
  lines.insert(0,'    <script type="application/javascript" >');
  {$endif}
  lines.add('   </script>  ');

  jsText:=lines.text;

  dm:=UIRootNode.GetAttribute('DeploymentMode',false).AttribValue ;
  wholesystem := CreateHTMLWrapper('XIDEMain',dm,true,jsText);
  lines.Free;
  myCopyToClip('HTML System',wholesystem );

  {$else}
  ShowGreyOverlay(SystemRootName,'Grey1','Completing Deployment. Please Wait...');
  // timeout here so the grey overlay appears
  asm
    myTimeout(pas.XObjectInsp.CompleteDeployFromBrowser,20,'CompleteDeployFromBrowser',0,deployname);
  end;

  {$endif}

  ExtraDirectives.Free;
end;

procedure OIComponentCopy(nodeId:string;myValue:string);
begin
  PickItem( AvailableResourcesSelectedNode);
end;

procedure ShowHideObjectInspector(show:Boolean);
var
  aNode:TdataNode;
  lr:String;
begin
   if not show then
   begin
     DeHighlightObject(ObjectInspectorSelectedNavTreeNode);
     ClearInspectors;
   end;

   aNode:=FindDataNodeById(UIRootNode,'InnerRootVBox','',true);
   if aNode<>nil then
     ShowHideNode(aNode,show);

   aNode:=FindDataNodeById(UIRootNode,'ResourceInspectorTabs','',true);
   lr:=UIRootNode.GetAttribute('ShowResources',true).AttribValue;
   if aNode<>nil then
     if lr<>'Hide' then
       ShowHideNode(aNode,show);

   if show then
   begin
     if lr<>'Hide' then
       TXScrollBox(MainFormProjectRoot.ScreenObject).ContainerWidth:='60%'
     else
       TXScrollBox(MainFormProjectRoot.ScreenObject).ContainerWidth:='80%';
   end
   else
   begin
     TXScrollBox(MainFormProjectRoot.ScreenObject).ContainerWidth:='100%';
   end;

end;

{$ifndef JScript}
procedure RedisplayResourceTree;
var
  hbox:TWinControl;
  aNode:TdataNode;
  ob1,ob2,ob3:TControl;
  lr:String;
begin
  aNode:=FindDataNodeById(UIRootNode,'RootHBox','',true);
  hbox:=TWinControl(aNode.ScreenObject);
  aNode:=FindDataNodeById(UIRootNode,'ResourceInspectorTabs','',true);
  ob1:=TControl(aNode.ScreenObject);
  lr:=UIRootNode.GetAttribute('ShowResources',true).AttribValue;
  if lr='Hide' then
  begin
    TXTabControl(ob1).IsVisible:=false;
  end
  else
  begin
    aNode:=FindDataNodeById(UIRootNode,'InnerRootVBox','',true);
    ob2:=TControl(aNode.ScreenObject);
    aNode:=FindDataNodeById(UIRootNode,UIProjectRootName,'',true);
    ob3:=TControl(aNode.ScreenObject);

    hbox.RemoveControl(ob1);
    hbox.RemoveControl(ob2);
    hbox.RemoveControl(ob3);

    //if ShowResourceTree='Left' then
    if lr='Left' then
    begin
      ob1.Parent:=hbox;
      ob2.Parent:=hbox;
      ob3.Parent:=hbox;
    end
    //else if ShowResourceTree='Right' then
    else
    begin
      ob2.Parent:=hbox;
      ob3.Parent:=hbox;
      ob1.Parent:=hbox;
    end;
    TXTabControl(ob1).IsVisible:=true;
  end;
  if DesignMode then
    ShowHideObjectInspector(true);
  DoFormResize(MainForm, MainFormTopControl);
end;
{$else}
procedure RedisplayResourceTree;
var
  aNode:TdataNode;
  ob1:TObject;
  lr:String;
begin
  aNode:=FindDataNodeById(UIRootNode,'ResourceInspectorTabs','',true);
  ob1:=aNode.ScreenObject;
  lr:=UIRootNode.GetAttribute('ShowResources',true).AttribValue;
  if lr='Hide' then
  begin
    TXTabControl(ob1).IsVisible:=false;
  end
  else
  begin
    TXTabControl(ob1).IsVisible:=true;
    asm
    var hbox=document.getElementById('RootHBoxContents');
    ob1=document.getElementById('ResourceInspectorTabs');
    var ob2=document.getElementById('InnerRootVBox');
    var ob3=document.getElementById('UIRoot');

    hbox.removeChild(ob1);
    hbox.removeChild(ob2);
    hbox.removeChild(ob3);

    //if (pas.XObjectInsp.ShowResourceTree=='Left') {
    if (lr=='Left') {
      hbox.appendChild(ob1);
      hbox.appendChild(ob2);
      hbox.appendChild(ob3);
    }
    //if (pas.XObjectInsp.ShowResourceTree=='Right') {
    else {
      hbox.appendChild(ob2);
      hbox.appendChild(ob3);
      hbox.appendChild(ob1);
    }
    end;
  end;
  if DesignMode then
    ShowHideObjectInspector(true);
end;
{$endif}

procedure DisplayDllCompileErrors;
{$ifdef JScript}
var
  nm,nm2:String;
{$endif}
begin
  if CodeEditForm.CodeEdit.MessageLines<>'' then
  begin

    CodeEditForm.Mode:='dll';
    CodeEditForm.InitialiseOnShow('Compiler Errors','','');

    ShowXForm('CodeEditForm',true);     // the relevant text and message contents have already been populated

    {$ifdef JScript}
    nm:=CodeEditor.CodeEditForm.CodeEdit.NodeName;
    nm2:=CodeEditor.CodeEditForm.CodeEditInit.NodeName;
    asm
      pas.XCode.DoKeyUp(nm2+'Contents',nm2,'',null);
      pas.XCode.DoKeyUp(nm+'Contents',nm,'',null);
    end;
    {$endif}
  end;

end;

{$ifdef JScript}
procedure CompleteToggleToRunMode(ok:Boolean);
begin
  SuppressUserEvents:=false;

  if ok then
  begin
    asm
      console.log('CompleteToggleToRunMode calling event OnEnterRunMode');
    end;
    {$ifdef Python}
    asm
    async function waitForNewPackages() {
    await loadPyPkgs(doAfterPyPaksLoaded);  //pysrcLoaded();  // make sure all required python packages are loaded
    }
    waitForNewPackages();
    function doAfterPyPaksLoaded() {
      console.log("doAfterPyPaksLoaded");
      testPyPkLoaded('xarray');
    end;
    if (PyXUtils.PyPkTest=1) then
       BuildXArrays(true);
    //exec all defined python scripts
    GatherAndRunPythonScriptsLater;
    {$endif}
    asm
    myTimeout(pas.Events.handleEvent,5,'handleEvent',0,null,'OnEnterRunMode',pas.NodeUtils.SystemRootName,'','');
    end;
    {$ifdef Python}
    asm
    }
    end;
    {$endif}
  end
  else
  begin
    // Revert to design mode
    SetScreenToDesignMode;
    DeleteGreyOverlay('Grey1');
  end;
end;
{$endif}

procedure ToggleToRunModeAfterCompile(ok:boolean);
var
  MenuItemNode:TDataNode;
  MenuItem,SysMenuItem:TXMenuItem;
  v:integer;
begin
  MenuItemNode:=FindDataNodeById(UIRootNode,'ToggleDesignRunMode','',true);
  MenuItem:=TXMenuItem(MenuItemNode.ScreenObject);
  if ok then
  begin
    // event code has compiled successfully
    DesignMode:=false;
    MenuItem.Caption:='Design Mode';
    // Hide Object Inspector
    ShowHideObjectInspector(false);

    {$ifndef JScript}
    //Unload the events dll, if previously loaded
    if MyLibC <>  DynLibs.NilHandle then
      if FreeLibrary(MyLibC) then
        MyLibC:= DynLibs.NilHandle;

    GlobalSuppressFrameDisplay:=false;
    UnsuspendFrames(SystemNodeTree);

    DoFormResize(MainForm, MainFormTopControl);
    TWinControl(MainFormProjectRoot.ScreenObject).SetFocus;

    SysMenuItem:=XIDEForm.SystemMenu;
    SysMenuItem.IsVisible:=false;

    //GatherSourcedAttributes(SystemNodeTree);
    GatherSourcedAttributes(UIRootNode);
    PushAllSourcesToAttributes;

    ok:=BuildLocalDB(StrToInt(UIRootNode.GetAttribute('DBVersion',false).AttribValue));

    SuppressUserEvents:=false;

    HandleEventLater(nil,'OnEnterRunMode',SystemRootName,'','');
    {$ifdef Python}
    //Clear the python engine and re-initialise
    DoPy_InitEngine;
    RunInitialScript;
    BuildXArrays(true);
    //do later .... exec all defined python scripts
    GatherAndRunPythonScriptsLater;
    {$endif}
    {$else}
    GatherSourcedAttributes(UIRootNode);
    PushAllSourcesToAttributes;
    v:=StrToInt(UIRootNode.GetAttribute('DBVersion',false).AttribValue);
    asm
      ok=pas.XDataModel.BuildLocalDB(v,pas.XObjectInsp.CompleteToggleToRunMode);
    end;

    {$endif}
  end
  else
  begin
    // Pascal compilation of event code has failed
    SuppressUserEvents:=true;
    {$ifndef JScript}
    DisplayDllCompileErrors;
    {$else}
    if RunningDeployedRuntime   then
    begin
      showmessage('Unable to compile event code.  Cannot load this system');
      //!! need to clear the system here, or something.....
      OIClearSystem;
    end
    else
    begin
      DeleteGreyOverlay('Grey1');
      DisplayDllCompileErrors;
      CodeEditForm.NavigateToFirstError;

    end;
    {$endif}
  end;
end;

{$ifdef JScript}
procedure ContinueToggleToRunMode;
var
  ok:Boolean;
begin
    // Compile the user-created event code, using embedded pas2js compiler
    // and generate js
    ok:=CompileEventCode(CodeEditForm.CodeEdit,'JSJS');
    //asm console.log('calling ToggleToRunModeAfterCompile'); end;
    ToggleToRunModeAfterCompile(ok);
    {$ifndef Python}
    DeleteGreyOverlay('Grey1');
    {$endif}
end;
{$endif}

procedure SetScreenToDesignMode;
var
  MenuItemNode:TDataNode;
  MenuItem:TXMenuItem;
begin
  MenuItemNode:=FindDataNodeById(UIRootNode,'ToggleDesignRunMode','',true);
  MenuItem:=TXMenuItem(MenuItemNode.ScreenObject);
  DesignMode:=true;
  SuppressUserEvents:=true;
  SetLength(SourcedAttribs,0);        // keep these during design mode !!!!????
  MenuItem.Caption:='Run Mode';
  XIDEForm.SystemMenu.IsVisible:=true;
  // Show Object Inspector
  ShowHideObjectInspector(true);
  {$ifndef JScript}
  GlobalSuppressFrameDisplay:=true;
  DoFormResize(MainForm,MainFormTopControl);
  {$endif}

end;

procedure DoToggleDesignRunMode(Sender:TXMenuItem);
var
  ok:Boolean;
  GPUNodes:TNodesArray;
  i:integer;
begin
  ok:=true;

  if (DesignMode=true)
  then
  begin
    // Go to Run Mode
    EditAttributeValue('XMemo1','','ItemValue','',false);
    {$ifdef Python}
    PyScriptsExecuted:=false;

    if (PyMemoComponent<>nil)
    and (assigned(PyMemoComponent)) then
    begin
      PyMemoComponent.ItemValue:='';
    end;
    {$endif}

    SetLength(SourcedAttribs,0);
    {$ifndef JScript}
    // Check pas2js Compilation of the user-created event code first.
    ok:=CompileEventCode(CodeEditForm.CodeEdit,'LazJS');
    if ok then
    begin
      // Now Compile the user-created event code into the dll.
      ok:=CompileEventCode(CodeEditForm.CodeEdit,'LazDll');
      ToggleToRunModeAfterCompile(ok);
    end
    else
    begin
      DisplayDllCompileErrors;
    end;
    {$else}
    RunModeAttempts:=RunModeAttempts+1;
    for i:=length(OpenXForms)-1 downto 0 do
    begin
      if OpenXForms[i].NameSpace='' then
        CloseXFormForDesign(OpenXForms[i].NodeName);
    end;

    {$ifdef Python}
    asm
      if (readyForRunMode==false) {
        if (pas.XObjectInsp.RunModeAttempts>3) {
          alert('Pyodide still not ready. Please check console for possible problems.');
        }
        else {
          alert('Pyodide is not ready. Please wait and try again.');
        }
        ok = false;
      }
    end;
    {$endif}
    if ok then
    begin
      RunModeAttempts:=0;
      ShowGreyOverlay(SystemRootName,'Grey1','Compiling System. Please Wait...');
      // timeout here so the grey overlay appears
      asm
        myTimeout(pas.XObjectInsp.ContinueToggleToRunMode,5,'ContinueToggleToRunMode',0);
      end;
    end;
    {$endif}
  end
  else
  begin
    // Go to Design Mode
    PyScriptsExecuted:=false;
    if StartingUp=false then
    begin
      // First, STOP any running GPU components
      GPUNodes:=FindNodesOfType(UIRootNode,'TXGPUCanvas');
      for i:=0 to length(GPUNodes)-1 do
      begin
        if GPUNodes[i].ScreenObject<>nil then
        begin
          //showmessage('stopping gpu '+GPUNodes[i].NodeName);
          TXGPUCanvas(GPUNodes[i].ScreenObject).Active:=false;
        end;
      end;
    end;
    {$ifdef JScript}
    if RunningDeployedRuntime then
    begin
      showmessage('Design Mode not available');
      EXIT;
    end;
    {$endif}

    HandleEvent(nil,'OnExitRunMode',SystemRootName,'','');

    SaveLocalDB;
    CloseLocalDB;

    SetScreenToDesignMode;
  end;
end;



procedure EditResourceAttributeValue(NodeName,AttrName,NewValue:string);
// Editing a node in  the Resources Navigator tree
var
   myNode:TDataNode;
   myAttrib:TNodeAttribute;
begin
   // find the given node
   myNode:=FindDataNodeById(ResourcesNodeTree,NodeName,'',true);
   if myNode<>nil then
   begin
     myAttrib:=myNode.GetAttribute(AttrName,false);
     if myAttrib.AttribValue<>NewValue then
     begin
       // update the attribute value
       myNode.SetAttributeValue(AttrName,NewValue);
     end;
   end;
end;

procedure EditEventCode(NodeNameToEdit,EventToEdit,MainCode,InitCode:String);
var
  targetNode:TDataNode;
  i:integer;
begin
  //targetNode:=FindDataNodeById(SystemNodetree,NodeNameToEdit,'',true);
  targetNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true);

  for i:=0 to targetNode.myEventTypes.count-1 do
  begin
    if targetNode.myEventTypes[i] = EventToEdit then
    begin
      targetNode.myEventHandlers[i].TheCode:=MainCode;
      targetNode.myEventHandlers[i].InitCode:=InitCode;
    end;
  end;

end;

procedure TOIEventWrapper.TestButtonClick(e:TEventStatus;nodeId:string;myValue:string);
begin
            //showmessage('OIEditProperty '+nodeId+' '+myValue);
            EditAttributeValue('TestDynamic1','','Caption','dynamic event ok');
end;

procedure TOIEventWrapper.OIEditPropertyButtonClick(e:TEventStatus;nodeId:string;myValue:string);
var
  bits:TStringList;
  NodeNameToEdit,PropertyToEdit, EditBoxName, SourceName:String;
  targetNode:TDataNode;
  targetAttribute:TNodeAttribute;
begin
  bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
  if bits.Count = 4 then
  begin
    if (bits[0]='OI') or (bits[0]='RI') or (bits[0]='DM') then       // OI, Editboxname, NodeName, suffix
    begin
      NodeNameToEdit:=bits[1];
      PropertyToEdit:=bits[2];
    end;
  end;

  if  (bits[0]<>'DM') then
    targetNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true)
  else
    targetNode:=FindDataNodeById(DMRoot,NodeNameToEdit,'',true);
  if targetNode=nil then
    EXIT;
  targetAttribute:= targetNode.GetAttribute(PropertyToEdit,false);

  if ((targetNode.NodeType<>'TXGPUCanvas')
  or ((targetAttribute.AttribName<>'AnimationCode') and (targetAttribute.AttribName<>'InitStageData')))
  and ((targetNode.NodeType<>'DMOp') or (targetAttribute.AttribName<>'Code')) then
  begin
    // pop up the property editor.
    PropertyEditForm.TargetNode:=targetNode;
    PropertyEditForm.TargetAttribute:=targetAttribute;

    // show validation details (eg range, function,...) according to type....
    // and display data source details...
    PropertyEditForm.PropertyEditName.ItemValue:=PropertyToEdit;
    PropertyEditForm.PropertyEditType.ItemValue:=targetAttribute.AttribType;
    PropertyEditForm.PropertyEditTabSheet3.IsVisible:=true;
    if targetNode.NodeType = 'TXCompositeIntf' then
    begin
      PropertyEditForm.PropertyEditHint.IsVisible:=true;
      PropertyEditForm.PropertyEditHint.ItemValue:=targetAttribute.AttribHint;
      if targetAttribute.AttribReadOnly=true then
        PropertyEditForm.PropertyEditTabSheet3.IsVisible:=false;
    end
    else
      PropertyEditForm.PropertyEditHint.IsVisible:=false;
    SourceName:=targetAttribute.AttribSource.InputNodeName;
    if targetAttribute.AttribSource.InputAttribName<>'' then
    begin
      SourceName:=SourceName+'.'+targetAttribute.AttribSource.InputAttribName;
//      showmessage('This attribute has source '+SourceName);
    end;
    PropertyEditForm.PropertyEditSourceBox.ItemValue:=SourceName;
    // PropertyEditBox is the editbox (or combobox, checkbox etc) in the object inspector...
    // associated EditBox has same name, minus the Btn suffix...
    EditBoxName:=bits[0]+AttributeEditorNameDelimiter
                 +NodeNameToEdit+AttributeEditorNameDelimiter
                 +PropertyToEdit+AttributeEditorNameDelimiter
                 +myStringReplace(bits[3],'Btn','',1,9999);         //myStringReplace(nodeId,'Btn','',1,9999);
    //if bits[0]='DM' then
    //  PropertyEditBox:=FindDataNodeById(DMAttribsNode,EditBoxName,'',true)
    //else
      PropertyEditBox:=FindDataNodeById(OITabs,EditBoxName,'',true);

    PropertyEditForm.SetupPages;
    ShowXForm('PropertyEditForm',true);
  end
  else
  if (targetNode.NodeType='DMOp')
  and (targetAttribute.AttribName='Code') then
  begin
    ShowDMOpCodeEditor(targetNode,nodeId);
  end
  else
  // Special Case - edit the AnimationCode in a TXGPUCanvas component using the dedicated popup editor...
  // Special Case - edit the InitStageData in a TXGPUCanvas component using the dedicated popup editor...
  begin
    if targetAttribute.AttribName='InitStageData' then
      ShowGPUEditor(targetNode,2)
    else
      ShowGPUEditor(targetNode,0);
  end;

end;

procedure TOIEventWrapper.OIDelPropertyButtonClick(e:TEventStatus;nodeId:string;myValue:string);
var
  bits:TStringList;
  NodeNameToEdit,PropertyToDelete:String;
  targetNode:TDataNode;
  targetAttribute:TNodeAttribute;
begin
  bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
  if bits.Count = 4 then
  begin
    if (bits[0]='OI') then       // OI, Editboxname, NodeName, suffix
    begin
      NodeNameToEdit:=bits[1];
      PropertyToDelete:=bits[2];
    end
    else
      EXIT;
  end
  else
    EXIT;

  targetNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true);
  targetAttribute:= targetNode.GetAttribute(PropertyToDelete,false);

  if (targetNode.NodeType='TXCompositeIntf') then
  begin
    targetNode.DeleteAttribute(targetAttribute.AttribName);
  end
  else
    showmessage('This property cannot be deleted');

  // refresh the object inspector
  RefreshObjectInspectorLater(targetNode);
end;

procedure TOIEventWrapper.OIDelEventButtonClick(e:TEventStatus;nodeId:string;myValue:string);
var
  bits:TStringList;
  NodeNameToEdit,EventToDelete:String;
  targetNode:TDataNode;
begin
  bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
  if bits.Count = 4 then
  begin
    if (bits[0]='OI') then       // OI, Editboxname, NodeName, suffix
    begin
      NodeNameToEdit:=bits[1];
      EventToDelete:=bits[2];
    end
    else
      EXIT;
  end
  else
    EXIT;

  targetNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true);

  if (targetNode.NodeType='TXCompositeIntf') then
  begin
    targetNode.DeleteEvent(EventToDelete);
  end
  else
    showmessage('This event cannot be deleted');

  // refresh the object inspector
  RefreshObjectInspectorLater(targetNode);
end;

procedure TOIEventWrapper.OIEditProperty(e:TEventStatus;nodeId:string;myValue:string);
var
  bits:TStringList;
  NodeNameToEdit,AttrNameToEdit:String;
  {$ifdef JScript}
  ok:boolean;
  {$endif}
begin
  //showmessage('OIEditProperty '+nodeId+' '+myValue);
  // fudge...
  {$ifdef JScript}
  ok:=true;
  asm
    if (myValue==undefined) {ok=false};
  end;
  if not ok then EXIT;
  {$endif}

  bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
  if bits.Count = 4 then
  begin
    if (bits[0]='OI') or (bits[0]='RI') or (bits[0]='DM') then       // OI, Editboxname, NodeName, Attrname, suffix
    begin
      NodeNameToEdit:=bits[1];
      AttrNameToEdit:=bits[2];
      if bits[0]='OI' then
      begin
        //showmessage('OIEditProperty '+nodeId+' '+AttrNameToEdit+' '+myValue);
        EditAttributeValue(NodeNameToEdit,'',AttrNameToEdit,myValue);
        RefreshObjectInspector(ObjectInspectorSelectedNavTreeNode);
      end
      else if bits[0]='DM' then
      begin
        EditAttributeValue(NodeNameToEdit,'',AttrNameToEdit,myValue,false,DMRoot);
        DMAttribsCrosscheck(NodeNameToEdit);
        RebuildDMTreeLater;         //!!!! must do 'later' because may be processing eg. combobox event
        //RefreshObjectInspector(DMSelectedDataTreeNode);
        DMChanged:=true;
      end
      else
        EditResourceAttributeValue(NodeNameToEdit,AttrNameToEdit,myValue);
    end;
  end;
end;

procedure TOIEventWrapper.OIEditEvent(e:TEventStatus;nodeId:string;myValue:string);
var
  bits:TStringList;
  NodeNameToEdit,EventToEdit,EventInitCode:String;
  EventNode:TDataNode;
begin
      bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
      if bits.Count = 4 then
      begin
        if (bits[0]='OI') or (bits[0]='RI') then       // OI, Editboxname, NodeName, suffix
        begin
          NodeNameToEdit:=bits[1];
          EventToEdit:=bits[2];
          EventNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true);
          EventInitCode:=EventNode.GetEventCode(EventToEdit);
          //showmessage('OIEditEvent '+nodeId+' '+myValue);
          EditEventCode(NodeNameToEdit,EventToEdit,myValue,EventInitCode);
        end;
      end;
end;

procedure TOIEventWrapper.CloseGPUEditor(e:TEventStatus;nodeId:string;myValue:string);
begin
  GPUEvents.CloseCodeEditor(e,nodeId,myValue);
  if ObjectInspectorSelectedNavTreeNode<>nil then
    RefreshObjectInspector(ObjectInspectorSelectedNavTreeNode);
end;

procedure CodeEditorClosed(EditBoxNode:TdataNode);
var
  tmp:string;
  CodeNode:TDataNode;
begin
  //showmessage('CodeEditorClosed. Mode='+CodeEditForm.Mode+' return status '+ CodeEditStatus);
  if CodeEditStatus = 'ok' then
  begin
    tmp:=CodeEditForm.CodeEdit.ItemValue;
    if CodeEditForm.Mode='EventCode' then
    begin
      if EditBoxNode<>nil then
         // set value of the associated edit box
         TXEditBox(EditBoxNode.ScreenObject).ItemValue:=CodeEditForm.CodeEdit.ItemValue;
         // nb. above doesn't trigger the onchange event for the edit box.
      // update the Code data...
      EditEventCode(CodeEditForm.TargetNodeName,CodeEditForm.EventType,CodeEditForm.CodeEdit.ItemValue,
                                                                       CodeEditForm.CodeEditInit.ItemValue);
      if ObjectInspectorSelectedNavTreeNode<>nil then
        RefreshObjectInspector(ObjectInspectorSelectedNavTreeNode);

    end
    else if (CodeEditForm.Mode='UnitCode')
      or (CodeEditForm.Mode='PasUnitCode')
      or (CodeEditForm.Mode='PythonScriptCode')
      or (CodeEditForm.Mode='DMOpCode')
    then
    begin
      tmp:=CodeEditForm.TargetNodeName;
      // name of unit is in TargetNodeName
      if (CodeEditForm.Mode='DMOpCode') then
        CodeNode:=FindDataNodeById(DMRoot,CodeEditForm.TargetNodeName,'',true)
      else
        CodeNode:=FindDataNodeById(CodeRootNode,CodeEditForm.TargetNodeName,'',true);
      tmp:=CodeEditForm.CodeEdit.ItemValue;
      CodeNode.SetAttributeValue('Code',tmp,'String');
    end
    else if (CodeEditForm.Mode='SearchCode')  then
    begin
      // do nothing
    end;

    RebuildCodeTree;
  end;
  OIEditBox:=nil;
end;

procedure PropertyEditorClosed(EditBoxNode:TdataNode);
var
  tmp:string;
  SourceBits:TStringList;
  SourceNode, SourceAttrib, NewValue:String;
  EditNode:TDataNode;
begin
  if PropertyEditStatus = 'ok' then
  begin
    EditNode:= PropertyEditForm.EditNode;
    // Set the value of TargetAttribute from the widget on the Edit tabpage...
    if EditNode.NodeType='TXEditBox' then
      NewValue:=EditNode.GetAttribute('ItemValue',false).AttribValue
    else if EditNode.NodeType='TXMemo' then
      NewValue:=EditNode.GetAttribute('ItemValue',false).AttribValue
    else if EditNode.NodeType='TXTree' then
      NewValue:=EditNode.GetAttribute('TreeData',false).AttribValue
    else if EditNode.NodeType='TXTable' then
    begin
      NewValue:=EditNode.GetAttribute('TableData',false).AttribValue;
      PropertyEditForm.TargetAttribute.AttribValue:= NewValue;
      if PropertyEditForm.TargetAttribute.AttribName='TableData' then
      begin
        PropertyEditForm.TargetNode.SetAttributeValue('NumRows',EditNode.GetAttribute('NumRows',false).AttribValue);
        PropertyEditForm.TargetNode.SetAttributeValue('NumCols',EditNode.GetAttribute('NumCols',false).AttribValue);
      end;
    end
    else if EditNode.NodeType='TXCheckBox' then
      NewValue:=EditNode.GetAttribute('Checked',false).AttribValue
    else if EditNode.NodeType='TXComboBox' then
      NewValue:=EditNode.GetAttribute('ItemValue',false).AttribValue
    else if EditNode.NodeType='TXColorPicker' then
      NewValue:=EditNode.GetAttribute('ItemValue',false).AttribValue;

    // Update the property value in the target node...
    if (PropertyEditForm.TargetAttribute.AttribName<>'MapData')
    and (PropertyEditForm.TargetAttribute.AttribName<>'MapColors') then
      PropertyEditForm.TargetAttribute.AttribValue:= NewValue;
    EditAttributeValue(PropertyEditForm.TargetNode,PropertyEditForm.TargetAttribute.AttribName,NewValue);
    //if PropertyEditForm.TargetNode.NodeClass='DM' then
    //  DMChanged:=true;

    // Set the data source for the property, if specified...
    tmp:=trim(PropertyEditForm.PropertyEditSourceBox.ItemValue);
    if tmp<>'' then
    begin
      SourceBits:=stringsplit(tmp,'.');
      SourceNode:=SourceBits[0];
      if SourceBits.Count>1 then
        SourceAttrib:=SourceBits[1];
      PropertyEditForm.TargetNode.SetAttributeSource(PropertyEditForm.TargetAttribute.AttribName,SourceNode,'',SourceAttrib);
    end
    else
    begin
      PropertyEditForm.TargetNode.SetAttributeSource(PropertyEditForm.TargetAttribute.AttribName,'','','');
    end;

    if PropertyEditForm.TargetNode.NodeType = 'TXCompositeIntf' then
    begin
      PropertyEditForm.TargetAttribute.AttribHint:=PropertyEditForm.PropertyEditHint.ItemValue;
    end;

    RefreshObjectInspector(PropertyEditForm.TargetNode);
  end;
  PropertyEditBox:=nil;
end;

procedure TOIEventWrapper.OIEditEventCodeFromCodeTree(NodeNameToEdit:string;EventToEdit:string);
var
  EventCode, EventInitCode:string;
  targetNode:TDataNode;
begin
  // pop up the syntax editor.
  OIEditBox:=nil;
  targetNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true);
  EventCode:=targetNode.GetEventCode(EventToEdit);
  EventInitCode:=targetNode.GetEventInitCode(EventToEdit);
  if Trim(EventCode)='' then
  begin
    // provide a template event procedure
    EventCode:= 'begin' + LineEnding +
                ' ' + LineEnding +
                'end;' + LineEnding;
  end;
  CodeEditForm.CodeEdit.ItemValue := EventCode;
  CodeEditForm.CodeEditInit.ItemValue := EventInitCode;
  //EventNum:=targetNode.EventNum(EventToEdit);
  CodeEditForm.CodeEdit.MessageLines:='';
  CodeEditForm.CodeEditInit.MessageLines:='';

  CodeEditForm.CodeEditMainTabs.TabIndex:=0;
  CodeEditForm.Mode:='EventCode';
  CodeEditForm.InitialiseOnShow('Event Handler',NodeNameToEdit,EventToEdit);
  ShowXForm('CodeEditForm',true);

end;

procedure TOIEventWrapper.OIEditEventCode(e:TEventStatus;nodeId:string;myValue:string);
var
  EditBoxName, EventCode:string;
  bits:TStringList;
  NodeNameToEdit,EventToEdit:String;
  targetNode:TDataNode;
begin
  bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
  if bits.Count = 4 then
  begin
    if (bits[0]='OI') or (bits[0]='RI') then       // OI, Editboxname, NodeName, suffix
    begin
      NodeNameToEdit:=bits[1];
      EventToEdit:=bits[2];
    end;
  end;
  bits.Free;
  // pop up the syntax editor.
  // remove the 'Btn' suffix
  EditBoxName:=Copy(nodeId,1,length(nodeId)-3);
  OIEditBox:=FindDataNodeById(UIRootNode,EditBoxName,'',true);
  EventCode:=TXEditBox(OIEditBox.ScreenObject).ItemValue;
  if Trim(EventCode)='' then
  begin
    // provide a template event procedure
    if (FoundString(EventToEdit,'Thread')=1)
    and (FoundString(EventToEdit,'ThreadVars')<>1) then
      EventCode:= DfltThreadEventCode(NodeNameToEdit)
    else if (EventToEdit='DropAccepted') then
      EventCode := DfltTreeNodeEventCode
    else
      EventCode:= DfltEventCode;
  end;
  CodeEditForm.CodeEdit.ItemValue := EventCode;
  targetNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true);
  CodeEditForm.CodeEditInit.ItemValue := targetNode.GetEventInitCode(EventToEdit);

  CodeEditForm.CodeEdit.MessageLines:='';
  CodeEditForm.CodeEditInit.MessageLines:='';

  CodeEditForm.Mode:='EventCode';
  CodeEditForm.CodeEditMainTabs.TabIndex:=0;
  CodeEditForm.InitialiseOnShow('Event Handler',NodeNameToEdit,EventToEdit);

  ShowXForm('CodeEditForm',true);

end;

procedure TOIEventWrapper.OIEditIntfEvent(e:TEventStatus;nodeId:string;myValue:string);
var
  bits:TStringList;
  targetNode:TDataNode;
  NodeNameToEdit,EventNameToEdit:String;
  EventToEdit:TEventHandlerRec;
begin
  bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
  if bits.Count = 4 then
  begin
    if (bits[0]='OI') or (bits[0]='RI') then       // OI, Editboxname, NodeName, suffix
    begin
      NodeNameToEdit:=bits[1];
      EventNameToEdit:=bits[2];
    end;
  end;
  bits.Free;
  targetNode:=FindDataNodeById(UIRootNode,NodeNameToEdit,'',true);
  if targetNode<>nil then
  begin
    EventToEdit:=targetNode.GetEvent(EventNameToEdit);
    if EventToEdit<>nil then
    begin
      IntfEventUnit.EventToEdit:=EventToEdit;
      IntfEventUnit.IntfEventForm.IntfEventName.ItemValue:=EventNameToEdit;
      IntfEventUnit.IntfEventForm.IntfEventName.ReadOnly:=true;
      if EventToEdit.ReadOnlyInterface=true then
        IntfEventUnit.IntfEventForm.IntfEventType.ItemValue:='Read-Only'
      else
        IntfEventUnit.IntfEventForm.IntfEventType.ItemValue:='Editable';
 //     IntfEventUnit.IntfEventForm.IntfEventType.Enabled:=false;
      IntfEventUnit.IntfEventForm.IntfEventHint.ItemValue:=EventToEdit.EventHint;
      ShowXForm('IntfEventForm',true);
    end;
  end;
end;


{$ifdef JScript}
procedure TOIEventWrapper.OIPasteTarget(e:TEventStatus;nodeId:string;myValue:string);
var
  i:integer;
  PasteEvent:TEventStatus;
begin
//  happens when user hits ctrl-V on the paste dialog form

  PasteEvent:=PasteDialogUnit.CompletionEvent;

  if PasteEvent<>nil then
  begin
    PasteEvent.ClearAsync('CopyFromClip');
  end;
  FinishHTMLPasteAction(myValue);

end;
{$endif}



function OITreeNodeHint(TreeLabelStr:String):String;
var
   SystemNodeName,p1:string;
begin
  //showmessage('OINodeTreeHint '+TreeLabelStr);
  SystemNodeName:=TreeLabelToID( TreeLabelStr,'NavTree',p1);
  //result := GetNavigatorHint(SystemNodeTree,SystemNodeName);
  result := GetNavigatorHint(UIRootNode,SystemNodeName);
end;

function OIResTreeNodeHint(TreeLabelStr:String):String;
var
   SystemNodeName,p1:string;
begin
  SystemNodeName:=TreeLabelToID( TreeLabelStr,'ResourceTree',p1);
  result := GetNavigatorHint(SystemNodeTree,SystemNodeName);
end;

procedure AddPropertyEditButton(BoxName:String; HBoxNode:TDataNode;CannotEdit:Boolean);
//                     HBox     <name>HB
//                       widget   <name>
//                       Button    <name>Btn
var
  NewBtn:TXButton;
begin
    NewBtn:=TXButton(AddDynamicWidget('TXButton',MainForm,HBoxNode,BoxName+'Btn','','Top',-1).ScreenObject);
    NewBtn.ButtonWidth:='30px';
    NewBtn.SpacingAround:=0;

    if CannotEdit then
    begin
      NewBtn.Caption:=' ';
      NewBtn.Enabled:=false
    end
    else
    begin
      NewBtn.Caption:='...';
      NewBtn.Hint:='Edit Property';
      NewBtn.myNode.RegisterEvent('ButtonClick',@OIEventWrapper.OIEditPropertyButtonClick);
    end;
end;

procedure AddPropertyDelButton(BoxName:String; HBoxNode:TDataNode);
//                     HBox     <name>HB
//                       widget   <name>
//                       editButton    <name>Btn
//                       delButton    <name>DelBtn
var
  NewBtn:TXButton;
begin
    NewBtn:=TXButton(AddDynamicWidget('TXButton',MainForm,HBoxNode,BoxName+'DelBtn','','Top',-1).ScreenObject);
    NewBtn.ButtonWidth:='30px';
    NewBtn.SpacingAround:=0;

    NewBtn.Caption:='Del';
    NewBtn.Hint:='Delete Property';
    NewBtn.myNode.RegisterEvent('ButtonClick',@OIEventWrapper.OIDelPropertyButtonClick);
end;

procedure AddEventDelButton(BoxName:String; HBoxNode:TDataNode);
//                     HBox     <name>HB
//                       widget   <name>
//                       editButton    <name>Btn
//                       delButton    <name>DelBtn
var
  NewBtn:TXButton;
begin
    NewBtn:=TXButton(AddDynamicWidget('TXButton',MainForm,HBoxNode,BoxName+'DelBtn','','Top',-1).ScreenObject);
    NewBtn.ButtonWidth:='30px';
    NewBtn.SpacingAround:=0;

    NewBtn.Caption:='Del';
    NewBtn.Hint:='Delete Event';
    NewBtn.myNode.RegisterEvent('ButtonClick',@OIEventWrapper.OIDelEventButtonClick);
end;

function AddPropertyContainer(ParentNode:TDataNode;BoxName:String; var VBoxNode,HBoxNode:TdataNode):TXHbox;
var
  NewHBox:TXHBox;
  NewVBox:TXVBox;
   //                   VBox     <name>VB
   //                     HBox     <name>HB
begin
  VBoxNode:=AddDynamicWidget('TXVBox',MainForm,ParentNode,BoxName+'VB','','Left',-1);
  NewVBox:=TXVBox(VBoxNode.ScreenObject);
  HBoxNode:=AddDynamicWidget('TXHBox',MainForm,VBoxNode,BoxName+'HB','','Right',-1);
  NewHBox:=TXHBox(HBoxNode.ScreenObject);

  NewVBox.Border:=false;
  NewVBox.ContainerHeight:='';
  NewVBox.ContainerWidth:='96%';
  NewHBox.Border:=false;
  NewHBox.ContainerHeight:='';
  NewHBox.ContainerWidth:='';

  NewHBox.Alignment:='Right';

  result:=NewHBox;
end;

procedure AddCheckBox(PropertiesNode,TargetNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean;attribHint:String);
var
  NewCheckBox:TXCheckBox;
  NewHBox:TXHBox;
  HBoxNode, VBoxNode:TDataNode;
begin
  NewHBox:=AddPropertyContainer(PropertiesNode,BoxName,VBoxNode,HBoxNode);
  NewCheckBox:=TXCheckBox(AddDynamicWidget('TXCheckBox',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  if (TargetNode.NodeClass<>'DM') then
    AddPropertyEditButton(BoxName,HBoxNode,ro);

  NewCheckBox.Checked:=myStrToBool(ItmValue);
  NewCheckBox.LabelText:=LblText;
  NewCheckBox.ReadOnly:=ro;
  NewCheckBox.LabelPos:='Left';
  NewCheckBox.Hint:=attribHint;

  NewHBox.Alignment:='Right';

  NewCheckBox.myNode.registerEvent('Click',@OIEventWrapper.OIEditProperty);
end;

procedure AddPropertyEditBox(ParentNode,TargetNode:TDataNode;BoxName,LblText,ItmValue:String;ro,CanDelete:Boolean;attribHint:String);
var
  NewEditBox:TXEditBox;
  VBoxNode,HBoxNode:TDataNode;
  NewHBox:TXHBox;
  CannotEdit:Boolean;
begin
  NewHBox:=AddPropertyContainer(ParentNode,BoxName,VBoxNode,HBoxNode);
  NewEditBox:=TXEditBox(AddDynamicWidget('TXEditBox',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  CannotEdit:=ro;
  if CanDelete then
    CannotEdit:=false;

  if (TargetNode.NodeClass<>'DM')
  or ((TargetNode.NodeType='DMOp') and (LblText='Code')) then
    AddPropertyEditButton(BoxName,HBoxNode,CannotEdit);
  if CanDelete then
    AddPropertyDelButton(BoxName,HBoxNode);

  NewEditBox.LabelText:=LblText;
  NewEditBox.BoxWidth:='120px';
  NewEditBox.ReadOnly:=ro;
  NewEditBox.LabelPos:='Left';
  NewEditBox.Hint:=attribHint;
  NewHBox.ContainerHeight:='';

  NewHBox.Alignment:='Right';     //!!!! why do I have to set this again here, for it to take effect??

  NewEditBox.ItemValue:=ItmValue;

  // event handler....
  // by default, the XEditBox has an onExit event handler that calls HandleEvent with an
  // event type of 'Change'.
  // HandleEvent will expect to find a function of type TEventHandler defined within the main or popup form, which
  // was created at design time in the Lazarus IDE.
  // The name of the expected function is <EditBoxName>HandleChange.
  // HOWEVER - Here, where the editbox is dynamically created,
  // have to register an event in the data node instead.
  NewEditBox.myNode.registerEvent('Change',@OIEventWrapper.OIEditProperty);
end;

procedure AddPropertyColorPicker(ParentNode,TargetNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean;IsResource:Boolean;attribHint:String);
var
  NewColorPicker:TXColorPicker;
  VBoxNode,HBoxNode:TDataNode;
  NewHBox:TXHBox;
begin
  NewHBox:=AddPropertyContainer(ParentNode,BoxName,VBoxNode,HBoxNode);
  NewHBox.ContainerHeight:='';
  NewColorPicker:=TXColorPicker(AddDynamicWidget('TXColorPicker',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  if (not IsResource)
  and (TargetNode.NodeClass<>'DM') then
    AddPropertyEditButton(BoxName,HBoxNode,ro);

  NewColorPicker.ItemValue:=ItmValue;
  NewColorPicker.LabelText:=LblText;
  NewColorPicker.BoxWidth:='120px';
  //NewColorPicker.ReadOnly:= ro;  //!!!! Do we need to use 'Enabled' instead? Sort out for Browser.
  NewColorPicker.LabelPos:='Left';
  NewColorPicker.Hint:=attribHint;

  NewHBox.Alignment:='Right';     //!!!! why do I have to set this again here, for it to take effect??

  // event handler....
  NewColorPicker.myNode.registerEvent('Change',@OIEventWrapper.OIEditProperty);
end;

procedure AddEventEditBox(ParentNode:TDataNode;BoxName,LblText,ItmValue,HintText:String;CanEditCode:Boolean;enableButton,CanDelete:Boolean);
var
  NewHBox:TXHBox;
  NewButton:TXButton;
  NewEditBox:TXEditBox;
  NewNode1,NewNode2:TDataNode;
    //                   VBox     <name>VB
    //                     HBox     <name>HB
    //                       EditBox   <name>
    //                       Button    <name>Btn
begin
  NewHBox:=AddPropertyContainer(ParentNode,BoxName,NewNode1,NewNode2);
  NewHBox.ContainerHeight:='';
  NewEditBox:=TXEditBox(AddDynamicWidget('TXEditBox',MainForm,NewNode2,BoxName,'','Top',-1).ScreenObject);

  begin
    NewButton:=TXButton(AddDynamicWidget('TXButton',MainForm,NewNode2,BoxName+'Btn','','Top',-1).ScreenObject);
    NewButton.Caption:='...';
    if enableButton then
    begin
      NewButton.Enabled:=true;
      if (CanDelete=false) or (CanEditCode=true) then
      begin
        NewButton.Hint:='Edit Event Code';
        NewButton.myNode.RegisterEvent('ButtonClick',@OIEventWrapper.OIEditEventCode);
      end
      else
      begin
        NewButton.Hint:='Edit Event Hint';
        NewButton.myNode.RegisterEvent('ButtonClick',@OIEventWrapper.OIEditIntfEvent);
      end;
    end
    else
    begin
      NewButton.Enabled:=false;
    end;
  end;

  NewEditBox.ItemValue:=ItmValue;
  NewEditBox.LabelText:=LblText;
  NewEditBox.BoxWidth:='100px';
  NewEditBox.LabelPos:='Left';
  NewEditBox.Hint:=HintText;
  NewEditBox.ReadOnly:=(not CanEditCode);


  if CanDelete then
    AddEventDelButton(BoxName,NewHBox.myNode);

  NewHBox.Alignment:='Right';

  // event handlers....
  NewEditBox.myNode.registerEvent('Change',@OIEventWrapper.OIEditEvent);
end;

procedure AddComboBox(PropertiesNode,TargetNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean;Options:TStringList;attribHint:String);
var
  NewComboBox:TXComboBox;
  NewHBox:TXHBox;
  VBoxNode,HBoxNode:TDataNode;
begin
  NewHBox:=AddPropertyContainer(PropertiesNode,BoxName,VBoxNode,HBoxNode);
  NewHBox.ContainerHeight:='';
  NewComboBox:=TXComboBox(AddDynamicWidget('TXComboBox',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  if (TargetNode.NodeClass<>'DM') then
    AddPropertyEditButton(BoxName,HBoxNode,ro);

  NewComboBox.LabelText:=LblText;
  NewComboBox.BoxWidth:='120px';
  //NewComboBox.ReadOnly:=ro;      //!!!! use Enabled instead?  Sort out for Browser.
  NewComboBox.LabelPos:='Left';
  NewComboBox.Alignment:='Right';
  NewComboBox.Hint:=attribHint;
  NewComboBox.OptionList:=StringListToJSONString(Options);

  NewComboBox.ItemValue:=ItmValue;  // this will set itemindex

  NewHBox.Alignment:='Right';

  NewComboBox.myNode.registerEvent('Change',@OIEventWrapper.OIEditProperty);
end;

procedure PopulateObjectInspector(CurrentNode:TDataNode);
var Prefix1,AttributePrefix:string;
  i:integer;
  MainPanel,ShowOn,IntfNode:TDataNode;
  IntfEvent:TEventHandlerRec;
  myAttribs:TNodeAttributesArray;
  ro:boolean;
  s:boolean;
  AttribOptions:TStringList;
  tabIndex,DisplayType,HintText:String;
  dfltAttrib:TDefaultAttribute;
  NodesList:TNodesArray;
begin
  s:=SuppressEvents;
  SuppressEvents:=true;

  EditAttributeValue(PropertiesNode,'IsVisible','false');
  DeleteNodeChildren(PropertiesNode);
  EditAttributeValue(InterfacePropsNode,'IsVisible','false');
  DeleteNodeChildren(InterfacePropsNode);
  EditAttributeValue(EventsNode,'IsVisible','false');
  DeleteNodeChildren(EventsNode);
  EditAttributeValue(DMAttribsNode,'IsVisible','false');
  DeleteNodeChildren(DMAttribsNode);
  MainPanel:=PropertiesNode;
  Prefix1:='OI';

  if (CurrentNode<>nil)  then
  begin
    if (CurrentNode.NodeClass = 'UI')
    or (CurrentNode.NodeClass = 'NV')
    or (CurrentNode.NodeClass = 'SVG')
    or (CurrentNode=UIRootNode) then
    begin
      tabIndex:=OITabs.GetAttribute('TabIndex',false).AttribValue;
      if tabIndex = '0' then
        DisplayType:='UIProps'
      else if tabIndex = '1' then
        DisplayType:='UIEvents'
      else if tabIndex = '2' then
        DisplayType:='Intf';
    end
    else if CurrentNode.NodeClass = 'DM' then
    begin
      DisplayType:='DM';
      MainPanel:=DMAttribsNode;
      Prefix1:='DM';
    end;

    if CurrentNode.NodeType='TXComposite' then
    begin
      IntfNode:=nil;
      NodesList:=FindNodesOfType(CurrentNode,'TXCompositeIntf',true,CurrentNode.NodeName);  // should be only 1 of these
      if length(NodesList)>0 then
        IntfNode:=NodesList[0];
    end;

    if (DisplayType='UIProps')
    or (DisplayType='Intf')
    or (DisplayType='DM')
    then
    begin
      if CurrentNode<>nil then
      begin
        AttributePrefix:=Prefix1+AttributeEditorNameDelimiter+CurrentNode.NodeName;

        AddPropertyEditBox(MainPanel,CurrentNode,AttributePrefix+AttributeEditorNameDelimiter
                                  +'Name'+AttributeEditorNameDelimiter
                                  +'0','Name',CurrentNode.NodeName,true,false,CurrentNode.NodeName);

        myAttribs:=CurrentNode.NodeAttributes;
        for i:=0 to length(myAttribs)-1 do
        if (CurrentNode.NodeAttributes[i]<>nil)
        and (CurrentNode.NodeAttributes[i].AttribName <> '') then
        begin
          dfltAttrib:=GetDefaultAttrib(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName);
          ro:=CurrentNode.NodeAttributes[i].AttribReadOnly;
          //exclude Suppressed properties that user shouldn't see
          if (FindSuppressedProperty(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName)<0)
          and (CurrentNode.NodeAttributes[i].AttribName<>'ParentName')
          and ((DisplayType<>'DM')
               or (CurrentNode.NodeAttributes[i].AttribName<>'MakeXArrays')
               or (CurrentNode.NodeName<>'Dimensions'))
          and ((DisplayType<>'DM')
               or (CurrentNode.NodeAttributes[i].AttribName<>'Multiplicity')
               or (CurrentNode.NodeType<>'DMAttrib')
               or ((CurrentNode.NodeType='DMAttrib') and (CurrentNode.NodeParent.GetAttribute('MakeXArrays',false).AttribValue='False')))
          then
          begin
            if (CurrentNode.NodeType<>'TXComposite')
            or (IsADefaultAttrib(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName)) then
            begin
              ShowOn:=MainPanel;
              if (CurrentNode.NodeType='TXCompositeIntf')
              or (dfltAttrib.AttribHint = '')
              then
                HintText:=CurrentNode.NodeAttributes[i].AttribHint
              else
                HintText:= dfltAttrib.AttribHint;
            end
            else
            begin   // is TXComposite, and no default hint
              ShowOn:=InterfacePropsNode;
              if (IntfNode<>nil)
              and (IntfNode.HasAttribute(CurrentNode.NodeAttributes[i].AttribName)) then
                HintText:=IntfNode.GetAttribute(CurrentNode.NodeAttributes[i].AttribName,false).AttribHint;
            end;

            if CurrentNode.NodeAttributes[i].AttribType = 'Boolean' then
            begin
              AddCheckBox(ShowOn,CurrentNode,AttributePrefix+AttributeEditorNameDelimiter
                      +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                      +IntToStr(i+2),
                       CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,HintText);
            end
            else
            begin
              AttribOptions:=LookupAttribOptions(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName);
              if AttribOptions<>nil then
              begin
                AddComboBox(ShowOn,CurrentNode,AttributePrefix+AttributeEditorNameDelimiter
                      +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                      +IntToStr(i+2),
                       CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,
                       AttribOptions,HintText);
              end
              else if CurrentNode.NodeAttributes[i].AttribType='Color' then
                AddPropertyColorPicker(ShowOn,CurrentNode,AttributePrefix+AttributeEditorNameDelimiter
                      +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                      +IntToStr(i+2),
                       CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,false,HintText)
              else
                AddPropertyEditBox(ShowOn,CurrentNode,AttributePrefix+AttributeEditorNameDelimiter
                    +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                    +IntToStr(i+2),
                     CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,
                     ro,(CurrentNode.NodeType='TXCompositeIntf'),
                     HintText);
            end;
          end;
        end;
        EditAttributeValue(MainPanel,'IsVisible','true');
        if CurrentNode.NodeType='TXComposite' then
          EditAttributeValue(InterfacePropsNode,'IsVisible','true');

      end;
    end
    else if DisplayType='UIEvents' then
    begin
      //------------------- Display the registered Events -----------------------

      if CurrentNode<>nil then
      if ((CurrentNode.IsDynamic) or (CurrentNode=UIRootNode)) then
      begin
        AttributePrefix:=Prefix1+AttributeEditorNameDelimiter+CurrentNode.NodeName;

        for i:=0 to CurrentNode.MyEventTypes.count-1 do
        begin
          HintText:= CurrentNode.myEventHandlers[i].EventHint;
          if (CurrentNode.NodeType='TXComposite')
          and (IntfNode<>nil) then
          begin
            // get the event hint from the underlying interface node
            IntfEvent:=IntfNode.GetEvent(CurrentNode.MyEventTypes[i]);
            if IntfEvent<>nil then
              HintText:= IntfEvent.EventHint;
          end;
          AddEventEditBox(EventsNode,AttributePrefix+AttributeEditorNameDelimiter //OI__nodename__
                            +CurrentNode.MyEventTypes[i]+AttributeEditorNameDelimiter   //eventtype__
                            +IntToStr(i+2),                                             //suffix
                             CurrentNode.MyEventTypes[i],
                             CurrentNode.myEventHandlers[i].TheCode,
                             HintText,
                             //CanEditCode...
                             (((CurrentNode.NodeType<>'TXComposite') and (CurrentNode.NodeType<>'TXCompositeIntf'))
                             or ((CurrentNode.NodeType='TXCompositeIntf') and (CurrentNode.myEventHandlers[i].ReadOnlyInterface=true))
                             or ((CurrentNode.NodeType='TXComposite') and (CurrentNode.myEventHandlers[i].ReadOnlyInterface=false))
                             ),
                             //EnableButton...
                             ((CurrentNode.NodeType<>'TXComposite')
                             or ((CurrentNode.NodeType='TXComposite') and (CurrentNode.myEventHandlers[i].ReadOnlyInterface=false))
                             ),
                             //CanDelete...
                             (CurrentNode.NodeType='TXCompositeIntf')
                             );

        end;
      end;
      EditAttributeValue(EventsNode,'IsVisible','true');
    end;

    if CurrentNode.NodeType='TXComposite' then
      EditAttributeValue(InterfaceTabNode,'IsVisible','true')
    else
      EditAttributeValue(InterfaceTabNode,'IsVisible','false');
    if (CurrentNode.NodeType='TXCompositeIntf') then
      EditAttributeValue(AddInterfacePropsButtonNode,'IsVisible','true')
    else
      EditAttributeValue(AddInterfacePropsButtonNode,'IsVisible','false');
  end;

  SuppressEvents:=s;

end;

procedure OIPropsEventsTabChange;
begin
  PopulateObjectInspector(ObjectInspectorSelectedNavTreeNode);
end;


procedure PopulateResourceInspector(CurrentNode:TDataNode);
var AttributePrefix:string;
  i:integer;
  PropertiesNode,btnNode:TDataNode;
  myAttribs:TNodeAttributesArray;
  ro:boolean;
  s:boolean;
begin
  s:=SuppressEvents;
  SuppressEvents:=true;

  if (CurrentNode<>nil)
  and ((CurrentNode.NodeClass='RSS')
    or (CurrentNode.NodeType='TXComposite')) then
  begin
    btnNode:=FindDataNodeById(UIRootNode,'ResourceTreeDelBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=true;
    btnNode:=FindDataNodeById(UIRootNode,'ResourceTreeLoadBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=true;
  end
  else
  begin
    btnNode:=FindDataNodeById(UIRootNode,'ResourceTreeDelBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=false;
    btnNode:=FindDataNodeById(UIRootNode,'ResourceTreeLoadBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=false;
  end;

  SuppressEvents:=s;
end;

procedure OIAddCodeUnitNode(UnitType:String);
var
  NewName,UnitCode:String;
  NewNode:TDataNode;
begin
  // create a data node for the new unit, and add under CodeRootNode.
  // Dialog for name entry
  NewName:=GetComponentName('Enter Unit Name:');

  // Is the source node named uniquely?
  if (NewName<>'') and (ComponentNameIsUnique(NewName,'')) then
  begin
    NewNode:=TDataNode.Create('Code',NewName,'',UnitType,true);
    NewNode.NodeName:=NewName;
    UnitCode:= DfltUnitCode(NewName,UnitType);
    NewNode.SetAttributeValue('Code',UnitCode,'String');

    ObjectInspectorSelectedCodeTreeNode:=InsertSystemNode(CodeRootNode,NewNode,-1);
    OISelectedCodeProcName:='';
    ObjectInspectorSelectedCodeNodeText:='';

    // rebuild the code tree
    RebuildCodeTree;

  end;
end;

procedure OIEditCodeUnit;
var
  UnitCode, EventType:string;
  tmp,NodeNameToEdit:String;
  bits:TStringList;
  //AllKernels:TAnimCodeArray;
begin
  if ObjectInspectorSelectedCodeTreeNode<>nil then
  begin
    if (ObjectInspectorSelectedCodeTreeNode.NodeType='PasUnit')
    or (ObjectInspectorSelectedCodeTreeNode.NodeType='PythonScript')
    then
    begin
      NodeNameToEdit:=ObjectInspectorSelectedCodeTreeNode.NodeName;

      // pop up the syntax editor.
      UnitCode:=ObjectInspectorSelectedCodeTreeNode.GetAttribute('Code',true).AttribValue;
      if UnitCode='' then
      begin
        // provide a template unit
        if ObjectInspectorSelectedCodeTreeNode.NodeType='PasUnit' then
          UnitCode:= DfltUnitCode(NodeNameToEdit,'PasUnit')
        else if ObjectInspectorSelectedCodeTreeNode.NodeType='PythonScript' then
          UnitCode:= DfltPythonCode;
      end;
      CodeEditForm.CodeEdit.ItemValue := UnitCode;
      CodeEditForm.CodeEdit.MessageLines:='';

      CodeEditForm.Mode:=ObjectInspectorSelectedCodeTreeNode.NodeType+'Code';
      CodeEditForm.InitialiseOnShow(ObjectInspectorSelectedCodeTreeNode.NodeType,NodeNameToEdit,'');
      {$ifdef JScript}
      ShowXForm('CodeEditForm',true);
      {$endif}
      if ObjectInspectorSelectedCodeTreeNode.NodeType='PasUnit' then
      begin
        CodeEditForm.SetCursorPosition(OISelectedCodeLineNum,1);
      end;
      {$ifndef JScript}
      ShowXForm('CodeEditForm',true);
      {$endif}

    end
    else
    if (ObjectInspectorSelectedCodeTreeNode.NodeClass='UI')
    or (ObjectInspectorSelectedCodeTreeNode.NodeClass='NV')
    or (ObjectInspectorSelectedCodeTreeNode.NodeClass='SVG')
    or ((ObjectInspectorSelectedCodeTreeNode.NodeClass='Root')
      and (ObjectInspectorSelectedCodeTreeNode.NodeName=SystemRootName))
    then
    begin
      // selected node is UI so we are looking at some event handler code
      // OR Selected node is the UI Root so we are looking at one of the 'system' level events
      // navigate to the relevant UI element
      DoSelectNavTreeNode(ObjectInspectorSelectedCodeTreeNode,false);
      // Find which event to edit...
      // delete to first )
      tmp:=ObjectInspectorSelectedCodeNodeText;
      bits:=StringSplit(tmp,')');
      if bits.Count=2 then
      begin
        // event follows the space
        EventType:=TrimWhiteSpace(bits[1]);
        if EventType<>'' then
          // Pop up the event code editor
          OIEventWrapper.OIEditEventCodeFromCodeTree(ObjectInspectorSelectedCodeTreeNode.Nodename,EventType)
        else if (ObjectInspectorSelectedCodeTreeNode.NodeType='TXGPUCanvas')
        then
        begin
          ShowGPUEditor(ObjectInspectorSelectedCodeTreeNode,0);
        end;
      end;
    end
    else
      showmessage('Select a Code Node first - selected node is not an appropriate type');

  end
  else
  begin
    showmessage('Select a Code Node first');
  end;
end;

procedure OICodeSearch;
begin
  // pop up the syntax editor.
  CodeEditForm.CodeEdit.ItemValue := '';
  CodeEditForm.CodeEdit.MessageLines:='';

  CodeEditForm.Mode:='SearchCode';
  if (ObjectInspectorSelectedCodeTreeNode<>nil)
  and (ObjectInspectorSelectedCodeTreeNode.NodeType='PasUnit') then
  begin
    CodeEditForm.CodeEditFindTxt.ItemValue:=OISelectedCodeProcName;
  end;
  CodeEditForm.InitialiseOnShow('Search Results','','');
  ShowXForm('CodeEditForm',true);
  {$ifdef JScript}
  CodeEditForm.CodeEditFindTxt.HasFocus:=true;
  {$endif}
end;

procedure OIDeleteCodeUnit;
begin
  if ObjectInspectorSelectedCodeTreeNode<>nil then
  begin
    if  (ObjectInspectorSelectedCodeTreeNode.NodeType='PasUnit')
    or  (ObjectInspectorSelectedCodeTreeNode.NodeType='PythonScript')
    then
    begin
      if XIDEConfirm('OK to delete '+ObjectInspectorSelectedCodeTreeNode.NodeName+'?') then
      begin
        DeleteItem(CodeRootNode,ObjectInspectorSelectedCodeTreeNode);
        OISelectedCodeProcName:='';
      end;
    end
    else
      if  (ObjectInspectorSelectedCodeTreeNode.NodeType='Root') then
        showmessage('Select a Code Node (Unit or Function) first')
      else
        showmessage('Cannot delete event code here');
  end
  else
  begin
    showmessage('Select a Code Node (Unit or Function) first');
  end;
end;

Procedure OIEncapsulate;
var
  fullstring,itemName,PromptString,DefaultString:String;
begin
  // Take the current user-defined system, and store its structure in the resource tree under 'Composites', with
  // a name provided by the user, and save to local storage.
  DefaultString:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  if DefaultString='XIDESystem' then
    DefaultString:='';
  if pos('_xcmp',DefaultString)=length(DefaultString)-4 then
  begin
    Delete(DefaultString,length(DefaultString)-4,5);
    PromptString:='Enter Name of Composite Item:';
  end
  else
    PromptString:='Enter Name of new Composite Item:';
  itemName:=GetValidItemName(PromptString,DefaultString );
  if itemName='' then
    EXIT;

  fullstring:=BuildSystemString(true);
  itemName:=itemName+'.xcmp';
  {$ifndef JScript}
  itemName:='SavedSystems/'+itemName;
  {$endif}
  WriteToLocalStore(itemName,fullstring);

  RebuildResourcesTree;

end;

procedure OIAddInterfaceElement;
var
  myIntf:TXCompositeIntf;
  NewName,tabIndex:String;
  ok,IsReadOnly,tf:Boolean;
begin
  // for a TXCompositeIntf item, user can add interface properties, or bespoke events.
  if (ObjectInspectorSelectedNavTreeNode=nil)
  or (ObjectInspectorSelectedNavTreeNode.NodeType<>'TXCompositeIntf') then
    showmessage('Please select a Composite Interface (TXCompositeIntf) element first')
  else
  begin
    myIntf:=TXCompositeIntf(ObjectInspectorSelectedNavTreeNode.ScreenObject);

    tabIndex:=OITabs.GetAttribute('TabIndex',false).AttribValue;

    if tabIndex='0' then
    begin

      // create a new attribute/property for the selected node
      ok:=false;
      // Dialog for name and type entry
      IntfParamUnit.InterfaceElement := myIntf;
      IntfParamUnit.ClearForm;
      ShowXForm('IntfParamForm',true);

    end
    else
    begin
      // create a new event for the selected node
      ok:=false;
      IntfEventUnit.InterfaceElement := myIntf;
      IntfEventUnit.ClearForm;
      ShowXForm('IntfEventForm',true);

    end;
  end;
end;

{$ifdef JScript}

function CheckForSavedSystemOnLoad:Boolean;
var
  ok:boolean;
  sysname,tempstr:String;
begin
//   showmessage('not running from Laz, so loading last system description');
   //look for a system description with the same name as the one we just loaded.
   sysname:=UIRootNode.getAttribute('SystemName',false).AttribValue;
//    showmessage('looking for saved system XIDESavedData'+sysname);
   tempstr:=trim(ReadFromLocalStore('XIDESavedData'+sysname));
   //showmessage(tempstr);
   if tempstr<>'' then
   begin
     //showmessage('found '+sysname);
     ok:=StringUtils.confirm('Press OK to resume your previous "'+sysname+'" session, or Cancel to continue with the basic load');
     if ok then
     begin
       ok:=DoSystemLoad(tempstr,sysname);
       if ok then
       begin
         RebuildNavigatorTree;
         RebuildCodeTree;
       end
       else
       begin
         //showmessage('load failed');
         OIClearSystem;
       end;
     end
     else ok:=true;
   end;
   result:=ok;
end;
{$endif}



begin
  AddAttribOptions('Root','DeploymentMode',DeploymentModeOptions);
  AddAttribOptions('Root','ShowResources',ShowResourcesOptions);

  DesignMode:=true;
  RunModeAttempts:=0;
  OIEventWrapper := TOIEventWrapper.Create;
  ObjectInspectorSelectedNavTreeNode:=nil;
  ObjectInspectorSelectedCodeTreeNode:=nil;
  OISelectedCodeProcName:='';
  OISelectedCodeLineNum:=1;
  AvailableResourcesSelectedNode:=nil;
  ObjectInspectorSelectedCodeNodeText:='';
  {$ifndef JScript}
  GlobalSuppressFrameDisplay:=true;
  {$endif}
  AdditionalScript:=
     '<script> ' +LineEnding

     +'var glbTimeoutWaiting=false;  '  +LineEnding
     +'var jobId=0;     '  +LineEnding
     +'var JobsWaiting=[];  '  +LineEnding
     +'var db; '+LineEnding

     +'function runFn(fn,ms,fname,job,...args) {  '  +LineEnding
     +'  var ars='''';  '  +LineEnding
     +'  for (var i = 2; i < arguments.length; i++) {ars=ars+'' ''+arguments[i];} '  +LineEnding
  //   +'  console.log(''running ''+fname+'' : ''+ars); '  +LineEnding
     +'  fn(...args);  '  +LineEnding
     +'  glbTimeoutWaiting=false;  '  +LineEnding
     +'  tryNextJob();  '  +LineEnding
     +'}                           '  +LineEnding
     +'function tryNextJob() { '  +LineEnding
     +'  if (JobsWaiting.length==0) return; '  +LineEnding
     +'  if (glbTimeoutWaiting==false) '+LineEnding
     +'    {  '  +LineEnding
     +'    var nextjob=JobsWaiting.shift();  '  +LineEnding
     +'    job=nextjob.jobid;  '  +LineEnding
     +'    glbTimeoutWaiting=true;  '  +LineEnding
  //   +'    console.log(''Queueing ''+nextjob.fname);  '  +LineEnding
  //   +'    console.log(nextjob); '  +LineEnding
     +'    setTimeout(runFn,nextjob.msec,   ...nextjob.args);    //args===[fn,msec,fname,job,...args]   '  +LineEnding
     +'    }   '  +LineEnding
     +'  else {   '  +LineEnding
     +'    setTimeout(tryNextJob,100);  '  +LineEnding
     +'    }  '  +LineEnding
     +'}  '  +LineEnding
     +'function myTimeout(fn,msec,fname,job,...args) {  '  +LineEnding
     +'  var ars='''';   '  +LineEnding
     +'  for (var i = 3; i < arguments.length; i++) {ars=ars+'' ''+arguments[i];}   '  +LineEnding
     +'    jobId=jobId+1;  '  +LineEnding
     +'    if (jobId>30000) jobId=1;  '  +LineEnding
     +'    job=jobId;  '  +LineEnding
  //   +'    console.log(''myTimeout ***** new job ''+job+'' ''+fname);   '  +LineEnding
     +'    JobsWaiting.push({jobid:job, func:fn, msec:msec, fname:fname, args:arguments});  '  +LineEnding
     +'    tryNextJob();  '  +LineEnding
     +'  return job;     '  +LineEnding
     +'}  '  +LineEnding

     +'</script>';

end.

