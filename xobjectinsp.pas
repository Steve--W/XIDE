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
  Classes, SysUtils, TypInfo, Stringutils, NodeUtils, Events, PopupMemo,
{$ifndef JScript}
  LazsUtils, Menus, DynLibs,
  Controls,PropEdits, ExtCtrls, Dialogs,Forms,CompilerLogUnit,
{$else}
  HTMLUtils,
{$endif}
  UtilsJSCompile,XIFrame, XSVGContainer,
  WrapperPanel,  CompileUserCode,
  XHBox, XVBox, XCode,XColorPicker,
  XTree,  XButton, XScrollBox, XEditBox, XCheckBox, XComboBox, XTabControl,
  XForm, XTable, XMemo, XMenu, CodeEditor, PropertyEditUnit, EventsInterface,
  XGPUCanvas, XGPUEditor, XCompositeIntf, StylesUtils, EventLogging;

{$ifdef JScript}
procedure ShowXFormForDesign(XFormName:String);
procedure CompleteToggleToRunMode(ok:boolean);
procedure BrowserSaveData(TheData:String);
procedure ContinueToggleToRunMode;
procedure CompleteDeploySystem(deployname:String);
procedure InitialiseComposites;
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
    procedure TestButtonClick(e:TEventStatus;nodeId:string;myValue:string);
    procedure OIEditPropertyButtonClick(e:TEventStatus;nodeId:string;myValue:string);
    {$ifdef JScript}
    procedure OIPasteTarget(e:TEventStatus;nodeId:string;myValue:string);
    {$endif}
  end;

procedure DebugWriteNodeTree(StartNode:TdataNode;var txt:String;lvl:integer);
procedure InitialiseXIDE;
//procedure InitialiseComposites;
function XIDEConfirm(promptString:String):boolean;
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
function OIDeleteItem(NodeId,NameSpace:String;ShowNotFoundMsg:Boolean=true):Boolean;
procedure OIComponentCopy(nodeId:string;myValue:string);
procedure OISystemLoad(e:TEventStatus;nodeId:string);
procedure OIClearSystem;
procedure OIDeploySystem;
function OITreeNodeHint(TreeLabelStr:String):String;
procedure PopulateObjectInspector(CurrentNode:TDataNode);
procedure OIPropsEventsTabChange;
function PopulateResourceInspector(CurrentNode:TDataNode):String;
function TreeLabelToID(TreeLabelStr:String):String;
function GetNavigatorHint(InTree:TDataNode;SystemNodeName:String):String;
function CompositeResourcesString(QuotedString:Boolean):String;
function BuildSystemString(Encapsulate:Boolean):String;
Procedure SaveSystemToClip;
Procedure SaveSystemToFile;
procedure ClearResourceInspector;
procedure ClearInspectors;
//procedure OILoadSavedSystem;
procedure OILoadSavedSystem2(SysName:String);
function DoSystemLoad(SystemDescription,SysName:string):Boolean;
procedure CodeEditorClosed(EditBoxNode:TdataNode);
procedure PropertyEditorClosed(EditBoxNode:TdataNode);
function isValidSystemData(SystemDescription:string):boolean;
procedure EditEventCode(NodeNameToEdit,EventToEdit,MainCode,InitCode:String);
procedure OIAddCodeUnitNode(UnitType:String);
procedure OIAddCodeFuncNode;
procedure OIEditCodeUnit;
procedure OIDeleteCodeUnit;
procedure DoToggleDesignRunMode(Sender:TXMenuItem);
procedure DisplayDllCompileErrors;
function OINavTreeAllowDrop(DstNode:TDataNode):Boolean;
procedure SelectNavTreeNode(CurrentNode:TDataNode; refresh:boolean);
Function ConstructSystemTreeString(CurrentItem:TDataNode; level:Integer; IncludeTypes,IncludeProperties:Boolean;
                                   Exclusions:TStringList;PropType:String):String;
//function   DfltEventCode:string;
procedure OIMoveItem(nodeId,NameSpace:string;NewParentId:string);
procedure OICopyToNewParent(nodeId,NameSpace:string;NewParentId:string;NewName:String);  overload;
{$ifndef JScript}
procedure SaveCompositesToIncFile;
procedure OICopyToNewParent(nodeId,NameSpace:string;NewParentId:string;NewName:PChar);  overload;
{$else}
procedure InitAutomatedCursor;
{$endif}
procedure OIMoveNavSiblingUpDown(UpDown:String);
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


const
  ResourceDataRootName=  'ResourceRoot' ;
  CodeDataRootName=  'dllRoot' ;
  UIProjectRootName = 'UIRoot';                    //the scrollbox containing the user-designed (mainform) system
  PropertyEditorScrollboxName:string = 'PropertyEditorScrollbox';
  EventsEditorScrollboxName:string = 'EventsEditorScrollbox';
  ResourceEditorScrollboxName:string = 'ResourceEditorScrollbox';
  DeploymentModeOptions:Array[0..1] of string = ('Design','Run');
  ShowResourcesOptions:Array[0..1] of string = ('Left','Right');


var
  MainFormProjectRoot,UIRootItem:TDataNode;
  DesignMode:Boolean;
  NavTreeComponent,ResourcesNodeTree, ResourceTreeComponent, CodeTreeComponent:TDataNode;
  OIEventWrapper:TOIEventWrapper;
  LastLazUserInterfaceItemSelected,LastHTMLUserInterfaceItemSelected:String;
  LastHTMLUserInterfaceItemHadBorder:Boolean;
  TreeInFocus, ObjectInspectorSelectedNavTreeNode, ObjectInspectorSelectedCodeTreeNode, ObjectInspectorSourceNode:TDataNode;

  {$ifndef JScript}
  UITopControl:TWinControl;
  {$else}
  RunningDeployedRuntime:Boolean;
  CompositesString:String;
  {$endif}


implementation
uses PasteDialogUnit, XIDEMain;

const
  AttributeEditorNameDelimiter:string = '__';

  //SandraMode:Boolean = true;

var
  ObjectInspectorSelectedCodeNodeText:String;
  AvailableResourcesSelectedNode:TDataNode;
  ObjectInspectorSourceCut:Boolean;

type
  TCompositeResource = record
    ResourceName:String;
    SourceString:String;
  end;
//var
//  ListOfComposites:Array of TCompositeResource;

function XIDEConfirm(promptString:String):boolean;
var
  ok:boolean;
  macroevent:TMacroEvent;
begin
  if not EventLogging.MacroEventList.Replaying then
    ok:=confirm(promptString)
  else
  begin
    macroevent:=EventLogging.AdvanceEventLog;
    if macroevent.EventType<>'UserConfirm' then
    begin
      showmessage('unable to replay user confirmation...');
      ok:=confirm(promptString);
    end
    else
    begin
      ok:=myStrToBool(macroevent.eventvalue);
      if ok then showmessage('User confirmed OK to:'+LineEnding+promptString)
      else showmessage('User confirmed Cancel to:'+LineEnding+promptString);
    end;
  end;

  result:=ok;
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
//    {$ifdef JScript}
//    if CurrentItem.NodeClass='SVG' then
//    asm
//      console.log(CurrentItem.NodeName+' '+CurrentItem.NodeType+' '+CurrentItem.IsDynamic);
//    end;
//    {$endif}
    if (CurrentItem.NameSpace='')
    and (
    ((CurrentItem.NodeClass='UI') and (CurrentItem.IsDynamic=true))
    or ((CurrentItem.NodeClass='SVG') and (CurrentItem.IsDynamic=true))
    or ((CurrentItem.NodeClass='UI') and (CurrentItem.NodeType='TXForm') and (CurrentItem.NodeName = MainForm.Name))
    or ((CurrentItem.NodeClass='UI') and (CurrentItem.NodeType='TXMainMenu'))
    or ((CurrentItem.NodeClass='NV') and (CurrentItem.IsDynamic=true))
    or (CurrentItem=SystemNodeTree)
    or (CurrentItem=ResourcesNodeTree)
    or (CurrentItem=CodeRootNode)
    or (CurrentItem=UIRootNode)
    or (CurrentItem=UIRootItem)
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
        and ((CurrentItem.NodeClass='UI') or (CurrentItem.NodeClass='NV'))
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

          ArrayString:=ArrayString+ConstructSystemTreeString(UIRootitem,level+1,IncludeTypes,IncludeProperties,Exclusions,PropType);

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


function TreeLabelToID(TreeLabelStr:String):String;
//e.g. "EditBox(MySimpleID)"  =>  "MySimpleID"
// or  "EditBox(MySimpleID) Click"  =>  "MySimpleID"
var i,ln:integer;
  newstring,tempstr,TreeLabel2:String;
  StartOfIdFound:boolean;
begin
  TreeLabel2:=TreeLabelStr;
  StartOfIdFound:=false;
  newstring:='';
  ln:=length(TreeLabel2);
  i:=1;
  while i< ln+1 do
  begin
    tempstr:=TreeLabel2[i];
    if (StartOfIdFound=true) and  (tempstr<>')') then
      newstring:=newstring+tempstr;
    if tempstr='(' then
      StartOfIdFound:=true;
    if tempstr=')' then
      i:=ln;
    i:=i+1;
  end;
  if StartOfIdFound=false then // no brackets
    newstring:=TreeLabelStr;
  newstring:=myStringReplace( newstring,'Contents','',999,999);
  result :=  trim(newstring);
end;


Function ConstructEventsTreeString(CurrentItem:TDataNode;var e:integer):String;
// Recursive
var ArrayString,dflt,tmp:String;
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
var ArrayString,dflt,tmp:String;
    i,numchildren:integer;
begin
  dflt:=DfltEventCode;
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
    foundattrib:=SystemNode.GetAttribute('Hint',true);
    if foundattrib.AttribName='Hint' then
      result:=foundattrib.AttribValue;
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
    ResourcesNodeTree.AddAttribute('ParentName','String', SystemRootName,true);
    // This represents the set of library 'resource' elements that are available in the resources tree
  end;
end;

function RegisterResource(ClassName,ScreenObjectType,ScreenObjectName,ParentName:string; position:integer;
                          Attribs:TNodeAttributesArray):TDataNode;  overload;
 var
      dfltAttribs:TDefaultAttributesArray;
      myAttribs:TNodeAttributesArray;
      myparent,myself:TDataNode;
      n,i:integer;
      TempChildNodes:TChildNodesArray;
  begin

    myparent:= FindDataNodeById(ResourcesNodeTree,ParentName,'',true);
    if myparent<>nil
    then
    begin
      dfltAttribs:=GetDefaultAttribs(ScreenObjectType);
      setlength(myAttribs,length(dfltAttribs));
      for i:=0 to length(dfltAttribs)-1 do
      begin
        myAttribs[i].AttribName:=dfltAttribs[i].AttribName;
        myAttribs[i].AttribType:=dfltAttribs[i].AttribType;
        myAttribs[i].AttribReadOnly:=dfltAttribs[i].AttribReadOnly;
        myAttribs[i].AttribValue:=dfltAttribs[i].AttribValue;
      end;

      myself:=AddChildToDataNode(myparent,ClassName,ScreenObjectName,ScreenObjectType,'',myAttribs,position);

      if myself<>nil then
      begin
       // mySelf.AddAttribute('Hint','String','',false);
        mySelf.AddAttribute('ParentName', ParentName,'String',true);

        // add or update the given attributes
        for i:=0 to length(Attribs)-1 do
        begin
          mySelf.SetAttributeValue(Attribs[i].AttribName,Attribs[i].AttribValue,Attribs[i].AttribType);
        end;
        mySelf.SetAttributeValue('ParentName', ParentName,'String');

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
  setlength(emptyAttribs,0);

  ResourcesNodeTree:=AddChildToDataNode(SystemNodeTree,'ResourceRoot',ResourceDataRootName,'','',emptyAttribs,-1);
  ResourcesNodeTree.AddAttribute('ParentName','String', SystemRootName,true);
  RegisterResource('RUI','','Composites','ResourceRoot',-1,emptyAttribs);
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
  //showmessage('saving system '+sysname);
  asm
   //alert('saving local XIDESavedData'+sysname);
   pas.HTMLUtils.WriteToLocalStore("XIDESavedData"+sysname,TheData);
  end;
end;
{$endif}

procedure InitialiseXIDE;
begin

  InitResourcesNodeTree;
  RebuildResourcesTree;
  // Populate navigator and code tree from SystemNodeTree
  RebuildNavigatorTree;
  RebuildCodeTree;

  {$ifdef JScript}
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
  composites:=FindDataNodeById(SystemNodetree,'Composites','',false);
  if (composites<>nil)
  and (CompositesString<>'</>') then
    XMLToNodeTree(CompositesString,composites,false);
  for i:=0 to length(composites.ChildNodes)-1 do
  begin
    itemName:=composites.ChildNodes[i].NodeName;
    // these names all have a _xcmp suffix.
//    itemName:=Copy(itemName,1,length(itemName)-5);
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
  composites:=FindDataNodeById(SystemNodetree,'Composites','',false);
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
      var object = JSON.parse(localStorage.getItem(namesArray[n]));
      ts = '';
      if ((object!=null)&&(suffix!='xcmp')) {
        if ( object.hasOwnProperty('timestamp') ) {
        var tsd = new Date(object.timestamp);
        ts = '      '+tsd.toLocaleDateString()+' '+tsd.toLocaleTimeString();
        }
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
  AttrParams:TNodeAttributesArray;
  NamesList:TStringList;
  i:integer;
  str:String;
begin

    ClearAttribs(AttrParams);
    RegisterResource('RUI','','UIComponents',ResourceDataRootName,-1,AttrParams);
    // Set up the tree of available components

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','','Layout','UIComponents',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'ContainerWidth','String','100',false);
     AddAttrib(AttrParams,'ContainerHeight','String','100',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     AddAttrib(AttrParams,'Border','Boolean','True',false);
     RegisterResource('RUI','TXVBox','TXVBox','Layout',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'ContainerWidth','String','',false);
     AddAttrib(AttrParams,'ContainerHeight','String','30',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     AddAttrib(AttrParams,'Border','Boolean','True',false);
     RegisterResource('RUI','TXHBox','TXHBox','Layout',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'ContainerWidth','String','300',false);
     AddAttrib(AttrParams,'ContainerHeight','String','200',false);
     AddAttrib(AttrParams,'Hint','String','Example hint: This is a group box',false);
     RegisterResource('RUI','TXGroupBox','TXGroupBox','Layout',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'ContainerWidth','String','100',false);
     AddAttrib(AttrParams,'ContainerHeight','String','100',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     AddAttrib(AttrParams,'Border','Boolean','True',false);
     AddAttrib(AttrParams,'ScrollType','String','Right',false);
     RegisterResource('RUI','TXScrollBox','TXScrollBox','Layout',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'ContainerWidth','String','200',false);
     AddAttrib(AttrParams,'ContainerHeight','String','200',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXTabControl','TXTabControl','Layout',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXTabSheet','TXTabSheet','Layout',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Width','Integer','600',false);
     AddAttrib(AttrParams,'Height','Integer','500',false);
     AddAttrib(AttrParams,'Caption','String','My Title',false);
     AddAttrib(AttrParams,'Top','Integer','50',false);
     AddAttrib(AttrParams,'Left','Integer','50',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXForm','TXForm','Layout',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','','Numeric','UIComponents',-1,AttrParams);

    ClearAttribs(AttrParams);
    ClearAttribs(AttrParams);
    AddAttrib(AttrParams,'MaxVal','Integer','100',false);
    AddAttrib(AttrParams,'LabelText','String','ProgressBar',false);
    AddAttrib(AttrParams,'ItemValue','Integer','5',false);
    AddAttrib(AttrParams,'BarWidth','String','200',false);
    AddAttrib(AttrParams,'Hint','String','',false);
    RegisterResource('RUI','TXProgressBar','TXProgressBar','Numeric',-1,AttrParams);

    ClearAttribs(AttrParams);
    AddAttrib(AttrParams,'MinVal','Integer','0',false);
    AddAttrib(AttrParams,'MaxVal','Integer','100',false);
    AddAttrib(AttrParams,'LabelText','String','Number Slider',false);
    AddAttrib(AttrParams,'ItemValue','Integer','5',false);
    AddAttrib(AttrParams,'BarWidth','String','200',false);
    AddAttrib(AttrParams,'Hint','String','',false);
    RegisterResource('RUI','TXNumericSlider','TXNumericSlider','Numeric',-1,AttrParams);

    ClearAttribs(AttrParams);
    AddAttrib(AttrParams,'MinVal','Integer','22',false);
    AddAttrib(AttrParams,'MaxVal','Integer','44',false);
    AddAttrib(AttrParams,'LabelText','String','NumberField',false);
    AddAttrib(AttrParams,'ItemValue','String','22',false);
    AddAttrib(AttrParams,'StepSize','Real','1',false);
    AddAttrib(AttrParams,'Hint','String','',false);
    RegisterResource('RUI','TXNumberSpinner','TXNumberSpinner','Numeric',-1,AttrParams);


     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','','Text','UIComponents',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'LabelCaption','String','caption',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXLabel','TXLabel','Text',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'LabelCaption','String','Hyperlink BBC News',false);
     AddAttrib(AttrParams,'URL','String','http://www.bbc.co.uk/news',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXHyperlink','TXHyperlink','Text',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'ItemValue','String','',false);
     AddAttrib(AttrParams,'LabelText','String','EditBox',false);
     AddAttrib(AttrParams,'ReadOnly','Boolean','False',false);
     AddAttrib(AttrParams,'ContainerWidth','','',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXEditBox','TXEditBox','Text',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'MemoHeight','String','200',false);
     AddAttrib(AttrParams,'MemoWidth','String','500',false);
     AddAttrib(AttrParams,'LabelText','String','Multi-Line EditBox',false);
     AddAttrib(AttrParams,'ItemValue','String','',false);
     AddAttrib(AttrParams,'ReadOnly','Boolean','False',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXMemo','TXMemo','Text',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'TableHeight','String','200',false);
     AddAttrib(AttrParams,'TableWidth','String','400',false);
     AddAttrib(AttrParams,'ColWidth','Integer','40',false);
     AddAttrib(AttrParams,'LabelText','String','',false);
     AddAttrib(AttrParams,'TableData','TableString','[["a","b","c"],["1","2","3"]]',false);
     AddAttrib(AttrParams,'ReadOnly','Boolean','False',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXTable','TXTable','Text',-1,AttrParams);


     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','','Selectors','UIComponents',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'ButtonWidth','String','',false);
     AddAttrib(AttrParams,'Caption','String','Press Me',false);
     AddAttrib(AttrParams,'Enabled','Boolean','True',false);
     RegisterResource('RUI','TXButton','TXButton','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Checked','Boolean','false',false);
     AddAttrib(AttrParams,'LabelText','String','checkbox label',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     AddAttrib(AttrParams,'Enabled','Boolean','True',false);
     RegisterResource('RUI','TXCheckBox','TXCheckBox','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
    // AddAttrib(AttrParams,'OptionList','String',ExampleOptionList,false);
     AddAttrib(AttrParams,'ItemValue','String','',false);
     AddAttrib(AttrParams,'Caption','String','radio buttons',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     AddAttrib(AttrParams,'ReadOnly','Boolean','False',false);
     RegisterResource('RUI','TXRadioBtns','TXRadioBtns','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
  // //  AddAttrib(AttrParams,'OptionList','String',ExampleOptionList,false);
     AddAttrib(AttrParams,'ItemIndex','Integer','0',false);
     AddAttrib(AttrParams,'LabelText','String','ComboBox',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     AddAttrib(AttrParams,'ReadOnly','Boolean','False',false);
     RegisterResource('RUI','TXComboBox','TXComboBox','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
     // AddAttrib(AttrParams,'TreeData','TreeString',ExampleNodeTree,false);
     AddAttrib(AttrParams,'ItemText','String','Tree',false);
     AddAttrib(AttrParams,'OpenToLevel','Integer','1',false);
     AddAttrib(AttrParams,'TreeHeight','String','200',false);
     AddAttrib(AttrParams,'TreeWidth','String','350',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXTree','TXTree','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'LabelText','String','Date Picker',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXDatePicker','TXDatePicker','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'LabelText','String','Colour Picker',false);
     AddAttrib(AttrParams,'ItemValue','String','#FF8040',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXColorPicker','TXColorPicker','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
     RegisterResource('RUI','TXMainMenu','TXMainMenu','Selectors',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Caption','String','Menu Item',false);
     RegisterResource('RUI','TXMenuItem','TXMenuItem','Selectors',-1,AttrParams);


  //   ClearAttribs(AttrParams);
  //   AddAttrib(AttrParams,'LabelText','String','File Picker',false);
  //   AddAttrib(AttrParams,'ItemText','String','*.pas',false);
  //   AddAttrib(AttrParams,'ItemValue','String','',false);
  //   AddAttrib(AttrParams,'Hint','String','',false);
  //   RegisterResource('RUI','FilePicker','FilePicker','Selectors',-1,AttrParams);


     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','','Media','UIComponents',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'LabelText','String','Image',false);
     AddAttrib(AttrParams,'Source','String','',false);       //loading.gif?
     AddAttrib(AttrParams,'ImageHeight','String','200px',false);
     AddAttrib(AttrParams,'ImageWidth','String','250px',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXImage','TXImage','Media',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'LabelText','String','Image',false);
     AddAttrib(AttrParams,'ImageHeight','String','500px',false);
     AddAttrib(AttrParams,'ImageWidth','String','500px',false);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','TXBitMap','TXBitMap','Media',-1,AttrParams);

     // missing.....Audio
     // missing.....Video
     // missing.....Chat

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','','IFrame','UIComponents',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'HTMLSource','String','',false);
     AddAttrib(AttrParams,'FrameHeight','Integer','300',false);
     AddAttrib(AttrParams,'FrameWidth','Integer','300',false);
     RegisterResource('RUI','TXIFrame','TXIFrame','IFrame',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'HTMLSource','String','',false);
     AddAttrib(AttrParams,'FrameHeight','Integer','300',false);
     AddAttrib(AttrParams,'FrameWidth','Integer','300',false);
     AddAttrib(AttrParams,'AnimationCode','String','',false);
     AddAttrib(AttrParams,'ParamNumList','String','',false);
     AddAttrib(AttrParams,'ConstIntList','String','',false);
     AddAttrib(AttrParams,'ParamImgList','String','',false);
     AddAttrib(AttrParams,'Active','Boolean','False',false);
     AddAttrib(AttrParams,'Animated','Boolean','False',false);
     AddAttrib(AttrParams,'MaxIterations','Integer','512',false);
     AddAttrib(AttrParams,'StartIteration','Integer','1',false);
     AddAttrib(AttrParams,'NumFrames','Integer','100',false);
     AddAttrib(AttrParams,'MaxFramesPerSec','Integer','15',false);
     RegisterResource('RUI','TXGPUCanvas','TXGPUCanvas','IFrame',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'HTMLSource','String','',false);
     AddAttrib(AttrParams,'FrameHeight','Integer','300',false);
     AddAttrib(AttrParams,'FrameWidth','Integer','300',false);
     AddAttrib(AttrParams,'IsEmbedded','Boolean','True',false);
     AddAttrib(AttrParams,'SourceText','String','...text...',false);
     AddAttrib(AttrParams,'Showing','Boolean','False',false);
     RegisterResource('RUI','TXHTMLEditor','TXHTMLEditor','IFrame',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'FrameHeight','Integer','300',false);
     AddAttrib(AttrParams,'FrameWidth','Integer','300',false);
     AddAttrib(AttrParams,'SourceText','String','...text...',false);
     RegisterResource('RUI','TXHTMLText','TXHTMLText','IFrame',-1,AttrParams);


     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'FrameHeight','Integer','300',false);
     AddAttrib(AttrParams,'FrameWidth','Integer','300',false);
     RegisterResource('RUI','TXSVGContainer','TXSVGContainer','IFrame',-1,AttrParams);


     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'Hint','String','',false);
     RegisterResource('RUI','','SVGComponents','IFrame',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'XPos','Integer','50',false);        //y
     AddAttrib(AttrParams,'YPos','Integer','50',false);         //x
     AddAttrib(AttrParams,'TextString','String','SVG Text Example',false);
     AddAttrib(AttrParams,'Height','Integer','20',false);
     AddAttrib(AttrParams,'FontFamily','String',' impact, georgia, times, serif;',false);
     AddAttrib(AttrParams,'FontWeight','String',' normal',false);
     AddAttrib(AttrParams,'FontStyle','String',' normal',false);
     AddAttrib(AttrParams,'Rotate','Integer','0',false) ;
     RegisterResource('RSVG','TXSVGText','TXSVGText','SVGComponents',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'XPos','Integer','0',false);        //y
     AddAttrib(AttrParams,'YPos','Integer','110',false);         //x
     AddAttrib(AttrParams,'Width','Integer','30',false);
     AddAttrib(AttrParams,'Height','Integer','30',false);
     AddAttrib(AttrParams,'StrokeColor','Color','#000000',false);          // stroke
     //AddAttrib(AttrParams,'FillColor','Color','none',false);            //fill
     AddAttrib(AttrParams,'FillColor','Color','#FFFFFF',false);            //fill
     AddAttrib(AttrParams,'StrokeWidth','Integer','5',false);
     AddAttrib(AttrParams,'Rotate','Integer','0',false);
     RegisterResource('RSVG','TXSVGRect','TXSVGRect','SVGComponents',-1,AttrParams);

     ClearAttribs(AttrParams);
     AddAttrib(AttrParams,'XPos','Integer','60',false);        //y
     AddAttrib(AttrParams,'YPos','Integer','10',false);         //x
     AddAttrib(AttrParams,'Rx','Integer','10',false);
     AddAttrib(AttrParams,'Ry','Integer','10',false);
     AddAttrib(AttrParams,'Width','Integer','30',false);
     AddAttrib(AttrParams,'Height','Integer','30',false);
     AddAttrib(AttrParams,'StrokeColor','Color','#000000',false);          // stroke
     //AddAttrib(AttrParams,'FillColor','Color','none',false);            //fill
     AddAttrib(AttrParams,'FillColor','Color','#FFFFFF',false);            //fill
     AddAttrib(AttrParams,'StrokeWidth','Integer','5',false);
     AddAttrib(AttrParams,'Rotate','Integer','0',false);
     RegisterResource('RSVG','TXSVGRoundedRect','TXSVGRoundedRect','SVGComponents',-1,AttrParams);

      ClearAttribs(AttrParams);
      AddAttrib(AttrParams,'XPos','Integer','25',false);        //cy
      AddAttrib(AttrParams,'YPos','Integer','75',false);         //cy
      AddAttrib(AttrParams,'Radius','Integer','20',false);       //r
      AddAttrib(AttrParams,'StrokeColor','Color','#00FF00',false);          // stroke
      AddAttrib(AttrParams,'FillColor','Color','#FFFF00',false);            //fill
      AddAttrib(AttrParams,'Strokewidth','Integer','5',false);
      AddAttrib(AttrParams,'Rotate','Integer','0',false);
      RegisterResource('RSVG','TXSVGCircle','TXSVGCircle','SVGComponents',-1,AttrParams);

      ClearAttribs(AttrParams);
      AddAttrib(AttrParams,'XPos','Integer','100',false);        //cy
      AddAttrib(AttrParams,'YPos','Integer','75',false);         //cy
      AddAttrib(AttrParams,'Rx','Integer','20',false);       //rx
      AddAttrib(AttrParams,'Ry','Integer','50',false);       //ry
      AddAttrib(AttrParams,'StrokeColor','Color','#000000',false);          // stroke
      //AddAttrib(AttrParams,'FillColor','Color','none',false);            //fill
      AddAttrib(AttrParams,'FillColor','Color','#FFFFFF',false);            //fill
      AddAttrib(AttrParams,'StrokeWidth','Integer','5',false);
      AddAttrib(AttrParams,'Rotate','Integer','0',false);
      RegisterResource('RSVG','TXSVGEllipse','TXSVGEllipse','SVGComponents',-1,AttrParams);

      ClearAttribs(AttrParams);
      AddAttrib(AttrParams,'X1','Integer','10',false);        //x1
      AddAttrib(AttrParams,'Y1','Integer','110',false);         //y1
      AddAttrib(AttrParams,'X2','Integer','50',false);       //x2
      AddAttrib(AttrParams,'Y2','Integer','150',false);       //y2
      AddAttrib(AttrParams,'StrokeColor','Color','#FF9900',false);          // stroke
      AddAttrib(AttrParams,'StrokeWidth','Integer','5',false);
      AddAttrib(AttrParams,'Rotate','Integer','0',false);
      RegisterResource('RSVG','TXSVGLine','TXSVGLine','SVGComponents',-1,AttrParams);

      ClearAttribs(AttrParams);
      AddAttrib(AttrParams,'XCoords','IntegerVector','60 , 65 , 70 , 75  , 80 , 85 , 90,  95 , 100 ',false);
      AddAttrib(AttrParams,'YCoords','IntegerVector','110, 120, 115 ,130 ,125 ,140, 135 ,150 , 145',false);
      AddAttrib(AttrParams,'StrokeColor','Color','#FF9900',false);          // stroke
      //AddAttrib(AttrParams,'FillColor','Color','none',false);            //fill
      AddAttrib(AttrParams,'FillColor','Color','#FFFFFF',false);            //fill
      AddAttrib(AttrParams,'StrokeWidth','Integer','5',false);
      AddAttrib(AttrParams,'Rotate','Integer','0',false);
      RegisterResource('RSVG','TXSVGPolyLine','TXSVGPolyLine','SVGComponents',-1,AttrParams);

      ClearAttribs(AttrParams);
      AddAttrib(AttrParams,'XCoords','IntegerVector','50 ,  55 ,   70 , 60 ,  65 ,  50 ,  35  , 40 ,  30 ,  45 ',false);
      AddAttrib(AttrParams,'YCoords','IntegerVector','160 , 180 , 180 , 190 , 205 , 195 , 205 , 190 , 180,  180',false);
      AddAttrib(AttrParams,'StrokeColor','Color','#FF9900',false);          // stroke
      AddAttrib(AttrParams,'FillColor','Color','#AAAAAA',false);            //fill
      AddAttrib(AttrParams,'StrokeWidth','Integer','5',false);
      AddAttrib(AttrParams,'Rotate','Integer','0',false);
      RegisterResource('RSVG','TXSVGPolyGon','TXSVGPolyGon','SVGComponents',-1,AttrParams);


      ClearAttribs(AttrParams);
      AddAttrib(AttrParams,'Hint','String','',false);
      RegisterResource('RUI','','Composites','UIComponents',-1,AttrParams);

      // Set up the list of previously saved composite items
      NamesList:=TStringList.Create;
      DiscoverSavedComposites(NamesList);
      for i:=0 to NamesList.count-1 do
      begin
        ClearAttribs(AttrParams);
        AddAttrib(AttrParams,'CompositeType','String',NamesList[i],true);
        {$ifndef JScript}
        str:=ReadFromLocalStore('SavedSystems/'+NamesList[i]+'.xcmp');
        {$else}
        str:=ReadFromLocalStore(NamesList[i]+'.xcmp');
        {$endif}
        AddAttrib(AttrParams,'SourceString','String',str,true);
        AddAttrib(AttrParams,'ContainerWidth','String','',false);
        AddAttrib(AttrParams,'ContainerHeight','String','',false);
        AddAttrib(AttrParams,'InheritColor','Boolean','True',false);
        RegisterResource('RUI','TXComposite',NamesList[i]+'_xcmp','Composites',-1,AttrParams);   //suffix to protect nodename-uniqueness
      end;

       ClearAttribs(AttrParams);
       AddAttrib(AttrParams,'Hint','String','',false);
       RegisterResource('RNV','','Non-Visual Components',ResourceDataRootName,-1,AttrParams);

       ClearAttribs(AttrParams);
       AddAttrib(AttrParams,'KeyName','String','',false);
       AddAttrib(AttrParams,'DataValue','String','',false);
       RegisterResource('RNV','TXStore','TXStore','Non-Visual Components',-1,AttrParams);

       ClearAttribs(AttrParams);
       RegisterResource('RNV','TXTrapEvents','TXTrapEvents','Non-Visual Components',-1,AttrParams);

       ClearAttribs(AttrParams);
       AddAttrib(AttrParams,'Active','Boolean','False',false);
       RegisterResource('RNV','TXThreads','TXThreads','Non-Visual Components',-1,AttrParams);

       ClearAttribs(AttrParams);
       RegisterResource('RNV','TXCompositeIntf','TXCompositeIntf','Non-Visual Components',-1,AttrParams);

//       ClearAttribs(AttrParams);
//       RegisterResource('RNV','','Saved Systems',ResourceDataRootName,-1,AttrParams);
//
//       // Set up the list of previously saved systems
//       NamesList.Clear;
//       DiscoverSavedSystems(NamesList);
//       ClearAttribs(AttrParams);
//       for i:=0 to NamesList.count-1 do
//       begin
//         if NamesList[i]<>'' then
//           RegisterResource('RSS','System',NamesList[i]+'_xide','Saved Systems',-1,AttrParams);   //suffix to protect nodename-uniqueness
//       end;

       NamesList.Free;

end;


//function MyInputBox(PromptString,DefaultString:string):string;
function XIDEPrompt(PromptString,DefaultString:string):string;
var
  str:string;
begin
  if MacroEventList.Replaying = false then
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
      HandleEvent(nil,'UserInput','UIRootNode','',str);
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
        pas.Events.handleEvent(null,'UserInput','UIRootNode','',str);
      }
    end;
    {$endif}
  end
  else
  begin
    str:=ReplayUserInput(PromptString);    //!!!! (browser) this will throw Timeouts, so any following stuff may occur out of sequence
  end;
  result:=str;
end;

function getname(PromptString:string):string;
var resultstring,DefaultString:string;
begin
   DefaultString := 'Created_'+DateTimeToStr(Now);
   DefaultString :=myStringReplace( DefaultString,' ','_',999,999);
   DefaultString :=myStringReplace( DefaultString,':','_',999,999);
   DefaultString :=myStringReplace( DefaultString,'-','_',999,999);
   DefaultString :=myStringReplace( DefaultString,'/','_',999,999);

   resultstring:= XIDEPrompt(PromptString,DefaultString);
   result:=resultstring;
end;

function ComponentNameIsUnique(ScreenObjectName,NameSpace:string):Boolean;
var
  myresult:Boolean;
  founditem:TDataNode;
begin
  myresult:=true;
  founditem:=FindDataNodeById(SystemNodeTree,ScreenObjectName,NameSpace,false);
  if (founditem<>nil)
//  and ((founditem.ScreenObject<>nil) or (founditem.NodeClass='SVG'))
//  and (founditem.NodeName=ScreenObjectName)
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
  XFormNode:=FindDataNodeById(SystemNodeTree,XFormName,'',true);
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
  mf:TForm;
  tmp,cfn,mfn, Nodeid:string;
  Border:Boolean;
  i:integer;
  ParentNode:TDataNode;
  TabPage:TXTabSheet;
begin
  tmp:=CurrentNode.GetAttribute('Border',true).AttribValue;
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
  ParentNode:=FindParentOfNode(SystemNodeTree,CurrentNode);
  while (ParentNode<>nil) and (ParentNode.NodeType<>'TXForm') do
  begin
    if ParentNode.NodeType='TXTabSheet' then
    begin
      TabPage:=TXTabSheet(ParentNode.ScreenObject);
      ParentNode:=FindParentOfNode(SystemNodeTree,ParentNode);
      {$ifndef JScript}
      TXTabControl(ParentNode.ScreenObject).ActivePage:=TabPage;
      {$else}
      ChangeTabPage(TabPage.NodeName,ParentNode.NodeName,'');
      {$endif}
    end;
    ParentNode:=FindParentOfNode(SystemNodeTree,ParentNode);
  end;

end;

procedure RefreshObjectInspector(CurrentNode:TDataNode);
var AttributePrefix:string;
  i:integer;
  PropertiesNode,EventsNode,WidgetNode:TDataNode;
  myAttribs:TNodeAttributesArray;
  s:boolean;
  BoxName,AttribValue:String;
  MyWidget:TObject;
begin
  s:=SuppressEvents;
  SuppressEvents:=true;
  //showmessage('RefreshObjectInspector. Node='+CurrentNode.NodeName);
  PropertiesNode:=FindDataNodeById(SystemNodeTree,PropertyEditorScrollboxName,'',true);
  EventsNode:=FindDataNodeById(SystemNodeTree,EventsEditorScrollboxName,'',true);

  if (PropertiesNode<>nil) and (CurrentNode<>nil)  then
  begin

      AttributePrefix:='OI'+AttributeEditorNameDelimiter+CurrentNode.NodeName;
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
  end;

  //------------------- Refresh the registered Events Tabpage -----------------------
  if (EventsNode<>nil) and (CurrentNode<>nil)  then
  begin

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

procedure DoSelectNavTreeNode(CurrentNode:TDataNode; refresh:boolean);
var
  mynodeText,tmp, NodeId:string;
  IFrameNode,tmpnode:TDataNode;
  okToContinue:Boolean;
  i:integer;
begin
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
//  asm
//    console.log('DoSelectNavTreeNode '+CurrentNode.NodeName+' ok='+okToContinue);
//  end;
  {$endif}
   //!! Should to operate this by node id, not text  :: HOWEVER, in the nav tree, node texts are all unique.
  //showmessage('DoSelectNavTreeNode '+CurrentNode.NodeClass+' '+CurrentNode.NodeType+' '+CurrentNode.NodeName+' ');
  if (DesignMode) and (okToContinue) then
  begin
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
//       if not EventLogging.MacroEventList.Replaying then
//       begin
//         asm
//         setTimeout(function(){},10);              //alert('refresher timeout');
//         end;
//       end;
       {$endif}

       // Highlight the selected object (dotted border)
       HighlightNavigatorObject(ObjectInspectorSelectedNavTreeNode);
     end
     else  if (refresh) then
     begin
       // just refresh all the displayed property values
      // showmessage('RefreshObjectInspector');
       RefreshObjectInspector(CurrentNode);
     end;

  end;
end;

procedure  SelectNavTreeNode(CurrentNode:TDataNode; refresh:boolean);
begin
  {$ifndef JScript}
  DoSelectNavTreeNode(CurrentNode,refresh);
  {$else}
  asm
  //console.log('SelectNavTreeNode '+CurrentNode.NodeName);
  myTimeout(pas.XObjectInsp.DoSelectNavTreeNode,5,'DoSelectNavTreeNode',0,CurrentNode,refresh);
  end;
  {$endif}

end;


procedure  SelectCodeTreeNode(CurrentNode:TDataNode; refresh:boolean; TreeNodeText:String);
 var
    mynodeText,tmp, NodeId:string;
 begin
     //!!!! Should to operate this by node id, not text  :: HOWEVER, in the code tree, node texts are all unique.

     //showmessage('SelectCodeTreeNode '+CurrentNode.Nodename+' >'+TreeNodeText+'<');
     //mynodeText := CurrentNode.NodeType+'('+CurrentNode.Nodename+')';
     mynodeText := TreeNodeText;

     TreeInFocus := CodeRootNode;

     {$ifndef JScript}
     TXTree(CodeTreeComponent.ScreenObject).SelectedNodeText:=mynodeText;  // this selects the node in the codetree component, if changed.
     {$endif}

     if (refresh)
     or (ObjectInspectorSelectedCodeTreeNode=nil)
     or (ObjectInspectorSelectedCodeTreeNode<>CurrentNode) then
     begin

       if DesignMode then
       begin
         ObjectInspectorSelectedCodeTreeNode:=CurrentNode;
         {$ifdef JScript}
         TXTree(CodeTreeComponent).SelectedNodeText:=myNodeText;  // this selects the node in the codetree component, if changed.
         {$endif}

       end;
     end;
     ObjectInspectorSelectedCodeNodeText:=TreeNodeText;
 end;

procedure RebuildResourcesTree;
 var
     newtreestring, nodetext:string;
     AttrParams:TNodeAttributesArray;
     ResTree:TXTree;
     i:integer;
     mynode:TDataNode;
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
    newtreestring,formstring:string;
    AttrParams:TNodeAttributesArray;
    i,j:integer;
    fm:TForm;
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
    newtreestring,formstring:string;
    AttrParams:TNodeAttributesArray;
    i,e:integer;
    fm:TForm;
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
  newtreestring:= ConstructSystemTreeString(CodeRootNode,0,true,false,nil,'');

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
    CurrentNode:=FindDataNodeById(SystemNodeTree,DataNodeId,'',true);
    if CurrentNode=nil then
      showmessage('Cannot find node '+DataNodeId+' in XObjectInsp.HandleNavTreeClickEvent')
    else
    begin
      SelectNavTreeNode(CurrentNode,false);
    end;

  end;
end;

procedure HandleCodeTreeClickEvent(TreeNodeId:String; TreeNodeText:String);
var CurrentNode :TDataNode;
begin
//ShowMessage('ntc event. node='+TreeNodeId);
  if (TreeNodeText<>CodeRootName)
  and (TreeNodeText<>'Root')
  and (TreeNodeText<>'Root(Events)')
  and (TreeNodeText<>'Root(GPUCode)') then
  begin
    CurrentNode:=FindDataNodeById(SystemNodeTree,TreeNodeId,'',true);
    if CurrentNode<>nil then
      SelectCodeTreeNode(CurrentNode,false,TreeNodeText);
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
   end
   else ShowMessage('Select an item (from the Navigation Tree) to copy before calling this action');
end;

function PickItem( SelectedResourcesTreeNode:TDataNode):string;
begin
   // Copy an item from the Resources tree, ready for paste elsewhere
   if (SelectedResourcesTreeNode<>nil)
   and (SelectedResourcesTreeNode.NodeName<>'') and (SelectedResourcesTreeNode.NodeType<>'')
   and (SelectedResourcesTreeNode.NodeType<>'Demo') and (SelectedResourcesTreeNode.NodeType<>'Test') then
   begin
      ObjectInspectorSourceNode:=CopyNode(SelectedResourcesTreeNode,true);
      ObjectInspectorSourceCut:=false;
   end;
//   else
//     ShowMessage('Select an item (from the Resource Tree) to copy before calling this action');
end;

procedure  SelectResourceTreeNode(TreeNodeText:string;CurrentNode:TDataNode);
var
   mynodeid:string;
   ResourceTreeNode:TDataNode;
begin
  //!!!! Should to operate this by node id, not text  :: HOWEVER, in the resource tree, node texts are all unique.
  //showmessage('SelectResourceTreeNode '+TreeNodeText);
  mynodeid:=CurrentNode.NodeName;
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
  //ParentNode:=FindParentOfNodeByName(InTree,SelectedNode.NodeName);
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
 //          else if InTree.NodeName=ResourceDataRootName then
 //          begin
 //             RebuildResourcesTree;
 //          end
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

procedure CutItemQuietly(InTree,SelectedNode:TDataNode);
begin
  ObjectInspectorSourceNode:=CopyNode(SelectedNode,true);
  //ShowMessage('cutting '+ObjectInspectorSourceNode.NodeName);
  ObjectInspectorSourceCut:=true;
  DeleteItemQuietly(InTree,SelectedNode);
  //ShowMessage('finished cutting.  oi node is '+ObjectInspectorSourceNode.NodeName);
end;

function CutItem(InTree,SelectedNode:TDataNode):string;
var
   myName:string;
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
        CutItemQuietly(InTree,SelectedNode);

        if designMode then
        begin
          SaveSystemData;

          if InTree.NodeName=SystemRootName then
          begin
            RebuildNavigatorTree;
            RebuildCodeTree;
          end;
  //        else if InTree.NodeName=ResourceDataRootName then
  //          RebuildResourcesTree;
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

procedure RenameCompositeNodes(MyNode:TDataNode;NameSpace:string);
var
   i:integer;
begin
  for i:=0 to length(MyNode.ChildNodes)-1 do
  begin
    MyNode.ChildNodes[i].NameSpace:=NameSpace;
    RenameChildNodes(MyNode.ChildNodes[i],NameSpace);
  end;
end;


procedure PasteItemQuietly(InTree:TDataNode;pos:integer;ParentNode,SourceNode:TDataNode);
var
   TreePos:integer;
   glb:Boolean;
   //pn:String;
begin
  //showmessage('PasteItemQuietly');
  SourceNode.ScreenObject:=nil;
  TreePos:=pos;
  {$ifndef JScript}
  glb:=  GlobalSuppressFrameDisplay;
  {$endif}

  if ObjectInspectorSourceCut=false then
    // this is a copy - have to rename all child nodes
    if SourceNode.NodeType <> 'TXComposite' then
      RenameChildNodes(SourceNode,SourceNode.NodeName);    //!!!! this will break any references in event code to these nodes

  if InTree.NodeName=SystemRootName then
  begin
    // un-highlight selected item in nav tree
    if ObjectInspectorSelectedNavTreeNode<>nil then
    {$ifndef JScript}
      if ObjectInspectorSelectedNavTreeNode.NodeClass='UI' then
        SetBooleanProperty(ObjectInspectorSelectedNavTreeNode.ScreenObject,'IsSelected',false)
      else
        SetBooleanProperty(ObjectInspectorSelectedNavTreeNode,'IsSelected',false);
      {$else}
      if IsPublishedProp(ObjectInspectorSelectedNavTreeNode,'IsSelected') then
        SetBoolProp(ObjectInspectorSelectedNavTreeNode,'IsSelected',false);
      {$endif}

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
  //else if (ParentNode.NodeType<>'TXForm') and (SourceNode.NodeClass='NV') then
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
//showmessage('OINavTreeAllowDrop. Dest='+DstNode.NodeName+' Src='+SrcNode.NodeName);
      // cannot drop to Root Node.
      if (DstNode.NodeClass='Root')
      and (SrcNode.NodeType <> 'TXForm') then
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
//   UIRootNode.SetAttributeValue('SystemName',sysname);
end;

procedure LoadCompositeResource(CompNode:TdataNode);
var
  SystemDescription:String;
begin
   SystemDescription:=CompNode.GetAttribute('SourceString',false).AttribValue;

   DoSystemLoad(SystemDescription,CompNode.NodeName);
 //  UIRootNode.SetAttributeValue('SystemName',CompNode.NodeName);

end;

function PasteItem(NavTreeDestinationNode:TDataNode;OrigSourceNode:TDataNode;NewName:String):boolean;
var
   SourceNode,ParentNode, NewNode:TDataNode;
   TreePos:integer;
   ok,TargetIsContainer:Boolean;
   CompositeResource:Boolean;
   InterfaceNodes:TNodesArray;
   i,j:integer;
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
    if (SourceNode.NodeClass='RUI')
    and (SourceNode.NodeType='TXComposite') then
      CompositeResource:=true;

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
      //ParentNode:=FindParentOfNodeByName(SystemNodeTree,NavTreeDestinationNode.NodeName);
      ParentNode:=FindParentOfNode(SystemNodeTree,NavTreeDestinationNode);
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
             NewName:=trim(getname('Enter Component Name:'));
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
            InterfaceNodes:=FindNodesOfType(NewNode,'TXCompositeIntf');
            for i:=0 to length(InterfaceNodes)-1 do     //!!!! only allow one of these???
            begin
              for j:=0 to length(InterfaceNodes[i].NodeAttributes)-1 do
                NewNode.AddAttribute(InterfaceNodes[i].NodeAttributes[j].AttribName,'String',InterfaceNodes[i].NodeAttributes[j].AttribValue,false);
              for j:=0 to InterfaceNodes[i].myEventTypes.Count-1 do
                NewNode.AddEvent(InterfaceNodes[i].myEventTypes[j],'','');
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

(*
function  repchar(instring,refstring:string):string;
var i,j,k,m,num1,num2, wrap,testlength:integer;
    newstring:string;
begin
  if length(refstring)< 1 then showmessage('Error ....refstring is too short in ComponentWrapper.repchar');
  j:=0;
  k:=0;
  newstring:='';
  testlength:=8;
  if length(instring)> testlength
  then
  begin
    for i:=1 to testlength do
    begin
      newstring:= newstring+instring[i];
    end;
    if (ord(instring[i]) > 126)
    then
    begin
       newstring:= newstring+instring[i];
       if ord(instring[i])=160 then showmessage('Error ....char 160 is a reserved value in ComponentWrapper.repchar');
    end
    else
    for i:=testlength+1 to length(instring) do
    begin
      k:=k+1;
      if k > 13 then k:=1;
      m:= (k xor 5);
      num1:=ord(instring[i]);
      if num1=160 then num1:=127;
      num1:=num1-32;
      j:=j+1;
      if j>length(refstring)then j:=1;
      wrap:= 4 * m + ord(refstring[j]);
      if  wrap>126 then  wrap := wrap -95;
      num2:=(wrap-32);
      num2:=trunc(num2/4);
      num1:=(num1 xor num2)+32;
      if num1=127 then num1:=160;
      if (num1>126)and(num1<>160) then showmessage('Error ....char out of range in ComponentWrapper.repchar  '+inttostr(num1)+' '+chr(num1));
      newstring:=newstring+ chr(num1);
    end;
  end else showmessage('Error ....instring too short in ComponentWrapper.repchar');
  result:=newstring;
end;


function addpoochar(instring:string):string;
var i,j,k,m,num:integer;
    newstring:string;
begin
  k:=0;
  instring:='sdfkisbdoisopjwepojwegaegohaspeoulvdoidfvd;ldfhddbldv;l,sc;ascethpnij[pfgb][lwefp;asckbjefog;lkdrpojgpoiejrgoigerauiervoieroiheg';
  instring:=instring+ instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring;
   instring:=instring+ instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring;
   instring:=instring+ instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring;
   instring:=instring+ instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring+instring;
  newstring:= instring+'mpXiWmfG';
  for i:=1 to length(instring) do
  begin
    k:=k+1;
    if k > 11 then k:=1;
    m:= (k xor 5);
    j:=1+(i xor 5);
    if ord(instring[i]) > 126
    then newstring[j]:=instring[i]
    else
    begin
      num:=ord(instring[i])+m;
      if num>126 then num := num - 95;
      newstring[j]:= chr(num);
    end ;
    if (j>length(newstring)) or (j<1) then showmessage('poo j = '+ inttostr(j));
    if (ord(instring[i]) > 126) or ( ord(instring[i]) <32  )  then showmessage('odd char = >'+instring[i]+'<');
    if (ord(newstring[j]) > 126) or ( ord(newstring[j]) <32  )  then showmessage('oops odd char j = >'+newstring[j]+'<');
  end;
  newstring:=repchar(newstring,'HelloWorld');
  result:=newstring;
end;

function addchar(instring:string):string;
var i,j,k,m,num:integer;
    newstring:string;
begin
  k:=0;
  newstring:= instring+'mpXiWmfG';
  for i:=1 to length(instring) do
  begin
    k:=k+1;
    if k > 11 then k:=1;
    m:= (k xor 5);
    j:=1+(i xor 5);
    if ord(instring[i]) > 126
    then newstring[j]:=instring[i]
    else
    begin
      num:=ord(instring[i])+m;
      if num>126 then num := num - 95;
      newstring[j]:= chr(num);
    end ;
  end;
  newstring:=repchar(newstring,'HelloWorld');
  result:=newstring;
end;

function subchar(instring:string):string;
var i,j,k,m,num:integer;
    newstring,newstring2:string;
begin
  instring:=repchar(instring,'HelloWorld');
  k:=0;
  newstring:= instring;
  newstring2:='';
  for i:=1 to length(instring)-8 do
  begin
    k:=k+1;
    if k> 11 then k:=1;
    m:= (k xor 5);
    j:=1+(i xor 5);
    if ord(instring[j]) > 126
    then newstring[i]:=instring[j]
    else
    begin
      num:=ord(instring[j])-m;
      if num<32 then num:=num+95;
      newstring[i]:= chr(num);
    end;
  end;
  for i:=  length(newstring)-8 to length(newstring)-1 do newstring[i+1]:=' ';
  for i:=1 to length(instring)-8 do newstring2 := newstring2+newstring[i];
  result:=newstring2;
end;
*)

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
var
thisnode:TDataNode;
begin
  if NavTreeComponent.ScreenObject <>nil then
  begin
    TXTree(NavTreeComponent.ScreenObject).DeSelectNode;
    DeHighlightObject(ObjectInspectorSelectedNavTreeNode);
    ObjectInspectorSelectedNavTreeNode:=nil;

    TXTree(CodeTreeComponent.ScreenObject).DeSelectNode;
    ObjectInspectorSelectedCodeTreeNode:=nil;
    ObjectInspectorSelectedCodeNodeText:='';

    PopulateObjectInspector(nil);
    ClearResourceInspector;
    if CodeEditform<>nil then
    begin
      thisnode:=CodeEditForm.myNode;
      CodeEditForm.CodeEdit.ItemValue:='';
      CodeEditForm.CodeEditInit.ItemValue:='';
    end;
  end;
end;

function CompositeResourcesString(QuotedString:Boolean):String;
var
  StartNode,TempNode,CompNode,TempComp:TdataNode;
  systemstring:String;
  i:integer;
begin
  StartNode:=FindDataNodeById(SystemNodeTree,'Composites','',true);
  systemstring:=NodeTreeToXML(StartNode,UIRootItem,false,QuotedString);
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
procedure DebugWriteNodeTree(StartNode:TdataNode;var txt:String;lvl:integer);
var
  i:integer;
begin
  txt:=txt+LineEnding;
  for i:=0 to lvl-1 do
    txt:=txt+'  ';
  txt:=txt+StartNode.NodeClass+' '+ StartNode.NodeType+' '+StartNode.NodeName+' '+myBoolToStr(StartNode.IsDynamic);
  for i:=0 to length(StartNode.ChildNodes)-1 do
    DebugWriteNodeTree(StartNode.ChildNodes[i],txt,lvl+1);
end;

function BuildSystemString(Encapsulate:Boolean):String;
var
  systemstring,eventstring,fullstring,tmp:string;
  StartNode,UINode, MenuNode,StyleTreeParent:TDataNode;
  i:integer;
  TopType,TopClass:String;
begin
  //showmessage('BuildSystemString');
  ClearInspectors;
  // Save just the user-design portions of the system node tree.
  // Mainform Menu items, Mainform centre section, PLUS dynamic XForms added.

//  showmessage('2');
  UINode:=UIRootItem;
  TopType:=UINode.NodeType;
  TopClass:=UINode.NodeClass;
  UINode.NodeType:='Root';    // so that this item will be skipped on load
  UINode.NodeClass:='Root';    // so that this item will be skipped on load
  UINode.IsDynamic:=true;

  MenuNode:=FindDataNodeById(SystemNodeTree,'XIDEMainMenu','',true);

//  showmessage('3');

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

//  showmessage('4');
  systemstring:= NodeTreeToXML(StartNode,nil,true,false);

//  showmessage('5');
  UINode.NodeType:=TopType;
  UINode.NodeClass:=TopClass;
  UINode.IsDynamic:=false;

  // add non-visual components
  StartNode:=FindDataNodeById(SystemNodeTree,SystemRootName,'',true);
  //StartNode:=FindDataNodeById(SystemNodeTree,MainForm.Name,'',true);
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
    if (StartNode.ChildNodes[i].NodeType='RawUnit')
    and (StartNode.ChildNodes[i].IsDynamic) then
    begin
      systemstring:=systemstring+NodeTreeToXML(StartNode.ChildNodes[i],CodeRootNode,true,false);
    end;
  end;

//  showmessage('8');
  // add the tree 'StyleSheet' so that its data is preserved with the user's project
  StyleTreeParent:=FindDataNodeById(systemnodetree,'StyleDesigner','',true);
  systemstring:=systemstring+NodeTreeToXML(StylesNode,StyleTreeParent,false,false);

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
  fullstring,sysname:String;
  ok:Boolean;
begin
  ok:=false;
  sysname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;

  while not ok do
  begin
    sysname:= XIDEPrompt('Enter System Name',sysname);
    if FoundString(sysname,'.')>0 then
      showmessage('Enter the name without dot characters ''.''')
    else
      ok:=true;
  end;
  sysname:=sysname+'.xide';
  {$ifndef JScript}
  sysname:='SavedSystems/'+sysname;
  {$endif}
  fullstring:=BuildSystemString(false);
  WriteToLocalStore(sysname,fullstring);
  RebuildResourcesTree;
end;

function isValidSystemData(SystemDescription:string):boolean;
var teststring,teststring2,sys:string;
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

{$ifndef JScript}
procedure TLoadTimerTag.DoXMLToNodeTree(sender:TObject);
var
   myTimer:tTimer;
   myTag:TLoadTimerTag;
   glb:Boolean;
begin
  glb:=GlobalSuppressFrameDisplay;
  GlobalSuppressFrameDisplay:=true;

  OIClearSystem;
  ClearAllComposites;

  myTimer:=TTimer(sender);
  myTimer.Enabled:=false;
  myTag:=TLoadTimerTag(myTimer.Tag);
  XMLToNodeTree(myTag.systemstring,UIRootNode);

  if myTag.SysName<>'' then
  begin
    SetSystemName(myTag.SysName);
  end;

  RebuildResourcesTree;
  RedisplayResourceTree;

  // un-suspend all the 'iframe' components...
  GlobalSuppressFrameDisplay:=false;
  UnSuspendFrames(SystemNodeTree);

  GlobalSuppressFrameDisplay:=glb;

  RebuildNavigatorTree;
  RebuildCodeTree;
  SelectNavTreeNode(MainFormProjectRoot,true);

  //make sure UIRoot width attribute is still at 60% (design mode)
  //if ShowResourceTree<>'Hide' then
    UIRootItem.SetAttributeValue('ContainerWidth','60%');
  //else
  //  UIRootItem.SetAttributeValue('ContainerWidth','80%');

  sender.Destroy;
end;

function DoSystemLoad(SystemDescription,SysName:string):Boolean;
var
  i:integer;
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
    InitAutomatedCursor;

    SelectNavTreeNode(MainFormProjectRoot,true);


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
  NodeText,TreeNodeId:String;
  //myNode:TdataNode;
  myTree:TXTree;
begin
  {$ifndef JScript}
  myTree:=TXTree(NavTreeComponent.ScreenObject);
  //NodeText:=myValue;
  NodeText:=myTree.SelectedNodeText;
  {$else}
  myTree:=TXTree(NavTreeComponent);
  NodeText:=myTree.SelectedNodeText;
//showmessage('OINavTreeNodeChange. NodeText='+NodeText);
  {$endif}
  TreeNodeId:=TreeLabelToID(NodeText);

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
  NodeText,TreeNodeId:String;
  //myNode:TdataNode;
  myTree:TXTree;
begin
  {$ifndef JScript}
  myTree:=TXTree(ResourceTreeComponent.ScreenObject);
  //NodeText:=myValue;
  NodeText:=myTree.SelectedNodeText;
  {$else}
  //myNode:=findDataNodeById(systemnodetree,nodeid,NameSpace,true);
  myTree:=TXTree(ResourceTreeComponent);
  NodeText:=myTree.SelectedNodeText;
  {$endif}
  TreeNodeId:=TreeLabelToID(NodeText);

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
  NodeText,TreeNodeId:String;
  myTree:TXTree;
begin
  {$ifndef JScript}
  myTree:=TXTree(CodeTreeComponent.ScreenObject);
  NodeText:=myTree.SelectedNodeText;
  {$else}
  myTree:=TXTree(CodeTreeComponent);
  NodeText:=myTree.SelectedNodeText;
  {$endif}
  TreeNodeId:=TreeLabelToID(NodeText);

  if TreeNodeId<>'' then
  begin
    //showmessage('OICodeTreeNodeChange. nodeId='+nodeId+' TreeNodeId='+TreeNodeId);
     HandleCodeTreeClickEvent(TreeNodeId,NodeText);
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
var dst:string;
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
        dst:=TreeLabelToID( myValue);  // destination node
        DestNode:=FindDataNodeById(SystemNodeTree,dst,'',true);      //!!namespace - assuming top design level only
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
         ShowMessage('Copy an item first');
  result:=ok;
end;

procedure OIDropItem(e:TEventStatus;nodeId:string;myValue:string);
var
  TreeNodeId:string;
  OriginalSource, OriginalParent:TDataNode;
  values:TNodeEventValue;
  OriginalPos:integer;
  ok,ItemWasCut:boolean;
begin
  // Drop an item on the Navigator tree
  //showmessage('OIDropItem.  myValue='+myValue);
//  if e<>nil then
//  begin
//    values:=TNodeEventValue(e.ValueObject);
//    if values<>nil then showmessage('values.SourceName='+values.SourceName+' DstText='+values.DstText);
//  end;

  TreeNodeId:=TreeLabelToID( myValue);  // destination node

  TreeInFocus:=UIRootNode;
  ObjectInspectorSelectedNavTreeNode:=FindDataNodeById(SystemNodeTree,TreeNodeId,'',true);
  //ShowMessage('drop '+ObjectInspectorSourceNode.NodeName+' at '+ObjectInspectorSelectedNavTreeNode.NodeName);

  // if an intra-tree drag/drop, then cut the source node first
  // find the original source node (still in the nav tree)
  OriginalSource:=FindDataNodeById(SystemNodeTree,ObjectInspectorSourceNode.NodeName,'',false);
  OriginalParent:=FindParentOfNode(SystemNodeTree,OriginalSource,true,OriginalPos);
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
    //myParent:=FindParentOfNodeByName(SystemNodeTree,thisNode.NodeName,false);
    myParent:=FindParentOfNode(SystemNodeTree,thisNode);
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


procedure OIMoveItem(nodeId,NameSpace:string;NewParentId:string);
// Interface function (available to user code block) to re-parent a node in the nav tree.
var
  OriginalSource, DestNode:TDataNode;
begin
  // find the source node
  OriginalSource:=FindDataNodeById(SystemNodeTree,NodeId,NameSpace,false);
  if (OriginalSource<>nil) then
  begin

    TreeInFocus:=UIRootNode;
    // find the new parent node
    DestNode:=FindDataNodeById(SystemNodeTree,NewParentId,NameSpace,true);
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
begin
  // find the source node
  OriginalSource:=FindDataNodeById(SystemNodeTree,NodeId,NameSpace,false);
  if (OriginalSource<>nil) then
  begin

    TreeInFocus:=UIRootNode;
    // find the new parent node
    DestNode:=FindDataNodeById(SystemNodeTree,NewParentId,NameSpace,true);
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
//procedure DeleteCompositeResource(ResourceName:String);
//var
//  i,j:integer;
//begin
//  i:=0;
//  while i<length(ListOfComposites) do
//  begin
//    if ListOfComposites[i].ResourceName=ResourceName then
//    begin
//      for j:=i+1 to length(ListOfComposites)-1 do
//        ListOfComposites[j-1]:=ListOfComposites[j];
//      SetLength(ListOfComposites,length(ListOfComposites)-1);
//      i:=length(ListOfComposites)+1;
//    end;
//    i:=i+1;
//  end;
//end;

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

//procedure OIDeleteSavedSystem;
//var
//  filename:string;
//begin
//  if (AvailableResourcesSelectedNode<>nil) and (AvailableResourcesSelectedNode.NodeClass='RSS') then
//  begin
//    filename:=myStringReplace(AvailableResourcesSelectedNode.NodeName,'_xide','.xide',1,-1);
//    if confirm('OK to delete stored system '+filename+'?') then
//    begin
//      ClearLocalStore( filename);
//      RebuildResourcesTree;
//    end;
//  end
//  else
//    ShowMessage('Select a saved system first');
//end;
procedure OIDeleteComposite;
var
  filename:string;
begin
  if (AvailableResourcesSelectedNode<>nil) and (AvailableResourcesSelectedNode.NodeClass='RUI') then
  begin
    filename:=myStringReplace(AvailableResourcesSelectedNode.NodeName,'_xcmp','.xcmp',1,-1);
    if XIDEConfirm('OK to delete composite resource '+filename+'?') then
    begin
      ClearLocalStore( filename);
      RebuildResourcesTree;
    end;
  end
  else
    ShowMessage('Select a composite resource first');
end;
procedure OIDeleteResource;
begin
//  if (AvailableResourcesSelectedNode<>nil) and (AvailableResourcesSelectedNode.NodeClass='RSS') then
//    OIDeleteSavedSystem
//  else
  if (AvailableResourcesSelectedNode<>nil)
    and (AvailableResourcesSelectedNode.NodeClass='RUI')
    and (AvailableResourcesSelectedNode.NodeType='TXComposite') then
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


//procedure OILoadSavedSystem;
//begin
//  if  (AvailableResourcesSelectedNode<>nil) and (AvailableResourcesSelectedNode.NodeClass='RSS') then
//  begin
//    if confirm('OK to load system '+AvailableResourcesSelectedNode.NodeName+'?') then
//      LoadNamedSystem(AvailableResourcesSelectedNode.NodeName);
//  end
//  else
//    ShowMessage('Select a saved system first');
//end;
procedure OILoadSavedSystem2(SysName:String);
begin
    if XIDEConfirm('OK to load system '+SysName+'?') then
      LoadNamedSystem(SysName);
end;

procedure OILoadResource;
begin

//  if (AvailableResourcesSelectedNode<>nil) and (AvailableResourcesSelectedNode.NodeClass='RSS') then
//    OILoadSavedSystem
//  else
  if (AvailableResourcesSelectedNode<>nil)
    and (AvailableResourcesSelectedNode.NodeClass='RUI')
    and (AvailableResourcesSelectedNode.NodeType='TXComposite') then
    OILoadComposite;
end;

procedure OIDragItem(e:TEventStatus;nodeId:string;myValue:string);
var
  TreeNodeId:string;
begin
  // Nav Tree, and Resource Tree.

  TreeNodeId:=TreeLabelToID( myValue);
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
      ObjectInspectorSelectedNavTreeNode:=FindDataNodeById(SystemNodeTree,TreeNodeId,'',true);
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
      //showmessage('OIDeleteItem '+ObjectInspectorSelectedNavTreeNode.NodeName);
      DeleteItem(UIRootNode,ObjectInspectorSelectedNavTreeNode)
    end;
  end;
end;

function OIDeleteItem(NodeId,NameSpace:String;ShowNotFoundMsg:Boolean=true):Boolean;
var
  NodeToDelete:TdataNode;
  Deleted:Boolean;
begin
  Deleted:=false;
  NodeToDelete:=FindDataNodeById(UIRootNode,NodeId,NameSpace,ShowNotFoundMsg);
  if NodeToDelete<>nil then
    Deleted:=DeleteItem(UIRootNode,NodeToDelete);
  result:=Deleted;
end;

procedure OISystemLoad(e:TEventStatus;nodeId:string);
var
  SystemDescription:String;
  macroEvent:TMacroEvent;
begin
   {$ifdef JScript}
   if not EventLogging.MacroEventList.Replaying then
   begin
   {$endif}

   if (e=nil)  or (e.InitRunning=false) then
   begin
     //showmessage('new e');
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
     SystemDescription:=mygetClipboardData('System');
   end;

   if e.EventHasWaitingAsyncProcs = false then
   // this is lazarus and a confirm dialog is not needed
   // otherwise this is HTML and we have waited for a ctrl-V event from the PasteDialog form
   begin
     {$ifndef JScript}
     // Lazarus only
     //showmessage('call DoSystemLoad '+SystemDescription);
     DoSystemLoad(SystemDescription,'');
     {$else}
     asm
       pas.NodeUtils.StartingUp=false;
       var pasteTarget = document.getElementById('PasteTargetContents');
       var PasteString = pasteTarget.value;
       //alert('Paste string = >'+PasteString+'<' );
       pas.XObjectInsp.DoSystemLoad(PasteString,'');
     end;
     {$endif}
   end;
{$ifdef JScript}
  end
  else
  begin
    // replaying an event, so can't handle async stuff...
    // Instead, pop the original pasted string off the eventlist.
    macroEvent:=AdvanceEventLog;
    if macroEvent.EventType<>'MemoPaste' then
    begin
      showmessage('oops cannot retrieve original pasted input');
      showmessage('found event '+macroEvent.EventType+' '+macroevent.NodeId);
    end
    else
    begin
      SystemDescription:=macroEvent.eventvalue;
      asm
        pas.NodeUtils.StartingUp=false;
        alert('Pasted string = >'+SystemDescription+'<' );
        pas.XObjectInsp.DoSystemLoad(SystemDescription,'');
      end;
    end;
  end;
{$endif}
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
    //UIRootNode.SetAttributeValue('SystemName','XIDESystem');

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
procedure CompleteDeploySystem(deployname:String);
var
  wholesystem,currentNodeTree,dm2,sysname,dpstr:String;
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
    wholesystem:='<!DOCTYPE HTML>'  +LineEnding;
    // the resulting javascript will have been inserted in the HTML page.
    // dump the contents of the page onto the clipboard
    // However, for deployed startup, this needs to be amended in 3 places:
    // wholesystem is a snapshot of the current page.  It also includes the javascript initialisation code which
    // loads up the user's system description on startup....
    // 1) Delete the contents of <body>. (is rebuilt on startup).
    // 2) Set the variable LoadedSystemString instead to supply the current system state.
    // 3) Set the variable myDeployedMode (Run, or Design).

    asm
      var htmlElement = document.getElementsByTagName("HTML")[0];
      if (htmlElement!=null) {
        var myBody = document.getElementsByTagName("BODY")[0];
        if (myBody!=null) {
          var saveme = myBody.innerHTML;
          myBody.innerHTML = '<div  id = "UIRootNode" class="vbox" style="height:100%; width:100%;top:0px;left:0px; position:relative; z-index:0;">'
            +'<div  id = "XIDEForm" class="vbox" style="height:100%; width:100%;top:0px;left:0px">'
            +'</div>'
          +'</div>';
          wholesystem = wholesystem+'<HTML lang="en">'+htmlElement.innerHTML+'</HTML>';
          myBody.innerHTML = saveme;
          }
        else {alert('CompleteDeploySystem. body element not found');}
        }
      else {alert('CompleteDeploySystem. html element not found');}
    end;

    // set myDeployedMode...
    wholesystem:=SetMyDeployedMode(wholesystem,dm2);

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
// Save a web-ready copy of the whole system (including XIDE framework) to the clipboard
var
  wholesystem,jsText,sysname,deployname:String;
  lines:TStringList;
  ok:boolean;
  dm:String;
begin
  // First delete object inspector dynamic property editor fields
  ClearInspectors;

  sysname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  deployname:=XIDEPrompt('Name of Deployed System',sysname);

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

     // now cross compile from a saved copy of this source with the conditional define
     // switch (JScript) set to compile the JS version instead of the Lazarus version
     TranspileMyProgram('XIDEMain',ProjectDirectory,'resources/project/',CodeEditForm.CodeEdit,false);
     if FileExists('XIDEMain.js') then
       lines.LoadFromFile('XIDEMain.js');
     Screen.Cursor := crDefault;

  end
  else
  begin
    DisplayDllCompileErrors;
  end;

  jsText:=lines.text;
  dm:=UIRootNode.GetAttribute('DeploymentMode',false).AttribValue ;
  wholesystem := CreateHTMLWrapper('XIDEMain',dm,true,jsText);
  lines.Free;
  myCopyToClip('HTML System',wholesystem );

  {$else}
  ShowGreyOverlay('UIRootNode','Grey1');
  // timeout here so the grey overlay appears
  asm
    myTimeout(pas.XObjectInsp.CompleteDeploySystem,20,'CompleteDeploySystem',0,deployname);
  end;

  {$endif}

end;

procedure OIComponentCopy(nodeId:string;myValue:string);
begin
  PickItem( AvailableResourcesSelectedNode);
end;

procedure ShowHideObjectInspector(show:Boolean);
var
  aNode:TdataNode;
begin
   if not show then
   begin
     DeHighlightObject(ObjectInspectorSelectedNavTreeNode);
     ClearInspectors;
   end;

   aNode:=FindDataNodeById(SystemNodeTree,'InnerRootVBox','',true);
   if aNode<>nil then
     ShowHideNode(aNode,show);

   aNode:=FindDataNodeById(SystemNodeTree,'ResourceInspectorTabs','',true);
   if aNode<>nil then
     //if ShowResourceTree<>'Hide' then
       ShowHideNode(aNode,show);

   if show then
   begin
     //if ShowResourceTree<>'Hide' then
       TXScrollBox(UIRootItem.ScreenObject).ContainerWidth:='60%'
     //else
     //  TXScrollBox(UIRootItem.ScreenObject).ContainerWidth:='80%';
   end
   else
   begin
     TXScrollBox(UIRootItem.ScreenObject).ContainerWidth:='100%';
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
  aNode:=FindDataNodeById(SystemNodeTree,'RootHBox','',true);
  hbox:=TWinControl(aNode.ScreenObject);
  aNode:=FindDataNodeById(SystemNodeTree,'ResourceInspectorTabs','',true);
  ob1:=TControl(aNode.ScreenObject);
  //if ShowResourceTree='Hide' then
  //begin
  //  TXTabControl(ob1).IsVisible:=false;
  //end
  //else
  begin
    aNode:=FindDataNodeById(SystemNodeTree,'InnerRootVBox','',true);
    ob2:=TControl(aNode.ScreenObject);
    aNode:=FindDataNodeById(SystemNodeTree,'UIRoot','',true);
    ob3:=TControl(aNode.ScreenObject);

    hbox.RemoveControl(ob1);
    hbox.RemoveControl(ob2);
    hbox.RemoveControl(ob3);

    lr:=UIRootNode.GetAttribute('ShowResources',true).AttribValue;
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
end;
{$else}
procedure RedisplayResourceTree;
var
  aNode:TdataNode;
  ob1:TObject;
  lr:String;
begin
  aNode:=FindDataNodeById(SystemNodeTree,'ResourceInspectorTabs','',true);
  ob1:=aNode.ScreenObject;
//  if ShowResourceTree='Hide' then
//  begin
//    TXTabControl(ob1).IsVisible:=false;
//  end
//  else
  begin
    TXTabControl(ob1).IsVisible:=true;
    lr:=UIRootNode.GetAttribute('ShowResources',true).AttribValue;
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
var
  nm,nm2:String;
begin
  CodeEditForm.Mode:='dll';
  CodeEditForm.InitialiseOnShow('Compiler Errors','','');

  ShowXForm('CodeEditForm',true);     // the relevant text and message contents have already been populated

  {$ifndef JScript}
  {$else}
  nm:=CodeEditor.CodeEditForm.CodeEdit.NodeName;
  nm2:=CodeEditor.CodeEditForm.CodeEditInit.NodeName;
  asm
  pas.XCode.DoKeyUp(nm2+'Contents',nm2,'',null);
  pas.XCode.DoKeyUp(nm+'Contents',nm,'',null);
  end;
  {$endif}

end;

procedure CompleteToggleToRunMode(ok:boolean);
var
  MenuItemNode:TDataNode;
  MenuItem:TXMenuItem;
//  Messages:TStringList;
  dm:String;
  i:integer;
begin
  //showmessage('CompleteToggleToRunMode');

  MenuItemNode:=FindDataNodeById(SystemNodeTree,'ToggleDesignRunMode','',true);
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

    GatherSourcedAttributes(SystemNodeTree);
    PushAllSourcesToAttributes;

    HandleEventLater(nil,'OnEnterRunMode','UIRootNode','','');
    {$else}
    GatherSourcedAttributes(SystemNodeTree);
    PushAllSourcesToAttributes;

    HandleEvent(nil,'OnEnterRunMode','UIRootNode','','');
    {$endif}
  end
  else
  begin
    // compilation of event code has failed
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
 //     Messages:=TStringList.Create;
 //     Messages.Text:=CodeEditForm.CodeEdit.MessageLines;
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
    CompleteToggleToRunMode(ok);
    DeleteGreyOverlay('Grey1');
end;
{$endif}


procedure DoToggleDesignRunMode(Sender:TXMenuItem);
var
  TargetNode :TDataNode;
  myNode:TDataNode;
  tmp,dm:string;
  ok:Boolean;
  GPUNodes:TNodesArray;
  i:integer;
begin
  ok:=true;
//  showmessage('DoToggleDesignRunMode 1');

  if (DesignMode=true)
  then
  begin
    // Go to Run Mode
    EditAttributeValue('XMemo1','','ItemValue','',false);
    SetLength(SourcedAttribs,0);
    {$ifndef JScript}
    // Check pas2js Compilation of the user-created event code first.
    ok:=CompileEventCode(CodeEditForm.CodeEdit,'LazJS');
    if ok then
    begin
      // Now Compile the user-created event code into the dll.
      ok:=CompileEventCode(CodeEditForm.CodeEdit,'LazDll');
      CompleteToggleToRunMode(ok);
    end
    else
    begin
      DisplayDllCompileErrors;
    end;
    {$else}
    for i:=length(OpenXForms)-1 downto 0 do
    begin
      if OpenXForms[i].NameSpace='' then
        CloseXFormForDesign(OpenXForms[i].NodeName);
    end;

    ShowGreyOverlay('UIRootNode','Grey1');
    // timeout here so the grey overlay appears
    asm
      myTimeout(pas.XObjectInsp.ContinueToggleToRunMode,5,'ContinueToggleToRunMode',0);
    end;
    {$endif}
  end
  else
  begin
    // Go to Design Mode
    if StartingUp=false then
    begin
      // First, STOP any running GPU components
      GPUNodes:=FindNodesOfType(SystemNodeTree,'TXGPUCanvas');
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

    DesignMode:=true;
    SetLength(SourcedAttribs,0);        // keep these during design mode !!!!????
    TXMenuItem(Sender).Caption:='Run Mode';
    // Show Object Inspector
    ShowHideObjectInspector(true);
    {$ifndef JScript}
    GlobalSuppressFrameDisplay:=true;
    DoFormResize(MainForm,MainFormTopControl);
    {$endif}
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

procedure SaveEditedInputs(targetNode:TDataNode;myInputs:TCodeInputs);
var
  //j,k:integer;
  InputStr:String;
begin
  //if EventNum>-1 then
  //begin
  //  k:=0;
  //  for j:=0 to length(myInputs)-1 do
  //    if trim(myInputs[j].InputNodeName)<>'' then
  //    begin
  //        setlength(targetNode.myEventHandlers[EventNum].TheInputs,k+1);
  //        targetNode.myEventHandlers[EventNum].TheInputs[k].InputSynonym:=myInputs[j].InputSynonym;
  //        targetNode.myEventHandlers[EventNum].TheInputs[k].InputNodeName:=myInputs[j].InputNodeName;
  //        targetNode.myEventHandlers[EventNum].TheInputs[k].InputAttribName:=myInputs[j].InputAttribName;
  //        targetNode.myEventHandlers[EventNum].TheInputs[k].InputValue:='';
  //        k:=k+1;
  //    end;
  //end
  //else
  begin
    InputStr:=CodeInputsToString(myInputs);
    targetNode.SetAttributeValue('Inputs',InputStr);
  end;

end;

procedure EditEventCode(NodeNameToEdit,EventToEdit,MainCode,InitCode:String);
var
  targetNode:TDataNode;
  i,j,k:integer;
begin
  targetNode:=FindDataNodeById(SystemNodetree,NodeNameToEdit,'',true);

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
  NodeNameToEdit,PropertyToEdit, EditBoxName, SourceName, tempstr:String;
  targetNode:TDataNode;
  targetAttribute:TNodeAttribute;
  AllKernels:TAnimCodeArray;
begin
  bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
  if bits.Count = 4 then
  begin
    if (bits[0]='OI') or (bits[0]='RI') then       // OI, Editboxname, NodeName, suffix
    begin
      NodeNameToEdit:=bits[1];
      PropertyToEdit:=bits[2];
    end;
  end;

  targetNode:=FindDataNodeById(SystemNodeTree,NodeNameToEdit,'',true);
  targetAttribute:= targetNode.GetAttribute(PropertyToEdit,true);

  if (targetNode.NodeType<>'TXGPUCanvas')
  or ((targetAttribute.AttribName<>'AnimationCode') and (targetAttribute.AttribName<>'InitStageData')) then
  begin
    // pop up the property editor.
    PropertyEditForm.TargetNode:=targetNode;
    PropertyEditForm.TargetAttribute:=targetAttribute;

    // show validation details (eg range, function,...) according to type....
    // and display data source details...
    PropertyEditForm.PropertyEditName.ItemValue:=PropertyToEdit;
    PropertyEditForm.PropertyEditType.ItemValue:=targetAttribute.AttribType;
    SourceName:=targetAttribute.AttribSource.InputNodeName;
    if targetAttribute.AttribSource.InputAttribName<>'' then
    begin
      SourceName:=SourceName+'.'+targetAttribute.AttribSource.InputAttribName;
//      showmessage('This attribute has source '+SourceName);
    end;
    PropertyEditForm.PropertyEditSourceBox.ItemValue:=SourceName;
    // PropertyEditBox is the editbox (or combobox, checkbox etc) in the object inspector...
    EditBoxName:=myStringReplace(nodeId,'Btn','',1,9999);
    PropertyEditBox:=FindDataNodeById(SystemNodeTree,EditBoxName,'',true);

    PropertyEditForm.SetupPages;
    ShowXForm('PropertyEditForm',true);
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

procedure TOIEventWrapper.OIEditProperty(e:TEventStatus;nodeId:string;myValue:string);
var
  bits:TStringList;
  NodeNameToEdit,AttrNameToEdit:String;
  ok:boolean;
begin
  //showmessage('OIEditProperty '+nodeId+' '+myValue);
  // fudge...
  ok:=true;
  {$ifdef JScript}
  asm
    if (myValue==undefined) {ok=false};
  end;
  if not ok then EXIT;
  {$endif}
      bits:=stringsplit(nodeId,AttributeEditorNameDelimiter);
      if bits.Count = 4 then
      begin
        if (bits[0]='OI') or (bits[0]='RI') then       // OI, Editboxname, NodeName, Attrname, suffix
        begin
          NodeNameToEdit:=bits[1];
          AttrNameToEdit:=bits[2];
          if bits[0]='OI' then
          begin
            //showmessage('OIEditProperty '+nodeId+' '+AttrNameToEdit+' '+myValue);
            EditAttributeValue(NodeNameToEdit,'',AttrNameToEdit,myValue);
            RefreshObjectInspector(ObjectInspectorSelectedNavTreeNode);
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
          EventNode:=FindDataNodeById(SystemNodeTree,NodeNameToEdit,'',true);
          EventInitCode:=EventNode.GetEventCode(EventToEdit);
          //showmessage('OIEditEvent '+nodeId+' '+myValue);
          EditEventCode(NodeNameToEdit,EventToEdit,myValue,EventInitCode);
        end;
      end;
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
      or (CodeEditForm.Mode='RawUnitCode')
      or (CodeEditForm.Mode='FunctionCode') then
    begin
      tmp:=CodeEditForm.TargetNodeName;
      // name of unit is in TargetNodeName
      CodeNode:=FindDataNodeById(CodeRootNode,CodeEditForm.TargetNodeName,'',true);
      if (CodeNode.NodeType='RawUnit')
      and (CodeEditForm.Mode='FunctionCode') then
      begin
        // we are displaying compiler errors in the unit...
        // !!!! needs sorting out. Unit code is partly auto-generated so cannot allow edits here !!!!
        showmessage('arg - see CodeEditorClosed');
      end
      else
      begin
        tmp:=CodeEditForm.CodeEdit.ItemValue;
        CodeNode.SetAttributeValue('Code',tmp,'String');
        tmp:=CodeEditForm.codeeditFunctionResultType.ItemValue;
        CodeNode.SetAttributeValue('Type',tmp,'String');
        SaveEditedInputs(CodeNode,CodeEditForm.MyInputs);
      end;
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
  SourceNode, SourceAttrib:String;
  EditNode:TDataNode;
begin
  //showmessage('PropertyEditorClosed. return status '+ PropertyEditStatus);
  if PropertyEditStatus = 'ok' then
  begin
    EditNode:= PropertyEditForm.EditNode;
    // Set the value of TargetAttribute from the widget on the Edit tabpage...
    if EditNode.NodeType='TXEditBox' then
      PropertyEditForm.TargetAttribute.AttribValue:=EditNode.GetAttribute('ItemValue',false).AttribValue
    else if EditNode.NodeType='TXMemo' then
      PropertyEditForm.TargetAttribute.AttribValue:=EditNode.GetAttribute('ItemValue',false).AttribValue
    else if EditNode.NodeType='TXTree' then
     PropertyEditForm.TargetAttribute.AttribValue:=EditNode.GetAttribute('TreeData',false).AttribValue
    else if EditNode.NodeType='TXTable' then
    begin
      PropertyEditForm.TargetAttribute.AttribValue:=EditNode.GetAttribute('TableData',false).AttribValue;
      if PropertyEditForm.TargetAttribute.AttribName='TableData' then
      begin
        PropertyEditForm.TargetNode.SetAttributeValue('NumRows',EditNode.GetAttribute('NumRows',false).AttribValue);
        PropertyEditForm.TargetNode.SetAttributeValue('NumCols',EditNode.GetAttribute('NumCols',false).AttribValue);
      end;
    end
    else if EditNode.NodeType='TXCheckBox' then
      PropertyEditForm.TargetAttribute.AttribValue:=EditNode.GetAttribute('Checked',false).AttribValue
    else if EditNode.NodeType='TXComboBox' then
      PropertyEditForm.TargetAttribute.AttribValue:=EditNode.GetAttribute('ItemValue',false).AttribValue
    else if EditNode.NodeType='TXColorPicker' then
      PropertyEditForm.TargetAttribute.AttribValue:=EditNode.GetAttribute('ItemValue',false).AttribValue;

    //showmessage('PropertyEditorClosed. calling EditAttributeValue');
    // Update the property value in the target node...
    //EditAttributeValue(PropertyEditForm.TargetNode.NodeName,'',PropertyEditForm.TargetAttribute.AttribName,PropertyEditForm.TargetAttribute.AttribValue);
    EditAttributeValue(PropertyEditForm.TargetNode,PropertyEditForm.TargetAttribute.AttribName,PropertyEditForm.TargetAttribute.AttribValue);

    // Set the data source for the property, if specified...
    tmp:=trim(PropertyEditForm.PropertyEditSourceBox.ItemValue);
    if tmp<>'' then
    begin
      SourceBits:=stringsplit(tmp,'.');
      SourceNode:=SourceBits[0];
      if SourceBits.Count>1 then
        SourceAttrib:=SourceBits[1];
      PropertyEditForm.TargetNode.SetAttributeSource(PropertyEditForm.TargetAttribute.AttribName,SourceNode,SourceAttrib);
    end
    else
    begin
      PropertyEditForm.TargetNode.SetAttributeSource(PropertyEditForm.TargetAttribute.AttribName,'','');
    end;

    RefreshObjectInspector(PropertyEditForm.TargetNode);
  end;
  PropertyEditBox:=nil;
end;

procedure TOIEventWrapper.OIEditEventCodeFromCodeTree(NodeNameToEdit:string;EventToEdit:string);
var
  EditBoxName, EventCode, EventInitCode:string;
  bits:TStringList;
  targetNode:TDataNode;
  EventNum,i:integer;
begin
  // pop up the syntax editor.
//  CodeEditForm.Initialise('Event Handler',NodeNameToEdit,EventToEdit);
//  EditBoxName:=myStringReplace(nodeId,'Btn','',1,9999);
//  OIEditBox:=FindDataNodeById(SystemNodeTree,EditBoxName,'',true);
  OIEditBox:=nil;
  targetNode:=FindDataNodeById(SystemNodeTree,NodeNameToEdit,'',true);
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
  EventNum:=targetNode.EventNum(EventToEdit);
  CodeEditForm.CodeEdit.MessageLines:='';
  CodeEditForm.CodeEditInit.MessageLines:='';

  CodeEditForm.CodeEditMainTabs.TabIndex:=0;
  CodeEditForm.Mode:='EventCode';
  CodeEditForm.InitialiseOnShow('Event Handler',NodeNameToEdit,EventToEdit);
  ShowXForm('CodeEditForm',true);

  {$ifndef JScript}
  // on return....
  //CodeEditorClosed(OIEditBox);
  {$endif}
end;

procedure TOIEventWrapper.OIEditEventCode(e:TEventStatus;nodeId:string;myValue:string);
var
  EditBoxName, EventCode, EventInitCode:string;
  bits:TStringList;
  NodeNameToEdit,EventToEdit:String;
  targetNode:TDataNode;
  EventNum,i:integer;
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
  // pop up the syntax editor.
  // remove the 'Btn' suffix
  EditBoxName:=Copy(nodeId,1,length(nodeId)-3);
  OIEditBox:=FindDataNodeById(SystemNodeTree,EditBoxName,'',true);
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
  targetNode:=FindDataNodeById(SystemNodeTree,NodeNameToEdit,'',true);
  CodeEditForm.CodeEditInit.ItemValue := targetNode.GetEventInitCode(EventToEdit);

  CodeEditForm.CodeEdit.MessageLines:='';
  CodeEditForm.CodeEditInit.MessageLines:='';

  CodeEditForm.Mode:='EventCode';
  CodeEditForm.CodeEditMainTabs.TabIndex:=0;
  CodeEditForm.InitialiseOnShow('Event Handler',NodeNameToEdit,EventToEdit);

  ShowXForm('CodeEditForm',true);

end;

{$ifdef JScript}
procedure TOIEventWrapper.OIPasteTarget(e:TEventStatus;nodeId:string;myValue:string);
var
  i:integer;
  PasteEvent:TEventStatus;
begin
//  happens when user hits ctrl-V on the paste dialog form
//  showmessage('OIPasteTarget. value='+myValue);

  PasteEvent:=PasteDialogUnit.CompletionEvent;

  if PasteEvent<>nil then
  begin
    i:=PasteEvent.AsyncProcsRunning.IndexOf('CopyFromClip');
    if i>-1 then
    begin
      PasteEvent.AsyncProcsRunning.Delete(i);
    end;
  end;
  FinishHTMLPasteAction(myValue);

end;
{$endif}



function OITreeNodeHint(TreeLabelStr:String):String;
var
   SystemNodeName:string;
begin
  //showmessage('OINodeTreeHint '+TreeLabelStr);
  SystemNodeName:=TreeLabelToID( TreeLabelStr);
  result := GetNavigatorHint(SystemNodeTree,SystemNodeName);
end;

procedure AddPropertyEditButton(BoxName:String; PropHBox:TXHBox; HBoxNode:TDataNode;ro:Boolean);
//                     HBox     <name>HB
//                       widget   <name>
//                       Button    <name>Btn
var
  NewBtn:TXButton;
  BtnNode:TDataNode;
begin
    NewBtn:=TXButton(AddDynamicWidget('TXButton',MainForm,HBoxNode,BoxName+'Btn','','Top',-1).ScreenObject);
    NewBtn.ButtonWidth:='30px';
    NewBtn.SpacingAround:=0;

    if ro then
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
  NewHBox.Border:=false;
  NewHBox.ContainerHeight:='';

  NewHBox.Alignment:='Right';

  result:=NewHBox;
end;

procedure AddCheckBox(PropertiesNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean;attribHint:String);
var
  NewCheckBox:TXCheckBox;
  NewHBox:TXHBox;
  NewNode, HBoxNode, VBoxNode:TDataNode;
  fn:TAddComponentFunc;
begin
  NewHBox:=AddPropertyContainer(PropertiesNode,BoxName,VBoxNode,HBoxNode);
  NewCheckBox:=TXCheckBox(AddDynamicWidget('TXCheckBox',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  AddPropertyEditButton(BoxName,NewHBox,HBoxNode,ro);

  NewCheckBox.Checked:=myStrToBool(ItmValue);
  NewCheckBox.LabelText:=LblText;
  NewCheckBox.ReadOnly:=ro;
  NewCheckBox.LabelPos:='Left';
  NewCheckBox.Hint:=attribHint;

  NewHBox.Alignment:='Right';

  NewCheckBox.myNode.registerEvent('Click',@OIEventWrapper.OIEditProperty);
end;

procedure AddPropertyEditBox(ParentNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean;IsResource:Boolean;attribHint:String);
var
  NewEditBox:TXEditBox;
  VBoxNode,HBoxNode,NewNode:TDataNode;
  NewHBox:TXHBox;
  fn:TAddComponentFunc;
begin
  NewHBox:=AddPropertyContainer(ParentNode,BoxName,VBoxNode,HBoxNode);
  NewEditBox:=TXEditBox(AddDynamicWidget('TXEditBox',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  if (not IsResource) then
    AddPropertyEditButton(BoxName,NewHBox,HBoxNode,ro);

  NewEditBox.LabelText:=LblText;
  NewEditBox.BoxWidth:='120px';
  NewEditBox.ReadOnly:=ro;
  NewEditBox.LabelPos:='Left';
  NewEditBox.Hint:=attribHint;
  NewHBox.ContainerHeight:='';

  NewHBox.Alignment:='Right';     //!!!! why do I have to set this again here, for it to take effect??

//  if length(ItmValue) < 131073 then
    NewEditBox.ItemValue:=ItmValue;
//  else
//  begin
//    NewEditBox.ItemValue:='...';
//    NewEditBox.Enabled:=false;
//  end;

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

procedure AddPropertyColorPicker(ParentNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean;IsResource:Boolean;attribHint:String);
var
  NewColorPicker:TXColorPicker;
  VBoxNode,HBoxNode:TDataNode;
  NewHBox:TXHBox;
begin
  NewHBox:=AddPropertyContainer(ParentNode,BoxName,VBoxNode,HBoxNode);
  NewHBox.ContainerHeight:='';
  NewColorPicker:=TXColorPicker(AddDynamicWidget('TXColorPicker',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  if (not IsResource) then
    AddPropertyEditButton(BoxName,NewHBox,HBoxNode,ro);

  NewColorPicker.ItemValue:=ItmValue;
  NewColorPicker.LabelText:=LblText;
  NewColorPicker.BoxWidth:='120px';
  NewColorPicker.ReadOnly:=ro;
  NewColorPicker.LabelPos:='Left';
  NewColorPicker.Hint:=attribHint;

  NewHBox.Alignment:='Right';     //!!!! why do I have to set this again here, for it to take effect??

  // event handler....
  NewColorPicker.myNode.registerEvent('Change',@OIEventWrapper.OIEditProperty);
end;

procedure AddEventEditBox(ParentNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean);
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
  NewButton:=TXButton(AddDynamicWidget('TXButton',MainForm,NewNode2,BoxName+'Btn','','Top',-1).ScreenObject);

  NewEditBox.ItemValue:=ItmValue;
  NewEditBox.LabelText:=LblText;
  NewEditBox.BoxWidth:='100px';
  NewEditBox.ReadOnly:=ro;
  NewEditBox.LabelPos:='Left';

  NewButton.Caption:='...';
  NewButton.Hint:='Edit Event Code';

  NewHBox.Alignment:='Right';

  // event handlers....
  NewEditBox.myNode.registerEvent('Change',@OIEventWrapper.OIEditEvent);
  NewButton.myNode.RegisterEvent('ButtonClick',@OIEventWrapper.OIEditEventCode);
end;

procedure AddComboBox(PropertiesNode:TDataNode;BoxName,LblText,ItmValue:String;ro:Boolean;Options:TStringList;attribHint:String);
var
  NewComboBox:TXComboBox;
  NewHBox:TXHBox;
  VBoxNode,HBoxNode:TDataNode;
begin
  NewHBox:=AddPropertyContainer(PropertiesNode,BoxName,VBoxNode,HBoxNode);
  NewHBox.ContainerHeight:='';
  NewComboBox:=TXComboBox(AddDynamicWidget('TXComboBox',MainForm,HBoxNode,BoxName,'','Top',-1).ScreenObject);

  AddPropertyEditButton(BoxName,NewHBox,HBoxNode,ro);

  NewComboBox.LabelText:=LblText;
  NewComboBox.BoxWidth:='120px';
  NewComboBox.ReadOnly:=ro;
  NewComboBox.LabelPos:='Left';
  NewComboBox.Alignment:='Right';
  NewComboBox.Hint:=attribHint;
  //NewComboBox.OptionList:=StringListToListString(Options);
  NewComboBox.OptionList:=StringListToJSONString(Options);

  NewComboBox.ItemValue:=ItmValue;  // this will set itemindex

  NewHBox.Alignment:='Right';

  NewComboBox.myNode.registerEvent('Change',@OIEventWrapper.OIEditProperty);
end;

procedure PopulateObjectInspector(CurrentNode:TDataNode);
var AttributePrefix:string;
  i:integer;
  PropertiesNode, EventsNode,OITabs:TDataNode;
  myAttribs:TNodeAttributesArray;
  ro:boolean;
  s:boolean;
  AttribOptions:TStringList;
  tmp1,tmp2:string;
  tabIndex:String;
  dfltAttrib:TDefaultAttribute;
begin
  if CurrentNode=nil then EXIT;
  s:=SuppressEvents;
  SuppressEvents:=true;

  //if CurrentNode<>nil then showmessage('PopulateObjectInspector. Node='+CurrentNode.NodeName);
  PropertiesNode:=FindDataNodeById(SystemNodeTree,PropertyEditorScrollboxName,'',true);
  EventsNode:=FindDataNodeById(SystemNodeTree,EventsEditorScrollboxName,'',true);
  OITabs:=FindDataNodeById(SystemNodeTree,'OITabs','',true);
  if OITabs<>nil then
    tabIndex:=OITabs.GetAttribute('TabIndex',false).AttribValue
  else
  begin
    showmessage('cannot find OITabs');
    EXIT;
  end;

  //showmessage('TabIndex='+TabIndex);
  if (PropertiesNode<>nil) then
  begin
    EditAttributeValue(PropertiesNode,'IsVisible','false');
    DeleteNodeChildren(PropertiesNode);
  end;
  if (EventsNode<>nil) then
  begin
    EditAttributeValue(EventsNode,'IsVisible','false');
    DeleteNodeChildren(EventsNode);
  end;

  if (PropertiesNode<>nil) and (CurrentNode<>nil)  then
  begin
    if TabIndex='0' then
    begin
      if CurrentNode<>nil then
      begin
      //showmessage('show properties');
        {$ifdef JScript}
        TXTabControl(OITabs.ScreenObject).TabIndex:=0;   //fudge. (browser)...make sure tab is still visible //   openTab(TabName,TabControlName,NameSpace:string);
        {$endif}
        AttributePrefix:='OI'+AttributeEditorNameDelimiter+CurrentNode.NodeName;
        AddPropertyEditBox(PropertiesNode,AttributePrefix+AttributeEditorNameDelimiter
                                  +'Name'+AttributeEditorNameDelimiter
                                  +'0','Name',CurrentNode.NodeName,true,false,CurrentNode.NodeName);

        myAttribs:=CurrentNode.NodeAttributes;
        for i:=0 to length(myAttribs)-1 do
        begin
          dfltAttrib:=GetDefaultAttrib(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName);
          ro:=CurrentNode.NodeAttributes[i].AttribReadOnly;
          //exclude Suppressed properties that user shouldn't see
          if (FindSuppressedProperty(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName)<0)
          and (CurrentNode.NodeAttributes[i].AttribName<>'ParentName') then
            if CurrentNode.NodeAttributes[i].AttribType = 'Boolean' then
            begin
              //showmessage('bool property '+CurrentNode.NodeAttributes[i].AttribName+' '+CurrentNode.NodeAttributes[i].AttribValue);
              AddCheckBox(PropertiesNode,AttributePrefix+AttributeEditorNameDelimiter
                      +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                      +IntToStr(i+2),
                       CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,dfltAttrib.AttribHint);
            end
            else
            begin
              AttribOptions:=LookupAttribOptions(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName);
              if AttribOptions<>nil then
              begin
                AddComboBox(PropertiesNode,AttributePrefix+AttributeEditorNameDelimiter
                      +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                      +IntToStr(i+2),
                       CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,
                       AttribOptions,dfltAttrib.AttribHint);
              end
              else if CurrentNode.NodeAttributes[i].AttribType='Color' then
                AddPropertyColorPicker(PropertiesNode,AttributePrefix+AttributeEditorNameDelimiter
                      +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                      +IntToStr(i+2),
                       CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,false,dfltAttrib.AttribHint)
              else
                AddPropertyEditBox(PropertiesNode,AttributePrefix+AttributeEditorNameDelimiter
                    +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                    +IntToStr(i+2),
                     CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,false,dfltAttrib.AttribHint);
            end;

        end;
        EditAttributeValue(PropertiesNode,'IsVisible','true');
       end;
      end;
      //------------------- Display the registered Events -----------------------
      if EventsNode<>nil  then
      begin
 //       EditAttributeValue('OITabs','TabIndex','1');     // raise the events tab (fudge...sigsiv avoidance...?)
        if TabIndex='1' then
        begin
          {$ifdef JScript}
          TXTabControl(OITabs.ScreenObject).TabIndex:=1;   //fudge. (browser)...make sure tab is still visible //   openTab(TabName,TabControlName,NameSpace:string);
          {$endif}

          if CurrentNode<>nil then
          if ((CurrentNode.IsDynamic) or (CurrentNode=UIRootNode)) then
          begin
            AttributePrefix:='OI'+AttributeEditorNameDelimiter+CurrentNode.NodeName;

              for i:=0 to CurrentNode.MyEventTypes.count-1 do
              begin

                AddEventEditBox(EventsNode,AttributePrefix+AttributeEditorNameDelimiter //OI__nodename__
                            +CurrentNode.MyEventTypes[i]+AttributeEditorNameDelimiter   //eventtype__
                            +IntToStr(i+2),                                             //suffix
                             CurrentNode.MyEventTypes[i],
                             CurrentNode.myEventHandlers[i].TheCode,
                             true);

              end;
          end;
          EditAttributeValue(EventsNode,'IsVisible','true');
        end;
      end;
 //     EditAttributeValue('OITabs','TabIndex',TabIndex);     // put back as it was
  end;

  SuppressEvents:=s;

end;

procedure OIPropsEventsTabChange;
begin
  PopulateObjectInspector(ObjectInspectorSelectedNavTreeNode);
end;


function PopulateResourceInspector(CurrentNode:TDataNode):String;
var AttributePrefix:string;
  i:integer;
  PropertiesNode,btnNode:TDataNode;
  myAttribs,AttrParams:TNodeAttributesArray;
  ro:boolean;
  s:boolean;
begin
  s:=SuppressEvents;
  SuppressEvents:=true;

  if (CurrentNode<>nil)
  and ((CurrentNode.NodeClass='RSS')
    or (CurrentNode.NodeType='TXComposite')) then
  begin
    btnNode:=FindDataNodeById(systemnodetree,'ResourceTreeDelBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=true;
    btnNode:=FindDataNodeById(systemnodetree,'ResourceTreeLoadBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=true;
  end
  else
  begin
    btnNode:=FindDataNodeById(systemnodetree,'ResourceTreeDelBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=false;
    btnNode:=FindDataNodeById(systemnodetree,'ResourceTreeLoadBtn','',true);
    TXButton(btnNode.ScreenObject).Enabled:=false;
  end;

  PropertiesNode:=FindDataNodeById(SystemNodeTree,ResourceEditorScrollboxName,'',true);
  if PropertiesNode<>nil  then
  begin
    EditAttributeValue(PropertiesNode,'IsVisible','false');
    DeleteNodeChildren(PropertiesNode);

    if (CurrentNode<>nil) and (CurrentNode.NodeType<>'') then     // eg. on node headers (placeholders)
    begin

      AttributePrefix:='RI'+AttributeEditorNameDelimiter+CurrentNode.NodeName;
      AddPropertyEditBox(PropertiesNode,AttributePrefix+AttributeEditorNameDelimiter
                                +'Type'+AttributeEditorNameDelimiter
                                +'1','Type',CurrentNode.NodeType,true,true,'');

      myAttribs:=CurrentNode.NodeAttributes;
      for i:=0 to length(myAttribs)-1 do
      begin
        if CurrentNode.NodeAttributes[i].AttribName<>'ParentName' then
        begin
          ro:=CurrentNode.NodeAttributes[i].AttribReadOnly;
          AddPropertyEditBox(PropertiesNode,AttributePrefix+AttributeEditorNameDelimiter
                    +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                    +IntToStr(i+2),
                     CurrentNode.NodeAttributes[i].AttribName,CurrentNode.NodeAttributes[i].AttribValue,ro,true,'');
        end;
      end;
    end;
    EditAttributeValue(PropertiesNode,'IsVisible','True');
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
  NewName:=getname('Enter Unit Name:');

  // Is the source node named uniquely?
  if (NewName<>'') and (ComponentNameIsUnique(NewName,'')) then
  begin
    NewNode:=TDataNode.Create('Code',NewName,'',UnitType,true);
    NewNode.NodeName:=NewName;
    UnitCode:= DfltUnitCode(NewName,UnitType);
    NewNode.SetAttributeValue('Code',UnitCode,'String');

    ObjectInspectorSelectedCodeTreeNode:=InsertSystemNode(CodeRootNode,NewNode,-1);
    ObjectInspectorSelectedCodeNodeText:='';

    // rebuild the code tree
    RebuildCodeTree;

  end;
end;


procedure OIAddCodeFuncNode;
var
  NewName,FnCode:String;
  NewNode:TDataNode;
begin
  if (ObjectInspectorSelectedCodeTreeNode=nil)
  or (ObjectInspectorSelectedCodeTreeNode.NodeName<>'CodeUnits') then
    showmessage('Please select the parent node ''CodeUnits'' first')
  else
  begin
  // create a data node for the new function, and add under the selected Unit node.
    // Dialog for name entry
    NewName:=getname('Enter Function Name:');

     // Is the function node named uniquely?
    if (NewName<>'') and (ComponentNameIsUnique(NewName,'')) then
    begin
      NewNode:=TDataNode.Create('Code',NewName,'','Function',true);
      NewNode.NodeName:=NewName;
      FnCode:= DfltFunctionCode(NewName);
      NewNode.SetAttributeValue('Code',FnCode,'String');

      ObjectInspectorSelectedCodeTreeNode:=InsertSystemNode(ObjectInspectorSelectedCodeTreeNode,NewNode,-1);
      ObjectInspectorSelectedCodeNodeText:='';
      // rebuild the code tree
      RebuildCodeTree;

    end;
  end;
end;




procedure OIEditCodeUnit;
var
  UnitCode, Inputs, EventType:string;
  tmp,NodeNameToEdit:String;
  FuncInputs:TCodeInputs;
  i:integer;
  bits:TStringList;
  AllKernels:TAnimCodeArray;
begin
  if ObjectInspectorSelectedCodeTreeNode<>nil then
  begin
    if (ObjectInspectorSelectedCodeTreeNode.NodeType='RawUnit')
    or (ObjectInspectorSelectedCodeTreeNode.NodeType='Function') then
    begin
      NodeNameToEdit:=ObjectInspectorSelectedCodeTreeNode.NodeName;

      // pop up the syntax editor.
      //CodeEditForm.Initialise('Unit',NodeNameToEdit,'');
      UnitCode:=ObjectInspectorSelectedCodeTreeNode.GetAttribute('Code',true).AttribValue;
      if UnitCode='' then
      begin
        // provide a template unit or function
        if ObjectInspectorSelectedCodeTreeNode.NodeType='RawUnit' then
          UnitCode:= DfltUnitCode(NodeNameToEdit,'RawUnit')
        else
          UnitCode:= DfltFunctionCode(NodeNameToEdit);
      end;
      CodeEditForm.CodeEdit.ItemValue := UnitCode;
      CodeEditForm.CodeEdit.MessageLines:='';

      if ObjectInspectorSelectedCodeTreeNode.NodeType<>'RawUnit' then
      begin
        Inputs:=ObjectInspectorSelectedCodeTreeNode.GetAttribute('Inputs',true).AttribValue;
        FuncInputs:=StringToCodeInputs(Inputs);
        for i:=0 to length(FuncInputs)-1 do
        begin
          CodeEditForm.AddInputBox(FuncInputs[i]);
        end;
      end;

      CodeEditForm.Mode:=ObjectInspectorSelectedCodeTreeNode.NodeType+'Code';
      CodeEditForm.InitialiseOnShow(ObjectInspectorSelectedCodeTreeNode.NodeType,NodeNameToEdit,'');
      ShowXForm('CodeEditForm',true);

      {$ifndef JScript}
      // on return....
      //CodeEditorClosed(nil);
      {$endif}
    end
    else
    if (ObjectInspectorSelectedCodeTreeNode.NodeClass='UI')
    or (ObjectInspectorSelectedCodeTreeNode.NodeClass='NV')
    or (ObjectInspectorSelectedCodeTreeNode.NodeClass='SVG')
    or ((ObjectInspectorSelectedCodeTreeNode.NodeClass='Root')
      and (ObjectInspectorSelectedCodeTreeNode.NodeName='UIRootNode'))
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
    if  (ObjectInspectorSelectedCodeTreeNode.NodeType='RawUnit')
    or  (ObjectInspectorSelectedCodeTreeNode.NodeType='Function') then
    begin
      DeleteItem(CodeRootNode,ObjectInspectorSelectedCodeTreeNode)
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
  fullstring,itemName:String;
  ok:Boolean;
begin
  // Take the current user-defined system, and store its structure in the resource tree under 'Composites', with
  // a name provided by the user, and save to local storage.
  ok:=false;

  while not ok do
  begin
    itemName:=trim(getname('Enter Name of new Composite Item:'));
    if FoundString(itemName,'.')>0 then
      showmessage('Enter the name without dot characters ''.''')
    else
      ok:=true;
  end;
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
  ok:Boolean;
  OITabs:TDataNode;
  i:integer;
begin
  if (ObjectInspectorSelectedNavTreeNode=nil)
  or (ObjectInspectorSelectedNavTreeNode.NodeType<>'TXCompositeIntf') then
    showmessage('Please select a Composite Interface (TXCompositeIntf) element first')
  else
  begin
    myIntf:=TXCompositeIntf(ObjectInspectorSelectedNavTreeNode.ScreenObject);

    OITabs:=FindDataNodeById(SystemNodeTree,'OITabs','',true);
    if OITabs<>nil then
      tabIndex:=OITabs.GetAttribute('TabIndex',false).AttribValue;

    if tabIndex='0' then
    begin

      // create a new attribute for the selected node
      ok:=false;
      while not ok do
      begin
        // Dialog for name entry
        NewName:= XIDEPrompt('Enter Property Name',NewName);

        ok:=true;
         // Is the property named uniquely?
        if (NewName<>'') then
        begin
        if (myIntf.PropertyNameIsUnique(NewName)) then
          begin
            ObjectInspectorSelectedNavTreeNode.AddAttribute(NewName,'String','',false);
            PopulateObjectInspector(ObjectInspectorSelectedNavTreeNode);
          end
          else
          begin
            ok:=false;
            showmessage(NewName+' already exists');
          end;
        end;
      end;

    end
    else
    begin
      // create a new event for the selected node
      ok:=false;
      while not ok do
      begin
        // Dialog for name entry
        NewName:= XIDEPrompt('Enter Event Name',NewName);

        ok:=true;
         // Is the event named uniquely?
        if (NewName<>'') then
        begin
        if (myIntf.EventNameIsUnique(NewName)) then
          begin
            ObjectInspectorSelectedNavTreeNode.AddEvent(NewName,'','');
            PopulateObjectInspector(ObjectInspectorSelectedNavTreeNode);
          end
          else
          begin
            ok:=false;
            showmessage(NewName+' already exists');
          end;
        end;
      end;

    end;
  end;
end;

{$ifdef JScript}
procedure InitAutomatedCursor;
begin
  asm
  // Find the animation rule by name
   var ss = document.styleSheets[0];
   var anim;
     for (var i=0; i<ss.cssRules.length; i++) {
          //alert('i='+i+' name='+ss.cssRules[i].name);
   	if (ss.cssRules[i].name === 'anim') {
   	  anim = ss.cssRules[i];
   	  break;
           }
         }
      if (anim==null) {
        //alert('did not find anim');
        // Dynamically create a keyframe animation..... Initalise to top left corner
        document.styleSheets[0].insertRule("@keyframes anim {from {top: 0px;  left: 0px;} to {top: 0px;  left: 0px;}}");
        var AutomatedCursorDiv = document.getElementById("AutomatedCursor");
        AutomatedCursorDiv.style.animation = "anim 1s linear forwards";
        //AutomatedCursorDiv.style.animation = "anim 1s linear";
      }
  end;
end;
{$endif}



begin
//  setlength(ListOfComposites,0);

  AddAttribOptions('Root','DeploymentMode',DeploymentModeOptions);
  AddAttribOptions('Root','ShowResources',ShowResourcesOptions);

  DesignMode:=true;
  OIEventWrapper := TOIEventWrapper.Create;
  ObjectInspectorSelectedNavTreeNode:=nil;
  ObjectInspectorSelectedCodeTreeNode:=nil;
  AvailableResourcesSelectedNode:=nil;
  ObjectInspectorSelectedCodeNodeText:='';
  {$ifndef JScript}
  GlobalSuppressFrameDisplay:=true;
  {$endif}

end.

