(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

      Handle a data model tree
      Functions to generate classes in Pascal  (which can translate to JS)
      Functions to add data to and fetch data from datasets.

      !!!! a fundamental issue here is node names.  All must be unique, so we cannot
      for example have class A with an attribute named 'key1', and then have
      class B also with a 'key1' attribute...!!!! tbd

      more TBD...
      (Desktop)...Filenames.  Need either a separate directory per system (DB), or a prefix for the Dataset file names.
      (Browser)...Not a problem, as the local database is named for the system.

      Sort out what happens when system name is changed (eg save-as)

 *)

unit XDataModel;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, TypInfo, Stringutils, StrUtils, NodeUtils, Events,
{$ifndef JScript}
  Variants, LazsUtils, FileUtil, LazFileUtils,
  Controls, ComCtrls, ExtCtrls, Dialogs,Forms,DB,memds,bufdataset,DBGrids,
{$else}
  HTMLUtils, pas2jswebcompiler,
{$endif}
  webTranspilerUtils,CompileUserCode,WrapperPanel,
  XHBox, XVBox, XCode,
  XTree,  XButton, XScrollBox, XEditBox, XCheckBox, XComboBox, XTabControl,
  XForm, XTable, XMemo, XMenu, CodeEditor, EventsInterface;

type
  TClassFieldDef = record
    FieldName:String;
    FieldType:String;
    DfltValue:String;
  end;
  TClassFieldDefs=array of TClassFieldDef;
type
    TRefKey = record
      RefKeyName:String;
      RefKeyType:String;
    end;
    TRefKeys = Array of TRefKey;

const
  DMAttribsScrollboxName:string = 'DMAttribsScrollbox';

var
  DMSelectedDataTreeNode :TDataNode;
  DMChanged,DSBuilt:Boolean;

procedure InitDMTree;
procedure RebuildDMTree;
procedure RebuildDMTreeLater;
procedure DMDataTreeNodeChange(e:TEventStatus;nodeId,NameSpace:string;myValue:string);
procedure DMAddElement(AddType:String='');
procedure DMDeleteElement;
procedure ConstructPascalDM(RunMode:String;PascalCode:TStringList);
procedure DoSelectDataTreeNode(CurrentNode:TDataNode; refresh:boolean);
procedure DMClearSelection;
procedure ShowDMOpCodeEditor(targetNode:TDataNode;BtnNodeId:String);
procedure DMMoveSiblingUpDown(UpDown:String);
function DMGetClassFields(DMClassName:String):TClassFieldDefs;
function DMGetKeyFields(ClassName:String):TNodesArray;
function DMRefKeys(refAttribNode:TDataNode):TRefKeys;
procedure DMAttribsCrosscheck(NodeName:String);

function BuildLocalDB(iterationNum:integer; callbackFunc:TObject=nil):Boolean;
procedure CloseLocalDB;
procedure SaveLocalDB;
procedure DeleteLocalDB(DBName:String;ask:Boolean;CallbackFunc:TObject=nil);
procedure DBSaveAs(oldname,newname:String);
function AllDataClassNames(RootName:String):TStringList;
function DSAppendRecordFromObject(DSName,AsyncProcName:String;recObject:TObject;ReturnEvent:TEventStatus):Boolean;
function DSEmptyDataset(ReturnEvent:TEventStatus;DSName,AsyncProcName:String):Boolean;
function DMTreeNodeHint(TreeLabelStr:String):String;

{$ifndef JScript}
type
  // record type for queued async functions
  TDMQueueRec = record
    QDummy: string;
  end;
  PDMQueueRec = ^TDMQueueRec;
  TDMProcs = class(TObject)
    procedure RebuildTheTreeAndInspector(dummy:PtrInt);
  end;
var
  DMProcs:TDMProcs;
type TVarArray = array of variant;
function GetDataset(DSName:String;var idx:integer;showError:Boolean=true):TBufDataset;
procedure DSDataToFile(DSName,prefix:String);
procedure DSLoadFromSaved(DSName:String;oldds,newds:TBufDataset);
function DSGetIndexedRecordAsObject(DSName,AsyncProcName,keynames:String;keyvalues:TVarArray;recObject:TObject;ReturnEvent:TEventStatus):Boolean;
function DSSetRecordFromObject(DSName:String;recObject:TObject):Boolean;
function DSDeleteARow(ReturnEvent:TEventStatus;DSName,AsyncProcName,keynames:String;DSKeyValues:TVarArray):Boolean;
{$else}
function DSGetIndexedRecordAsObject(DSName,AsyncProcName:String;keyvalues:TStringArray;ReturnEvent:TEventStatus):Boolean;
function DSDeleteARow(ReturnEvent:TEventStatus;DSName,AsyncProcName:String;DSKeyValues:TStringArray):Boolean;
procedure DSAddAllRows(DSName:String;allrows,newstore:TObject);
procedure DSCopyData(DSName:String;oldstore,newstore:TObject);
procedure DSCopyDataFromOldDB(DSName:String;oldstore:TObject);
procedure IndexedDBOpenWithVersion(DBName:String;vnum:integer;dsnames:TStringArray;callbackFunc:TObject);
{$endif}
{$ifdef Python}
procedure BuildXArrays(DefaultDims:Boolean);
{$endif}


implementation
uses XIDEMain,XObjectInsp,PyXUtils
{$ifndef JScript}
, XDBTable ,DllInterface
{$endif}
;
const DMAttribTypes:Array[0..3] of string = ('String','Integer','Float','Boolean');

var
  DMClassDefaultAttribs:TDefaultAttributesArray;
  DMContainsDefaultAttribs:TDefaultAttributesArray;
  DMRefDefaultAttribs:TDefaultAttributesArray;
  DMAttribDefaultAttribs:TDefaultAttributesArray;
  DMOpDefaultAttribs:TDefaultAttributesArray;
  DMParamDefaultAttribs:TDefaultAttributesArray;

{$ifndef JScript}
  BufDatasets:Array of TBufDataset;
  LocalDBPath:String;
{$endif}
  dsnames:TStringArray;


procedure ClearAllDMNodes(StartNode:TDataNode);
var
  i:integer;
begin
  for i:=length(StartNode.ChildNodes)-1 downto 0 do
  begin
    ClearAllDMNodes(StartNode.ChildNodes[i]);
    DeleteNode(StartNode,StartNode.ChildNodes[i]);
  end;
end;

procedure InitDMTree;
begin
  ClearAllDMNodes(DMRoot);
end;

function DMNameIsValid(myNode,ParentNode:TDataNode;NewName: String):Boolean;
var
  chkNode:TdataNode;
  i:integer;
  ok:Boolean;
begin
  ok:=true;
  i:=1;
  // additional checks
  //   - package names must be unique
  //   - class names must be unique across the system (as they may become dataset names)
  //   - attribute names must be unique in the class
  if (myNode.NodeType = 'DMClass') then
    chkNode:=FindDataNodeById(DMRoot,NewName,'',false)
  else
    chkNode:=FindDataNodeById(ParentNode,NewName,'',false);
  if (chkNode<>nil) and (chkNode<>myNode) then
  begin
    showmessage('Name '+NewName+' is not unique');
    ok:=false;
  end;
  result:=ok;
end;

function CreateNewDMNode(NodeParent:TDataNode;NodeType,NodeName:String):TDataNode;
var
  newNode:TDataNode;
begin
  NewNode:=TDataNode.Create('DM',NodeName,'',NodeType,true);
  NewNode.MyForm:=TForm(MainForm);

  if NodeType = 'DMClass' then
  begin
    AddDefaultAttribs(nil,NewNode,DMClassDefaultAttribs);
  end
  else if NodeType = 'DMAttrib' then
  begin
    AddDefaultAttribs(nil,NewNode,DMAttribDefaultAttribs);
  end;
  DMChanged:=true;

  AddChildToParentNode(NodeParent,NewNode,-1);
  result:=NewNode;
end;

procedure RefreshDMInspector(CurrentNode:TDataNode);
var AttributePrefix:string;
  i:integer;
  DataNode,WidgetNode:TDataNode;
  myAttribs:TNodeAttributesArray;
  s:boolean;
  BoxName,AttribValue:String;
  MyWidget:TObject;
begin
  s:=SuppressEvents;
  SuppressEvents:=true;

  if (CurrentNode<>nil)  then
  begin

      AttributePrefix:='DM'+AttributeEditorNameDelimiter+CurrentNode.NodeName;
      myAttribs:=CurrentNode.NodeAttributes;

      for i:=0 to length(myAttribs)-1 do
      begin
        //exclude Suppressed properties that user shouldn't see
        if (FindSuppressedProperty(CurrentNode.NodeType,CurrentNode.NodeAttributes[i].AttribName)<0)
        and (CurrentNode.NodeAttributes[i].AttribName<>'ParentName')
        and ((CurrentNode.NodeAttributes[i].AttribName<>'MakeXArrays') or (CurrentNode.NodeName<>'Dimensions'))
        then
        begin
          BoxName:=AttributePrefix+AttributeEditorNameDelimiter
                    +CurrentNode.NodeAttributes[i].AttribName+AttributeEditorNameDelimiter
                    +IntToStr(i+2);
          AttribValue:=CurrentNode.NodeAttributes[i].AttribValue;
          WidgetNode:=FindDataNodeById(DMAttribsNode,BoxName,'',false);
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
            end;
          end;
       end;
     end;

  end;

  SuppressEvents:=s;
end;

function DMFindClassOwners(ClassName:String):TNodesArray;
var
  allrefs:TNodesArray;
  resultSet:TNodesArray;
  i,j:integer;
begin
  setlength(resultSet,0);
  allRefs:=FindNodesOfType(DMRoot,'DMContains');
  j:=0;
  for i:=0 to length(allRefs)-1 do
  begin
    if allRefs[i].GetAttribute('Class',false).AttribValue = ClassName then
    begin
      setlength(resultSet,j+1);
      resultSet[j]:=allRefs[i];
      j:=j+1;
    end;
  end;
  result:=resultSet;
end;

procedure DMFindClassOwnerHierarchy(ClassName:String; var resultset:TNodesArray; var ownerkeys:TStringArray);
var
  allrefs,keys:TNodesArray;
  i,j:integer;
begin
  allRefs:=FindNodesOfType(DMRoot,'DMContains');
  j:=length(resultset);
  for i:=0 to length(allRefs)-1 do
  begin
    if allRefs[i].GetAttribute('Class',false).AttribValue = ClassName then
    begin
      setlength(resultSet,j+1);
      setlength(ownerkeys,j+1);
      resultSet[j]:=allRefs[i];
      keys:=DMGetKeyFields(allRefs[i].NodeParent.NodeName);
      if length(keys)>0 then
      begin
        ownerkeys[j]:=keys[0].NodeName;    //!!!! PROBLEM IF ANY COMPOSITE KEYS
      end
      else
      begin
        ownerkeys[j]:= '1';     // missing key
        showmessage('Warning - Class '+allRefs[i].NodeParent.NodeName+' has no primary key attribute defined');
      end;
      j:=j+1;
      DMFindClassOwnerHierarchy(allRefs[i].NodeParent.NodeName,resultset,ownerkeys);
    end;
  end;
end;

(*function DMOwnerKeys(ClassNode:TDataNode):TStringArray;
var
  ContainedBy,keys:TNodesArray;
  i,j:integer;
  keynames:TStringArray;
begin
  ContainedBy:=DMFindClassOwners(ClassNode.NodeName);
  setlength(keynames,0);
  j:=0;
  for i:=0 to length(ContainedBy)-1 do
  begin
    // this linkage must reference the primary key of the 'parent' class
    keys:=DMGetKeyFields(ContainedBy[i].NodeParent.NodeName);
    if length(keys)>0 then
    begin
      for j:=0 to length(keys)-1 do
      begin
        setlength(keynames,length(keynames)+1);
        keynames[length(keynames)-1]:= keys[j].NodeName;
      end;
    end
    else
    begin
      setlength(keynames,1);
      keynames[0]:= '1';     // missing key
      showmessage('Warning - Class '+ContainedBy[i].NodeParent.NodeName+' has no primary key attribute defined');
    end;
  end;
  result:=keynames;
end;
*)
function DMNodeText(CurrentItem:TDataNode):String;
var
  NodeStr:String;
  ownerkeys:TStringArray;
  i:integer;
  keys:TNodesArray;
begin
  NodeStr:='';
  setlength(ownerkeys,0);
  if CurrentItem.NodeType='DMContains' then
    NodeStr:=CurrentItem.NodeType+'('+CurrentItem.NodeName+') - '+CurrentItem.GetAttribute('Class',true).AttribValue
  else if CurrentItem.NodeType='DMRef' then
    NodeStr:=CurrentItem.NodeType+'('+CurrentItem.NodeName+') - '+CurrentItem.GetAttribute('Class',true).AttribValue
  else if CurrentItem.NodeType='DMAttrib' then
  begin
    if myStrToBool(CurrentItem.GetAttribute('IsId',true).AttribValue) = True then
      NodeStr:=CurrentItem.NodeType+'('+CurrentItem.NodeName+') *'
    else
      NodeStr:=CurrentItem.NodeType+'('+CurrentItem.NodeName+')';
  end
  else if CurrentItem.NodeType='DMClass' then
  begin
    NodeStr:=CurrentItem.NodeType+'('+CurrentItem.NodeName+')';
    //ownerkeys:=DMOwnerKeys(CurrentItem);
    DMFindClassOwnerHierarchy(CurrentItem.NodeName,keys,ownerkeys);
    for i:=0 to length(ownerkeys)-1 do
      NodeStr:=NodeStr+' *';
  end
  else
    NodeStr:=CurrentItem.NodeType+'('+CurrentItem.NodeName+')';
  result:=NodeStr;
end;

function GetDMTreeTextHierarchy(StartNode,TopRootNode:TDataNode):String;
var
  ParentNode:TDataNode;
  treeText:String;
begin
  treeText:=DMNodeText(StartNode);
  ParentNode:=StartNode.NodeParent;
  while (ParentNode<>nil) and (ParentNode<>TopRootNode.NodeParent) do
  begin
    treeText:=DMNodeText(ParentNode)+'/'+treeText;
    ParentNode:=ParentNode.NodeParent;
  end;
  result:=treeText;
end;

procedure DoSelectDataTreeNode(CurrentNode:TDataNode; refresh:boolean);
var
  mynodeText:string;
  okToContinue:Boolean;
  ContainedClassOptions, RefClassOptions, AllTypes, AllDims:Array of String;
  parentNode:TDataNode;
  ClassNode,DimsParent:TDataNode;
  ClassNodes,DimNodes:TNodesArray;
  i,j:integer;

  procedure SetMultHints(thisNode:TDataNode);
  begin
    thisNode.GetAttribute('Multiplicity',false).AttribHint:='default = 1, Integer value,  or paramname';
  end;

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

   //!!  in the data tree, node texts are NOT all unique, so need the parent-path here.
  if (DesignMode) and (okToContinue) then
  begin

     //mynodeText := CurrentNode.NodeType+'('+CurrentNode.Nodename+')';
     TreeInFocus := DMRoot;

     {$ifndef JScript}
     mynodetext := GetDMTreeTextHierarchy(CurrentNode,DMRoot);
     TXTree(DataTreeComponent.ScreenObject).SelectTreeNodeByPath(mynodeText); // (Windows) this selects the node in the datatree component, if changed.
     {$endif}

     // set the options for the ClassRef 'Class' attribute, and also OP ReturnType and Param Paramtype...
     SetLength(ContainedClassOptions,0);
     SetLength(RefClassOptions,0);
     SetLength(AllDims,0);
     //PackageNode:=FindAncestorByType(CurrentNode,'DMPkg',false);
     //if (PackageNode<>nil)
     if ((CurrentNode.NodeType = 'DMContains')
       or (CurrentNode.NodeType = 'DMRef')
       or (CurrentNode.NodeType = 'DMAttrib')
       or (CurrentNode.NodeType = 'DMParam')
       or (CurrentNode.NodeType = 'DMOp'))
     then
     begin
       if (CurrentNode.NodeType = 'DMContains')
       or (CurrentNode.NodeType = 'DMAttrib') then
       begin
         DeleteAttribOptions('DMAttrib','Multiplicity');
         DeleteAttribOptions('DMContains','Multiplicity');
         SetMultHints(CurrentNode);
         if (CurrentNode.NodeType = 'DMContains')
         and (CurrentNode.NodeParent.GetAttribute('MakeXArrays',true).AttribValue='True')
         then
         begin
           DimsParent:=FindDataNodeById(DMRoot,'Dimensions','',true);
           DimNodes:= FindNodesOfType(DimsParent,'DMAttrib');
           setlength(AllDims,length(DimNodes));
           for i:=0 to length(DimNodes)-1 do
           begin
             AllDims[i]:=DimNodes[i].NodeName;
           end;
           AddAttribOptions('DMContains','Multiplicity',AllDims);
         end;
       end;

       if ((CurrentNode.NodeType = 'DMContains')
       or (CurrentNode.NodeType = 'DMRef')
       or (CurrentNode.NodeType = 'DMParam')
         or (CurrentNode.NodeType = 'DMOp'))
       then
       begin
         AllTypes := ['String','Integer','Float','Boolean'];
         ClassNodes := FindNodesOfType(DMRoot,'DMClass',false,'');
         ClassNode := FindAncestorByType(CurrentNode,'DMClass',false);
         SetLength(ContainedClassOptions,Length(ClassNodes));
         SetLength(RefClassOptions,Length(ClassNodes));
         SetLength(AllTypes,Length(ClassNodes)+4);
         j:=0;
         for i:=0 to length(ClassNodes)-1 do
         begin
           if ClassNodes[i]<>ClassNode then
           begin
             ContainedClassOptions[j]:=ClassNodes[i].NodeName;
             RefClassOptions[j]:=ClassNodes[i].NodeName;
             AllTypes[j+4]:=ClassNodes[i].NodeName;
             j:=j+1;
           end;
         end;
         AddAttribOptions('DMContains','Class',ContainedClassOptions);
         AddAttribOptions('DMRef','Class',RefClassOptions);
         AddAttribOptions('DMOp','ReturnType',AllTypes);
         AddAttribOptions('DMParam','ParamType',AllTypes);
       end;
     end;

     if (DMSelectedDataTreeNode=nil)
     or (DMSelectedDataTreeNode<>CurrentNode) then
     begin
       //display the attributes of the selected node in the object inspector
       DMSelectedDataTreeNode:=CurrentNode;
       PopulateObjectInspector(CurrentNode);
       {$ifdef JScript}
       mynodetext := GetTreeTextHierarchyWithIndices(CurrentNode,DMRoot);
       //asm console.log('DoSelectDataTreeNode mynodetext='+mynodeText); end;
       //TXTree(DataTreeComponent).SelectedNodeText:=mynodeText;   // (Browser) this selects the node in the navtree component, if changed.
       TXTree(DataTreeComponent).SelectTreeNodeByPath(mynodeText);   // (Browser) this selects the node in the navtree component, if changed.
       {$endif}

     end
     else  if (refresh) then
     begin
       // just refresh all the displayed property values
        RefreshDMInspector(CurrentNode);
     end;

     // sort out the buttons
     XIDEForm.DMTreeAddLabel.IsVisible:=false;
     XIDEForm.DMTreeAddButton.IsVisible:=false;
     XIDEForm.DMTreeAddContainsBtn.IsVisible:=false;
     XIDEForm.DMTreeAddRefBtn.IsVisible:=false;
     XIDEForm.DMTreeAddOpBtn.IsVisible:=false;
     if  DMSelectedDataTreeNode = DMRoot then
     begin
       XIDEForm.DMTreeAddLabel.IsVisible:=true;
       XIDEForm.DMTreeAddButton.IsVisible:=true;
       XIDEForm.DMTreeAddButton.Caption:='Class';
       XIDEForm.DMTreeAddButton.Hint:='Add a class definition';
     end
     else if  DMSelectedDataTreeNode.NodeType = 'DMClass' then
     begin
       XIDEForm.DMTreeAddLabel.IsVisible:=true;
       XIDEForm.DMTreeAddButton.IsVisible:=true;
       XIDEForm.DMTreeAddButton.Caption:='Attrib';
       XIDEForm.DMTreeAddButton.Hint:='Add an attribute for this class';
       XIDEForm.DMTreeAddContainsBtn.IsVisible:=true;
       XIDEForm.DMTreeAddRefBtn.IsVisible:=true;
       XIDEForm.DMTreeAddOpBtn.IsVisible:=true;
     end
     else if  DMSelectedDataTreeNode.NodeType = 'DMOp' then
     begin
       XIDEForm.DMTreeAddLabel.IsVisible:=true;
       XIDEForm.DMTreeAddButton.IsVisible:=true;
       XIDEForm.DMTreeAddButton.Caption:='Param';
       XIDEForm.DMTreeAddButton.Hint:='Add a parameter for this operation';
     end;

  end;
end;

procedure  SelectDataTreeNode(CurrentNode:TDataNode; refresh:boolean);
begin
  if CurrentNode.NameSpace='' then
  begin
    DMSelectedDataTreeNode:=nil;
    {$ifndef JScript}
    DoSelectDataTreeNode(CurrentNode,refresh);
    {$else}
    asm
    //console.log('SelectDataTreeNode calling DoSelectDataTreeNode with CurrentNode='+CurrentNode.NodeName);
    myTimeout(pas.XDataModel.DoSelectDataTreeNode,5,'DoSelectDataTreeNode',0,CurrentNode,refresh);
    end;
    {$endif}
  end;
end;

procedure DMClearSelection;
begin
  DMSelectedDataTreeNode:=nil;
  XIDEForm.DMTreeAddLabel.IsVisible:=false;
  XIDEForm.DMTreeAddButton.IsVisible:=false;
  XIDEForm.DMTreeAddContainsBtn.IsVisible:=false;
  XIDEForm.DMTreeAddRefBtn.IsVisible:=false;
  XIDEForm.DMTreeAddOpBtn.IsVisible:=false;
end;

Function ConstructDataTreeString(CurrentItem:TDataNode; level:Integer):String;
// Recursive
var ArrayString:String;
    i,j,numchildren:integer;
begin
  // starts at DMRoot node
  if CurrentItem<>nil then
  begin
    j:=0;
    //showmessage('Currentitem='+CurrentItem.NodeClass+' '+CurrentItem.NodeType+' '+CurrentItem.Nodename);
    numchildren:=length(CurrentItem.ChildNodes);

    if level = 0 then ArrayString:=' ';

    if level=0
    then ArrayString:='['
    else
    begin
      if (NumChildren>0) then
        ArrayString:=ArrayString+',['
      else
        ArrayString:=ArrayString+',';
    end;

    ArrayString:=ArrayString+ '"'+DMNodeText(CurrentItem)+'"';

    for i:=0 to numchildren-1 do
    begin
      ArrayString:=ArrayString+ConstructDataTreeString(CurrentItem.ChildNodes[i],level+1);
    end;

    if ( NumChildren>0)
    or (level=0)  then
      ArrayString:=ArrayString+']';
  end;

  result:= ArrayString;
end;

procedure RebuildDMTree;
var
  newtreestring:string;
begin
(*
what we want to see on the navigator is....
DataRootNode
   |
   Classes...

*)

  // construct string for the data model tree
  newtreestring:= ConstructDataTreeString(DMRoot,0);
  {$ifndef JScript}
 // WriteToFile('datatree.txt',newtreestring);
  TXTree(DataTreeComponent.ScreenObject).TreeData:=newtreestring;
  {$else}
  TXTree(DataTreeComponent).TreeData:=newtreestring;
  {$endif}
  if DMSelectedDataTreeNode<>nil then
  begin
    SelectDataTreeNode(DMSelectedDataTreeNode,true);
  end;
end;

{$ifndef JScript}
procedure TDMProcs.RebuildTheTreeAndInspector(dummy:PtrInt);
begin
  RebuildDMTree;
end;
{$endif}

procedure RebuildDMTreeLater;
{$ifndef JScript}
var
  QueueRecToSend: PDMQueueRec;
begin
  New(QueueRecToSend);
  Application.QueueAsyncCall(@DMProcs.RebuildTheTreeAndInspector,PtrInt(QueueRecToSend)); // put msg into queue that will be processed from the main thread after all other messages
{$else}
begin
  RebuildDMTree;
{$endif}
end;

function TreeNodePathToNodesList(TreeNodePath:String):TNodesArray;
var
  DataNodes:TNodesArray;
  i,j:integer;
  p1:string;
  TreeNodeId:string;
  bits,bits2:TStringList;
begin
  setlength(DataNodes,0);
  // split to levels by '~/~'
  bits:= stringSplit(TreeNodePath,'~/~',false);
  j:=0;
  for i:=0 to bits.count-1 do
  begin
    //split to index and NodeText
    bits2:=stringsplit(bits[i],'~:~',false);
    if bits2.count>1 then
    begin
      setlength(DataNodes,length(DataNodes)+1);
      j:=length(DataNodes)-1;
      TreeNodeId:=TreeLabelToID(bits2[1],'DataTree',p1);
      if j=0 then
        DataNodes[j]:=FindDataNodeById(DMRoot,TreeNodeId,'',true)
      else
        DataNodes[j]:=FindDataNodeById(DataNodes[j-1],TreeNodeId,'',true);
    end;
  end;
  bits.Free;
  bits2.Free;
  result:=DataNodes;
end;

procedure HandleDataTreeClickEvent(TreeNodePath:String);
var
  CurrentNode :TDataNode;
  DataNodes:TNodesArray;
begin
  DataNodes:= TreeNodePathToNodesList(TreeNodePath);

  CurrentNode:=DataNodes[length(DataNodes)-1];
  if CurrentNode=nil then
    showmessage('Cannot find node in XObjectInsp.HandleNavTreeClickEvent')
  else
  begin
    SelectDataTreeNode(CurrentNode,false);
  end;

end;

function GetDMTreeHint(Context:TDataNode;SystemNodeName:String):String;
var
SystemNode:TDataNode;
ownerkeys:TStringArray;
keys:TNodesArray;
strg:String;
i:integer;
begin
  result:='';
  SystemNode:=FindDataNodeById(Context,SystemNodeName,'',false);
  if SystemNode<>nil then
  begin
    if SystemNode.NodeType='DMClass' then
    begin
      //ownerkeys:=DMOwnerKeys(SystemNode);
      DMFindClassOwnerHierarchy(SystemNodeName,keys,ownerkeys);
      for i:=length(ownerkeys)-1 downto 0 do
      begin
        if i<length(ownerkeys)-1 then
          strg:=strg+';';
        strg:=strg+ownerkeys[i];
      end;
      if strg<>'' then
        strg:='Parent index:'+strg+'.  ';
      strg:=strg+'index:';
      keys:=DMGetKeyFields(SystemNode.NodeName);
      for i:=0 to length(keys)-1 do
      begin
        if i>0 then
          strg:=strg+';';
        strg:=strg+keys[i].NodeName;
      end;
      if  SystemNode.NodeName = 'Dimensions' then
        strg:=strg + '  This provides a set of dimension variables for use in generating (Python) xarrays.';
      result:=strg;
    end
    else if SystemNode.NodeType='DMAttrib' then
    begin
      if SystemNode.NodeParent.GetAttribute('MakeXArrays',false).AttribValue='True' then
      begin
        strg:='xarray:  '+SystemNode.NodeParent.NodeName + '_' + SystemNode.NodeName;
      end
      else
        strg:=SystemNode.GetAttribute('Annotation',true).AttribValue;
      result:=strg;
    end;

  end;
end;

function DMTreeNodeHint(TreeLabelStr:String):String;
var
   SystemNodeName,p1:string;
begin
  SystemNodeName:=TreeLabelToID( TreeLabelStr,'DataTree',p1);   //!! may not be unique?
  result := GetDMTreeHint(DMRoot,SystemNodeName);
end;

procedure DMDataTreeNodeChange(e:TEventStatus;nodeId,NameSpace:string;myValue:string);
var
  myTree:TXTree;
  sPath:String;
begin
  {$ifndef JScript}
  myTree:=TXTree(DataTreeComponent.ScreenObject);
  {$else}
  myTree:=TXTree(DataTreeComponent);
  {$endif}
  sPath:=myTree.SelectedTextPath;

  HandleDataTreeClickEvent(sPath);
end;

procedure DeleteDMItem(SelectedNode:TDataNode);
begin
  if  (SelectedNode=nil) then
      ShowMessage('Select an item on the Data Model Tree before calling this action')
  else
  begin
    if (SelectedNode.NodeName='DMRoot')
    then
      ShowMessage('Cannot remove the root node')
    else
    begin
      DeleteNode(SelectedNode.NodeParent,SelectedNode);
      DMChanged:=true;
      if DesignMode then
      begin
        {$ifdef JScript}
        if not RunningDeployedRuntime then
        {$endif}
        SaveSystemData;
        RebuildDMTree;
      end;
    end;
  end;
end;


function AddNewDMElement(DMDestinationNode:TDataNode;AddType:String=''):boolean;
var
   ParentNode,NewNode:TDataNode;
   TreePos:integer;
   ok,TargetIsContainer:Boolean;
   NewName,NewType:String;
   DfltAttribs:TDefaultAttributesArray;
begin
  ok:=false;
  setlength(DfltAttribs,0);

  if (DMDestinationNode<>nil) then
  begin
    if DMDestinationNode=DMRoot then
    begin
      if AddType='' then
      begin
        NewType:='DMClass';
        DfltAttribs:=DMClassDefaultAttribs;
      end
      else
      begin
        NewType:=AddType;
      end;

    end
    else if DMDestinationNode.NodeType='DMClass' then
    begin
      if AddType='' then
      begin
        NewType:='DMAttrib';
      end
      else
      begin
        NewType:=AddType;
      end;
      if NewType='DMAttrib' then
        DfltAttribs:=DMAttribDefaultAttribs
      else if NewType='DMContains' then
        DfltAttribs:=DMContainsDefaultAttribs
      else if NewType='DMRef' then
        DfltAttribs:=DMRefDefaultAttribs
      else if NewType='DMOp' then
        DfltAttribs:=DMOpDefaultAttribs;
    end
    else if DMDestinationNode.NodeType='DMOp' then
    begin
      NewType:='DMParam';
      DfltAttribs:=DMParamDefaultAttribs;
    end
    else
    begin
      NewType:='DMAttrib';
      DfltAttribs:=DMAttribDefaultAttribs;
    end;

    NewNode:=CreateNewDMNode(nil,NewType,'');

    // Is the destination node a container (paste new child) or not (paste sibling)
    if (DMDestinationNode = DMRoot)
    //or (DMDestinationNode.NodeType='DMPkg')
    or (DMDestinationNode.NodeType='DMClass')
    or (DMDestinationNode.NodeType='DMOp') then
      TargetIsContainer:=true
    else
      TargetIsContainer:=false;

    if TargetIsContainer = true then
    begin
      //ShowMessage('paste under container');
      ParentNode:=DMDestinationNode;
      TreePos:=-1;
    end
    else
    begin
      // ShowMessage('paste as sibling');
      ParentNode:=DMDestinationNode.NodeParent;
      TreePos:=ParentNode.GetChildIndex(DMDestinationNode);
    end;

    if CanAddChildToParent(ParentNode,NewNode) then
    begin
      // Dialog for name entry
      NewName:=GetValidItemName('Enter name for new '+NewType,'');
      ok:=false;
      // Is the new node named uniquely?
      if (NewName<>'') and (DMNameIsValid(NewNode,ParentNode,NewName)) then
      begin
        NewNode.NodeName:=NewName;
        AddDefaultAttribs(nil,NewNode,DfltAttribs);
        AddChildToParentNode(ParentNode,NewNode,TreePos);
        ok:=true;
      end;
    end;
    if ok then
      DMChanged:=true;
  end;
  if ok then
    //SelectDataTreeNode(NewNode,false)
    DMSelectedDataTreeNode := NewNode
  else
    if NewNode<>nil then
      NewNode.DeleteMe;
  result:=ok;
end;

procedure DMAddElement(AddType:String='');
var
  ok:boolean;
begin
  if DMSelectedDataTreeNode<>nil then
  begin
    ok:=AddNewDMElement(DMSelectedDataTreeNode,AddType);
    RebuildDMTree;
  end
  else
    showmessage('Please select a DataModel node first');
end;

procedure DMDeleteElement;
begin
  if DMSelectedDataTreeNode<>nil then
  begin
    if XIDEConfirm('OK to delete element '+DMSelectedDataTreeNode.NodeName+'?') then
      DeleteDMItem(DMSelectedDataTreeNode);
  end;
end;

function DMGetKeyFields(ClassName:String):TNodesArray;
var
  classnode:TDataNode;
  i:integer;
  keyattribs:TNodesArray;
begin
  setlength(keyattribs,0);
  classNode:=FindDataNodeById(DMRoot,ClassName,'',true);
  if (classNode<>nil) and (classNode.NodeType='DMClass') then
  begin
    for i:=0 to length(classNode.ChildNodes)-1 do
    begin
       if (classNode.ChildNodes[i].NodeType='DMAttrib')
       and (myStrToBool(classNode.ChildNodes[i].GetAttribute('IsId',false).AttribValue) = True) then
       begin
         setlength(keyattribs,length(keyattribs)+1);
         //keyfields[length(keyfields)-1] := classNode.ChildNodes[i].NodeName;
         keyattribs[length(keyattribs)-1] := classNode.ChildNodes[i];
       end;
    end;
  end;
  result:=keyattribs;
end;

procedure DMAttribsCrosscheck(nodeName:String);
var
  checkNode:TDataNode;
  i,d:integer;
  f:double;
  tst,oldval:string;
  ok:boolean;
begin
  checkNode:=FindDataNodeByid(DMRoot,nodeName,'',true);
  if checkNode<>nil then
  begin
    for i:=0 to length(checkNode.NodeAttributes)-1 do
    begin
      if checkNode.NodeAttributes[i].AttribName = 'AttribType' then
      begin
        oldval:=checkNode.GetAttribute('DefaultValue',true).AttribValue;
        if checkNode.NodeAttributes[i].AttribValue = 'String' then
        begin
          tst:=checkNode.GetAttribute('StringLength',true).AttribValue;
          if checkNode.GetAttribute('StringLength',true).AttribValue = '' then
            checkNode.SetAttributeValue('StringLength','20');
        end
        else if checkNode.NodeAttributes[i].AttribValue = 'Boolean' then
        begin
          checkNode.SetAttributeValue('StringLength','');
          if (oldval <> 'True')
          and (oldval <> 'False') then
            checkNode.SetAttributeValue('DefaultValue','False');
        end
        else if checkNode.NodeAttributes[i].AttribValue = 'Integer' then
        begin
          checkNode.SetAttributeValue('StringLength','');
          ok:=TryStrToInt(oldval,d);
          if not ok then
          begin
            // old value is not an integer
            checkNode.SetAttributeValue('DefaultValue','0');
          end;
//          try
//            d:=strtoint(oldval);
//          except
//            on e:exception do
//            begin
//              // old value is not an integer
//              checkNode.SetAttributeValue('DefaultValue','0');
//            end;
//          end;

        end
        else
        begin
          checkNode.SetAttributeValue('StringLength','');
          try
            f:=strtofloat(oldval);
          except
            on e:exception do
            begin
              // old value is not a number
              checkNode.SetAttributeValue('DefaultValue','0');
            end;
          end;
        end;
      end;
    end;
  end;
end;

function DMRefKeys(refAttribNode:TDataNode):TRefKeys;
var
  refClassName:String;
  j:integer;
  keys:TNodesArray;
  refkeys:TRefKeys;
begin
  // find the id field(s) of the referenced class
  refClassName:=refAttribNode.GetAttribute('Class',false).AttribValue;
  keys:=DMGetKeyFields(refClassName);
  setlength(refkeys,length(keys));
  for j:=0 to length(keys)-1 do
  begin
    refkeys[j].RefKeyName:=refAttribNode.NodeName+'_'+keys[j].NodeName;
    refkeys[j].RefKeyType:=keys[j].GetAttribute('AttribType',false).AttribValue;
  end;
  result:=refkeys;
end;

procedure GatherAttribs1(ClassNode:TDataNode;UnitCode:TStringList);
// generates list of private fields f...
var
  i,j:integer;
  attribLine:String;
  ContainedBy,keys:TNodesArray;

  procedure AddField(AttribNode:TdataNode);
  begin
    attribLine:='    f'+AttribNode.NodeName+':';
    attribLine:=attribLine + AttribNode.GetAttribute('AttribType',false).AttribValue+';';
    UnitCode.Add(attribLine);
  end;
  procedure AddRefField(AttribNode:TdataNode);
  var
    j:integer;
    refkeys:TRefKeys;
  begin
    // find the id field(s) of the referenced class
    refkeys:=DMRefKeys(AttribNode);
    for j:=0 to length(refkeys)-1 do
    begin
      attribLine:='    f'+refkeys[j].RefKeyName+':';
      attribLine:=attribLine + refkeys[j].RefKeyType+';';
      UnitCode.Add(attribLine);
    end;
  end;

begin
  for i:=0 to length(ClassNode.ChildNodes)-1 do
  begin
    if (ClassNode.ChildNodes[i].NodeType = 'DMAttrib') then
    begin
      AddField(ClassNode.ChildNodes[i]);
    end;
    if (ClassNode.ChildNodes[i].NodeType = 'DMRef') then
    begin
      AddRefField(ClassNode.ChildNodes[i]);
    end;
  end;

  // for every 'contains' reference to this class from other classes, create a pointer field (eg owner key linkage)
  ContainedBy:=DMFindClassOwners(ClassNode.NodeName);
  for i:=0 to length(ContainedBy)-1 do
  begin
    // this linkage must reference the primary key of the 'parent' class
    keys:=DMGetKeyFields(ContainedBy[i].NodeParent.NodeName);
    for j:=0 to length(keys)-1 do
    begin
      AddField( keys[j]);
    end;
  end;

end;

procedure GatherAttribs2(ClassNode:TDataNode;UnitCode:TStringList);
// generates:
//  <name>:<type>;
//  <name>:array of <type>;  ////removed this
var
  i,j:integer;
  ContainedBy,keys:TNodesArray;

  procedure AddProperty(AttribNode:TdataNode);
  var
  {mult1,} attribLine, attrtype:String;
  begin
    //mult1:=ClassNode.ChildNodes[i].GetAttribute('Multiplicity',true).AttribValue;
    attribLine:='    property '+AttribNode.NodeName+':';
    //if (mult1 = '1') or (mult1 = '0') or (mult1 = '') then
      attribLine:=attribLine;
    //else
    //  attribLine:=attribLine + 'array of ';             //!!!! do we want this ?????
    attrtype:=AttribNode.GetAttribute('AttribType',false).AttribValue;
    attribLine:=attribLine + attrtype +' read f'+AttribNode.NodeName+' write f'+AttribNode.NodeName+';';
    UnitCode.Add(attribLine);
  end;
  procedure AddRefProperty(AttribNode:TdataNode);
  var
    refkeys:TRefKeys;
    j:integer;
    attribLine:String;
  begin
    refkeys:=DMRefKeys(AttribNode);
    for j:=0 to length(refkeys)-1 do
    begin
      attribLine:='    property '+refkeys[j].RefKeyName+':';
      attribLine:=attribLine + refkeys[j].RefKeyType +' read f'+refkeys[j].RefKeyName+' write f'+refkeys[j].RefKeyName+';';
      UnitCode.Add(attribLine);
    end;
  end;

begin
  for i:=0 to length(ClassNode.ChildNodes)-1 do
  begin
    if (ClassNode.ChildNodes[i].NodeType = 'DMAttrib') then
    begin
      Addproperty(ClassNode.ChildNodes[i]);
    end;
    if (ClassNode.ChildNodes[i].NodeType = 'DMRef') then
    begin
      AddRefProperty(ClassNode.ChildNodes[i]);
    end;
  end;

  // for every reference to this class from other classes, create a pointer field (eg owner key linkage)
  ContainedBy:=DMFindClassOwners(ClassNode.NodeName);
  for i:=0 to length(ContainedBy)-1 do
  begin
    // this linkage must reference the primary key of the 'parent' class
    keys:=DMGetKeyFields(ContainedBy[i].NodeParent.NodeName);
    for j:=0 to length(keys)-1 do
    begin
      AddProperty( keys[j]);
    end;
  end;

end;

procedure GatherOps(Compiler:TObject;ClassNode:TDataNode;DoCode:Boolean;UnitCode:TStringList);
// generates:
//  function <name>(...params...):<returntype>;
var
  i,j:integer;
  thisOp:TDataNode;
  rtype, opline, opCode, dflt:String;
  IncCode:TStringList;
begin
  if DoCode then
  begin
    UnitCode.Add('constructor '+ClassNode.NodeName+'.Create;');
    UnitCode.Add('begin');
    UnitCode.Add('end;');
  end;
  IncCode:=TStringList.Create;
  for i:=0 to length(ClassNode.ChildNodes)-1 do
  begin
    IncCode.Clear;
    if ClassNode.ChildNodes[i].NodeType = 'DMOp' then
    begin
      thisOp:=ClassNode.ChildNodes[i];
      dflt:=DfltOpCode;
      opCode:=thisOp.GetAttribute('Code',true).AttribValue;
      if (trim(opCode)<>'')
      and (opCode<>Dflt) then
      begin
        rtype:=thisOp.GetAttribute('ReturnType',true).AttribValue;
        if rtype='' then rtype:='String';
        if doCode then
          opLine:='function '+ClassNode.NodeName + '.' + thisOp.NodeName+'('
        else
          opLine:='    function '+thisOp.NodeName+'(';
        for j:=0 to length(thisOp.ChildNodes)-1 do
          if thisOp.ChildNodes[j].NodeType='DMParam' then
          begin
            if j>0 then
              opLine:=opLine+';';
            opLine:=opLine+thisOp.ChildNodes[j].NodeName+':'+thisOp.ChildNodes[j].GetAttribute('ParamType',false).AttribValue;
          end;
        opLine:=opLine + '):'+rtype+';';
        if not doCode then
          UnitCode.Add(opLine)
        else
        begin
          IncCode.Add(opLine);
          IncCode.Add(opCode);
          WriteIncFile(Compiler,thisOp.NodeName, '','tempinc/', UnitCode, IncCode);
        end;
      end;
    end;
  end;
  FreeAndNil(IncCode);
end;

procedure GatherClasses(RootNode:TDataNode;UnitCode:TStringList);
var
  i:integer;
begin

  for i:=0 to length(RootNode.ChildNodes)-1 do
    if RootNode.ChildNodes[i].NodeType = 'DMClass' then
    begin
      UnitCode.Add('  type '+RootNode.ChildNodes[i].NodeName+'=class(TPersistent)');
      UnitCode.Add('    private');
      GatherAttribs1(RootNode.ChildNodes[i],UnitCode);
      UnitCode.Add('    public');
      UnitCode.Add('      constructor Create;');
      GatherOps(nil,RootNode.ChildNodes[i],false,UnitCode);
      UnitCode.Add('    published');
      GatherAttribs2(RootNode.ChildNodes[i],UnitCode);
      UnitCode.Add('  end;');
    end;
end;

procedure GatherClassesReg(RootNode:TDataNode;UnitCode:TStringList);
var
  i:integer;
begin
  for i:=0 to length(RootNode.ChildNodes)-1 do
    if RootNode.ChildNodes[i].NodeType = 'DMClass' then
    begin
     // UnitCode.Add('       RegisterClass('+PackageNode.ChildNodes[i].NodeName+');');
    end;
end;

procedure GatherOpsCode(Compiler:TObject;RootNode:TDataNode;UnitCode:TStringList);
var
  i:integer;
begin

  for i:=0 to length(RootNode.ChildNodes)-1 do
    if RootNode.ChildNodes[i].NodeType = 'DMClass' then
    begin
      GatherOps(Compiler,RootNode.ChildNodes[i],true,UnitCode);
    end;
end;

procedure ConstructPascalDM(RunMode:String;PascalCode:TStringList);
// Create pascal code to declare the model classes
//    unit DMRoot;
//    interface
//      ...
//      <class definitions>
//      ...
//    implementation
//    end.
var
  UnitCode:TStringList;
begin
  UnitCode:=TStringList.Create;
      UnitCode.Clear;
      UnitCode.Add('unit DMRoot;');
      UnitCode.Add('{$ifdef Dll}');
      UnitCode.Add('{$mode objfpc}{$H+}{$R+}');
      UnitCode.Add('{$endif}');
      UnitCode.Add('interface');
      UnitCode.Add('uses Classes, SysUtils, Math, contnrs, dateutils,');
      UnitCode.Add('  rtlconsts, strutils, types, typinfo, EventsInterface');
      if RunMode='LazDll' then
        UnitCode.Add('  ,InterfaceTypesDll;')
      else
      begin
        UnitCode.Add('  ,InterfaceTypes;');
      end;

      GatherClasses(DMRoot,UnitCode);
      UnitCode.Add('implementation');
      {$ifndef JScript}
      GatherOpsCode(nil,DMRoot,UnitCode);
      {$else}
      GatherOpsCode(MyWebCompiler.Compiler,DMRoot,UnitCode);
      {$endif}
      UnitCode.Add('begin');
      GatherClassesReg(DMRoot,UnitCode);
      UnitCode.Add('end.');

      {$ifndef JScript}
      // save the generated pas file
      SysUtils.DeleteFile('tempinc/DMRoot.pas');
      UnitCode.SaveToFile('tempinc/DMRoot.pas');
      {$else}
      TPas2JSWebCompiler(MyWebCompiler.Compiler).WebFS.SetFileContent('DMRoot.pas',UnitCode.Text);
      {$endif}
      // add this unit to the uses list in the main module
      PascalCode.Add(',DMRoot');
//  WriteToLocalStore('DMtest.pas',PascalCode.text);
    FreeAndNil(UnitCode);
end;

procedure ShowDMOpCodeEditor(targetNode:TDataNode;BtnNodeId:String);
var
  EditBoxName, OpCode:String;
begin
  // pop up the syntax editor.
  if BtnNodeId<>'' then
  begin
    // remove the 'Btn' suffix
    EditBoxName:=Copy(BtnNodeId,1,length(BtnNodeId)-3);
    OIEditBox:=FindDataNodeById(UIRootNode,EditBoxName,'',true);
  end
  else
  begin
    OIEditBox:=nil;
  end;
  OpCode := targetNode.GetAttribute('Code',true).AttribValue;
  if Trim(OpCode)='' then
    // provide a template op function
    OpCode:= DfltOpCode;

  CodeEditForm.CodeEdit.ItemValue := OpCode;
  CodeEditForm.CodeEditInit.ItemValue := '';

  CodeEditForm.CodeEdit.MessageLines:='';
  CodeEditForm.CodeEditInit.MessageLines:='';

  CodeEditForm.Mode:='DMOpCode';
  CodeEditForm.CodeEditMainTabs.TabIndex:=0;
  CodeEditForm.InitialiseOnShow('DM Operation',targetNode.NodeName,'');
  ShowXForm('CodeEditForm',true);
end;

procedure DMMoveSiblingUpDown(UpDown:String);
var
  thisNode, myParent:TdataNode;
  c,n,i:integer;
begin
  thisNode:=DMSelectedDataTreeNode;
  if thisNode<>nil then
  begin
    myParent:=FindParentOfNode(DMRoot,thisNode);
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
        myParent.RemoveChildNode(thisNode);
        AddChildToParentNode(myParent,thisNode,n);
      end;
    end;
    RebuildDMTree;
  end;
end;

function ClassIsContained(RootNode:TDataNode;className:String):Boolean;
var i,j:integer;
  classNode:TDataNode;
  ref:String;
  isref:Boolean;
begin
  isref:=false;
  for i:=0 to length(RootNode.ChildNodes)-1 do
    if RootNode.ChildNodes[i].NodeName<>className then
    begin
      classNode := RootNode.ChildNodes[i];
      for j:=0 to length(classNode.ChildNodes)-1 do
        if classNode.ChildNodes[j].NodeType = 'DMContains' then
        begin
          ref:=classNode.ChildNodes[j].GetAttribute('Class',false).AttribValue;
          if ref = className then
            isref:=true;
        end;
    end;
  result:=isref;
end;

function AllDataClassNames(RootName:String):TStringList;
var
  i,j:integer;
  classes:TStringList;
  RootNode,classNode:TDataNode;
  className:String;
begin
  classes := TStringList.Create;
  RootNode:=FindDataNodeById(DMRoot,RootName,'',true);
  if RootNode<>nil then
  begin
    for i:=0 to length(RootNode.ChildNodes)-1 do
    begin
      if RootNode.ChildNodes[i].NodeType = 'DMClass' then
        if myStrToBool(RootNode.ChildNodes[i].GetAttribute('MakeDataset',true).AttribValue) = true then
        begin
          classNode := RootNode.ChildNodes[i];
          className := classNode.NodeName;
          classes.Add(className);
        end;
    end;
  end;
  result:=classes;
end;

function DMGetClassFields(DMClassName:String):TClassFieldDefs;
var
  cflds:TClassFieldDefs;
  i:integer;
  DMFielddef:TClassFieldDef;
  DMClassNode:TDataNode;
begin
  SetLength(cflds,0);
  DMClassNode := FindDataNodeById(dmroot,DMClassName,'',true);
  if DMClassNode<>nil then
  begin
    for i:=0 to length(DMClassNode.ChildNodes)-1 do
    begin
      if (DMClassNode.ChildNodes[i].NodeType='DMAttrib') then
      begin
        Setlength(cflds,length(cflds)+1);
        cflds[length(cflds)-1].FieldName := DMClassNode.ChildNodes[i].NodeName;
        cflds[length(cflds)-1].FieldType := DMClassNode.ChildNodes[i].GetAttribute('AttribType',false).AttribValue;
        cflds[length(cflds)-1].DfltValue := DMClassNode.ChildNodes[i].GetAttribute('DefaultValue',false).AttribValue;
      end;
    end;
  end;
  result:=cflds;
end;



{$ifndef JScript}
function GetDataset(DSName:String;var idx:integer;showError:Boolean=true):TBufDataset;
var
  i:integer;
begin
  result:=nil;
  idx:=-1;
  for i:=0 to length(BufDatasets)-1 do
  begin
    if BufDatasets[i].Name = DSName then
    begin
      result:=BufDatasets[i];
      idx:=i;
    end;
  end;
  if (showError) and (result=nil) then
    showmessage('Cannot find Dataset '+DSName); //....nb. this pauses the thread and allows queued events to happen
end;
function DSSetObjPropValue(ds:TBufDataset;fd:TFieldDef;recObject:TObject;f:TField):Boolean;
begin
  result:=true;
  if fd.DataType = ftString then
    SetStrProp(recObject,fd.Name,f.AsString)
  else if fd.DataType = ftFloat then
    SetFloatProp(recObject,fd.Name,f.AsFloat)
  else if fd.DataType = ftInteger then
    SetOrdProp(recObject,fd.Name,f.AsInteger)
  else if fd.DataType = ftBoolean then
    SetBooleanProperty(recObject,fd.Name,f.AsBoolean);
end;
function DSGetCurrentRecordAsObject(DSName:String;recObject:TObject):Boolean;
var
  i,oldidx:integer;
  fn:String;
  ds:TBufDataset;
  ok:Boolean;
begin
  result:=true;
  ds:=GetDataset(DSName,oldidx);
  if (ds <> nil) and (recObject<>nil) then
  begin
    ok:=true;
    for i:=0 to ds.FieldCount-1 do
    if ok then
    begin
      fn:=ds.FieldDefs[i].Name;
      ok:= DSSetObjPropValue(ds,ds.FieldDefs[i],recObject,ds.Fields[i]);
    end
    else
    begin
      showmessage('Failed to get field '+fn+' from the data store');
      result:=false;
      EXIT;
    end;
  end
  else
  begin
    result:=false;
  end;
end;
{$endif}

{$ifndef JScript}
function DSGetIndexedRecordAsObject(DSName,AsyncProcName,keynames:String;keyvalues:TVarArray;recObject:TObject;ReturnEvent:TEventStatus):Boolean;
var
  ok:Boolean;
  ds:TBufDataset;
  i:integer;
begin
  ok:=true;
  ds:=GetDataset(DSName,i);
  if (ds <> nil) then
  begin
    // emulate the JS IndexedDB pattern here.... table has a predefined 'key' which is a set of fields; value is a list of values...
    // In the TBufDataset in desktop mode, the Locate function wants the name(s) of predefined indexes, with a corresponding set of values.
    // Here,we want to return a single record using a single 'primary key' value.
    //ok:=ds.Locate(idxname,idxvalue,[]);    //(loCaseInsensitive, loPartialKey);
    //ok:=ds.Locate(DSName+'Idx',VarArrayOf(keyvalues),[]);    //(loCaseInsensitive, loPartialKey);
    ok:=ds.Locate(keynames,VarArrayOf(keyvalues),[]);    //(loCaseInsensitive, loPartialKey);
    //ok:=ds.Locate('c1key;s1attr1',VarArrayOf(['rec 2','99']),[]);    //(loCaseInsensitive, loPartialKey);
    if ok then
    begin
      ok:= DSGetCurrentRecordAsObject(DSName,recObject);
      if ok then
      begin
        SetLength(ReturnEvent.AsyncReturnObject,length(ReturnEvent.AsyncReturnObject)+1);
        ReturnEvent.AsyncReturnObject[length(ReturnEvent.AsyncReturnObject)-1]:=recObject;
      end;
    end;
  end
  else
    ok:=false;
  DSReturnToEvent(ReturnEvent,AsyncProcName);
{$else}
function DSGetIndexedRecordAsObject(DSName,AsyncProcName:String;keyvalues:TStringArray;ReturnEvent:TEventStatus):Boolean;
// this function is asynchronous.  The data object that is returned on success will be picked
// up in an event handler 'Main' section, in the e.AsyncReturnObject property.
var
  ok:Boolean;
begin
  ok:=true;
  asm
  console.log('looking for key '+keyvalues+' in '+DSName);
  var transaction = db.transaction([DSName],'readonly');
  // report on the success of the transaction completing, when everything is done
  transaction.oncomplete = function(event) {
    console.log('Transaction completed.');
    pas.InterfaceTypes.DSReturnToEvent(ReturnEvent,AsyncProcName);
  };

  transaction.onerror = function(event) {
    console.log('Transaction not opened due to error: ' + event.target.error);
  };

  var objectStore = transaction.objectStore(DSName);
  var request = objectStore.get(keyvalues);    //primary key is an array of values (for the 'IsId' attributes)

  request.onerror = function(event) {
    // Handle errors!
    console.log('Error in DSGetIndexedRecordAsObject ',event.target.error);
  };
  request.onsuccess = function(event) {
    var i=ReturnEvent.AsyncReturnObject.length;
    ReturnEvent.AsyncReturnObject.push(ReturnEvent.ValueObject);
    // Do something with the request.result...
    if (request.result) {
      //console.log("found record" );
      //console.log(request.result);
      //......... returnobject has to be the original class type...
      var value;
      Object.keys(request.result).forEach(function(key) {
        value = request.result[key];
        //console.log('key='+key+' value='+value);
        ReturnEvent.AsyncReturnObject[i]['f'+key] = value;
      });
      //console.log('returning...',i);
      //console.log(ReturnEvent.AsyncReturnObject[i]);

    }
    else {
      if (ReturnEvent) {ReturnEvent.AsyncReturnObject[i]=null;}
      console.log('record with key '+keyvalues+' not found in '+DSName);
    }
   };
  end;
{$endif}
  result:=ok;
end;

{$ifndef JScript}
function DSDeleteARow(ReturnEvent:TEventStatus;DSName,AsyncProcName,keynames:String;DSKeyValues:TVarArray):Boolean;
var
  ok:Boolean;
  ds:TBufDataset;
  i:integer;
begin
  ok:=true;
  ds:=GetDataset(DSName,i);
  if (ds <> nil) then
  begin
    // emulate the JS IndexedDB pattern here.... table has a predefined 'key' which is a set of fields; value is a list of values...
    // In the TBufDataset in desktop mode, the Locate function wants the name(s) of predefined indexes, with a corresponding set of values.
    // Here,we want to return a single record using a single 'primary key' value.
    ok:=ds.Locate(keynames,VarArrayOf(DSKeyValues),[]);    //(loCaseInsensitive, loPartialKey);
    if ok then
    begin
      ds.Delete;
    end;
  end
  else
    ok:=false;
  DSReturnToEvent(ReturnEvent,AsyncProcName);
end;
{$else}
function DSDeleteARow(ReturnEvent:TEventStatus;DSName,AsyncProcName:String;DSKeyValues:TStringArray):Boolean;
var
  ok:Boolean;
begin
  ok:=true;
  asm
  console.log('DSDeleteARow looking for key ',DSKeyValues,' in '+DSName);
  var transaction = db.transaction([DSName],'readwrite');
  // report on the success of the transaction completing, when everything is done
  transaction.oncomplete = function(event) {
    console.log('DSDeleteARow Transaction completed.');
    pas.InterfaceTypes.DSReturnToEvent(ReturnEvent,AsyncProcName);
  };

  transaction.onerror = function(event) {
    console.log('Transaction not opened due to error: ' + event.target.error);
  };

  var objectStore = transaction.objectStore(DSName);
  var request = objectStore.delete(DSKeyValues);    //primary key is an array of values (for the 'IsId' attributes)

  request.onerror = function(event) {
    console.log('Error in DSDeleteARow ',event.target.error);
  };
  request.onsuccess = function(event) {
    console.log('record deleted');
   };
  end;
  result:=ok;
end;
{$endif}

function DSAppendRecordFromObject(DSName,AsyncProcName:String;recObject:TObject;ReturnEvent:TEventStatus):Boolean;
var
  ok:Boolean;
  DMClassNode:TDataNode;
{$ifndef JScript}
  ds:TBufDataset;
  i:integer;
begin
  //XIDEMain.XIDEForm.XMemo1.ItemValue:=XIDEMain.XIDEForm.XMemo1.ItemValue+LineEnding+DSName+' DSAppendRecordFromObject';
  ok:=true;
  ds:=GetDataset(DSName,i);
  if (ds <> nil) then
  begin
    try

      ds.AppendRecord([]);       // empty record appended to dataset
      DSSetRecordFromObject(DSName,recObject);
    except
      on e:exception do
      begin
        showmessage('failed to append row to Dataset '+DSName+' '+E.Message);
        ok:=false;
      end;
    end;
  end
  else
  begin
    ok:=false;
  end;
  DSReturnToEvent(ReturnEvent,AsyncProcName);
{$else}

begin
  ok:=true;

    // find the data model description for this dsName
    DMClassNode := findDataNodeById(dmroot,DSName,'',true);
    if (DMClassNode=nil) then
    begin
      showmessage('Cannot find Data Model description for classname '+DSName);
      EXIT;
    end;

    asm
    // open a read/write db transaction, ready for adding the data
    console.log('opening readwrite transaction for '+DSName);
    var transaction = db.transaction([DSName], "readwrite");    // can be more datasets here in the array

    // report on the success of the transaction completing, when everything is done
    transaction.oncomplete = function(event) {
      console.log('DSAppendRecordFromObject readwrite transaction for '+DSName+' completed.');
      pas.InterfaceTypes.DSReturnToEvent(ReturnEvent,AsyncProcName);
    };

    transaction.onerror = function(event) {
      console.log('DSAppendRecordFromObject Transaction not opened due to error. ',event.target.error);
      pas.InterfaceTypes.DSReturnToEvent(ReturnEvent,AsyncProcName);
    };

    // create an object store on the transaction
    var objectStore = transaction.objectStore(DSName);

    // Make a JS object (DSItem) from the supplied class
    var DSItem = {};
    for (var i=0; i<rtl.length(DMClassNode.ChildNodes); i++) {
      if (DMClassNode.ChildNodes[i].NodeType=='DMAttrib') {
        var cmd = 'DSItem.'+DMClassNode.ChildNodes[i].NodeName+' = recObject.f'+DMClassNode.ChildNodes[i].NodeName+';';
        //console.log(cmd);
        eval(cmd)
      }
      if (DMClassNode.ChildNodes[i].NodeType=='DMRef') {
        var keys = pas.XDataModel.DMRefKeys(DMClassNode.ChildNodes[i]);
        for (var j=0; j<keys.length; j++) {
          var cmd = 'DSItem.'+keys[j].RefKeyName+' = recObject.f'+keys[j].RefKeyName+';';
          //console.log(cmd);
          eval(cmd)
        }
      }
    }

    // Make a request to add our DSItem object to the object store
    //console.log('Adding: '+DSItem.toString());
    //console.log('key: '+DSItem.c1att1);
    var objectStoreRequest = objectStore.add(DSItem);

    objectStoreRequest.onsuccess = function(event) {
      // report the success of our request
      console.log('DSAppendRecordFromObject Add request successful.');
    };
    objectStoreRequest.onerror = function(event) {
      // do something else if it didn't work....
      console.log('DSAppendRecordFromObject Add request failed. ',event.target.error);
    };
    end;
{$endif}
  result:=ok;
end;


procedure DoFilter(DSName:String;keyNames:TStringArray;keysLow,keysHigh:TStringArray;callbackfunc:TObject=nil);
{$ifndef JScript}
var
  ds:TBufDataset;
  i,numrecs:integer;
  //returnedRows:???array of records????
  ok:Boolean;
  filterText:String;
{$endif}
begin
  {$ifdef JScript}
  asm
    var returnedRows = [];
    var keyRangeValue = IDBKeyRange.bound(keysLow, keysHigh);     //upper and lower bounds
    const transaction = db.transaction([DSName], 'readonly');
    const theStore = transaction.objectStore(DSName);
    const cursorRequest = theStore.openCursor(keyRangeValue);
    cursorRequest.onsuccess = e => {
      const cursor = e.target.result;
      if (cursor) {
            //if (cursor.value.vendor === 'GE') {    //....further filtering???
              returnedRows.push(cursor.value);
            }
            cursor.continue();
      }
      else {
        callbackfunc(returnedRows);
      }
    }
    (*............composite keys example.....
    index.openCursor([lowX, lowY], [highX, highY]).onsuccess = function(e) {
    var cursor = e.target.result;
    if (!cursor) return; // done!

    var x = cursor.key[0], y = cursor.key[1];
    // assert(lowX <= x && x <= highX);
    if (y < lowY) {
        cursor.continue([x, lowY]);
    } else if (y > highY) {
        cursor.continue([x + 1, lowY]);
    } else {
        processRecord(cursor.value); // we got one!
        const updateRequest = cursor.update(cursor.value);
        cursor.continue();
    }
    };
      *)
  end;
  {$else}
  ok:=true;
  ds:=GetDataset(DSName,i);
  if (ds <> nil) then
  begin
  /// build filtertext from the keys parameters
    filterText:='(';
    for i:=0 to length(keyNames)-1 do
    begin
      filterText := filterText + '(' + keyNames[i] + '>=''' + keysLow[i] + ''')';
      filterText := filterText + ' and (' + keyNames[i] + '<=''' + keysHigh[i] + ''')';
    end;
    filterText:=filterText + ')';

    ds.Filter:=filterText;        // also see filteroptions
    ds.Filtered:=true;
    try
        numrecs := 0;
        ds.disablecontrols;
        ds.first;
        while not ds.eof do
        begin
           numrecs := numrecs + 1;
           //add to returnedRows???
           ds.next
        end;
        showmessage('filtered count '+inttostr(numrecs));
      finally
        ds.enablecontrols;
      end;
  end;
  (*
  function DSApplyFilter(DSName:String; filterText:String):Boolean;
  var
    ds:TBufDataset;
    i:integer;
    ok:Boolean;
  begin
    ok:=true;
    ds:=GetDataset(DSName,i);
    if (ds <> nil) then
    begin
      ds.Filter:=filterText;        // also see filteroptions
      ds.Filtered:=true;
    end;
  end;
  *)
  {$endif}
end;

{$ifndef JScript}
function DSFieldType(AttribNode:TDataNode):TFieldType;
var
  ft:TFieldType;
  DMType,mult:String;  //('String','Integer','Float','Boolean');
  makeds:Boolean;
begin
  DMType:=AttribNode.GetAttribute('AttribType',false).AttribValue;
  mult:=AttribNode.GetAttribute('Multiplicity',false).AttribValue;
  makeds:=MyStrToBool(AttribNode.NodeParent.GetAttribute('MakeDataset',false).AttribValue);
  if ((mult='1') or (mult='0') or (mult=''))
  or (makeds) then
  begin
    if (DMType = 'String') then ft:=ftString
    else if (DMType = 'Integer') then ft:=ftInteger
    else if (DMType = 'Float') then ft:=ftFloat
    else if (DMType = 'Boolean') then ft:=ftBoolean;
  end;
  result:=ft;
end;

function DSSetFieldValue(ds:TBufDataset;fd:TFieldDef;recObject:TObject):Boolean;
var
  v:variant;
begin
  result:=true;
  v:=GetPropValue(recObject,fd.Name);
  ds.Edit;
  ds.FieldByName(fd.Name).Value:=v;
end;

function DSSetRecordFromObject(ds:TBufDataset;recObject:TObject):Boolean;
var
  i:integer;
  fn:String;
  ok:Boolean;
begin
  result:=true;
    ok:=true;
    ds.Edit;
    for i:=0 to ds.FieldCount-1 do
    if ok then
    begin
      fn:=ds.FieldDefs[i].Name;
      //ok:= DSSetObjPropValue(ds,ds.FieldDefs[i],recObject,ds.Fields[i]);
      //XIDEMain.XIDEForm.XMemo1.ItemValue:=XIDEMain.XIDEForm.XMemo1.ItemValue+LineEnding+ds.Name+' DSSetRecordFromObject';
      ok:= DSSetFieldValue(ds,ds.FieldDefs[i],recObject);
    end
    else
    begin
      showmessage('Failed to get field '+fn+' from the data store ');
      result:=false;
      EXIT;
    end;
    ds.Post;

end;
function DSSetRecordFromObject(DSName:String;recObject:TObject):Boolean;
var
  ds:TBufDataset;
  i:integer;
begin
  result:=true;
  ds:=GetDataset(DSName,i);
  if (ds <> nil) then
  begin
    result:=DSSetRecordFromObject(ds,recObject);
  end
  else
  begin
    result:=false;
  end;
end;

function DSInsertRecordFromObject(DSName:String;recObject:TObject):Boolean;
var
  ok:Boolean;
  ds:TBufDataset;
  i:integer;
begin
  ok:=true;
  ds:=GetDataset(DSName,i);
  if (ds <> nil) then
  begin
    try
      //BufDatasets[0].InsertRecord(['1','2']);
      ds.InsertRecord([]);        // empty record inserted at current cursor location
      DSSetRecordFromObject(DSName,recObject);
    except
      on e:exception do
      begin
        showmessage('failed to insert row into Dataset '+DSName+' '+E.Message);
        ok:=false;
      end;
    end;
  end
  else
  begin
    ok:=false;
  end;
  result:=ok;
end;
{$endif}

{$ifndef JScript}
procedure DeleteDataset(DSName:String);
var
  i:integer;
  found:boolean;
  ds:TBufDataset;
begin
  found:=false;
  for i:=0 to length(BufDatasets)-1 do
  begin
    if BufDatasets[i].Name = DSName then
    begin
      found:=true;
      ds:=BufDatasets[i];
    end
    else
      if found then
        BufDatasets[i-1]:=BufDatasets[i];
  end;
  if found then
    setLength(BufDatasets,length(BufDatasets)-1);
  ds.Delete;
end;

function CreateNewDS(DSName:String):TBufDataset;
var
  ds:TBufDataset;
  pk{,mult}:string;
  classnode:TDataNode;
  i,j:integer;
  ContainedBy,keys:TNodesArray;

  procedure AddFieldDef(AttribNode:TdataNode);
  var
    fn:String;
    f:TField;
    ft:TFieldType;
    slen:integer;
  begin
    fn:=AttribNode.NodeName;
    try
    f:=ds.FieldByName(fn);
    except
      on E:Exception do
      begin
        ft:=DSFieldType(AttribNode);
        if ft=ftString then
        begin
          slen:=StrToInt(AttribNode.GetAttribute('StringLength',true).AttribValue);
          ds.FieldDefs.Add(AttribNode.NodeName,ft,slen);
        end
        else
          ds.FieldDefs.Add(AttribNode.NodeName,ft);
      end;
    end;
  end;
  procedure AddRefFieldDef(AttribNode:TdataNode);
  var
    refClassName,fn:String;
    keys:TNodesArray;
    j:integer;
    f:TField;
    ft:TFieldType;
    slen:integer;
  begin
    // find the id field(s) of the referenced class
    refClassName:=AttribNode.GetAttribute('Class',false).AttribValue;
    keys:=DMGetKeyFields(refClassName);
    for j:=0 to length(keys)-1 do
    begin
      fn:=AttribNode.NodeName+'_'+keys[j].NodeName;
      try
      f:=ds.FieldByName(fn);
      except
        on E:Exception do
        begin
          ft:=DSFieldType(keys[j]);
          if ft=ftString then
          begin
            slen:=StrToInt(keys[j].GetAttribute('StringLength',true).AttribValue);
            ds.FieldDefs.Add(fn,ft,slen);
          end
          else
            ds.FieldDefs.Add(fn,ft);
        end;
      end;
    end;
  end;
begin
  ds := TBufDataSet.Create(nil);
  ds.Name := DSName;
  pk:='';
  classnode:=FindDataNodeById(DMRoot,DSName,'',true);
  for j:=0 to length(classnode.ChildNodes)-1 do
  begin
    if classnode.ChildNodes[j].NodeType = 'DMAttrib' then
    begin
      //mult:=classnode.ChildNodes[j].GetAttribute('Multiplicity',false).AttribValue;
      //if (mult='1') or (mult='0') or (mult='') then
      begin
        AddFieldDef( classnode.ChildNodes[j]);
        // Add an index for the 'IsId' field... (composite primary key) ???? enforce uniqueness ????
        if myStrToBool(classnode.ChildNodes[j].GetAttribute('IsId',true).AttribValue) = True then
        begin
          if pk<>'' then pk:=pk+';';
          pk:=pk+classnode.ChildNodes[j].NodeName;
        end;
      end;
    end;
    if classnode.ChildNodes[j].NodeType = 'DMRef' then
    begin
      AddRefFieldDef( classnode.ChildNodes[j]);
    end;
  end;

  // for every 'contains' reference to this class from other classes, create a pointer field (eg owner key linkage)
  ContainedBy:=DMFindClassOwners(ClassNode.NodeName);
  for i:=0 to length(ContainedBy)-1 do
  begin
    // this linkage must reference the primary key of the 'parent' class
    keys:=DMGetKeyFields(ContainedBy[i].NodeParent.NodeName);
    for j:=0 to length(keys)-1 do
    begin
      AddFieldDef( keys[j]);
    end;
  end;

  ds.IndexDefs.Add(classnode.NodeName+'Idx',pk,[ixCaseInsensitive,ixUnique]);   //!!!! ixUnique not working.
  //???? other indexes ??
  ds.CreateDataset;
  result:=ds;
end;

procedure RefreshDBGrids;
var
  allgrids:TNodesArray;
  i:integer;
begin
  XDBTable.ProjectBufDatasets:=BufDatasets;
  allgrids:=FindNodesOfType(UIRootNode,'TXDBTable');
  for i:=0 to length(allgrids)-1 do
  begin
    TXDBTable(allgrids[i].ScreenObject).DSName := TXDBTable(allgrids[i].ScreenObject).DSName;
  end;
end;

{$endif}

{$ifdef JScript}
procedure DSAddAllRows(DSName:String;allrows,newstore:TObject);
begin
  asm
    // copy data from oldstore to newstore, field-by-field, to ensure correspondence with the
    // latest data model.
    var classFieldNodes = pas.XDataModel.DMGetClassFields(DSName);
    allrows.forEach(data => {
      var dsobject = {};
      classFieldNodes.forEach(fldNode => {
        if (data[fldNode.FieldName]) {
          dsobject[fldNode.FieldName] = data[fldNode.FieldName]; }
        else {
          if (fldNode.FieldType == 'Integer') {dsobject[fldNode.FieldName] = parseInt(fldNode.DfltValue);}
          else if (fldNode.FieldType == 'Float') {dsobject[fldNode.FieldName] = parseFloat(fldNode.DfltValue);}
          else {dsobject[fldNode.FieldName] = fldNode.DfltValue }
          }
        });
      console.log('Add ',dsobject);
      try {
        let request = newstore.add(dsobject);        //// fails....transaction no longer active
        request.onsuccess = function(event) {
          console.log('Add row done');
          };
        request.onerror = function(event) {
          console.log('Add row failed. ',event.target.error);
          };
      } catch(err) {console.log(err.message); }
    });
  end;
end;
procedure DSCopyData(DSName:String;oldstore,newstore:TObject);
begin
  asm
    console.log('DSCopyData '+DSName);
    //console.log(oldstore);
    //console.log(newstore);
    try {
      var allRecords = oldstore.getAll();
    } catch(err) {console.log(err.message); }

    allRecords.onerror = function() {
      console.log('DSCopyData getAll failed. ',event.target.error);
    }
    allRecords.onsuccess = function() {
      console.log('DSCopyData allRecords success. ',event.target.result);

      pas.XDataModel.DSAddAllRows(DSName,event.target.result,newstore);
 //     event.target.result.forEach(data => {
 //       var dsobject = {};
 //       classFieldNodes.forEach(fldNode => {
 //         if (data[fldNode.FieldName]) {
 //           dsobject[fldNode.FieldName] = data[fldNode.FieldName]; }
 //         else {
 //           if (fldNode.FieldType == 'Integer') {dsobject[fldNode.FieldName] = parseInt(fldNode.DfltValue);}
 //           else if (fldNode.FieldType == 'Float') {dsobject[fldNode.FieldName] = parseFloat(fldNode.DfltValue);}
 //           else {dsobject[fldNode.FieldName] = fldNode.DfltValue }
 //           }
 //         });
 //       console.log('Add ',dsobject);
 //       try {
 //         let request = newstore.add(dsobject);        //// fails....transaction no longer active
 //       } catch(err) {console.log(err.message); }
 //       request.onsuccess = function(event) {
 //         console.log('Add row done');
 //         };
 //       request.onerror = function(event) {
 //         console.log('Add row failed. ',event.target.error);
 //         };
 //     });
    }
  end;
end;
procedure DSCopyDataFromOldDB(DSName:String;oldstore:TObject);
begin
  asm
    console.log('DSCopyDataFromOldDB '+DSName);
    try {
      var allRecords = oldstore.getAll();
    } catch(err) {console.log(err.message); }

    allRecords.onerror = function() {
      console.log('DSCopyDataFromOldDB getAll failed. ',event.target.error);
    }
    allRecords.onsuccess = function() {
      console.log('DSCopyDataFromOldDB allRecords success. ',event.target.result);


      var txNew = db.transaction([DSName], "readwrite");
      txNew.onerror = function(event) {
        console.log('DSCopyDataFromOldDB txNew failed: ', txNew.error );
      };
      var newstore = txNew.objectStore(DSName);
      console.log('newstore ',newstore);

      pas.XDataModel.DSAddAllRows(DSName,event.target.result,newstore);
    }
  end;
end;

procedure IndexedDBOpenWithVersion(DBName:String;vnum:integer;dsnames:TStringArray;callbackFunc:TObject);
begin
  asm
    function DSSaveOldData(dsnames,tx) {
      try {
      console.log('DSSaveOldData ',dsnames);
      console.log('Saving existing IndexedDB data ');

      for (var i=0; i<rtl.length(dsnames); i++) {
        try {
          var objectStore = tx.objectStore(dsnames[i]);
          try {
            var existingsavedstore = tx.objectStore('DSSave_'+dsnames[i]);
            db.deleteObjectStore('DSSave_'+dsnames[i]);
            }
          catch(e) {
            // nothing to delete
          }
          objectStore.name = 'DSSave_'+ objectStore.name;
          } catch(err) { console.log(err.message+'  in DSSaveOldData '); }
        }

      } catch(err) { alert(err.message+'  in DSSaveOldData '); }
    }

    function CreateObjectStores(dsnames,request) {
    // Create objectStores for the defined classes
    var objectStore;
    var storename;
    console.log('CreateObjectStores. num of defined classes = ',rtl.length(dsnames));
    for (var i=0; i<rtl.length(dsnames); i++) {
      storename = dsnames[i];

      var classnode = pas.NodeUtils.FindDataNodeById(pas.NodeUtils.DMRoot,storename,'',true);
      try {
        objectStore = request.transaction.objectStore(storename);
        console.log('deleting old objectStore '+storename);
        // since there was a version change, we have already saved the old data in this store to a string.
        // The action here therefore is to delete this objectstore, then recreate, and populate from the string.
        db.deleteObjectStore(storename);
      }
      catch(e) {//nothing to delete
      }

      console.log('creating new objectStore '+storename);
      // primary key info....
      var pk=[];
      for (var j=0; j<rtl.length(classnode.ChildNodes); j++) {
        if (classnode.ChildNodes[j].HasAttribute('IsId')) {
          var idattr = classnode.ChildNodes[j].GetAttribute('IsId',true).AttribValue.toUpperCase();
          if (idattr == 'TRUE') {
            pk.push(classnode.ChildNodes[j].NodeName );
            }
        }
      }
      console.log('  keyPath is '+pk);
      objectStore = db.createObjectStore(storename,{keyPath:pk});

      // if there is saved data for this store, then re-insert it here....
      try {
        var savedStore = request.transaction.objectStore('DSSave_'+storename);
        // copy back the saved data into the new objectstore
        pas.XDataModel.DSCopyData(storename,savedStore,objectStore);
      } catch(err) { console.log('No saved data for objectstore '+storename); }

      // ...more indexes????
      //objectStore.createIndex("hours", "hours", { unique: false });
    }

    db.onversionchange = function(event) {
      //note.innerHTML += '<li>a database change has occurred; you should refresh this
      //                   browser window, or close it down and use the other open version of
      //                   this application, wherever it exists.</li>';
      alert("a database change has occurred; you should refresh this browser window, or close it down and use the other open version of this application, wherever it exists.");
      };
    }

    console.log('openWithVersion opening '+DBName);
    var request = window.indexedDB.open(DBName, vnum);  //version number cannot be zero, and has to go UP.
    request.onerror = function(event) {
        console.log("Database "+DBName+" Open request failed"+" "+event.target.error);
        alert("Warning: Local Database "+DBName+" Open request failed"+" "+event.target.error);
        callbackFunc(false);     // continue to run mode.
        };
    request.onsuccess = function(event) {
        // db has opened at the requested version number.
        console.log("Local IndexedDB Database "+DBName+" Open OK");
        db = event.target.result;
        callbackFunc(true);
        };
    request.onblocked = function(event) {
        console.log('db open request blocked...');
        //event.target.result.close();
        }
    request.onupgradeneeded = function(event) {
        // this event happens when the version number passed in is higher than the version number
        // found on the existing database, OR if there is no database yet.
        db = event.target.result;
        console.log("onupgradeneeded event.  db version is "+db.version);
        console.log("DB Old Version: " + event.oldVersion);
        console.log("DB New Version: " + event.newVersion);

        DSSaveOldData(dsnames,event.target.transaction);
        CreateObjectStores(dsnames,event.target);
        }
  end;
end;

{$endif}

function BuildLocalDB(iterationNum:integer; callbackFunc:TObject=nil):Boolean;
// Called as part of the 'ToggleToRunMode' processes.
// Classes have been specified by the user in the data model, and incorporated into the user's compiled event code
// as class type definitions.
// This procedure generates a local database store, containing a dataset for each class.
//              (Browser - IndexedDB objectStore;  desktop - TBufDataset)
//
// tbd.......
//   make the data persist between design/run modes if there are no changes to the data model.
//   If the data model has been changed, what do we do????????????
//              - delete the whole database, and rebuild?
//              - find the changes, and ....what???
//                    - new attributes
//                    - deleted attributes
//                    - changed attributes (type change, index status)
//                    - new tables
//                    - deleted tables
//                    - changes to indexes
//                    - changes to subclass relationships
//
//
var
  ok:Boolean;
  classes:TStringList;
  i,j:integer;
  DBName:String;
  classnode:TDataNode;
  {$ifndef JScript}
  recbuf:TRecordBuffer;
  ft:TFieldType;
  pk:string;
  oldDS:TBufDataset;
  oldidx:integer;
  //LocalDBPath:String;
  {$else}
  lDSNames:TStringArray;
  {$endif}
begin
  ok:=true;
  dbname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;

  {$ifndef JScript}
  LocalDBPath:='SavedSystems/'+dbname;
  if not DirectoryExists(LocalDBPath) then
    CreateDir(LocalDBPath);
  {$endif}

  classes:=AllDataClassNames('DMRoot');

  setlength(dsnames,classes.count);
  for i:=0 to classes.Count-1 do
    dsnames[i]:=classes[i];
  if classes.count=0 then
  begin
    result:=true;
    {$ifdef JScript}
    asm callbackFunc(ok); end;    // eg. continue to run mode
    {$endif}
    EXIT;
  end;

  if DMChanged then    // user has made changes to the data model
  begin
    iterationNum:=iterationNum+1;
    UIRootNode.SetAttributeValue('DBVersion',IntToStr(iterationNum));
  end;

  {$ifndef JScript}
  // build one dataset (type TBufDataset) for each class.
  // Create one field def (myobject) for the defined object type.
  // Handle changes to class definitions since the last build.
  for i:=0 to classes.count-1 do
  begin
    oldDS := GetDataset(classes[i],oldidx,false);
    if (oldDS = nil) then
    begin
      // there may be saved data to load...
      if (fileexists(LocalDBPath +'/'+classes[i])) then
      begin
        oldds := TBufDataset.Create(nil);
        //oldds.LoadFromFile('Datasets/'+classes[i]);
        oldds.LoadFromFile(LocalDBPath +'/'+classes[i]);
      end;
      setlength(BufDatasets,length(BufDatasets)+1);
      BufDatasets[length(BufDatasets)-1] := CreateNewDS(classes[i]);
      if oldds<>nil then
        DSLoadFromSaved( classes[i],oldDS,BufDatasets[length(BufDatasets)-1]);
    end
    else
    begin
      oldDS.DisableControls;
      if (DMChanged=true) or (DSBuilt=false) then
      begin
        // Dataset already exists, so check for DM changes.
        // if the class definition has changed, then build a new empty dataset, and try to load the old data.

        // save the existing data to a copy dataset
        DSDataToFile(classes[i],'DSSave_');

        // delete the old ds, and create the new one
        BufDatasets[oldidx] := CreateNewDS(classes[i]);

        DSLoadFromSaved( classes[i],oldDS,BufDatasets[oldidx]);
        oldDs.Free;
      end;
    end;
  end;
  // now check for existing datasets that are no longer in the DM, and delete them.
  if (DMChanged=true) or (DSBuilt=false) then
  begin
    for i:=0 to length(BufDatasets)-1 do
    begin
      if classes.IndexOf(BufDatasets[i].Name)<0 then
      begin
        showmessage('deleting dataset '+BufDatasets[i].Name);
        DeleteDataset(BufDatasets[i].Name);
      end;
    end;
  end;

  //DoFilter('myclass1',['c1att1'],['rec 1'],['rec 2']);
  RefreshDBGrids;

  {$else}
  lDSNames:=dsnames;
  // build one Object Store for each class.
  // Handle changes to class definitions since the last build.......
  // Data is saved as an IndexedDB.  This persists in the browser between sessions.
  asm
    console.log('DB iterationNum is '+iterationNum);
    var useIterationNum=iterationNum;
    var glbSavedData=[];

    if (!window.indexedDB) {
      ok = false;
      console.log("Your browser doesn't support IndexedDB. Data Model IndexedDB instantiation will not be available.");
      callbackFunc(ok);     // eg. continue to run mode
    }

    if (ok) {
      // Let us open our database
      console.log('BuildLocalDB opening '+DBName);
      var request0 = window.indexedDB.open(DBName);  //no version number .... to discover current version

      request0.onerror = function(event) {
        console.log("Database "+DBName+" Open 0 request failed");
        ok = false;
        alert('Failed to open the local database '+DBName+' '+request0.error);
        callbackFunc(ok);     // continue to run mode????
        };

      request0.onsuccess = function(event) {
        db = event.target.result;
        console.log("IndexedDB Database "+DBName+" Open OK.  Existing version is "+db.version);
        if (db.version > iterationNum) {
          useIterationNum = db.version+1;
          console.log('resetting iteration number to '+useIterationNum);
          pas.NodeUtils.UIRootNode.SetAttributeValue('DBVersion',useIterationNum.toString(),'Integer',false);
          }
        db.close();
        pas.XDataModel.IndexedDBOpenWithVersion(DBName,useIterationNum,lDSNames,callbackFunc);
        }
      }

    else {callbackFunc(ok);}     // eg. continue to run mode

  end;
  {$endif}

  DSBuilt:=true;
  result:=ok;
end;

procedure CloseLocalDB;
var
  dbname:String;
  i:integer;
begin
  dbname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  {$ifndef JSCript}
  {$else}
  asm
    if (db) {
      console.log('CloseLocalDB closing db');
      db.close();
      }
  end;
  {$endif}
end;

procedure SaveLocalDB;
var
  dbname:String;
  i:integer;
begin
  dbname:=UIRootNode.GetAttribute('SystemName',false).AttribValue;
  {$ifndef JSCript}
  for i:=0 to length(BufDatasets)-1 do
    DSDataToFile(BufDatasets[i].Name,'');
  {$else}
  {$endif}
end;


{$ifndef JSCript}
procedure DSDataToFile(DSName,prefix:String);
var
  ds:TBufDataset;
  i:integer;
begin
  ds:=GetDataset(DSName,i,true);
  if (ds <> nil) then
  begin
    ds.Name:=prefix+DSName;
    //ds.SaveToFile(ProjectDirectory + '/Datasets/'+prefix+DSName);
    ds.SaveToFile(ProjectDirectory + LocalDBPath +'/'+prefix+DSName);
  end;
end;

procedure DSLoadFromSaved(DSName:String;oldds,newds:TBufDataset);
var
  oldfield:TField;
  i:integer;
  fn:string;
begin
  if (oldds <> nil) then
  begin
    if (newds <> nil) then
    begin
      // have to traverse every row....
      oldds.First;
      while not oldds.EOF do
      begin
        newds.AppendRecord([]);       // empty record appended to dataset
        newds.Edit;
        for i:=0 to newds.FieldCount-1 do
        begin
          fn:=newds.FieldDefs[i].Name;
          try
          oldfield:=oldds.FieldByName(fn);
          if oldfield<>nil then
            newds.Fields[i].Value:=oldfield.Value;
          except
          end;
        end;
        oldds.Next;
      end;

    end;
  end;
end;
{$else}
{$endif}

{$ifndef JScript}
procedure EmptyBufDataSet(BufDb:TBufDataSet);
var
  OldFieldDefs : TFieldDefs;
begin
  OldFieldDefs := TFielddefs.Create(nil);
  OldFieldDefs.Assign(BufDb.FieldDefs);
  BufDB.DisableControls;
  BufDb.Close;
  BufDb.Clear;
  BufDb.FieldDefs.Assign(OldFieldDefs);
  BufDb.CreateDataset;
  BufDb.Open;
  OldFieldDefs.Free;
end;
{$else}

{$endif}
function DSEmptyDataset(ReturnEvent:TeventStatus;DSName,AsyncProcName:String):Boolean;
var
  ok:Boolean;
  i:integer;
  {$ifndef JScript}
  ds:TBufDataset;
  {$endif}
begin
  ok:=true;
  {$ifndef JScript}
  ds:=GetDataset(DSName,i);
  if ds<>nil then
    EmptyBufDataSet(ds)
  else
    ok:=false;
  DSReturnToEvent(ReturnEvent,AsyncProcName);
  {$else}
  asm
    // open a read/write db transaction, ready for clearing the data
    var transaction = db.transaction([DSName], "readwrite");

    // report on the success of the transaction completing, when everything is done
    transaction.oncomplete = function(event) {
      pas.InterfaceTypes.DSReturnToEvent(ReturnEvent,AsyncProcName);
    };

    transaction.onerror = function(event) {
      console.log('DSEmptyDataset failed: ', transaction.error );
    };

    var objectStore = transaction.objectStore(DSName);

    // Make a request to clear all the data out of the object store
    var objectStoreRequest = objectStore.clear();

    objectStoreRequest.onsuccess = function(event) {
      // report the success of our request
      console.log('DSEmptyDataset Request successful.');
    };
  end;
  {$endif}
  result:=ok;
end;

procedure DeleteLocalDB(DBName:String;ask:Boolean;CallbackFunc:TObject=nil);
{$ifndef JSCript}
var
  i:integer;
  ok:Boolean;
{$endif}
begin
  {$ifndef JSCript}
  for i:=0 to length(BufDatasets)-1 do
  begin
    BufDatasets[i].Destroy;
  end;
  setlength(BufDatasets,0);
  // delete the local file folder as well
  ok:=true;
  if DirectoryExists('SavedSystems/'+DBName) then
  begin
    if ask then
      ok := XIDEConfirm('OK to delete stored datasets for system '+DBName+'?');
    if ok then
    begin
      ok:=DeleteDirectory('SavedSystems/'+DBName,True);
      if ok then
      begin
        ok:=RemoveDirUTF8('SavedSystems/'+DBName);
      end;
    end;
  end;
  {$else}
  asm
    console.log('DeleteLocalDB opening '+DBName);
    var ok=true;

    var request0 = window.indexedDB.open(DBName);  //no version number .... to discover if DB exists

    request0.onerror = function(event) {
      console.log("Database "+DBName+" DeleteLocalDB Open request failed "+request0.error);
      ok = false;
      alert('Failed to open the local database '+DBName+' '+request0.error);
      // continue to callback function
      CallbackFunc(false);
      };

    request0.onsuccess = function(event) {
      db = event.target.result;
      console.log("IndexedDB Database "+DBName+" Open OK.  Existing version is "+db.version);
      db.close();
      if (ask) {
        ok = (window.confirm("OK to delete local IndexedDB database "+DBName+"?"))};
      if (ok) {

        var DBDeleteRequest = window.indexedDB.deleteDatabase(DBName);

        DBDeleteRequest.onerror = function(event) {
          console.log("Error deleting database.");
        };

        DBDeleteRequest.onsuccess = function(event) {
          console.log("Database "+DBName+" deleted successfully");
          db = null;
          //console.log(event.result); // should be undefined
          CallbackFunc(true);
        };
      }
      else {CallbackFunc(true); }
   //   }
    db.onversionchange = function () { console.log('DBDeleteIfExists onversionchange closing '+DBName); db.close(); };
    }

  end;
  {$endif}

end;

procedure DBSaveAs(oldname,newname:String);
var
  ok:Boolean;
  {$ifndef JSCript}
  s:TSearchRec;
  {$else}
  lDSNames:TStringArray;
  {$endif}
begin
  {$ifndef JSCript}
  ok:=true;
  if DirectoryExists('SavedSystems/'+oldname) then
  begin
    if not DirectoryExists('SavedSystems/'+newname) then
      CreateDir('SavedSystems/'+newname);
    if FindFirst('SavedSystems/'+oldname+'/*',faAnyFile,s)=0 then
    begin
      repeat
        if (s.Name <> '.') and (s.Name <> '..') then
          CopyFile('SavedSystems/'+oldname+'/'+s.Name,'SavedSystems/'+newname+'/'+s.Name);
      until FindNext(s)<>0;
      FindClose(s);
    end;
  end;
  {$else}
  lDSNames:=dsnames;
  asm
    var olddb;

    function NewDBCreated(ok) {
      if ((ok)&&(olddb)) {
        console.log('NewDBCreated ',newname);
        // open a transaction on olddb
        var txOld = olddb.transaction(lDSNames, "readonly");
        txOld.onerror = function(event) {
          console.log('txOld failed: ', txOld.error );
        };
        // copy every (existing) old objectstore to the new db   //
        for (var i=0; i<rtl.length(lDSNames); i++) {
          try {
            var oldobjectStore = txOld.objectStore(lDSNames[i]);
            if (oldobjectStore) {
              try {
                pas.XDataModel.DSCopyDataFromOldDB(lDSNames[i],oldobjectStore);
                }
              catch(e) {
                // nothing to copy
              }
            }
          }
          catch(e) {console.log(e.message); }
        }
      }
    }
    function DBSaveAs_DeleteDone(ok) {
        //if oldname db exists, copy all to new.
        console.log('DBSaveAs opening '+oldname);

        var olddbrequest = window.indexedDB.open(oldname);  //no version number .... to discover current version

        olddbrequest.onerror = function(event) {
          console.log("Database "+oldname+" Open request failed");
          };

        olddbrequest.onsuccess = function(event) {
          olddb = event.target.result;
          // create newdb, keeping the old version number
          pas.XDataModel.BuildLocalDB(olddb.version,NewDBCreated);
          }
    }
    //delete newname db if already exists
    pas.XDataModel.DeleteLocalDB(newname,false,DBSaveAs_DeleteDone);
  end;
  {$endif}
end;

{$ifdef Python}
procedure BuildXArrays(DefaultDims:Boolean);
var
  i,j,k:integer;
  classNode,refnode:TDataNode;
  className,IdName,ArrayName,smult,dflt:String;
  ownerlist:TNodesArray;
  keynames,keynames2,mults,glbDims,glbDimVals:TStringArray;
  bits:TStringList;
  ok:Boolean;
begin
  // first look for a list of dimensions
  setlength(glbDims,0);
  setlength(glbDimVals,0);
  {$ifdef JScript}asm console.log('1'); end; {$endif}
  j:=0;
  for i:=0 to length(DMRoot.ChildNodes)-1 do
  begin
    if (DMRoot.ChildNodes[i].NodeType = 'DMClass')
    and (DMRoot.ChildNodes[i].NodeName = 'Dimensions') then
    begin
      classnode:=DMRoot.ChildNodes[i];
      for j:=0 to length(classNode.ChildNodes)-1 do
      begin
        if  classNode.ChildNodes[j].NodeType = 'DMAttrib' then
        begin
          setlength(glbDims,length(glbDims)+1);
          setlength(glbDimVals,length(glbDimVals)+1);
          glbDims[j]:=classNode.ChildNodes[j].NodeName;
          glbDimVals[j]:=classNode.ChildNodes[j].GetAttribute('DefaultValue',false).AttribValue;
          PyExeString('print(''dimension '+glbDims[j]+' default='+glbDimVals[j]+''')');

          ok:=TryStrToInt(glbDimVals[j],k);
          if not ok then
          begin
            PyExeString('print('''+glbDims[j]+'=1'')');
            glbDimVals[j]:='1';
          end;

          if DefaultDims = false then
            PyExeString('if '''+glbDims[j]+''' not in globals():' + glbDims[j]+'='+glbDimVals[j] + LineEnding +
                        'else: print('''+glbDims[j]+' current='','+glbDims[j]+')')
          else
            PyExeString(glbDims[j]+'='+glbDimVals[j]);
          PyExeString('print('''+glbDims[j]+''','+glbDims[j]+')');
        end;
      end;
    end;
  end;
{$ifdef JScript}asm console.log('2'); end; {$endif}

  ////!!!!!! no good if classes have composite primary keys !!!!
  for i:=0 to length(DMRoot.ChildNodes)-1 do
  begin
    if DMRoot.ChildNodes[i].NodeType = 'DMClass' then
      if myStrToBool(DMRoot.ChildNodes[i].GetAttribute('MakeXArrays',true).AttribValue) = true then
      begin
        classNode := DMRoot.ChildNodes[i];
        className := classNode.NodeName;
        IdName:=className+'_Id';     // sensible default??
        for j:=0 to length(classNode.ChildNodes)-1 do
        begin
          if  classNode.ChildNodes[j].NodeType = 'DMAttrib' then
            // !!!! expecting only one primary key attribute here
            if myStrToBool(classNode.ChildNodes[j].GetAttribute('IsId',false).AttribValue) = true then
            begin
              IdName:=classNode.ChildNodes[j].NodeName;
            end;
        end;

        setlength(ownerlist,0);
        setlength(keynames,0);
        DMFindClassOwnerHierarchy(className,ownerlist,keynames);   // hierarchy of 'DMContains' parent nodes
        setlength(mults,length(ownerlist));
        for j:=0 to length(ownerlist)-1 do
        begin
          smult:=trim(ownerlist[length(ownerlist)-j-1].GetAttribute('Multiplicity',true).AttribValue);
          // for xarrays, should be the name of a declared dimension
          if smult<>'' then
            mults[j]:=smult
          else
            mults[j]:='1';
        end;
        // keynames - reverse order
        setlength(keynames2,length(keynames));
        for j:=0 to length(keynames)-1 do       /// a,b,c,d => d,c,b,a
          keynames2[j]:=keynames[length(keynames)-j-1];
        // if >1 level, drop the first one
        if length(keynames2)>1 then
          for j:=0 to length(keynames2)-2 do     // => c,b,a,a
            keynames2[j]:=keynames2[j+1];
        // add one for the current class
        if length(keynames2)>0 then
          keynames2[length(keynames2)-1]:=IdName;   // => c,b,a,me

        if length(mults)>0 then
        begin
        // make an XArray for each DMAttrib element in this class
        // keynames and mults must be same length
        if length(keynames2)<>length(mults) then
          showMessage('Warning: Cannot reconcile dimensions for class '+className+' - XArrays not created')
        else
          for j:=0 to length(classNode.ChildNodes)-1 do
          begin
            if  classNode.ChildNodes[j].NodeType = 'DMAttrib' then
              //if myStrToBool(classNode.ChildNodes[j].GetAttribute('IsId',false).AttribValue) = false then     //why??
              begin
                ArrayName := className + '_' + classNode.ChildNodes[j].NodeName;
                dflt:=classNode.ChildNodes[j].GetAttribute('DefaultValue',false).AttribValue;
                //PyXUtils.BuildXarray(ArrayName,keynames2,mults,dflt);
                PyXUtils.BuildXarray(ArrayName,mults,mults,dflt);
              end;
          end;
        end;
      end;
  end;
{$ifdef JScript}asm console.log('3'); end; {$endif}
end;
{$endif}




begin
  DMSelectedDataTreeNode:=nil;
  DMChanged:=false;
  DSBuilt:=false;
  {$ifndef JSCript}
  setlength(BufDatasets,0);
  XDBTable.ProjectBufDatasets:=BufDatasets;
  DMProcs:=TDMProcs.Create;
  {$endif}

  //AddDefaultAttribute(DMPkgDefaultAttribs,'Annotation','String','','',false,true);
  //AddDefaultsToTable('DMPkg',DMPkgDefaultAttribs);

  // DMClass .....class definitions
  AddDefaultAttribute(DMClassDefaultAttribs,'Annotation','String','','',false,true);
  AddDefaultAttribute(DMClassDefaultAttribs,'MakeDataset','Boolean','False','At run mode, generate a local dataset for this class.',false,true);
  AddDefaultAttribute(DMClassDefaultAttribs,'MakeXArrays','Boolean','False','(Python only): generate XArrays for attributes in this class.',false,true);
  AddDefaultsToTable('DMClass',DMClassDefaultAttribs);

  // Classes have child attributes
  // Classes have child classes (equivalent ECore 'containment' references)
  // Classes have child operations
  // Operations have child parameters

  // DMContains ..... 'contained' class reference
  AddDefaultAttribute(DMContainsDefaultAttribs,'Annotation','String','','',false,true);
  AddDefaultAttribute(DMContainsDefaultAttribs,'Class','String','','',false,true);
  //AddDefaultAttribute(DMContainsDefaultAttribs,'IsContained','Boolean','True','True if this is a contained reference',false,true);
  AddDefaultAttribute(DMContainsDefaultAttribs,'Multiplicity','String','0','default = 0, Integer,  or paramname',false,true);
  AddDefaultsToTable('DMContains',DMContainsDefaultAttribs);

  // DMRef ..... un-contained class reference
  AddDefaultAttribute(DMRefDefaultAttribs,'Annotation','String','','',false,true);
  AddDefaultAttribute(DMRefDefaultAttribs,'Class','String','','',false,true);
  AddDefaultsToTable('DMRef',DMRefDefaultAttribs);

  // DMAttrib .....
  AddDefaultAttribute(DMAttribDefaultAttribs,'Annotation','String','','',false,true);
  AddDefaultAttribute(DMAttribDefaultAttribs,'IsId','Boolean','False','Marks this field as the identifier for this record type (indexed). A class may have multiple id attributes, making up a composite key.',false,true);
  AddDefaultAttribute(DMAttribDefaultAttribs,'AttribType','String','String','',false,true);
  AddDefaultAttribute(DMAttribDefaultAttribs,'DefaultValue','String','','',false,true);
  AddDefaultAttribute(DMAttribDefaultAttribs,'Multiplicity','String','1','default = 1, Integer,  or paramname',false,true);
  AddDefaultAttribute(DMAttribDefaultAttribs,'StringLength','Integer','20','Where type is String AND the class attribute "MakeDataset" is true, sets the maximum string length in the generated dataset.',false,true);
  AddDefaultsToTable('DMAttrib',DMAttribDefaultAttribs);
  AddAttribOptions('DMAttrib','AttribType',DMAttribTypes);
  // can an attribute contain an operation????????  In ecore, ops are only in the class !!!! could define attrib and op as a pair? !!!!
  //    eg. Calc<AttribName> (....parameters...)  { .... code using parameters, returns a value ....}

  // DMOp .....
  AddDefaultAttribute(DMOpDefaultAttribs,'ReturnType','String','String','',false,true);
  AddDefaultAttribute(DMOpDefaultAttribs,'Code','String','','',false,true);
  AddDefaultsToTable('DMOp',DMOpDefaultAttribs);
  AddAttribOptions('DMOp','ReturnType',DMAttribTypes); //!!!! return type may be a class

  // DMParam .....
  AddDefaultAttribute(DMParamDefaultAttribs,'ParamType','String','String','',false,true);
  AddDefaultsToTable('DMParam',DMParamDefaultAttribs);
  AddAttribOptions('DMParam','ParamType',DMAttribTypes); //!!!! param type may be a class


end.





