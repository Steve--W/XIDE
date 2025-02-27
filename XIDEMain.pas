(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)

unit XIDEMain;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$INTERFACES CORBA}
{$endif}

interface
uses
  Classes, SysUtils, strutils,
{$ifndef JScript}
  FileUtil, Forms, Controls, Graphics, Dialogs, LCLIntf, ExtCtrls, Menus,
  ComCtrls, StdCtrls, TypInfo, LazIDEIntf, LResources, {DBGrids, DBCtrls,DB,BufDataset,} Types,
   Events, DllInterface, LazsUtils, CompilerLogUnit,
{$else}
  HTMLUtils,
{$endif}
  // XComponents units...
  PyXUtils, StringUtils, NodeUtils, PasteDialogUnit,
  UtilsJSCompile, XIFrame, XSVGContainer, XMenu, XScrollBox, XVBox, XHBox, XTree, XMemo,
  XTabControl, XButton, XLabel, XEditBox, XCheckBox, XHyperLink, XRadioBtns,
  XForm, XTable, XProgressBar, XNumericSlider, XNumberSpinner,
  XComboBox, XDatePicker, XColorPicker, XImage, XGroupBox, XCode, XStore,
  XBitMap, XTrapEvents, XIDEHelpUnit,
  XHTMLText, XHTMLEditor, EventsInterface,
  // XIDEComponents units...
  XGPUCanvas, XGPUEditor, X3DTable, {XThreads,} XComposite,
  // XIDE project units...
  CompileUserCode, XObjectInsp,XIDESettings,
  CodeEditor, InputSelectUnit, PropertyEditUnit,
  PopupMemo, AboutUnit, SavedSystems, StylesUtils,
  IntfParamUnit, IntfEventUnit;


{$ifdef JScript}
procedure InitialisePage(dummy:string);
{$ifdef Python}
procedure StartupPython;
{$endif}
{$endif}

{ TXIDEForm }

type
TXIDEForm = class(TXForm)

  {$ifndef JScript}
  // Lazarus-only Form components...
  WebMenu: TMenuItem;
  CompileToJS: TMenuItem;
  CompilerShowLog: TMenuItem;
  {$endif}

  ToggleDesignRunMode: TXMenuItem;
  CodeTreePascalUnitBtn: TXButton;
  NavTreeUpBtn: TXButton;
  NavTreeUpDownHBox: TXHBox;
  NavTreeDownBtn: TXButton;
  ResourceTreeButtonsHBox: TXHBox;
  ResourceTreeDelBtn: TXButton;
  ResourceTreeLoadBtn: TXButton;
  OIAddPropertyButton: TXButton;
  StyleTreeButtonHBox: TXHBox;
  StyleTreeDelBtn: TXButton;
  StyleTreeApplyBtn: TXButton;
  StyleNodeQualifier: TXComboBox;
  StyleTreeEditBtn: TXButton;
  QualifierEditBox: TXEditBox;
  WatchBox: TXEditBox;
  CodeTreeSearchBtn: TXButton;
  CodeTreePythonBtn: TXButton;
  CompositePropsScrollbox: TXScrollBox;
  CodeTreeUpBtn: TXButton;
  CodeTreeDownBtn: TXButton;
  XEditBox1: TXEditBox;
  CodeTreeUpDownHBox: TXHBox;
  XIDEMainMenu: TXMainMenu;

  MyRootDiv: TXScrollBox;
  InnerRootVBox: TXVBox;
  ObjectInspectorTabs: TXTabControl;
  UIDesigner: TXTabSheet;
  CodeDesigner: TXTabSheet;
  CodeTree: TXTree;
  NavTree: TXTree;
  PropertyEditorScrollbox: TXScrollBox;
  OIButtonGroup: TXHBox;
  OICut: TXButton;
  OIPaste: TXButton;
  OICopy: TXButton;
  OIDelete: TXButton;
  RootHBox: TXHBox;
  UIRoot: TXScrollBox;
  ResourceInspectorTabs: TXTabControl;
  Resources: TXTabSheet;
  ResourceTree: TXTree;
  CodeTreeEditBtn: TXButton;
  CodeTreeDelBtn: TXButton;
  OIClear: TXButton;

  CodeTreeButtonHBox: TXHBox;
  XLabel3: TXLabel;
  EventsEditorScrollBox: TXScrollBox;
  SystemMenu: TXMenuItem;
  SystemSaveClip: TXMenuItem;
  SystemLoad: TXMenuItem;
  SystemClear: TXMenuItem;
  SystemDeploy: TXMenuItem;
  OITabs: TXTabControl;
  OIPropertiesTab: TXTabSheet;
  OIEventsTab: TXTabSheet;
  CodeTreeBtnsVB1: TXVBox;
  CodeTreeBtnsVB2: TXVBox;
  XIDETrapEvents1: TXTrapEvents;
  HelpMenu: TXMenuItem;
  HelpAbout: TXMenuItem;
  HelpOverview: TXMenuItem;
  CodeTreeBtnsVB3: TXVBox;
  SystemSettings: TXMenuItem;
  SystemSaveFile: TXMenuItem;
  SystemEncapsulate: TXMenuItem;
  SystemLoadFromStore: TXMenuItem;
  StyleDesigner: TXTabSheet;
  StyleSheet: TXTree;
  GeneratedStyleSheetText: TXMemo;
  StyleTreeScrollBox: TXScrollBox;
  StyleResourcesPage: TXTabSheet;
  StyleResources: TXTree;
  XMemo1: TXMemo;
  OICompositePropsTab: TXTabSheet;

  procedure CodeTreeDownBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure CodeTreePythonBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure CodeTreeUpBtnHandleButtonClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure DummyPositionMarker;   // DO not delete this line.

  {$ifndef JScript}
  // Lazarus-only methods...
  procedure CompilerShowLogClick(Sender: TObject);
  procedure CompileToJSClick(Sender: TObject);
  procedure FormActivate(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
  procedure FormCreate(Sender: TObject);
  procedure FormResize(Sender: TObject);
  procedure LoadIframes(dum:integer);
  {$endif}

  // Common Event Handlers - created at design time along with X components...
  function HandleGenericEvent(MyEventType,myValue:string;EventNode:TDataNode):Boolean;
  procedure CodeTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure HelpAboutHandleClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure HelpOverviewHandleClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure MyRootDivClick(Sender: TObject);
  procedure NavTreeDownBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure NavTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure NavTreeHandleDragStart(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure NavTreeHandleDrop(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure NavTreeHandleDropAccepted(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure ResourceTreeHandleDrop(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure ResourceTreeHandleDropAccepted(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure NavTreeUpBtnHandleButtonClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure OITabsHandleChange(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  function NavTreeTreeNodeHintFunc(TreeLabelStr: String): String;
  procedure ObjectInspectorTabsHandleChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure ResourceInspectorTabsHandleChange(e: TEventStatus; nodeID: AnsiString; myValue: AnsiString);
  procedure OICopyComponentHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure OICopyHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure OICutHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure OIDeleteHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure OIPasteHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure PropertyEditorScrollboxHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure OIAddPropertyButtonHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure ResourceTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure ResourceTreeHandleDragStart(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  function ResourceTreeTreeNodeHintFunc(TreeLabelStr: String): String;
  procedure StyleNodeQualifierHandleChange(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure StyleSheetHandleDrop(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure StyleSheetHandleDropAccepted(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure StyleSheetHandleTreeNodeClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure StyleTreeApplyBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure StyleTreeDelBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure SystemClearHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure SystemDeployHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure ResourceTreeDelBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure ResourceTreeLoadBtnHandleButtonClick(e: TEventStatus;
    nodeID: AnsiString; myValue: AnsiString);
  procedure SystemEncapsulateHandleClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure SystemLoadFromStoreHandleClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure SystemLoadHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure SystemSaveClipHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure SystemSaveFileHandleClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure SystemSettingsHandleClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure ToggleDesignRunModeHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure StyleTreeEditBtnHandleButtonClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure XButton1HandleButtonClick(e: TEventStatus; nodeID: AnsiString;
    myValue: AnsiString);
  procedure XIDETrapEvents1HandleAny(e: TEventStatus; nodeID: AnsiString;  myValue: AnsiString);
  procedure CodeTreeDelBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure CodeTreeEditBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString;myValue: AnsiString);
  procedure CodeTreePascalUnitBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
  procedure CodeTreeSearchBtnHandleButtonClick(e: TEventStatus;nodeID: AnsiString; myValue: AnsiString);

  procedure UIRootClick(Sender: TObject);

private
  { private declarations }
public
  { public declarations }
end;

var
XIDEForm: TXIDEForm;

{$ifdef Python}
procedure ResetPyConsole;
{$endif}

implementation

{$R *.lfm}

{$ifdef Python}
procedure StartupPython;
begin
  XIDEForm.CodeTreePythonBtn.IsVisible:=true;
  PyMemoComponent:=XIDEForm.XMemo1;
  RunInitialScript;
end;
procedure ResetPyConsole;
begin
  PyMemoComponent:=XIDEForm.XMemo1;
end;
{$endif}

function TXIDEForm.HandleGenericEvent(MyEventType,myValue:string;EventNode:TDataNode):Boolean;
var
  CompositeNode:TdataNode;
begin
  result:=true;
  // If the EventNode is part of the user's system definition, then select the node in the object inspector.
  if (DesignMode=true)
  and ((NodeIsDescendantOf(EventNode,UIProjectRootName) > -1)
    or (EventNode.NodeType='TXMenuItem')
    or (NodeIsInXForm(EventNode) = true)) then
  begin
    if ((MyEventType='Click')
      or ( MyEventType='ButtonClick')
      or ( MyEventType='TreeNodeClick')) then
    begin
      if ((EventNode.NodeType<>'TXMenuItem') or (EventNode.IsDynamic=true))  then
      begin
        ResourceInspectorTabs.TabIndex:=0;
        ObjectInspectorTabs.TabIndex:=0;  //(UIDesigner)
        if EventNode.NameSpace<>'' then
        begin
          CompositeNode := FindCompositeContainer(EventNode);
          if (CompositeNode<>nil) then
            SelectNavTreeNode(CompositeNode,false);
        end
        else
          SelectNavTreeNode(EventNode,false);
        result:=false;  //do no further event handling
      end;
    end
    else if (MyEventType='Change') then
    begin
      SelectNavTreeNode(EventNode,true);    //refresh
    end
    else if MyEventType='HTMLEditorBrowserClosed' then
    begin
        // a HTMLEditor browser window has closed...refresh the object inspector to reflect
        // the state of the 'Showing' flag.
        if ObjectInspectorSelectedNavTreeNode = EventNode then
        begin
          RefreshObjectInspector(EventNode);
        end;
    end;
  end
  else if (DesignMode=false)
  then
  begin
    // Run Mode.
    // Trap a property change event, to push new value to destination properties.
    if (MyEventType='Change') then
    begin
      //PushSourceToAttributes(EventNode,?????SourceAttrib);       //...we don't know which attribute has changed
      PushNodeSourcesToAttributes(EventNode);
    end;
  end;
end;

procedure TXIDEForm.DummyPositionMarker;     // do not delete this procedure
begin
end;

procedure TXIDEForm.CodeTreePythonBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OIAddCodeUnitNode('PythonScript');
end;

procedure TXIDEForm.CodeTreeDownBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  CodeTreeMoveSiblingUpDown('Down');
end;

procedure TXIDEForm.CodeTreeUpBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  CodeTreeMoveSiblingUpDown('Up');
end;

procedure TXIDEForm.NavTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OINavTreeNodeChange(e,nodeId,'',myValue);
end;

procedure TXIDEForm.NavTreeHandleDragStart(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIDragItem(e,nodeId,myValue);
end;

procedure TXIDEForm.NavTreeHandleDrop(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIDropItem(e,nodeId,myValue);
end;

procedure TXIDEForm.NavTreeHandleDropAccepted(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
  SourceIsNavigator,SourceIsResourceTree:Boolean;
  DstNodeName,Part1:String;
  DstNode:TDataNode;
  ok:Boolean;
//  values:TNodeEventValue;
begin
  //showmessage('checking drop. Sourcename='+SourceName+' DstText='+dstText);

  // e.ValueObject is an object of type TNodeEventValue.
  // values.myNode is of type TTreeNode, and contains data with a unique id.
  //values:=TNodeEventValue(e.ValueObject);

  SourceIsNavigator:=false;
  SourceIsResourceTree:=false;
  if (e.SourceName='ResourceTree') then
  begin
    SourceIsResourceTree:=true;
  end;
  if (e.SourceName='NavTree') then SourceIsNavigator:=true;
  ok:=(SourceIsNavigator )
          or (SourceIsResourceTree );

  if ok
  and SourceIsResourceTree then
  begin
    //showmessage('checking drop. DstText='+dstText);
    // additional node-level checks...
    if (e.dstText<>'') then
    begin
      DstNodename:=TreeLabelToId(e.DstText,'NavTree',Part1);
      DstNode:=FindDataNodeById(UIRootNode,DstNodeName,'',true);     //!!namespace - assuming top design level only
      if (DstNode<>nil) then
      begin
        ok:=OINavTreeAllowDrop(DstNode);
      end;
    end;
  end;
  e.ReturnString:=myBoolToStr(ok);
end;

procedure TXIDEForm.NavTreeUpBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OIMoveNavSiblingUpDown('Up');
end;

procedure TXIDEForm.OITabsHandleChange(e: TEventStatus; nodeID: AnsiString;
  myValue: AnsiString);
begin
  OIPropsEventsTabChange;
end;

procedure TXIDEForm.NavTreeDownBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OIMoveNavSiblingUpDown('Down');
end;

function TXIDEForm.NavTreeTreeNodeHintFunc(TreeLabelStr: String): String;
begin
  result := OITreeNodeHint(TreeLabelStr);
end;

procedure TXIDEForm.ObjectInspectorTabsHandleChange(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  {$ifdef JScript}
  if ObjectInspectorTabs.TabIndex=2 then    //(StyleDesigner)
  begin
     SetStyleOptions;
     ResourceInspectorTabs.TabIndex:=1;     //(StyleResourcesPage)
  end
  else if ObjectInspectorTabs.TabIndex=0 then   //(UIDesigner)
    ResourceInspectorTabs.TabIndex:=0;          //(Resources)
  {$endif}
end;

procedure TXIDEForm.OICopyComponentHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIComponentCopy(nodeId,myValue);
end;

procedure TXIDEForm.OICopyHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OICopySelectedItem;
end;

procedure TXIDEForm.OICutHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OICutItem(nodeId,myValue);
end;

procedure TXIDEForm.OIDeleteHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
   OIDeleteSelectedItem;
end;

procedure TXIDEForm.OIPasteHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIPasteItem(nodeId,myValue);
end;

procedure TXIDEForm.PropertyEditorScrollboxHandleClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin

end;

procedure TXIDEForm.ResourceTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIResourceTreeNodeChange(nodeId,myValue);
end;

procedure TXIDEForm.ResourceTreeHandleDragStart(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIDragItem(e,nodeId,myValue);
end;

function TXIDEForm.ResourceTreeTreeNodeHintFunc(TreeLabelStr: String): String;
begin
  result := OIResTreeNodeHint(TreeLabelStr);
end;


procedure TXIDEForm.StyleNodeQualifierHandleChange(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  {$ifdef JScript}
  UpdateNodeQualifierField;
  GenerateStyleSheets;
  {$endif}
end;

procedure TXIDEForm.StyleSheetHandleDrop(e: TEventStatus; nodeID: AnsiString;
  myValue: AnsiString);
{$ifdef JScript}
var
//  values:TNodeEventValue;
  TreeNodeId:String;
{$endif}
begin
  // e.ValueObject is an object of type TNodeEventValue.
  // values.myNode is of type TTreeNode, and contains data with a unique id.

  {$ifdef JScript}
//  values:=TNodeEventValue(e.ValueObject);
  asm
    TreeNodeId=e.myTreeNode.id;
  end;
  //showmessage('dropping on node '+TreeNodeId);
  StyleSheet.selectedNodeId:=TreeNodeId;
  PasteSelectedStyleResources(true,DropTarget,false,e.myTreeNode);// the boolean means this does the paste and reports errors rather than just checking it
  {$endif}
end;

procedure TXIDEForm.StyleSheetHandleDropAccepted(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
{$ifdef JScript}
var
//  values:TNodeEventValue;
  SourceName,SrcText,DstText,ReturnString:String;
  priorNodeText:string;
{$endif}
begin
  {$ifdef JScript}
//  values:=TNodeEventValue(e.ValueObject);
  SourceName:=e.SourceName;  // name of tree being dragged from
  SrcText:=e.SrcText;        // text of treenode being dragged
  DstText:=e.DstText;        // text of treenode being dragged over
  if SourceName='StyleResources' then
  begin
    // set e.ReturnString to "True" or "False"
    PriorNodeText:= StyleResources.SelectedNodeText;
    if SrcText <> priorNodeText then
      EditAttributeValue('StyleResources','','SelectedNodeText',SrcText);
    WatchBox.ItemValue:='StyleResources.SelectedNodeText='+StyleResources.SelectedNodeText;
    if PasteSelectedStyleResources(false,DstText,true,e.myTreeNode)=true  then
      ReturnString:='True'
    else
      ReturnString:='False';
  end
  else
    ReturnString:='False';
  e.ReturnString:=ReturnString;
  {$endif}
end;

procedure TXIDEForm.StyleSheetHandleTreeNodeClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  {$ifdef JScript}
  PopulateStyleEditor(false);// use drop downs if available
  {$endif}
end;

procedure TXIDEForm.StyleTreeApplyBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  {$ifdef JScript}
  UpdateNodeQualifierField;
  GenerateStyleSheets;
  {$endif}
end;

procedure TXIDEForm.StyleTreeDelBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  {$ifdef JScript}
  DeleteStyleNode;
  {$endif}
end;

procedure TXIDEForm.ResourceTreeHandleDrop(e: TEventStatus; nodeID: AnsiString;
  myValue: AnsiString);
begin
end;


procedure TXIDEForm.ResourceTreeHandleDropAccepted(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
  SourceIsResourceTree:Boolean;
  //values:TNodeEventValue;
begin
  //values:=TNodeEventValue(e.ValueObject);
  SourceIsResourceTree:=false;
  if (e.SourceName='ResourceTree') then SourceIsResourceTree:=true;
  e.ReturnString:=myBoolToStr(SourceIsResourceTree );
end;

procedure TXIDEForm.ResourceTreeDelBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OIDeleteResource;
end;

procedure TXIDEForm.ResourceTreeLoadBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OILoadResource;
end;

procedure TXIDEForm.SystemEncapsulateHandleClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OIEncapsulate;
end;

procedure TXIDEForm.SystemLoadFromStoreHandleClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  if DesignMode=false then
    ShowMessage('Please switch to Design Mode first')
  else
  begin
    SavedSystemsForm.Initialise('xide',true);
    XForm.ShowXForm('SavedSystemsForm',true);
  end;
end;

procedure TXIDEForm.UIRootClick(Sender: TObject);
begin
end;

procedure TXIDEForm.SystemClearHandleClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
var
ok:boolean;
begin
  ok:=XIDEConfirm('OK to clear the system?');

  if ok then
  begin
    OIClearSystem;
    SelectNavTreeNode(MainFormProjectRoot,true);

  end;
end;

procedure TXIDEForm.SystemDeployHandleClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  OIDeploySystem;
end;

procedure TXIDEForm.SystemLoadHandleClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  if DesignMode=false then
    ShowMessage('Please switch to Design Mode first')
  else
    OISystemLoad(e,nodeId);
end;

procedure TXIDEForm.SystemSaveClipHandleClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  SaveSystemToClip;
end;

procedure TXIDEForm.SystemSaveFileHandleClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  SaveSystemToFile;
end;

procedure TXIDEForm.ToggleDesignRunModeHandleClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
var
  ShowFiles:Boolean;
begin
  if DesignMode then
     ShowFiles:=SaveSystemData;
  {$ifdef JScript}
  if ShowFiles = true then
  begin
    SavedSystemsForm.Initialise('',false);
    XForm.ShowXForm('SavedSystemsForm',true);
  end
  else
{$endif}
  DoToggleDesignRunMode(ToggleDesignRunMode);
end;


procedure TXIDEForm.StyleTreeEditBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  {$ifdef JScript}
  PopulateStyleEditor(true);// edit instead of using drop downs when available
  {$endif}
end;

procedure TXIDEForm.XButton1HandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
//  ConstructPascalDM();
end;

procedure TXIDEForm.XIDETrapEvents1HandleAny(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
var
  eventNode:TDataNode;
  doContinue:Boolean;
begin
  eventNode:=FindDataNodeById(SystemNodeTree,e.NodeId,e.NameSpace,true);
  doContinue:=HandleGenericEvent(e.eventtype,myValue,eventNode);
  e.ContinueAfterTrappers:=doContinue;
end;

procedure TXIDEForm.CodeTreeHandleTreeNodeClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
   OICodeTreeNodeChange(nodeId,myValue);
end;

procedure TXIDEForm.HelpAboutHandleClick(e: TEventStatus; nodeID: AnsiString;
  myValue: AnsiString);
begin
  XForm.ShowXForm('AboutXIDEForm',true);
end;

procedure TXIDEForm.HelpOverviewHandleClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  ClearInspectors;
  XIDEHelpForm.InitialiseonShow;
  XForm.ShowXForm('XIDEHelpForm',false);
end;

procedure TXIDEForm.MyRootDivClick(Sender: TObject);
begin
end;

procedure TXIDEForm.CodeTreePascalUnitBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString;
  myValue: AnsiString);
begin
  OIAddCodeUnitNode('PasUnit');
end;

procedure TXIDEForm.CodeTreeEditBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIEditCodeUnit;
end;

procedure TXIDEForm.CodeTreeDelBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  OIDeleteCodeUnit;
end;

procedure XIDESetupUIRootNode;
begin
  UIRootNode.SetAttributeValue('DeploymentMode','Design');
  UIRootNode.SetAttributeValue('SystemName','XIDESystem');
  // Add root node attributes for 'settings' data...
  UIRootNode.SetAttributeValue('ShowResources','Right');     //Left,Right
  UIRootNode.SetAttributeValue('PythonPackages','');    //eg. numpy;matplotlib;scipy

  // Add root node events
  UIRootNode.myeventTypes.Add('OnEnterRunMode');
  UIRootNode.myeventTypes.Add('OnExitRunMode');
  UIRootNode.InitialiseEventHandlers;
  {$ifdef JScript}
  UIRootNode.MyForm:=nil;
  {$endif}
end;

procedure TXIDEForm.SystemSettingsHandleClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin

  XIDESettingsForm.InitialiseOnShow;
  ShowXForm('XIDESettingsForm',true);

end;

procedure TXIDEForm.OIAddPropertyButtonHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OIAddInterfaceElement;
end;

procedure TXIDEForm.ResourceInspectorTabsHandleChange(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  {$ifdef JScript}
  if ResourceInspectorTabs.TabIndex=1 then  //(StyleResourcesPage)
  begin
     ObjectInspectorTabs.TabIndex:=2;       //(StyleDesigner)
  end
  else if ResourceInspectorTabs.TabIndex=0 then   //(Resources)
    ObjectInspectorTabs.TabIndex:=0;              //(UIDesigner)
  {$endif}
end;

procedure TXIDEForm.CodeTreeSearchBtnHandleButtonClick(e: TEventStatus;
  nodeID: AnsiString; myValue: AnsiString);
begin
  OICodeSearch;
end;

{$ifndef JScript}


procedure InitialiseResources;
// define project-specific resources.
// required files have been pre-built into the resource file xide.lrs.
// the resource folders and files will be created from these lists by the procedure WriteResourceFiles in unit UtilsJSCompile.
begin
  gpujs:=ResourceToString('gpu-browser');

  // files needed for this project to be compiled by pas2js, to generate the project JS file...
  AddRequiredFile('webtranspilerutils','resources/project/webtranspilerutils.pas');
  //AddRequiredFile('xthreads','resources/project/xthreads.pas');
  AddRequiredFile('xgpucanvas','resources/project/xgpucanvas.pas');
  AddRequiredFile('xgpueditor','resources/project/xgpueditor.pas');
  AddRequiredFile('x3dtable','resources/project/x3dtable.pas');
  AddRequiredFile('xcomposite','resources/project/xcomposite.pas');
  AddRequiredFile('xcompositeintf','resources/project/xcompositeintf.pas');
  AddRequiredFile('gpu-browser','resources/project/gpu-browser.js');

  AddRequiredFile('xidemain','resources/project/XIDEMain.pas');
  AddRequiredFile('xobjectinsp','resources/project/xobjectinsp.pas');
  AddRequiredFile('codeeditor','resources/project/codeeditor.pas');
  AddRequiredFile('compileusercode','resources/project/compileusercode.pas');
  AddRequiredFile('inputselectunit','resources/project/inputselectunit.pas');
  AddRequiredFile('propertyeditunit','resources/project/propertyeditunit.pas');
  AddRequiredFile('popupmemo','resources/project/popupmemo.pas');
  AddRequiredFile('interfacetypes','resources/project/interfacetypes.pas');
  AddRequiredFile('interfacetypesdll','resources/project/interfacetypesdll.pas');
  AddRequiredFile('aboutunit','resources/project/aboutunit.pas');
  AddRequiredFile('xidehelpunit','resources/project/xidehelpunit.pas');
  AddRequiredFile('savedsystems','resources/project/savedsystems.pas');
  AddRequiredFile('stylesutils','resources/project/stylesutils.pas');
  AddRequiredFile('pyxutils','resources/project/pyxutils.pas');
  AddRequiredFile('xidesettings','resources/project/xidesettings.pas');
  AddRequiredFile('intfparamunit','resources/project/intfparamunit.pas');
  AddRequiredFile('intfeventunit','resources/project/intfeventunit.pas');

  // files needed for web-pas2jscompiler to be compilable by pas2js, and built into the project JS file...
  AddRequiredFile('fppas2js','resources/pas2jstranspiler/fppas2js.pp');
  AddRequiredFile('fppjssrcmap','resources/pas2jstranspiler/fppjssrcmap.pp');
  AddRequiredFile('pas2jscompiler','resources/pas2jstranspiler/pas2jscompiler.pp');
  AddRequiredFile('fpjson','resources/pas2jstranspiler/fpjson.pp');                       // used by filecache
  AddRequiredFile('pas2jsfilecache','resources/pas2jstranspiler/pas2jsfilecache.pp');
  AddRequiredFile('pas2jsfiler','resources/pas2jstranspiler/pas2jsfiler.pp');
  AddRequiredFile('pas2jsfileutils','resources/pas2jstranspiler/pas2jsfileutils.pp');
  AddRequiredFile('pas2jsfileutilsnodejs','resources/pas2jstranspiler/pas2jsfileutilsnodejs.inc');
  AddRequiredFile('pas2jslogger','resources/pas2jstranspiler/pas2jslogger.pp');
  AddRequiredFile('pas2jspparser','resources/pas2jstranspiler/pas2jspparser.pp');
  AddRequiredFile('pas2js_defines','resources/pas2jstranspiler/pas2js_defines.inc');
  AddRequiredFile('nodejsfs','resources/pas2jstranspiler/nodejsfs.pas');
  AddRequiredFile('contnrs','resources/pas2jstranspiler/contnrs.pas');
  AddRequiredFile('nodejs','resources/pas2jstranspiler/nodejs.pas');
  AddRequiredFile('jstree','resources/pas2jstranspiler/jstree.pp');
  AddRequiredFile('jswriter','resources/pas2jstranspiler/jswriter.pp');
  AddRequiredFile('jsbase','resources/pas2jstranspiler/jsbase.pp');
  AddRequiredFile('jssrcmap','resources/pas2jstranspiler/jssrcmap.pas');
  AddRequiredFile('jstoken','resources/pas2jstranspiler/jstoken.pp');
  AddRequiredFile('pscanner','resources/pas2jstranspiler/pscanner.pp');
  AddRequiredFile('pparser','resources/pas2jstranspiler/pparser.pp');
  AddRequiredFile('pastree','resources/pas2jstranspiler/pastree.pp');
  AddRequiredFile('pasresolver','resources/pas2jstranspiler/pasresolver.pp');
  AddRequiredFile('pasuseanalyzer','resources/pas2jstranspiler/pasuseanalyzer.pp');
  AddRequiredFile('pasresolveeval','resources/pas2jstranspiler/pasresolveeval.pas');
  AddRequiredFile('web','resources/pas2jstranspiler/web.pas');
  AddRequiredFile('pas2jsutils','resources/pas2jstranspiler/pas2jsutils.pp');
  AddRequiredFile('pas2jsfs','resources/pas2jstranspiler/pas2jsfs.pp');
  AddRequiredFile('webfilecache','resources/pas2jstranspiler/webfilecache.pp');
  AddRequiredFile('pas2jswebcompiler','resources/pas2jstranspiler/pas2jswebcompiler.pp');

  InitialiseCompilerResources('XIDE',ProjectDirectory);

  if not DirectoryExists('SavedSystems') then
    CreateDir('SavedSystems');
end;

{ TXIDEForm }
procedure TXIDEForm.FormCreate(Sender: TObject);
var
  SystemDescription:String;
begin

  MainForm:=self;
  MainFormTopControl:=MyRootDiv;
  UITopControl:=UIRoot;

  SystemNodeTree.ScreenObject:=nil;       // root node has no screen object.

  myNode:=DoXFormCreated(self);

  XIDESetupUIRootNode;

  MainFormProjectRoot:=FindDataNodeById(SystemNodeTree,UIProjectRootName,'',true);

  NavTreeComponent:=self.NavTree.myNode;
  ResourceTreeComponent:=self.ResourceTree.myNode;
  CodeTreeComponent:=self.CodeTree.myNode;

  InitialiseResources;
  InitialiseXIDE;
  InitialiseXComponentsProject;
  XGPUEditor.CreateGPUEditForm;
  GPUEditorDoneBtn.myNode.registerEvent('ButtonClick',@OIEventWrapper.CloseGPUEditor);

  InitialiseStyleDesigner;

  StyleResourcesPage.IsVisible:=false;      // this is browser/HTML only
  ObjectInspectorTabs.TabIndex:=0;          //(UIDesigner)
  ResourceInspectorTabs.TabIndex:=0;        //(Resources)

  // is there a saved system from a previous session?
  SystemDescription:=trim(ReadFromLocalStore('XIDESavedData.txt'));
  if SystemDescription<>'' then
  begin
    DoSystemLoad(SystemDescription,'');
  end
  else
    SelectNavTreeNode(MainFormProjectRoot,true);

  {$ifdef Python}
  StartupPython;
  {$endif}

  myNode.myEventTypes.Add('PythonEvent');   // generic handler for python scripted events
  myNode.RegisterEvent('PythonEvent',@OIEventWrapper.RunPyEvent);
end;

procedure TXIDEForm.LoadIframes(dum:integer);
begin
  // un-suspend all the 'iframe' components...
  GlobalSuppressFrameDisplay:=false;
  UnSuspendFrames(SystemNodeTree);
end;

procedure TXIDEForm.FormActivate(Sender: TObject);
begin
  LoadIframes(0);
 // Application.QueueAsyncCall(@LoadIframes, 0);
end;

procedure TXIDEForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSystemData;
  SuppressEvents:=true;
  //// suggested fix from https://forum.lazarus.freepascal.org/index.php?topic=61029.0
  //Application.Restore;
  //if (not Self.Visible) then
  //  Self.Show;
  //Self.BringToFront;
  CloseAction := caFree;
  //// fix didn't make any difference
  {$ifdef Python}
  if Assigned(PythonEngine1) then
  begin
    //PythonEngine1.Finalize;  //!! to prevent crash when main form is closed  !!!! not working?. not available
    //!! with this version (64bit, latest) as at 30/11/24, the form is left on the screen 'hanging' after close.  No error messages.
    PythonEngine1.Destroy;
  end;
  {$endif}
  //inherited FormClose(Sender, CloseAction);
  Application.ProcessMessages;
  Application.Terminate;
  halt;  // sledgehammer fix 30/11/24, to clear the mainform window and stop execution.

end;

procedure TXIDEForm.CompileToJSClick(Sender: TObject);
var
  ok:Boolean;
  i:integer;
  ExtraDirectives,ExtraHTML:TStringList;
begin

  // Start in Design Mode
  if not DesignMode then
    DoToggleDesignRunMode(ToggleDesignRunMode);

  // Close any open popups
  for i:=length(OpenXForms)-1 downto 0 do
  begin
    XFormClose(OpenXForms[i].NodeName,OpenXForms[i].NameSpace);
  end;

  // Delete object inspector dynamic property editor fields
  ClearInspectors;

  SaveSystemData;

  ExtraDirectives:=TStringList.Create;
  // Compile the user-created event code into a unit, to check for pas2js compile errors.
  ok:=true;
  if ConfigfpcPath<>'' then
    ok:=CompileEventCode(CodeEditForm.CodeEdit,'LazJS');
  ExtraHTML:=TStringList.Create;
  if ok then
  begin
    // additional inc file for composite resources
    SaveCompositesToIncFile;
    {$ifdef Python}
    ExtraDirectives.add('-dPython');
    ExtraHTML:=PyodideScript;
    {$endif}
    {$ifdef TensorflowJS}
    ExtraHTML.Add('<script src="https://cdn.jsdelivr.net/npm/@tensorflow/tfjs@3.6.0/dist/tf.min.js"> </script> '); //##### <!-- Import @tensorflow/tfjs -->
    {$endif}

    //ExtraHTML.Add('<script src="http://asterius.netlify.app/demo/pandoc/pandoc.js"></script> ');  //###### pandoc test
    //ExtraHTML.Add('<script src="file:///C:/Laz19Projects/XIDE/pandoc/pandoc.js"></script> ');  //###### pandoc test
    //ExtraHTML.Add('<script src="file:///C:/Laz19Projects/XIDE/pandoc/tryserver.js"></script> ');  //###### pandoc test

    CompileJSandExecute('resources/project/',ExtraDirectives,ExtraHTML);
    if not FileExists('XIDEMain.js') then
      ShowCompilerLog;
  end
  else
  begin
    DisplayDllCompileErrors;
  end;
  ExtraDirectives.free;
  ExtraHTML.Free;
end;


procedure TXIDEForm.CompilerShowLogClick(Sender: TObject);
begin
  ShowCompilerLog;
end;

procedure TXIDEForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, MyRootDiv);
end;


{$else}
procedure InitialisePage(dummy:string);
var
  tempstr,dm:string;
  i:integer;
  s,str:string;
  ok:Boolean;
begin
  ok:=true;
  StartingUp:=true;// suppress event handlers while starting up
  CheckBrowser;

  // this include file contains create statements for all the interface objects in main form and other forms
  // XForm nodes are added as children of UIRootNode.
  {$I systemintface.inc}
  MainForm:=XIDEForm;
  //showmessage('mainform node is '+MainForm.myNode.NodeName+' class='+MainForm.myNode.NodeClass+' type='+MainForm.myNode.NodeType);

  asm
  try{
     // now do Javascript specific start up code
     pas.HTMLUtils.addHandVBoxStyles();
     pas.HTMLUtils.addWidgetInnerStyles();
     }catch(err) { alert(err.message+' in XIDEMain');}
  end;

  XIDESetupUIRootNode;
  //InitDMTree;

  NavTreeComponent:=XIDEForm.NavTree.myNode;
  ResourceTreeComponent:=XIDEForm.ResourceTree.myNode;
  CodeTreeComponent:=XIDEForm.CodeTree.myNode;

  MainFormProjectRoot:=FindDataNodeById(SystemNodeTree,UIProjectRootName,'',true);

  BuildSkeletonResourceTree;
  InitialiseStyleDesigner;

  // this include file (systemnodetree.inc) contains the system description to be loaded at startup.
  // It includes the XIDE framework components.
  // It reflects the system as it was defined when the 'Run in Browser' button was last pressed in the Desktop environment.
  {$I systemnodetree.inc}
  XMLToNodeTree(LoadedSystemString,UIRootNode);   //! has been saved by the 'Run in Browser' menu button
  InitialiseXIDE;

  PopupMemoForm.InitialiseMemo;
  CodeEditForm.Initialise;

  RedisplayResourceTree;
  InitialiseStyleResources;
  StyleRootNode:=XIDEForm.StyleSheet.myNode;
  GenerateStyleSheets;


  //systempas:='1' + LineEnding
  //+ '2' + LineEnding
  //+ '3';   // this construct was causing pas2js to produce a stack overflow.  Using \n instead.

  {$I eventsinterfacepas.inc}
  {$I interfacetypespas.inc}

  // minimal rtl set....
  //{$I systempas.inc}
  //{$I typespas.inc}
  //{$I classespas.inc}
  //{$I jspas.inc}
  //{$I rtlconstspas.inc}
  //{$I sysutilspas.inc}

  // common rtl set.....
  {$I classespas.inc}
  {$I contnrspas.inc}
  {$I dateutilspas.inc}
  {$I jspas.inc}
  {$I mathpas.inc}
  {$I rtlconstspas.inc}
  {$I strutilspas.inc}
  {$I systempas.inc}
  {$I sysutilspas.inc}
  {$I typespas.inc}
  {$I typinfopas.inc}
  //  {$I rttipas.inc}

  {$I gpu-browserjs.inc}

  // If this JS startup was NOT initiated from the Lazarus/Desktop runtime, then load up
  // the last stored system description. This replaces the user interface area being designed by the user, but not
  // the XIDE framework object inspector components.
  RunningDeployedRuntime:=false;

  asm
    dm = myDeployedMode;
  end;

  if dm='FromLaz' then
  begin
    {$I systemcomposites.inc}
    InitialiseComposites;
  end;

  asm console.log('dm='+dm); end;
  if dm<>'FromLaz' then
  begin
    if dm='Run' then
    begin
      // hide the XIDE framework
      ShowHideObjectInspector(false);
    end;

    {$ifndef Python}
    //showmessage('NOT python - checking for saved system');
    ok:=CheckForSavedSystemOnLoad;
    // when using python, we have to wait until pyodide is ready, so this step
    // is handled in the python load scripts.
    {$endif}
  end;

  StartingUp:=false;// suppress event handlers while starting up
  SelectNavTreeNode(MainFormProjectRoot,true);
  RunModeAttempts:=0;

  if ok then
  begin
      // Deal with the DeployedMode flag
      if dm='Run' then
      begin
        RunningDeployedRuntime:=true;
        XIDEForm.SystemDeploy.IsVisible:=false;
        XIDEForm.ToggleDesignRunMode.IsVisible:=false;
        XIDEForm.HelpMenu.IsVisible:=false;
        XIDEForm.SystemClear.IsVisible:=false;
        XIDEForm.SystemDeploy.IsVisible:=false;
        XIDEForm.SystemLoad.IsVisible:=false;
        XIDEForm.SystemLoadFromStore.IsVisible:=false;
        XIDEForm.SystemEncapsulate.IsVisible:=false;
        XIDEForm.SystemSettings.IsVisible:=false;
        ShowGreyOverlay(SystemRootName,'Grey1','Compiling Deployed System. Please Wait...');
        // timeout here so the grey overlay appears
        {$ifndef Python}
        asm
        myTimeout(pas.XObjectInsp.ContinueToggleToRunMode,5,'ContinueToggleToRunMode',0);
        end;
        // when using python, we have to wait until pyodide is ready, so this step
        // is handled in the python load scripts.
        {$endif}
      end;
  end;

end;
{$endif}

begin
  MainUnitName:='XIDEMain';

  {$ifndef JScript}
    Application.ShowHint:=true;
    {$I rtl.lrs}
    {$I xide.lrs}
  {$endif}
end.

