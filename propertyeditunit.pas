(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit PropertyEditUnit;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, StringUtils, NodeUtils,
  {$ifndef JScript}
  FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLIntf, ExtCtrls, Menus, ComCtrls, Grids, TypInfo, LazIDEIntf,
  LazsUtils,
  UtilsJSCompile, Events, Types, StdCtrls,
  {$else}
  HTMLUtils,
  {$endif}
  WrapperPanel,XVBox,XScrollBox, XHBox, XTree, XTable,XMemo, XTabControl, XButton, XLabel,
  XEditBox, XCheckBox, XHyperLink, XRadioBtns, XCode, XForm, XComboBox,
  XColorPicker, CodeEditor, InputSelectUnit,EventsInterface;

type

  { TPropertyEditForm }

  TPropertyEditForm = class(TXForm)
    PropertyEditFormXHBox2: TXHBox;
    PropertyEditFormXTabControl11: TXTabControl;
    PropertyEditSourceHBox: TXHBox;
    PropertyEditVBox1: TXVBox;
    PropertyEditTabSheet1: TXTabSheet;
    PropertyEditTab1VBox1: TXVBox;
    PropertyEditName: TXEditBox;
    PropertyEditType: TXEditBox;
    PropertyEditDoneBtn: TXButton;
    PropertyEditCancelBtn: TXButton;
    PropertyEditSourceBox: TXEditBox;
    PropertyEditSourceBtn: TXButton;
    PropertyEditTabSheet3: TXTabSheet;
    PropertyEditVBox99: TXVBox;
    PropertyEditLabel: TXLabel;
    PropertyEditVBox66: TXVBox;
    PropertyEditHint: TXEditBox;
    {$ifndef JScript}
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    {$endif}
    procedure PropertyEditCancelBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure PropertyEditDoneBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure PropertyEditSourceBtnHandleButtonClick(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure HandleTreeDataNodeSelect(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure HandleTreeDataDragStart(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure HandleTreeDataDrop(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure InsertSiblingNode(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure InsertChildNode(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure DeleteTreeNode(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure CopyTreeNode(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure PasteTreeNode(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure InsertTableRow(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure InsertTableColumn(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure DeleteTableRow(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure DeleteTableColumn(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
    procedure CopyStringToTable(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);

    procedure SetupPages;
  private

  public
    TargetNode:TDataNode;
    EditNode:TDataNode;
    TargetAttribute:TNodeAttribute;
  end;

var
  PropertyEditForm: TPropertyEditForm;
  PropertyEditBox:TDataNode;
  PropertyEditStatus:String;

  {$ifndef JScript}
  CopiedTreeNode:TTreeNode;
  {$else}
  CopiedTreeNode:String;
  {$endif}

implementation
{$R *.lfm}
uses xobjectinsp;

procedure TPropertyEditForm.SetupPages;
var
  NewEditBox:TXEditBox;
  NewCheckBox:TXCheckBox;
  NewColorPicker:TXColorPicker;
  NewComboBox:TXComboBox;
  NewMemo:TXMemo;
  NewTree:TXTree;
  NewHBox:TXHBox;
  NewTable:TXTable;
  NewBtn1,NewBtn2,NewBtn3,NewBtn4,NewBtn5:TXButton;
  AttribOptions:TStringList;
  str:String;
begin
  PropertyEditStatus:='Cancel';
  InputSelectForm.ExclNodeName:=TargetNode.NodeName;
  InputSelectForm.ExclPropertyName:=TargetAttribute.AttribName;
  InputSelectForm.PropType:=TargetAttribute.AttribType;

  DeleteNodeChildren(PropertyEditVBox66.myNode);

  self.Caption := 'Property Editor : '+TargetNode.Nodename + '.' + TargetAttribute.AttribName;
  PropertyEditLabel.LabelCaption:='Edit the value of property '+TargetAttribute.AttribName;

  //Build the necessary widget(s) for editing the property according to type
  if (TargetAttribute.AttribType='String')
  or (TargetAttribute.AttribType='Integer') then
  begin
    AttribOptions:=LookupAttribOptions(TargetNode.NodeType,TargetAttribute.AttribName);
    if AttribOptions<>nil then
    begin
      NewComboBox:=TXComboBox(AddDynamicWidget('TXComboBox',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget','','Left',-1).ScreenObject);
      NewComboBox.OptionList:=StringListToJSONString(AttribOptions);
      NewComboBox.LabelText:='';
      EditNode:=NewComboBox.myNode;
      NewComboBox.ItemValue:=TargetAttribute.AttribValue;
    end
    else
    begin
       // property Itemvalue in a TXMemo is type String, but needs a memo editor
      if ((TargetAttribute.AttribName='ItemValue') and (TargetNode.NodeType='TXMemo'))
            or ((TargetAttribute.AttribName='MapData') and (TargetNode.NodeType='TXBitMap'))
            or ((TargetAttribute.AttribName='MapColors') and (TargetNode.NodeType='TXBitMap'))
            or ((TargetAttribute.AttribName='MapPixelArray') and (TargetNode.NodeType='TXBitMap'))
            or ((TargetAttribute.AttribName='HTMLSource'))
            or ((TargetAttribute.AttribName='SourceText') and (TargetNode.NodeType='TXHTMLText'))
            or ((TargetAttribute.AttribName='SourceText') and (TargetNode.NodeType='TXHTMLEditor'))
            or ((TargetAttribute.AttribName='XMLString') and (TargetNode.NodeType='TXSVGContainer'))
            or ((TargetAttribute.AttribName='OptionList'))
      then
      begin
        NewMemo:=TXMemo(AddDynamicWidget('TXMemo',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget','','Left',-1).ScreenObject);
        NewMemo.LabelText:='';
        NewMemo.MemoHeight:='90%';
        NewMemo.MemoWidth:='95%';
        NewMemo.ItemValue:=TargetAttribute.AttribValue;
        EditNode:=NewMemo.myNode;
      end
      else
      begin
        // just a short string (or number) - EditBox
        NewEditBox:=TXEditBox(AddDynamicWidget('TXEditBox',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget','','Left',-1).ScreenObject);
        NewEditBox.LabelText:='';
        EditNode:=NewEditBox.myNode;
        NewEditBox.ItemValue:=TargetAttribute.AttribValue;
        NewEditBox.BoxWidth:='90%';
      end;
    end;
  end
  else if TargetAttribute.AttribType='TreeString' then
  // property TreeData is type String, but needs a special editor
  begin
     NewTree:=TXTree(AddDynamicWidget('TXTree',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget','','Left',-1).ScreenObject);
     NewTree.TreeData:=TargetAttribute.AttribValue;
     NewTree.TreeHeight:='80%';
     NewTree.TreeWidth:='100%';
     NewTree.LabelText:='';
     NewTree.ReadOnly:=false;
     NewTree.Draggable:=true;
     // tree needs event handlers for editing
     // copy, cut, paste, drag/drop, delete, insert
     NewTree.myNode.registerEvent('TreeNodeClick',@PropertyEditForm.HandleTreeDataNodeSelect);
     NewTree.myNode.registerEvent('DragStart',@PropertyEditForm.HandleTreeDataDragStart);
     NewTree.myNode.registerEvent('Drop',@PropertyEditForm.HandleTreeDataDrop);

     NewHBox:=TXHBox(AddDynamicWidget('TXHBox',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditHBox66','','Left',-1).ScreenObject);
     NewHBox.ContainerHeight:='';

     NewBtn1:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditBtn1','','Left',-1).ScreenObject);
     NewBtn1.Caption:='Insert Sibling';
     NewBtn1.myNode.registerEvent('ButtonClick',@PropertyEditForm.InsertSiblingNode);

     NewBtn2:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditBtn2','','Left',-1).ScreenObject);
     NewBtn2.Caption:='Insert Child';
     NewBtn2.myNode.registerEvent('ButtonClick',@PropertyEditForm.InsertChildNode);

     NewBtn3:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditBtn3','','Left',-1).ScreenObject);
     NewBtn3.Caption:='Delete Node';
     NewBtn3.myNode.registerEvent('ButtonClick',@PropertyEditForm.DeleteTreeNode);

     NewBtn4:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditCopyBtn','','Left',-1).ScreenObject);
     NewBtn4.Caption:='Copy';
     NewBtn4.myNode.registerEvent('ButtonClick',@PropertyEditForm.CopyTreeNode);

     NewBtn5:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditPasteBtn','','Left',-1).ScreenObject);
     NewBtn5.Caption:='Paste';
     NewBtn5.myNode.registerEvent('ButtonClick',@PropertyEditForm.PasteTreeNode);

     EditNode:=NewTree.myNode;
  end
  else if TargetAttribute.AttribType='TableString' then
  begin
    NewTable:=TXTable(AddDynamicWidget('TXTable',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget','','Left',-1).ScreenObject);
    NewTable.IsNumeric:=TXTable(TargetNode.ScreenObject).IsNumeric;
    NewTable.TableData:=TargetAttribute.AttribValue;
    str:= NewTable.TableData;
    NewTable.TableHeight:='70%';
    NewTable.TableWidth:='100%';
    NewTable.LabelText:='';
    if TargetNode.NodeType='TXTable' then
    begin
      str:=TargetNode.GetAttribute('ColWidth',false).AttribValue;
      if str<>'' then
        NewTable.ColWidth:=strtoint(str);
    end;

    NewTable.HasHeaderRow:=false;       // allow for editing col headers

    NewHBox:=TXHBox(AddDynamicWidget('TXHBox',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditHBox66','','Left',-1).ScreenObject);
    NewHBox.ContainerHeight:='';

    NewBtn1:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditBtn1','','Left',-1).ScreenObject);
    NewBtn1.Caption:='Insert Row';
    NewBtn1.myNode.registerEvent('ButtonClick',@PropertyEditForm.InsertTableRow);

    NewBtn2:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditBtn2','','Left',-1).ScreenObject);
    NewBtn2.Caption:='Insert Column';
    NewBtn2.myNode.registerEvent('ButtonClick',@PropertyEditForm.InsertTableColumn);

    NewBtn3:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditBtn3','','Left',-1).ScreenObject);
    NewBtn3.Caption:='Delete Row';
    NewBtn3.myNode.registerEvent('ButtonClick',@PropertyEditForm.DeleteTableRow);

    NewBtn4:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,NewHBox.myNode,'PropertyEditBtn4','','Left',-1).ScreenObject);
    NewBtn4.Caption:='Delete Column';
    NewBtn4.myNode.registerEvent('ButtonClick',@PropertyEditForm.DeleteTableColumn);

    EditNode:=NewTable.myNode;

    // option to edit the TableData string directly - EditBox
    NewEditBox:=TXEditBox(AddDynamicWidget('TXEditBox',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget2','','Left',-1).ScreenObject);
    NewEditBox.LabelText:='OR edit TableData as string....';
    NewEditBox.LabelPos:='Top';
    NewEditBox.ItemValue:=TargetAttribute.AttribValue;
    NewEditBox.BoxWidth:='90%';
    NewBtn5:=TXButton(AddDynamicWidget('TXButton',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditBtn5','','Left',-1).ScreenObject);
    NewBtn5.Caption:='Apply to Table';
    NewBtn5.Hint:='Apply the string value to the Table';
    NewBtn5.myNode.registerEvent('ButtonClick',@PropertyEditForm.CopyStringToTable);


  end
  else if TargetAttribute.AttribType='Boolean' then
  begin
    NewCheckBox:=TXCheckBox(AddDynamicWidget('TXCheckBox',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget','','Left',-1).ScreenObject);
    NewCheckBox.LabelText:='';
    EditNode:=NewCheckBox.myNode;
    NewCheckBox.Checked:=myStrToBool(TargetAttribute.AttribValue);
  end
  else if TargetAttribute.AttribType='Color' then
  begin
    NewColorPicker:=TXColorPicker(AddDynamicWidget('TXColorPicker',PropertyEditForm,PropertyEditVBox66.myNode,'PropertyEditWidget','','Left',-1).ScreenObject);
    NewColorPicker.LabelText:='';
    EditNode:=NewColorPicker.myNode;
    NewColorPicker.ItemValue:=TargetAttribute.AttribValue;
  end;
  if PropertyEditForm.PropertyEditTabSheet3.IsVisible then
    PropertyEditFormXTabControl11.TabIndex:=1
  else
    PropertyEditFormXTabControl11.TabIndex:=0;
end;

{$ifndef JScript}
procedure TPropertyEditForm.FormCreate(Sender: TObject);
begin
  myNode:=DoXFormCreated(self);
end;

procedure TPropertyEditForm.FormResize(Sender: TObject);
begin
  DoFormResize(self, PropertyEditVBox1);
end;

{$endif}


procedure TPropertyEditForm.PropertyEditCancelBtnHandleButtonClick(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  PropertyEditStatus:='Cancel';
  TXForm(self).Showing:='No';
  PropertyEditBox:=nil;
end;

procedure TPropertyEditForm.PropertyEditDoneBtnHandleButtonClick(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
  PropertyEditStatus:='ok';
  TXForm(self).Showing:='No';
  PropertyEditorClosed(PropertyEditBox);
end;

procedure TPropertyEditForm.PropertyEditSourceBtnHandleButtonClick(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin
    InputSelectForm.TargetEditBoxNode:=PropertyEditSourceBox.myNode;
    if TargetNode.NodeType<>'TXComposite' then                   //!!!! improve this
      InputSelectForm.PropType:=TargetAttribute.AttribType
    else
      InputSelectForm.PropType:='';
    InputSelectForm.Initialise;
    ShowXForm('InputSelectForm',true);
    {$ifndef JScript}
    // on return...
    InputSelectForm.InputSelectClosed;
    {$endif}
end;

procedure TPropertyEditForm.HandleTreeDataNodeSelect(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
begin

end;

procedure TPropertyEditForm.HandleTreeDataDragStart(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);           //!!!! do this with node id's....
var
  treeNode:TDataNode;
  myXTree:TXTree;
begin
  //showmessage('dragstart. value='+myValue);
  // value is the text of the selected (dragged) node
  {$ifndef JScript}
  treeNode:=FindDataNodeById(SystemNodeTree,nodeId,'',true);
  myXTree:=TXTree(treenode.ScreenObject);
  myXTree.SelectedNodeText:=myValue;

  {$else}
  //treeNode:=FindDataNodeById(SystemNodeTree,nodeId,'',true);
  //myXTree:=TXTree(treenode.ScreenObject);
  //myXTree.NodeBeingDragged:=myValue;
  {$endif}
end;

procedure TPropertyEditForm.HandleTreeDataDrop(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);         //!!!! node id's
var
  myXTree:TXTree;
  treeNode:TDataNode;
begin
  //showmessage('drop.  data='+myValue);
  // SelectedNodeText is the node being dragged
  // myValue is the target node
  treeNode:=FindDataNodeById(SystemNodeTree,nodeId,'',true);
  myXTree:=TXTree(treenode.ScreenObject);


  {$ifndef JScript}
  myXTree.MoveNode(myXTree.SelectedNodeText,myValue);
  {$else}
  myXTree.MoveNode(myXTree.NodeBeingDragged,myValue);         //!!!! change this to use node id instead of text
  myXTree.NodeBeingDragged:='';
  {$endif}

end;

procedure TPropertyEditForm.InsertSiblingNode(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  TheTree:TDataNode;
  i:integer;
begin
  TheTree:=FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true);
  i:=1;
  while (not TXtree(TheTree.ScreenObject).NodeTextIsUnique('New Node'+intToStr(i))) do
    i:=i+1;
  TXtree(TheTree.ScreenObject).InsertNewSiblingNode('New Node'+intToStr(i));
end;

procedure TPropertyEditForm.InsertChildNode(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  TheTree:TDataNode;
  i:integer;
begin
  TheTree:=FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true);
  i:=1;
  while (not TXtree(TheTree.ScreenObject).NodeTextIsUnique('New Node'+intToStr(i))) do
    i:=i+1;
  TXtree(TheTree.ScreenObject).InsertNewChildNode('New Node'+intToStr(i));
end;

procedure TPropertyEditForm.DeleteTreeNode(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  TheTree:TDataNode;
begin
  TheTree:=FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true);

  TXtree(TheTree.ScreenObject).DeleteSelectedNode;


end;

procedure TPropertyEditForm.CopyTreeNode(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  TheTree:TDataNode;
  {$ifndef JScript}
  myTree:TmyTreeView;
  {$endif}
begin
  TheTree:=FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true);
  {$ifndef JScript}
  myTree:=TmyTreeView(TXTree(TheTree.ScreenObject).myControl);
  if myTree.Selected<>nil then
  begin
    CopiedTreeNode:=myTree.Selected;
  end
  else
    showmessage('Please select a tree node first');
{$else}
  CopiedTreeNode:=TXTree(TheTree.ScreenObject).SelectedNodeId;
{$endif}
end;

{$ifndef JScript}
function CopyThisNode(myTree:TmyTreeView;ANode,NewParent:TTreeNode):TTreeNode;
var
  NodeParent:TTreeNode;
  NodeText:String;
begin
  NodeText:=ANode.Text;
//  NodeText:=myTree.MakeTextUnique(NodeText);
  NodeParent:=myTree.Items.AddChild(NewParent,NodeText);
  if ANode.HasChildren then          //!!!! suspect this built-in also needs uniqueness for node texts...
  begin
    ANode:=ANode.GetFirstChild;
    if ANode<>nil then
    begin
      CopyThisNode(myTree,ANode,NodeParent);
      repeat
        ANode := ANode.getNextSibling;
        if ANode<>nil then
        begin
          CopyThisNode(myTree,ANode,NodeParent);
        end;
      until ANode = nil;
    end;
  end;
  result:=NodeParent;
end;
{$endif}

procedure TPropertyEditForm.PasteTreeNode(
  e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  TheTree:TDataNode;
  {$ifndef JScript}
  myTree:TmyTreeView;
  {$endif}
begin
{$ifndef JScript}
  if (CopiedTreeNode<>nil) then
  begin
    TheTree:=FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true);
    myTree:=TmyTreeView(TXTree(TheTree.ScreenObject).myControl);
    if (myTree.Selected<>nil) then
    begin
      // traverse the copied tree node, inserting new node and children at selected spot
      myTree.Selected:=CopyThisNode(myTree,CopiedTreeNode,myTree.Selected);
    end
    else
      showmessage('Please select a tree node first');
  end
{$else}
  if (CopiedTreeNode<>'') then
  begin
    TheTree:=FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true);
    if TXTree(TheTree.ScreenObject).SelectedNodeId<>'' then
    begin
      //!!!!
    end
    else
      showmessage('Please select a tree node first');
  end
{$endif}
  else
    showmessage('Please copy a tree node first');
end;

procedure TPropertyEditForm.InsertTableRow(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  myTable:TXTable;
  myEditBox:TXEditBox;
begin
  myTable:=TXTable(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true).ScreenObject);
  myEditBox:=TXEditBox(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget2','',true).ScreenObject);
  myTable.AddTableRows(1);
  myEditBox.ItemValue:=myTable.TableData;
  {$ifndef JScript}
  TStringGrid(myTable.myControl).FixedRows:=0; //allow editing column headers
  {$endif}

end;

procedure TPropertyEditForm.InsertTableColumn(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  myTable:TXTable;
  myEditBox:TXEditBox;
begin
  myTable:=TXTable(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true).ScreenObject);
  myEditBox:=TXEditBox(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget2','',true).ScreenObject);
  myTable.AddTableColumns(1);
  myEditBox.ItemValue:=myTable.TableData;
  {$ifndef JScript}
  TStringGrid(myTable.myControl).FixedRows:=0; //allow editing column headers
  {$endif}
end;

procedure TPropertyEditForm.DeleteTableRow(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  myTable:TXTable;
  myEditBox:TXEditBox;
begin
  myTable:=TXTable(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true).ScreenObject);
  myEditBox:=TXEditBox(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget2','',true).ScreenObject);
  myTable.DeleteSelectedRow;
  myEditBox.ItemValue:=myTable.TableData;
end;

procedure TPropertyEditForm.DeleteTableColumn(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  myTable:TXTable;
  myEditBox:TXEditBox;
  r,c:integer;
begin
  myTable:=TXTable(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true).ScreenObject);
  myEditBox:=TXEditBox(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget2','',true).ScreenObject);
  r:=myTable.SelectedRow;
  c:=myTable.SelectedCol;
  //i:=TStringGridAccess(TStringGrid(myTable.myControl)).Col;
  if (r<0) or (c<0) then
    showmessage('column not selected')
  else if (c=0) and (myTable.NumCols=1) then
    showmessage('cannot delete all columns')
  else
  begin
    {$ifndef JScript}
    TStringGridAccess(TStringGrid(myTable.myControl)).DeleteCol(c);
    {$else}
    asm
        var ob = document.getElementById(myTable.NameSpace+myTable.NodeName+'Contents');
        if (ob!=null) {
        for (var i = 0, row; row = ob.rows[i]; i++) {
                row.deleteCell(c);
            } }
    end;
    {$endif}
    myTable.TableData:=myTable.ConstructDataString;
    myEditBox.ItemValue:=myTable.TableData;
    {$ifndef JScript}
    TStringGrid(myTable.myControl).FixedRows:=0; //allow editing column headers
    {$endif}
  end;
end;

procedure TPropertyEditForm.CopyStringToTable(e:TEventStatus;nodeID: AnsiString; myValue: AnsiString);
var
  myTable:TXTable;
  myEditBox:TXEditBox;
begin
  myTable:=TXTable(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget','',true).ScreenObject);
  myEditBox:=TXEditBox(FindDataNodeById(PropertyEditVBox66.myNode,'PropertyEditWidget2','',true).ScreenObject);
  myTable.TableData:=myEditBox.ItemValue;
end;


end.

