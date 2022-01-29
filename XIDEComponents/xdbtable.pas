unit XDBTable;
{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, EventsInterface,
  {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf,
    LCLType, bufdataset, db, DBGrids,
  {$else}
    HTMLUtils,
  {$endif}
    WrapperPanel, Events;

type
  TXDBTable = class(TWrapperPanel)
private
  { Private declarations }
  {$ifndef JScript}
  fHandleClick:TEventHandler;
  fDataSource:TDataSource;
  procedure Tableclick(Sender:TObject);
  {$endif}

  procedure SetMyEventTypes;

  function GetTableWidth:string;
  function GetTableHeight:string;
  function GetColWidth:integer;
  function GetDSName:string;

  procedure SetTableWidth(AValue:string);
  procedure SetTableHeight(AValue:string);
  procedure SetColWidth(AValue:integer);
  procedure SetDSName(AValue:string);

protected
  { Protected declarations }
  {$ifndef JScript}
  procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  property ParentColor;
  {$endif}
public
  { Public declarations }
  {$ifndef JScript}
  constructor Create(TheOwner: TComponent); override;
  constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
  {$else}
  constructor Create(MyForm:TForm;NodeName,NameSpace:String);
  {$endif}

published
  { Published declarations }

  // Properties defined for this class...
  property TableHeight: String read GetTableHeight write SetTableHeight;
  property TableWidth: String read GetTableWidth write SetTableWidth;
  property ColWidth:Integer read GetColWidth write SetColWidth;
  property DSName:string read GetDSName  write SetDSName;

  {$ifndef JScript}
  property DataSource:TDataSource read fDataSource write fDataSource;
  property HandleClick: TEventHandler read FHandleClick write FHandleClick;
  {$endif}
end;

{$ifndef JScript}
var
  ProjectBufDatasets:Array of TBufDataset;
{$endif}

procedure SetDSNameOptions(FromRoot:TDataNode);

implementation

const MyNodeType='TXDBTable';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXDBTable.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;

{$ifndef JScript}
constructor TXDBTable.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXDBTable.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXDBTable.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TDBGrid.Create(self);
  myControl.Parent:=self;

  self.DataSource:=TDataSource.Create(self);
  TDBGrid(myControl).DataSource := self.DataSource;

  myControl.OnClick:=@self.TableClick;

  self.SetMyEventTypes;

  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  AddLabel(myControl);
end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXDBTable',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXDBTable.TableClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
    CallHandleEvent('Click','',self);
end;

procedure TXDBTable.SetTableWidth(AValue:string);
var
  tc:TControl;
begin
  tc:=self.myControl;
  myNode.SetAttributeValue('TableWidth',AValue);
  SetHeightWidth(self.myNode,tc,'TableWidth','TableHeight');

end;

procedure TXDBTable.SetTableHeight(AValue:string);
var
  tc:TControl;
begin
  tc:=self.myControl;
  myNode.SetAttributeValue('TableHeight',AValue);
  SetHeightWidth(self.myNode,tc,'TableWidth','TableHeight');
end;

procedure TXDBTable.SetColWidth(AValue:integer);
var
  i:integer;
begin
  myNode.SetAttributeValue('ColWidth',IntToStr(AValue));
  //showmessage('colcount='+inttostr(TStringGrid(myControl).ColCount));
  //for i:=0 to TStringGrid(myControl).ColCount-1 do
  //    TStringGrid(myControl).ColWidths[i]:=AValue;
end;

function GetDataset(DSName:String;var idx:integer;showError:Boolean=true):TBufDataset;
var
  i:integer;
begin
  result:=nil;
  idx:=-1;
  for i:=0 to length(ProjectBufDatasets)-1 do
  begin
    if ProjectBufDatasets[i].Name = DSName then
    begin
      result:=ProjectBufDatasets[i];
      idx:=i;
    end;
  end;
  if (showError) and (result=nil) then
    showmessage('Cannot find Dataset '+DSName);
end;

procedure TXDBTable.SetDSName(AValue:string);
var
  tc:TControl;
  ds:TBufDataset;
  i:integer;
begin
  tc:=self.myControl;
  myNode.SetAttributeValue('DSName',AValue);
  ds:=GetDataset(AValue,i,false);
  if ds<>nil then
  begin
    self.DataSource.DataSet := ds;
  end;
end;

{$else}
constructor TXDBTable.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  SetNodePropDefaults(self,myDefaultAttribs);
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  Checked,ReadOnly,LabelText,LabelPos:string;
  OnChangeString, OnFocusOutString, OnClickString, OnKeyString:String;
begin
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();var rc=pas.XDBTable.CellClick(event.target,'''+ScreenObjectName+''','''+NameSpace+''');'+
                          'if (rc!='''') { '+
                          'pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', rc); }' +
                          '" ';
  asm
    try{
    //pas.XDBTable.AddTableStyles('');

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);
    wrapper.style.display = 'flex';
    var goright =  'flex-e'+'nd';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var wrapperid = NameSpace+ScreenObjectName;
    var MyObjectName=wrapperid+'Contents';


    HTMLString = '<div id='+MyObjectName+ ' '+
                 '>Please see your browser Dev Tools for inspect/amend local data objectstores</div>';

    var wrapper=document.getElementById(wrapperid);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
  }
  catch(err) { alert(err.message+'  in XCheckBox.CreateXCheckBox');}

end;

  MyNode.ScreenObject:=MyNode;
  RefreshComponentProps(myNode);

  result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXDBTable.Create(MyForm,NodeName,NameSpace));
end;

procedure TXDBTable.SetTableWidth(AValue:string);
begin
  myNode.SetAttributeValue('TableWidth',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXDBTable.SetTableHeight(AValue:string);
begin
  myNode.SetAttributeValue('TableHeight',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

procedure TXDBTable.SetColWidth(AValue:integer);
var
  oldcw:integer;
begin
  //showmessage('SetColWidth '+inttostr(AValue));
  oldcw:=self.ColWidth;
  myNode.SetAttributeValue('ColWidth',IntToStr(AValue));
end;

procedure TXDBTable.SetDSName(AValue:string);
begin
  myNode.SetAttributeValue('DSName',AValue);
  asm
  var ob = document.getElementById(this.NameSpace+this.NodeName);
  end;
end;
{$endif}

function TXDBTable.GetTableHeight:string;
begin
  result:=MyNode.getAttribute('TableHeight',true).AttribValue;
end;
function TXDBTable.GetTableWidth:string;
begin
  result:=MyNode.getAttribute('TableWidth',true).AttribValue;
end;
function TXDBTable.GetColWidth:integer;
begin
  result:=StrToInt(MyNode.getAttribute('ColWidth',true).AttribValue);
end;
function TXDBTable.GetDSName:string;
begin
  result:=MyNode.getAttribute('DSName',true).AttribValue;
end;

procedure SetDSNameOptions(FromRoot:TDataNode);
var
  ClassDSNodes:TStringArray;
  ClassNodes:TNodesArray;
  i:integer;
begin
  ClassNodes := FindNodesOfType(FromRoot,'DMClass',false,'');
  SetLength(ClassDSNodes,0);
  for i:=0 to length(ClassNodes)-1 do
  begin
    if MyStrtoBool(ClassNodes[i].GetAttribute('MakeDataset',false).AttribValue)=true then
    begin
      SetLength(ClassDSNodes,length(ClassDSNodes)+1);
      ClassDSNodes[length(ClassDSNodes)-1]:=ClassNodes[i].NodeName;
    end;
  end;
  AddAttribOptions('TXDBTable','DSName',ClassDSNodes);
end;

begin
  // this is the set of node attributes that each TXTable instance will have.
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'TableWidth','String','300','',false);
  AddDefaultAttribute(myDefaultAttribs,'TableHeight','String','200','',false);
  AddDefaultAttribute(myDefaultAttribs,'ColWidth','Integer','40','',false);
  AddDefaultAttribute(myDefaultAttribs,'DSName','String','','',false);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  AddAttribOptions(MyNodeType,'DSName',[]);

  {$ifndef JScript}
  RegisterClass(TXDBTable);
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}

  SuppressDesignerProperty(MyNodeType,'BgColor');
  SuppressDesignerProperty(MyNodeType,'Border');
  SuppressDesignerProperty(MyNodeType,'ContainerHeight');
  SuppressDesignerProperty(MyNodeType,'ContainerWidth');
end.

