unit XCompositeIntf;

//Non-visual component to provide dynamic interface properties and events when encapsulating a system.
// Also see XComposite.pas.

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, NodeUtils,StringUtils,
    {$ifndef JScript}
    Forms, Controls, StdCtrls, LResources, Graphics, Dialogs, ExtCtrls, PropEdits, RTTICtrls,
    LazsUtils,
    {$Else}
    HTMLUtils,
    {$endif}
    WrapperPanel;


type
    {$ifndef JScript}
    TXCompositeIntf = class(TComponent)
    {$Else}
    TXCompositeIntf = class(TWrapperPanel)
    {$endif}
  private
  {$ifndef JScript}
  FmyNode:TDataNode;
  FmyControl:TControl;
  {$endif}
    { Private declarations }

    procedure SetMyEventTypes;

  protected
    { Protected declarations }
  {$ifndef JScript}
  function GetName:string;
  procedure SetMyName(AValue:string);
  {$endif}
  public
    { Public declarations }
    {$ifndef JScript}
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean);
    destructor Destroy; override;
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    {$endif}

    function PropertyNameIsUnique(NewName:String):Boolean;
    function EventNameIsUnique(NewName:String):Boolean;

  published
    { Published declarations }
    {$ifndef JScript}
    property myControl:TControl read FmyControl write FmyControl;
    property Name: String read GetName write SetMyName;
    property myNode:TDataNode read FmyNode write FmyNode;
    {$endif}

    // Properties defined for this class...
  end;


implementation

var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXCompositeIntf.SetMyEventTypes;
begin
end;

{$ifndef JScript}

procedure TXCompositeIntf.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
var
  NewNode:TDataNode;
begin

    NewNode:=TDataNode.Create('NV',self.Name,'','TXCompositeIntf',false);
    NewNode.ScreenObject:=self;
    NewNode.MyEventTypes:=TStringList.Create;
    SetLength(NewNode.myEventHandlers,0);
    NewNode.MyForm:=TForm(TheOwner);
    NewNode.IsDynamic:=IsDynamic;
    self.myNode:=NewNode;
    // temporarily set as child of root node, so that name uniqueness checks can be done during design
    AddChildToParentNode(SystemNodetree,NewNode,-1);

    AddDefaultAttribs(self,NewNode,mydefaultAttribs);

end;

constructor TXCompositeIntf.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,false);
end;

constructor TXCompositeIntf.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner);
  DoConstructor(TheOwner,IsDynamic);
end;

destructor TXCompositeIntf.Destroy;
begin
  //ClearLocalStore(KeyName);
  inherited Destroy;
end;

function TXCompositeIntf.GetName:string;
begin
  result:=inherited Name;
end;

procedure TXCompositeIntf.SetMyName(AValue:string);
begin
  inherited Name:=AValue;

  if  (csLoading in componentState) then
    if myNode<>nil then
      myNode.NodeName:=AValue;
end;

function CreateIntf(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXCompositeIntf',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

{$else}

constructor TXCompositeIntf.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeClass:='NV';
  self.NodeType:='TXCompositeIntf';
  self.MyForm:=MyForm;

  self.myNode.MyEventTypes:=TStringList.Create;
  self.IsContainer:=false;
  SetNodePropDefaults(self,myDefaultAttribs);

end;

function CreateIntf(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
begin

MyNode.ScreenObject:=MyNode;

result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXCompositeIntf.Create(MyForm,NodeName,NameSpace));
end;

{$endif}

function TXCompositeIntf.PropertyNameIsUnique(NewName:String):Boolean;
var
  i:integer;
begin
  result:=true;
  for i:=0 to length(myNode.NodeAttributes)-1 do
    if myNode.NodeAttributes[i].AttribName=NewName then
      result:=false;
end;

function TXCompositeIntf.EventNameIsUnique(NewName:String):Boolean;
var
  i:integer;
begin
  result:=true;
  for i:=0 to myNode.myEventTypes.Count-1 do
    if myNode.myEventTypes[i]=NewName then
      result:=false;
end;

begin

{$ifndef JScript}
RegisterClass(TXCompositeIntf);
AddNodeFuncLookup('TXCompositeIntf',@CreateIntf);
{$else}
AddNodeFuncLookup('TXCompositeIntf',@CreateinterfaceObj,@CreateIntf);
{$endif}
SuppressDesignerProperty('TXCompositeIntf','Alignment');
SuppressDesignerProperty('TXCompositeIntf','IsVisible');
SuppressDesignerProperty('TXCompositeIntf','LabelPos');
SuppressDesignerProperty('TXCompositeIntf','LabelText');
SuppressDesignerProperty('TXCompositeIntf','Hint');
SuppressDesignerProperty('TXCompositeIntf','Border');
end.

