(*
    Copyright (c) 2018  Steve Wright

    This unit is part of the XIDEComponents package.

    This package is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit XComposite;

{$ifndef JScript}
{$mode objfpc}{$H+}
{$endif}

interface

uses
    Classes, SysUtils, TypInfo, StringUtils, NodeUtils, XIFrame, Math,
    UtilsJSCompile, XForm, XCode, XButton, XVBox, XTabControl, XMemo, EventsInterface,
    XCompositeIntf,
  {$ifndef JScript}
    LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls, Propedits, RTTICtrls,
    LazsUtils, LCLIntf,
    LCLType, gettext,
  {$else}
    HTMLUtils,
  {$endif}
    WrapperPanel, Events;


type
  TXComposite = class(TWrapperPanel)
  private
    { Private declarations }
    function GetInheritColor:Boolean;
    function GetCompositeType:String;
    function GetSourceString:String;

    procedure SetInheritColor(AValue:Boolean);
    procedure SetCompositeType(AValue:String);
    procedure SetSourceString(AValue:String);

    procedure SetMyEventTypes;

  protected
    { Protected declarations }
  public
    { Public declarations }
    {$ifndef JScript}
    procedure DoCompositeConstructor(TheOwner:TComponent);
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    destructor Destroy; override;
    {$else}
    constructor Create(MyForm:TForm;NodeName,NameSpace:String);
    {$endif}

  published
    { Published declarations }

    // Properties defined for this class...
    property InheritColor:Boolean read GetInheritColor write SetInheritColor;
    property CompositeType:String read GetCompositeType write SetCompositeType;
    property SourceString:String read GetSourceString write SetSourceString;
  end;

  procedure CompositePropertyChanged(myNode:TDataNode; propName:String);
  function FindCompositeContainer(StartNode:TDataNode):TdataNode;

implementation

const MyNodeType='TXComposite';
var
  myDefaultAttribs:TDefaultAttributesArray;

procedure TXComposite.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
end;



{$ifndef JScript}

function CreateCompositeWidget(ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=CreateDynamicLazWidget('TXComposite',ParentNode.MyForm,ParentNode,ScreenObjectName,NameSpace,Alignment,position);
  result:=NewNode;
end;

procedure TXComposite.DoCompositeConstructor(TheOwner:TComponent);
begin
  Caption:='';
  Constraints.MinHeight:=20;
  Constraints.MinWidth:=20;

  self.BorderSpacing.Around:=0;
  self.ParentColor:=false;
  self.IsContainer:=false;

  self.SetMyEventTypes;
  CreateComponentDataNode2(self,MyNodeType,myDefaultAttribs, self.myEventTypes, TheOwner,true);

  AlignChildrenVertical:=true;
end;

constructor TXComposite.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoCompositeConstructor(TheOwner);
end;

constructor TXComposite.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoCompositeConstructor(TheOwner);
end;

destructor TXComposite.Destroy;
begin
  if (not (csDesigning in componentState)) then
  begin
    myControl.Free;
  end;
  inherited Destroy;
end;

{$else} //JScript

constructor TXComposite.Create(MyForm:TForm;NodeName,NameSpace:String);
begin
  inherited Create(NodeName,NameSpace);
  self.NodeType:='TXComposite';
  self.MyForm:=MyForm;
  self.IsContainer:=false;

  self.SetMyEventTypes;

  SetNodePropDefaults(self,myDefaultAttribs);

end;

function CreateCompositeWidget(MyNode, ParentNode:TDataNode;ScreenObjectName,NameSpace:string;position:integer;Alignment:String):TDataNode;
var
  OnClickString:String;
begin

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(null,''Click'','''+ScreenObjectName+''','''+NameSpace+''', '''');" ';

  asm
  try{
      var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,NameSpace,$impl.MyNodeType,position);

      var HTMLString='';
      var wrapperid = NameSpace+ScreenObjectName;
      var MyObjectName=wrapperid+'Contents';

      HTMLString = '<div  id="'+MyObjectName+'" class="vboxNoStretch '+NameSpace+ScreenObjectName+'" '  +
                     ' style="height:100%;width:100%; "' +
                     OnClickString +
                     '></div>  ';

      var wrapper=document.getElementById(wrapperid);
      wrapper.insertAdjacentHTML('beforeend', HTMLString);

  }catch(err) { alert(err.message+'  in XVBox.CreateVHBox');}
  end;
  MyNode.ScreenObject:=MyNode;

  RefreshComponentProps(myNode);

  result:=myNode;
end;


function CreateinterfaceObj(MyForm:TForm;NodeName,NameSpace:String):TObject;
begin
  result:=TObject(TXComposite.Create(MyForm,NodeName,NameSpace));
end;

{$endif}
function TXComposite.GetInheritColor:Boolean;
begin
  result:=myStrToBool(myNode.GetAttribute('InheritColor',true).AttribValue);
end;

procedure TXComposite.SetInheritColor(AValue:Boolean);
var
  clr:String;
  parentNode:TDataNode;
begin
  if myNode<>nil then
  begin
    myNode.SetAttributeValue('InheritColor',myBoolToStr(AValue),'Boolean');
    parentNode:=FindParentOfNode(SystemNodeTree,myNode);
    if parentNode<>nil then
    begin
      if AValue=true then
      begin
        clr:= parentNode.GetAttribute('BgColor',true).AttribValue;
        myNode.SetAttributeValue('BgColor',clr,'Color');
        {$ifndef JScript}
        self.ParentColor:=true;
        {$else}
        asm
          var ob = document.getElementById(this.NameSpace+this.NodeName);
          if (ob!=null) {
            if (AValue==true ) {
               ob.style.backgroundColor='inherit';
          } }
        end;
        {$endif}
      end
      else
      begin
        clr:= myNode.GetAttribute('BgColor',true).AttribValue;
        {$ifndef JScript}
        self.ParentColor:=false;
        self.Color:=HexRGBToColor(clr);
        {$else}
        asm
          var ob = document.getElementById(this.NameSpace+this.NodeName);
          if (ob!=null) {
            if (AValue==true ) {
               ob.style.backgroundColor=clr;
          } }
        end;
        {$endif}
      end;
    end;
  end;
end;

function TXComposite.GetCompositeType:String;
begin
  result:=MyNode.getAttribute('CompositeType',true).AttribValue;
end;

function TXComposite.GetSourceString:String;
begin
  result:=MyNode.getAttribute('SourceString',true).AttribValue;
end;

procedure TXComposite.SetCompositeType(AValue:String);
begin
  myNode.SetAttributeValue('CompositeType',AValue);
end;

procedure TXComposite.SetSourceString(AValue:String);
begin
  myNode.SetAttributeValue('SourceString',AValue);
end;


procedure CompositePropertyChanged(myNode:TDataNode; propName:String);
var
  TargetNode:TDataNode;
  newValue:String;
begin
  if (myNode.NodeType='TXCompositeIntf')
  or (myNode.NodeType='TXComposite') then
  begin
    newValue:=myNode.GetAttribute(propName,false).AttribValue;
    if (myNode.NameSpace<>'') then
    begin
       // Expose new property value to the outside, for an interface definition
       if myNode.NodeType='TXCompositeIntf' then
       begin
          // find the composite container of this namespace
          TargetNode:=FindCompositeContainer(myNode);
          // set the equivalent property
          if TargetNode<>nil then
            TargetNode.SetAttributeValue(propName,newValue);
       end;
    end
    else if myNode.NodeType='TXComposite' then
    begin
      // update the interface property within the namespace
      TargetNode := FindInterfaceNode(myNode,myNode.NodeName,PropName);
      if TargetNode<>nil then
         EditAttributeValue(TargetNode,propName,newValue);
    end;
  end;
end;

function FindCompositeContainer(StartNode:TDataNode):TdataNode;
var
  SearchNode, PNode, CompositeNode:TDataNode;
  ok:Boolean;
begin
  // find the container of this composite namespace
  // work up the parent nodes until the namespace changes
   ok:=false;
   SearchNode:=StartNode;
   while (ok=false) do
   begin
     PNode:=FindParentOfnode(SystemNodeTree,SearchNode);
     if PNode<>nil then
     begin
       //if (SearchNode.NodeType<>'TXComposite')
       //or (SearchNode.NameSpace<>'') then
       if (PNode.NameSpace=StartNode.NameSpace) then
       begin
         ok:=false;
         SearchNode:=PNode;
       end
       else
       begin
         ok:=true;
         CompositeNode:=PNode;
       end;
     end
     else
       ok:=true;  // will return nil
   end;
   result:=CompositeNode;
end;

begin
  AddWrapperDefaultAttribs(myDefaultAttribs);
  AddDefaultAttribute(myDefaultAttribs,'ContainerWidth','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'ContainerHeight','String','','',false);
  AddDefaultAttribute(myDefaultAttribs,'Border','Boolean','True','',false);
  AddDefaultAttribute(myDefaultAttribs,'SpacingAround','Integer','0','',false);
  AddDefaultAttribute(myDefaultAttribs,'CompositeType','String','','',true);
  AddDefaultAttribute(myDefaultAttribs,'BgColor','Color','#FFFFFF','',false);
  AddDefaultAttribute(myDefaultAttribs,'InheritColor','Boolean','False','',false);
  AddDefaultAttribute(myDefaultAttribs,'SourceString','String','','',true);
  AddDefaultsToTable(MyNodeType,myDefaultAttribs);

  AddAttribOptions(MyNodeType,'Alignment',AlignmentOptions);
  AddAttribOptions(MyNodeType,'LabelPos',LabelPosOptions);
  {$IFndef JScript}
  RegisterClass(TXComposite);
  AddNodeFuncLookup(MyNodeType,@CreateCompositeWidget);
  {$else}
  AddNodeFuncLookup(MyNodeType,@CreateInterfaceObj,@CreateCompositeWidget);
  {$endif}


end.
