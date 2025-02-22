(*
    Copyright (c) 2020  Steve Wright

    This unit is part of the XIDE project.

    This project is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
 *)
unit StylesUtils;

{$ifdef Dll}
{$mode objfpc}{$H+}
{$endif}

interface
uses Classes, Sysutils, StringUtils, NodeUtils, EventsInterface
 {$ifndef JScript}
 , Dialogs//,XTree
 {$else}
 ,Math, contnrs, dateutils, rtlconsts, strutils, types, typinfo,
 XTree, XComboBox, InterfaceTypes
 {$endif}
 ;

// Priority has four levels
//   1) !important external rule
//   2) !important internal rule
//   3)  normal    external rule
//   4)  normal    internal rule
// where user style rules are generated in an external style sheet and author style rules are inline

//Expanding logical functions using Domorgan's law
//   or..........   A, B, C {}
//   and.........   ABC {}
//   nor.........  :not(A):not(B):not(C) {}
//   nand........  :not(A), :not(B), :not(C) {}

// <sup>.....</sup>  and  <sub>.....</sub> tags define superscript and subscript text - it is not done in style markup
 procedure InitStyleTreeDisplay;
 Procedure SetCSSNodeTypes;
 Procedure SetStyleOptions;
 procedure InitialiseStyleDesigner;
 Procedure SetCSSEditorStyles;
 procedure  UpdateOrCreateStyleSheet(StyleText,myStyleSheetTitle:string;pos:integer);


{$ifndef JScript}
{$else}

Var
  DropTarget:string;
  PreviousDropTarget:string;

//  SelectedStyleSheetNodePointer:TStyleTreeNode ;
  InlineCSSText,ExternalCSSText:string;
  StyleRootNode:TDataNode;

  // these procedures are called from the button event handlers
  Function PasteSelectedStyleResources(AllowDrop:Boolean;HoverOver:String;mute:Boolean;myNode:TObject):boolean;// this paste the selected style rescource to the style instance tree
  Procedure ValidateStyleTree;
  Procedure DeleteStyleNode;
  Procedure PopulateStyleEditor(editInsteadOfDropDown:boolean);
  Procedure GenerateStyleSheets;
  Procedure RefreshStyleSheet(InternalStyleText,ExternalStyleText:string);
  Function NotSystemClassName(TestString:String):Boolean;
  procedure  RemoveStyleSheet(myStyleSheetTitle:string);


  // these are forward declarations
  procedure InitialiseStyleResources;
  Function GetTypeFromString(instring:string):string;
  function GetOptionType(SNodeType:String):String;
  function GetQualifierValue(instring:String):string;
  Procedure UpdateNodeQualifierField;

{$endif}

var
  StylesNode:TDataNode;


implementation
uses XObjectInsp;

type

  TCSSTreeNodes = (All,Priority,Group,StyleResources,StyleSheet,StyleRule,StyleTargets,TargetGrouping,_And_,_Or_,Not_And_,Not_Or_,TargetTypes,WidgetType,Numeric,TXProgressBar,TXNumericSlider,TXNumberSpinner,Text,TXLabel,TXHyperlink,TXEditBox,TXMemo,TXTable,Selectors,TXButton,TXCheckBox,TXRadioBtns,TXComboBox,TXTree,TXDatePicker,TXColorPicker,TXMainMenu,TXMenuItem,LayoutWidgets,TXHBox,TXVBox,TXGroupBox,TXScrollBox,TXTabControl,TXTabSheet,TXForm,Identifier,ClassName,WidgetID,Relationship,ChildrenOf,DescendentsOf,SiblingsOf,State,Hover,Visited,Focused,StyleProperties,Widget,WidgetCorners,WidgetPadding,WidgetMargin,WidgetBorder,WidgetBackground,Font,FontFamily,FontSize,FontColor,FontBackgroundColor,FontWeight,FontStyle,TextDecor,Transformations,Rotate,Scale,Skew,States,Selectable,Cursor,Visibility,Effects,Transition,Filter,NotFound);

  TCSSTermList = Array of TStringList;

var
  StyleOptions:Array of TStringList;
  CSSTermList:TCSSTermList;

var
  AllowChildren            : Set of TCSSTreeNodes;
  StyleSheetNodes          : Set of TCSSTreeNodes;
  StyleRuleNodes           : Set of TCSSTreeNodes;
  ClassificationNodes      : Set of TCSSTreeNodes;
  TargetGroupingNodes      : Set of TCSSTreeNodes;
  RelationshipNodes        : Set of TCSSTreeNodes;
  StyletargetNodes         : Set of TCSSTreeNodes;
  StylePropertyNodes       : Set of TCSSTreeNodes;
  EditableTargetNodes      : Set of TCSSTreeNodes;
  TargetNodesWithOptions   : Set of TCSSTreeNodes;
  StyleRuleSelectors       : Set of TCSSTreeNodes;

{$ifndef JScript}

{$else}

Procedure Watch(instring:string);
begin
  setpropertyvalue('WatchBox','ItemValue',instring)
end;


  Function GetTypeFromString(instring:string):string;
  var Namestringstart:integer;
  begin
    //showmessage('GetTypeFromString <'+instring+'<');
    instring:=trim(instring);
    //showmessage('GetTypeFromString 2');
    if      (LeftStr(instring,5) = '(And)'   ) then GetTypeFromString:=LeftStr(instring,5)
    else if (LeftStr(instring,4) = '(Or)'    ) then GetTypeFromString:=LeftStr(instring,4)
    else if (LeftStr(instring,8) = 'Not(And)') then GetTypeFromString:=LeftStr(instring,8)
    else if (LeftStr(instring,7) = 'Not(Or)' ) then GetTypeFromString:=LeftStr(instring,7)
    else
    begin
      Namestringstart:=Pos('.',instring);
      if Namestringstart>0 then GetTypeFromString:=LeftStr(instring,Namestringstart-1)
      else GetTypeFromString:=instring ;
    end;
  end;

Function stripOutQualifierInfo(instring:string):string;
var outstr:string;
Qualifierstringstart,Qualifierstringend:integer;
begin
    outstr:=instring;
    if not(    (LeftStr(instring,5) = '(And)'   )
            or (LeftStr(instring,4) = '(Or)'    )
            or (LeftStr(instring,8) = 'Not(And)')
            or (LeftStr(instring,7) = 'Not(Or)' ) )
    then
    begin
      Qualifierstringstart:=Pos('(',instring);
      Qualifierstringend:=Pos(')',instring);
      if  ((Qualifierstringend-Qualifierstringstart)>0)
      then
      begin
        outstr:=LeftStr(instring,Qualifierstringstart-2)+Rightstr(instring,length(instring)-Qualifierstringend);
      end;
    end;
    stripOutQualifierInfo:=outstr;
 end;

Function GetName1(instring:string):string;
var outstr:string;
markerPos:integer;
begin
    outstr:=instring;
    markerPos:=RPos('.',instring);
    outstr:=Rightstr(instring,length(instring)-markerPos+1);
    //showmessage('GetName1 found '+outstr+' from '+instring);
    GetName1:=outstr;
 end;

function GetQualifierValue(instring:String):string;
var
  slist:TStringList;
  i:integer;
  markerPos1, markerPos2:integer;
  str:String;
begin
  slist:=TStringList.Create;
  slist.StrictDelimiter:=true;
  slist.Delimiter:='.';
  slist.LineBreak:='.';
  slist.SkipLastLineBreak:=true;
  slist.Text:=instring;

  // if the list has only 2 elements, then those are the type, and the name.
  // otherwise, the first element is the type, and the last is the name.
  // the 'middle' elements concatenate back together to give the qualifier string.

  // NEW: 2 elements should now be type.value.  (there is no name)

  str:='';
  if slist.count>2 then
  begin
    // old data...there was a 'name'. Remove it.
    str:=slist.Text;
    if str[length(str)]<>')' then
      slist.Delete(slist.count-1);
  end;
  if slist.count>=2 then
  begin
    slist.Delete(0);
    str:=slist.Text;
    //asm console.log('str=>'+str+'<'); end;
    if (length(str)>0) then
    begin
      // remove trailing delimiter (.)
      if str[length(str)]='.' then
        Delete(str,length(str),1);
      // remove parentheses around the qualifier value
      if str[1]='(' then
        Delete(str,1,1);
      if str[length(str)]=')' then
        Delete(str,length(str),1);
    end;

  end;

  result:=str;

  slist.Free;
end;


Procedure RefreshStyleSheet(InternalStyleText,ExternalStyleText:string);
var indirectExternalStyleText:string;
begin
    if (length(trim(InternalStyleText))>0) then
      UpdateOrCreateStyleSheet(InternalStyleText,'myInternalStyleSheet',-1)
    else
      RemoveStyleSheet('myInternalStyleSheet');
    if (length(trim(ExternalStyleText))>0)
    then
    begin
      RemoveStyleSheet('myExternalStyleSheet');
      asm
      var link = document.createElement('link');
      link.id='myExternalStyleSheet';
      link.rel = 'stylesheet';
      link.href = 'data:text/css;charset="UTF-8",' + encodeURIComponent(ExternalStyleText);
      document.getElementsByTagName('head')[0].appendChild(link);
      end;
    end
    else
      RemoveStyleSheet('myExternalStyleSheet');
end;

Procedure DeleteStyleNode;
var SelectedNodeId, ParentNodeId, NodeText, SNodeType:string;
begin
  SelectedNodeId:=XTree.TXTree(StylesNode).SelectedNodeId;
  ParentNodeId:=XTree.TXTree(StylesNode).GetParentOfNode(SelectedNodeId);
  NodeText:=XTree.TXTree(StylesNode).SelectedNodeText;
  SNodeType:=GetTypeFromString(NodeText);
  if (SNodeType='StyleTargets')
  or (SNodeType='StyleProperties')
  or (SNodeType='Priority')
  or (SNodeType='State')
  or (SNodeType='Group')
  or (SNodeType='StyleSheet')
  then
    showmessage('This node cannot be deleted')
  else
  begin
    // use DeleteSelectedNode to remove the visible tree node, and rebuild TreeData
    XTree.TXTree(StylesNode).DeleteSelectedNode;
    if ParentNodeId<>'' then
      XTree.TXTree(StylesNode).selectedNodeId:=ParentNodeId;

    PopulateStyleEditor(false);
  end;
end;


  Procedure ValidateStyleTree;
  begin
//https://www.w3.org/TR/CSS22/selector.html

    // check all logical groups and relationship nodes have at least one child.
    // check that classnames and ID values exist in the system.
    // transition can only work with targets of hover, selected, and visited.
    // check that there is at least one style target and one style property.
    // check that relationship qualifiers only have one child
    // :matches() cannot be nested   :-webkit-any(a, b)
//The ID selector.......................    #ID
//The Class Selector.....................   .ClassName
//type and class.........................   Div.Classname
//"or" group.............................   #ID , .Classname  , Div.Classname
//"and" group............................   a . b
//parentheses............................   : matches()     n.b. cannot be nested   :-webkit-any(a, b)
// logical inverse.......................   :not()  nb cannot be nested and only takes a simple selector

//  A . B = B . A
//  A + B = B + A

//  A(B + C)        =  A.B + A.C      // OR node child of an AND node
//  (A + B).(A + C) =  A + B . C      // OR node child of an AND node (term in common)
//  A(B.C)          =  A . B . C      // AND node child of an AND node
//  A + (B + C)     =  A + B + C      // OR node child of an OR node
//  A + (B.C)       =  A.B + A.C      // AND node child of an OR  node

//  A . 0       = 0
//  A . not(A)  = 0
//  A + not(A)  = 1
//  A + 1       = 1
//  A + 0       = A
//  A . 1       = A
//  A + A       = A
//  A . A       = A
//  A + (A.B)   = A
//  A(A + B)    = A
//  not(not(A)) = A

//  not(A+B)    = not(A) . not(B)
//  not(A.B)    = not(A) + not(B)

//a:hover MUST come after a:link and a:visited in the CSS definition in order to be effective! a:active MUST come after a:hover in the CSS definition in order to be effective! Pseudo-class names are not case-sensitive.


end;


  Function getqualifierdefault(SelectedStyleSheetNodeType:string):string;
  var i:integer;
      temp:string;
      tempstringlist:tstringlist;
  begin
     temp:='';
     for i:= 0 to length( StyleOptions)-1 do
     begin
       tempstringlist:=StyleOptions[i];
       if SelectedStyleSheetNodeType=tempstringlist[0]
       then temp:=tempstringlist[3];
     end;
     getqualifierdefault:=temp;
  end;

  Function getqualifierType(SelectedStyleSheetNodeType:string):string;
  var i:integer;
      temp:string;
      tempstringlist:tstringlist;
  begin
     temp:='';
     for i:= 0 to length( StyleOptions)-1 do
     begin
       tempstringlist:=StyleOptions[i];
       if SelectedStyleSheetNodeType=tempstringlist[0]
       then
       begin
         temp:=tempstringlist[2];
         //showmessage('found '+temp+' in SelectedStyleSheetNodeType');
       end;
     end;
      getqualifiertype:=temp;
  end;

function NotSystemClassName(TestString:String):Boolean;
begin
    if ((TestString<>'modal-background')
        and(TestString<>'modal-content')
        and(TestString<>'vbox')
        and(TestString<>'vboxNoStretch')
        and(TestString<>'vboxNoFlex')
        and(TestString<>'hbox')
        and(TestString<>'hboxNoStretch')
        and(TestString<>'AlignmentCentre')
        and(TestString<>'AlignmentRight')
        and(TestString<>'AlignmentLeft')
        and(TestString<>'AlignmentLeftContainer')
        and(TestString<>'AlignmentTop')
        and(TestString<>'AlignmentBottom')
        and(TestString<>'menu')
        and(TestString<>'menuItem')
        and(TestString<>'menuBar')
        and(TestString<>'highlight-border')
        and(TestString<>'normal-border')
        and(TestString<>'no-border')
        and(TestString<>'textAreaBorder')
        and(TestString<>'TabPage')
        and(TestString<>'TabButton')
        and(TestString<>'TabButtonDiv')
        and(TestString<>'hasChildren')
        and(TestString<>'noChildren')
        and(TestString<>'widgetinner'))
     then NotSystemClassName:=true
     else NotSystemClassName:=false;
end;


Function ResourceNodeSetIDOf(Snodename:string;quiet:boolean):TCSSTreeNodes;
var nodeID:TCssTreeNodes;
begin
  nodeID :=NotFound;
  if  Snodename<>'' then
  begin
    if Snodename ='Priority' then nodeID :=Priority
    else if Snodename ='State' then nodeID :=State
    else if Snodename ='Group' then nodeID :=Group
    else if Snodename ='StyleResources' then nodeID := StyleResources
    else if Snodename ='StyleSheet' then nodeID := StyleSheet
    else if Snodename ='StyleRule' then nodeID := StyleRule
    else if Snodename ='StyleTargets' then nodeID := StyleTargets
    else if Snodename ='TargetGrouping' then nodeID := TargetGrouping
    else if Snodename ='(And)' then nodeID := _And_
    else if Snodename ='(Or)' then nodeID := _Or_
    else if Snodename ='Not(And)' then nodeID := Not_And_
    else if Snodename ='Not(Or)' then nodeID := Not_Or_
    else if Snodename ='TargetTypes' then nodeID := TargetTypes
    else if Snodename ='WidgetType' then nodeID := WidgetType
    else if Snodename ='Numeric' then nodeID := Numeric
    else if Snodename ='TXProgressBar' then nodeID := TXProgressBar
    else if Snodename ='TXNumericSlider' then nodeID := TXNumericSlider
    else if Snodename ='TXNumberSpinner' then nodeID := TXNumberSpinner
    else if Snodename ='Text' then nodeID := Text
    else if Snodename ='TXLabel' then nodeID := TXLabel
    else if Snodename ='TXHyperlink' then nodeID := TXHyperlink
    else if Snodename ='TXEditBox' then nodeID := TXEditBox
    else if Snodename ='TXMemo' then nodeID := TXMemo
    else if Snodename ='TXTable' then nodeID := TXTable
    else if Snodename ='Selectors' then nodeID := Selectors
    else if Snodename ='TXButton' then nodeID := TXButton
    else if Snodename ='All' then nodeID := All
    else if Snodename ='TXCheckBox' then nodeID := TXCheckBox
    else if Snodename ='TXRadioBtns' then nodeID := TXRadioBtns
    else if Snodename ='TXComboBox' then nodeID := TXComboBox
    else if Snodename ='TXTree' then nodeID := TXTree
    else if Snodename ='TXDatePicker' then nodeID := TXDatePicker
    else if Snodename ='TXColorPicker' then nodeID := TXColorPicker
    else if Snodename ='TXMainMenu' then nodeID := TXMainMenu
    else if Snodename ='TXMenuItem' then nodeID := TXMenuItem
    else if Snodename ='LayoutWidgets' then nodeID := LayoutWidgets
    else if Snodename ='TXHBox' then nodeID := TXHBox
    else if Snodename ='TXVBox' then nodeID := TXVBox
    else if Snodename ='TXGroupBox' then nodeID := TXGroupBox
    else if Snodename ='TXScrollBox' then nodeID := TXScrollBox
    else if Snodename ='TXTabControl' then nodeID := TXTabControl
    else if Snodename ='TXTabSheet' then nodeID := TXTabSheet
    else if Snodename ='TXForm' then nodeID := TXForm
    else if Snodename ='Identifier' then nodeID := Identifier
    else if Snodename ='ClassName' then nodeID := ClassName
    else if Snodename ='WidgetID' then nodeID := WidgetID
    else if Snodename ='Relationship' then nodeID := Relationship
    else if Snodename ='ChildrenOf' then nodeID := ChildrenOf
    else if Snodename ='DescendentsOf' then nodeID := DescendentsOf
    else if Snodename ='SiblingsOf' then nodeID := SiblingsOf
    else if Snodename ='State' then nodeID := State
    else if Snodename ='Hover' then nodeID := Hover
    else if Snodename ='Visited' then nodeID := Visited
    else if Snodename ='Focused' then nodeID := Focused
    else if Snodename ='StyleProperties' then nodeID := StyleProperties
    else if Snodename ='Widget' then nodeID := Widget
    else if Snodename ='WidgetCorners' then nodeID := WidgetCorners
    else if Snodename ='WidgetPadding' then nodeID := WidgetPadding
    else if Snodename ='WidgetMargin' then nodeID := WidgetMargin
    else if Snodename ='WidgetBorder' then nodeID := WidgetBorder
    else if Snodename ='WidgetBackground' then nodeID := WidgetBackground
    else if Snodename ='Font' then nodeID := Font
    else if Snodename ='FontFamily' then nodeID := FontFamily
    else if Snodename ='FontSize' then nodeID := FontSize
    else if Snodename ='FontColor' then nodeID := FontColor
    else if Snodename ='FontBackgroundColor' then nodeID := FontBackgroundColor
    else if Snodename ='FontWeight' then nodeID := FontWeight
    else if Snodename ='FontStyle' then nodeID := FontStyle
    else if Snodename ='TextDecor' then nodeID := TextDecor
    else if Snodename ='Transformations' then nodeID := Transformations
    else if Snodename ='Rotate' then nodeID := Rotate
    else if Snodename ='Scale' then nodeID := Scale
    else if Snodename ='Skew' then nodeID := Skew
    else if Snodename ='States' then nodeID := States
    else if Snodename ='Selectable' then nodeID := Selectable
    else if Snodename ='Cursor' then nodeID := Cursor
    else if Snodename ='Visibility' then nodeID := Visibility
    else if Snodename ='Effects' then nodeID := Effects
    else if Snodename ='Transition' then nodeID := Transition
    else if Snodename ='Filter' then nodeID := Filter
    else if (quiet = false) then showmessage('node name >'+Snodename+'< not found');
  end;
  ResourceNodeSetIDOf:= nodeID;
end;


  Procedure LocalShowMessage( var DropAllowed:boolean;mute:boolean;mymessage:string);
  begin
    DropAllowed :=false;
    if mute=false
    then ShowMessage(mymessage)
    else watch(mymessage);
  end;



//Function PasteSelectedStyleResources(AllowDrop:Boolean;HoverOver:String;mute:Boolean;ValueObject:TNodeEventValue):boolean;
Function PasteSelectedStyleResources(AllowDrop:Boolean;HoverOver:String;mute:Boolean;myNode:TObject):boolean;
  //if mute=true then nodes are not added or alerts sent to the user it just tests if it is ok to drop on this node
  var
    SelectedStyleSheetNodeText,SelectedStyleSheetNodeId:string;
    SNodeType,RNodeType:string;
      Qualifier,RuleNodeId,NewNodeId,ANodeId:String;
      DropIsAllowed:boolean;
  begin
      DropIsAllowed:=true;
      RNodeType:=GetTypeFromString(trim(getPropertyValue('StyleResources','SelectedNodeText')));
      //if AllowDrop then showmessage('RNodeType='+RNodeType);
      asm
        SelectedStyleSheetNodeId=myNode.id;
        //if (AllowDrop==true) {alert('SelectedStyleSheetNodeId='+SelectedStyleSheetNodeId);}
        var ob=document.getElementById(myNode.id);
        if (ob!=null) {
          // make sure the nodeid is the 'DETAILS' element
          if (ob.tagName=='SUMMARY') {
            var summ=ob;
            SelectedStyleSheetNodeId=summ.parentNode.id;}
          else summ=ob.getElementsByTagName('SUMMARY')[0];
          }
          else {alert('SelectedStyleSheetNodeId '+SelectedStyleSheetNodeId+' not found'); }

        SelectedStyleSheetNodeText=summ.innerHTML;
      end;

      SNodeType:=GetTypeFromString(SelectedStyleSheetNodeText);
      SelectedStyleSheetNodeText:=stripOutQualifierInfo(SelectedStyleSheetNodeText);
      //if AllowDrop then showmessage('>'+RNodeType +'<    >'+SNodeType+'<');
      //EditAttributeValue('WatchBox','','ItemValue','>'+RNodeType +'<    >'+SNodeType+'<');
      //result:=false;
      //EXIT;

      if not(ResourceNodeSetIDOf(SNodeType,false )  in AllowChildren ) then
        LocalShowMessage( DropIsAllowed,mute,'This style tree node cannot have children')
      else if SNodeType ='Priority' then
        LocalShowMessage( DropIsAllowed,mute,'Priority options can be edited, but cannot have children')
      else if SNodeType ='' then
        LocalShowMessage( DropIsAllowed,mute,'Select a Style resource before pasting')
      else if SNodeType ='' then
        LocalShowMessage( DropIsAllowed,mute,'Select a StyleSheet node before pasting')
      else if (SNodeType ='StyleProperties')
      then
      begin
          if  (ResourceNodeSetIDOf(RNodeType,false) in StylePropertyNodes )
          then
          begin
             if (AllowDrop =true) then
               //!! assumes the selected node is the parent
               NewNodeId:=XTree.TXTree(StylesNode).InsertNewChildNode(RNodeType+'.'+'('+getqualifierdefault(RNodeType)+')');
          end
          else LocalShowMessage( DropIsAllowed,mute,'Only Style property types can be added to a set of style property nodes. (selected type is '+RNodeType+')');
      end
      else
      begin
        if (SNodeType ='StyleRule') then
          LocalShowMessage( DropIsAllowed,mute,'You can only add the the properties and targets of a style rule - not to the style rule itself')
        else if (SNodeType ='StyleSheet')
        then
        begin
          if (RNodeType <>'StyleRule') then
            LocalShowMessage( DropIsAllowed,mute,'Only a StyleRule can be added to the StyleSheet node')
          else
          begin
            Qualifier:=' ';//prompt( 'Please name this Style Rule :-', DateTimeToStr(Now));
            if (AllowDrop =true) then
            begin
              //!! assumes root node has been selected
              NewNodeId:=XTree.TXTree(StylesNode).InsertNewChildNode('StyleRule');
              //now need to select this node
              //TXTree(StylesNode).SelectedNodeId:=NewNodeId+'Summary';
              XTree.TXTree(StylesNode).SelectedNodeId:=NewNodeId;
              // and insert the standard set of child nodes
              XTree.TXTree(StylesNode).InsertNewChildNode('StyleTargets');
              XTree.TXTree(StylesNode).InsertNewChildNode('StyleProperties');
              ANodeId:=XTree.TXTree(StylesNode).InsertNewChildNode('Priority.('+getqualifierdefault('Priority')+')');
              XTree.TXTree(StylesNode).InsertNewChildNode('State.('+getqualifierdefault('State')+')');
              XTree.TXTree(StylesNode).InsertNewChildNode('Group.('+getqualifierdefault('Group')+')');
            end;
          end;
        end
        else
        begin
          //showmessage('SelectedStyleSheetNodeId='+SelectedStyleSheetNodeId);
          //showmessage('num children is '+inttostr(length(XTree.TXTree(StylesNode).GetChildNodes(SelectedStyleSheetNodeId))));
        if (SNodeType ='StyleTargets')
        and (length(XTree.TXTree(StylesNode).GetChildNodes(SelectedStyleSheetNodeId)) > 0)
        then
          LocalShowMessage( DropIsAllowed,mute,'To add more than one target use a Logical group node (e.g. (And)) then add them as children to it')
        else
        begin
           if ( (ResourceNodeSetIDOf(RNodeType,false) in TargetGroupingNodes)
           or (ResourceNodeSetIDOf(RNodeType,false) in StyletargetNodes)
           or (ResourceNodeSetIDOf(RNodeType,false) in RelationshipNodes))
           then
           begin
             if (ResourceNodeSetIDOf(RNodeType,false) in EditableTargetNodes)
             or (ResourceNodeSetIDOf(RNodeType,false) in TargetNodesWithOptions)
             then
             begin
               if (AllowDrop =true) then
               begin
                 NewNodeId:=XTree.TXTree(StylesNode).InsertNewChildNode(RNodeType+'.'+'('+getqualifierdefault(RNodeType)+')');
               end;
             end
             else
             begin
               if (AllowDrop =true) then
               begin
                 NewNodeId:=XTree.TXTree(StylesNode).InsertNewChildNode(RNodeType);
               end;
             end;
            end
           else LocalShowMessage( DropIsAllowed,mute,'Only Style target types (or logical group or relationship nodes) can be added to a style targets node');
        end;
        end;
      end;

      if (AllowDrop=true)
      and (NewNodeId<>'') then
      begin
        XTree.TXTree(StylesNode).SelectedNodeId:=NewNodeId;
        PopulateStyleEditor(false);
      end;

      DropTarget:=SNodeType;
      if DropIsAllowed then watch(' ');
      result:=DropIsAllowed;
  end;

function GetOptionType(SNodeType:String):String;
var
  i:integer;
  tempstringlist:tstringlist;
begin
  result:='';
  for i:= 0 to length( StyleOptions)-1 do
  begin
    tempstringlist:=StyleOptions[i];
    if SNodeType=tempstringlist[0]
    then
    begin
       result := tempstringlist[2];
    end;
  end;
end;

  Procedure UpdateNodeQualifierField;
  var  SelectedStyleSheetNodeText,SelectedStyleSheetNodeType,CurrentOptionValue,CurrentOptionType:string;
       itemIndex,i,j:integer;
       nodeid,s:String;
       MyCombo:TDataNode;
  begin
    // get the node type and text string
    SelectedStyleSheetNodeText:=trim(getPropertyValue('StyleSheet','SelectedNodeText'));
    SelectedStyleSheetNodeType:=GetTypeFromString(SelectedStyleSheetNodeText);
    CurrentOptionType:=GetOptionType(SelectedStyleSheetNodeType);
    if SelectedStyleSheetNodeType<>'' then
    begin
      s:=GetPropertyValue('StyleNodeQualifier','IsVisible');
      if getPropertyValue('StyleNodeQualifier','IsVisible') = 'True'
      then
      begin
         MyCombo:=FindDataNodeById(SystemNodeTree,'StyleNodeQualifier','',true);
         CurrentOptionValue := XComboBox.TXComboBox(MyCombo).ItemValue;       //getPropertyValue('StyleNodeQualifier','ItemValue');
      end
      else
      begin
        CurrentOptionValue := trim(getPropertyValue('QualifierEditBox','ItemValue'));
        if ((SelectedStyleSheetNodeType='ClassName') or (SelectedStyleSheetNodeType='WidgetID')) then
        begin
          while ((FoundString(CurrentOptionValue,' ')>0)
             or (FoundString(CurrentOptionValue,'.')>0)) do
             begin
               showmessage('Please enter '+SelectedStyleSheetNodeType+' without spaces or "." characters');
               CurrentOptionValue:= XIDEPrompt('Enter '+SelectedStyleSheetNodeType,CurrentOptionValue);
               setPropertyValue('QualifierEditBox','ItemValue',CurrentOptionValue);
             end;
        end;
      end;
      if  (ResourceNodeSetIDOf(SelectedStyleSheetNodeType,false) in StylePropertyNodes ) then
        SelectedStyleSheetNodeText := SelectedStyleSheetNodeType+'.('+CurrentOptionValue+')'
      else
        SelectedStyleSheetNodeText := SelectedStyleSheetNodeType+'.('+CurrentOptionValue+')';
      nodeid:=XTree.TXTree(StylesNode).SelectedNodeId;
      //showmessage('nodeid='+nodeid+' new text = '+ SelectedStyleSheetNodeText);
      XTree.TXTree(StylesNode).SetNodeText(nodeid,SelectedStyleSheetNodeText);
      XTree.TXTree(StylesNode).SelectedNodeId:=nodeid;
    end;
    setPropertyValue('StyleTreeApplyBtn','IsVisible','false');
  end;

Procedure PopulateStyleEditor(editInsteadOfDropDown:boolean);
var  SelectedStyleSheetNodeText,SelectedStyleSheetNodeType,CurrentOptionValue:string;
     nodeid:String;
     CurrentOptionIndex,i,j:integer;
     optionstring:string;
     tempstringlist:tstringlist;
begin
  // get the node type and text string
  SelectedStyleSheetNodeText:=trim(getPropertyValue('StyleSheet','SelectedNodeText'));
  nodeid:=XTree.TXTree(StylesNode).SelectedNodeId;
  //showmessage('PopulateStyleEditor '+nodeid);
  if (nodeid<>'') then
  begin
  SelectedStyleSheetNodeType:=GetTypeFromString(SelectedStyleSheetNodeText);
  CurrentOptionValue:=GetQualifierValue(SelectedStyleSheetNodeText);
  //showmessage('PopulateStyleEditor '+nodeid+' type='+SelectedStyleSheetNodeType+' qual='+CurrentOptionValue);
  if ((ResourceNodeSetIDOf(SelectedStyleSheetNodeType,false) in StylePropertyNodes)// to edit colors, line widths etc. using the drop down options
      or (ResourceNodeSetIDOf(SelectedStyleSheetNodeType,false) in  TargetNodesWithOptions)
      or (ResourceNodeSetIDOf(SelectedStyleSheetNodeType,false)in StyleRuleSelectors))
  and (editInsteadOfDropDown = false)
  then
  begin
     CurrentOptionIndex:=0;
     optionstring:='';
     // get option list for this resource type and set current option values" e.g.....["Option 1","Option 2","Option 3"]
     for i:= 0 to length( StyleOptions)-1 do
     begin
       tempstringlist:=StyleOptions[i];
       if trim(SelectedStyleSheetNodeType)=trim(tempstringlist[0])
       then
       begin
         setPropertyValue('StyleNodeQualifier','Hint',tempstringlist[1]);
         for j:=3 to tempstringlist.count-1
         do
         begin;
            //showmessage('>'+tempstringlist[j]+'<>'+CurrentOptionValue+'<');
            if trim(tempstringlist[j]) = trim(CurrentOptionValue )
            then  CurrentOptionIndex:=j-3;
         end;
         optionstring:='["'+tempstringlist[3]+'"';
         for j:=4 to tempstringlist.count-1 do
         begin
           optionstring:=optionstring+',"'+tempstringlist[j]+'"';
         end;
         optionstring:=optionstring+']';
  //       setPropertyValue('OptionEditorLabel','IsVisible','true');
       end;
     end;
      // make StyleNodeQualifier drop down list visible and hide the edit box
     setPropertyValue('QualifierEditBox','IsVisible','false');
     setPropertyValue('StyleNodeQualifier','IsVisible','true');
     setPropertyValue('StyleNodeQualifier','OptionList',optionstring);
     setPropertyValue('StyleNodeQualifier','ItemIndex',inttostr(CurrentOptionIndex));
     setPropertyValue('StyleTreeEditBtn','IsVisible','true');
     setPropertyValue('StyleTreeApplyBtn','IsVisible','false');
  end
  else
  // Editing.......................
  if ((ResourceNodeSetIDOf(SelectedStyleSheetNodeType,false) in StylePropertyNodes)and (editInsteadOfDropDown = true)) // to manually edit the css string instead of using the drop downs
  or (ResourceNodeSetIDOf(SelectedStyleSheetNodeType,false) in EditableTargetNodes)// to edit class and widget ids
  or (ResourceNodeSetIDOf(SelectedStyleSheetNodeType,false) in StyleRuleNodes)// to edit the style rule name
  then
  begin
     // make StyleNodeQualifier drop down list not visible and show the edit box
    //showmessage('CurrentOptionValue='+CurrentOptionValue);
     setPropertyValue('QualifierEditBox','IsVisible','true');
     setPropertyValue('StyleNodeQualifier','IsVisible','false');
     setPropertyValue('QualifierEditBox','ItemValue',CurrentOptionValue);
     setPropertyValue('StyleTreeEditBtn','IsVisible','true');
     setPropertyValue('StyleTreeApplyBtn','IsVisible','true');
  end
  else
  begin
     setPropertyValue('StyleTreeEditBtn','IsVisible','false');
     setPropertyValue('StyleTreeApplyBtn','IsVisible','true');
     setPropertyValue('QualifierEditBox','ItemValue',CurrentOptionValue);
     setPropertyValue('QualifierEditBox','IsVisible','true');
     setPropertyValue('StyleNodeQualifier','IsVisible','false');
  end;
  end
  else
  begin
     // there is no tree node selected
    setPropertyValue('StyleNodeQualifier','OptionList','[]');
    setPropertyValue('StyleTreeEditBtn','IsVisible','false');
    setPropertyValue('StyleTreeApplyBtn','IsVisible','false');
    setPropertyValue('QualifierEditBox','ItemValue',' ');
    setPropertyValue('QualifierEditBox','IsVisible','false');
    setPropertyValue('StyleNodeQualifier','IsVisible','false');
  end;
end;

//  A(B + C)        =  A.B + A.C      // OR node child of an AND node
//  (A + B).(A + C) =  A + B . C      // OR node child of an AND node (term in common)
//  A(B.C)          =  A . B . C      // AND node child of an AND node
//  A + (B + C)     =  A + B + C      // OR node child of an OR node
//  A + (B.C)       =  A.B + A.C      // AND node child of an OR  node
//Expanding logical functions using Domorgan's law
//   or..........   A, B, C {}
//   and.........   ABC {}
//   nor.........  :not(A):not(B):not(C) {}
//   nand........  :not(A), :not(B), :not(C) {}

Function GetStyleProperties(PropertiesNodeId,StylePriority:String):string;
var i,numchildren:integer;
    CurrentPropertyString,CSSText:string;
    props:TStringArray;
Begin
  CSSText:= '';
  // style properties are the children of this node.
  props:=XTree.TXTree(StylesNode).GetChildNodes(PropertiesNodeId);
  asm
    for (var i=0; i<props.length; i++) {
      var propid=props[i];
      //console.log('propid='+propid);
      var ob=document.getElementById(propid+'Summary');
      if (ob!=null) {
        var txt=ob.innerHTML;   }
      //console.log('txt='+txt);
      var qt=pas.StylesUtils.GetOptionType(pas.StylesUtils.GetTypeFromString(txt));
      var qv=pas.StylesUtils.GetQualifierValue(txt);
      //console.log('qt='+qt+' qv='+qv);
      CSSText=CSSText + qt + qv ;

      if ((StylePriority == '1;') || (StylePriority=='2;'))
      {
        // delete trailing spaces
        CSSText=CSSText.trim();
        //remove the final ; character
        if (CSSText[CSSText.length-1]==';') {CSSText=CSSText.substring(0,CSSText.length-1);}
        // make it important
        CSSText=CSSText+' !important;'
       }

      }
  end;
  //asm console.log('props='+CSSText); end;
  result:=CSSText;
end;

Function GetStylePriority(CurrentNode:String):string;
  Begin
    GetStylePriority:=' Priority ';
  end;

function MakeATermStringList:TStringList;
var
  newStringList:TStringList;
begin
  newStringList := TStringList.Create;
  newStringList.Delimiter := '.';
  newStringList.StrictDelimiter := True;
  newStringList.duplicates:=dupAccept;
  newStringList.Sorted := False;
  newStringList.SkipLastLineBreak:=true;
  newStringList.Linebreak:='';
  result:=newStringList;
end;

  Function  mergeTermLists2(SNodeType:String;list1,list2:TCSSTermList;invert:boolean):TCSSTermList;
  var i,j,k,n,m:integer;
      newOutlist:TCSSTermList;
  begin
    if ( (LeftStr(SNodeType,5) = '(And)') and (invert=false))
    or ((LeftStr(SNodeType,7) = 'Not(Or)' )and (invert=true))
     // (a+b).(c+d)  =>  a.c + a.d  + b.c + b.d
    then
    begin
      Setlength(newOutlist,0);
      For i:= 0 to length(list1)-1 do
      begin
        for j:= 0 to length(list2)-1 do
        begin
          k:=length(newOutlist);
          Setlength(newOutlist, k+1);
          newOutlist[ k] := MakeATermStringList;
          for n:= 0 to list1[i].count-1 do newOutlist[ k].add(list1[i][n]);
          for m:= 0 to list2[j].count-1 do newOutlist[ k].add(list2[j][m]);
        end;
      end;
    end
    else
    if ((LeftStr(SNodeType,4) = '(Or)')  and (invert=false))
    or  ((LeftStr(SNodeType,8) = 'Not(And)')and (invert=true))
    then
    begin
     // (a+b)+(c+d)  =>  a + b + c + d
     Setlength(newOutlist,0);
     For i:= 0 to length(list1)-1 do
      begin
        k:=length(newOutlist);
        Setlength(newOutlist, k+1);
        newOutlist[ k] := MakeATermStringList;
        newOutlist[ k].assign(list1[i]);
      end;
      for j:= 0 to length(list2)-1 do
      begin
        k:=length(newOutlist);
        Setlength(newOutlist, k+1);
        newOutlist[ k] := MakeATermStringList;
        newOutlist[ k].assign(list2[j]);
      end;
    end;
    mergeTermLists2:=newOutlist;
  end;

  Function FlattenSubTree2(CurrentNodeId:String;invert:boolean):TCSSTermList;
  var i, j, numchildren:integer;
      nodestring, SNodeType, SQualifier, str :string;
      childnodes:TStringArray;
      OutList1,OutList2:TCSSTermList;
  begin
    //showmessage('FlattenSubTree2 '+CurrentNodeId);
    childnodes:=XTree.TXTree(StylesNode).GetChildNodes(CurrentNodeId);
    SNodeType:=GetTypeFromString(XTree.TXTree(StylesNode).TextOfNode(CurrentNodeId));
    numchildren:= length(childnodes);
    if numchildren = 0 then
    begin
      //showmessage('targets '+CurrentNodeId+' no children');
      Setlength(OutList1,1);
      OutList1[0] := MakeATermStringList;
      //showmessage('type='+SNodeType);
      SQualifier:=GetQualifierValue(XTree.TXTree(StylesNode).TextOfNode(CurrentNodeId));
      //showmessage('qual='+SQualifier);
      if (SNodeType='ClassName')or(SNodeType='WidgetID')
      then nodestring :=SQualifier
      else nodestring :=SNodeType;
      nodestring:='.'+nodestring;
      if invert = false
      then OutList1[0].Add(nodestring )
      else OutList1[0].Add(':not('+nodestring +')')
    end
    else
    begin
     // showmessage('haschildren.  SNodeType='+SNodeType);
      if ((LeftStr(SNodeType,8) = 'Not(And)'))
      or ((LeftStr(SNodeType,7) = 'Not(Or)' ))
      then
      begin
        if invert=true then invert:=false else invert:=true;
      end;
      OutList1:= FlattenSubTree2(childnodes[0],invert);
      if numchildren >1 then
      for i := 1 to  numchildren - 1
      do
      begin
        OutList2:= FlattenSubTree2(childnodes[i],invert);
        OutList1:=mergeTermLists2(SNodeType,OutList1,OutList2,invert);
//        str:='';
//        for j:=0 to length(OutList1)-1 do str:=str+OutList1[j].Text;
//        showmessage('i='+inttostr(i)+' OutList1='+str+'<');
      end;
    end;
    result:=outlist1;
  end;

  Function AdjustTargetString(TargetString:string):string;
  var teststring:string;
  begin
    teststring:=stringreplace(TargetString,'"','',[rfReplaceAll]);  // delete double quotes
    teststring:=rightstr(teststring,length(teststring)-1); // delete leading full stop
    if TargetString = '"All"' then   TargetString:= '*';
    AdjustTargetString:=TargetString;
  end;

Function GetStyleTargets(CurrentNode:String):string;
var CSSTermList:TCSSTermList;
    CSSText,tmp:string;
    i,numterms:integer;
Begin
  setlength(CSSTermList,0);
  CSSTermList:=FlattenSubTree2(CurrentNode,false);

  numterms:=length(CSSTermList) ;
  if numterms >0 then
  begin
    tmp:=CSSTermList[0].Text;
    //asm console.log('CSSTermList[0] '+tmp); end;
    CSSText:=  AdjustTargetString(CSSTermList[0].Text);
    for i:=1 to numterms-1 do
    begin
      tmp:=CSSTermList[i].Text;
      //asm console.log('CSSTermList[i] '+tmp); end;
      CSSText:=CSSText+' , '+AdjustTargetString(CSSTermList[i].Text);
    end;
  end;
  result:=stringreplace(CSSText,'"','',[rfReplaceAll]);
end;


Function GenerateStyleRule(RuleNodeId:String):string;
var i:integer;
    Stylepriority,StyleState,StyleTargets,StyleProperties,StyleGroup,TargetString:String;
    ruleNodes:TStringArray;
    str:string;
    TopNode:TDataNode;
begin


  setlength(ruleNodes,0);
  ruleNodes:=XTree.TXTree(StylesNode).GetChildNodes(RuleNodeId);

  StyleTargets:=GetStyleTargets(ruleNodes[0]);
  Stylepriority:=trim(GetQualifierValue(XTree.TXTree(StylesNode).TextOfNode(RuleNodes[2])));
  StyleProperties:=GetStyleProperties(ruleNodes[1],Stylepriority);
  StyleState:=trim(GetQualifierValue(XTree.TXTree(StylesNode).TextOfNode(RuleNodes[3])));
  StyleGroup:=trim(GetQualifierValue(XTree.TXTree(StylesNode).TextOfNode(RuleNodes[4])));


  StyleTargets:=' :-webkit-any('+StyleTargets+').UI';

  if styleState<>'Normal;' then StyleTargets:=StyleTargets+StyleState;
  if StyleGroup<>'Self;'
  then
  begin
    if StyleGroup ='SiblingsOf;' then StyleTargets:=StyleTargets+'~*' ;
    if StyleGroup ='ChildrenOf;' then StyleTargets:=StyleTargets+'>*' ;
    if StyleGroup ='DescendentsOf;' then StyleTargets:=StyleTargets+ ' *' ;
  end;

  TargetString:='#UIRootContents'+StyleTargets;
  //!!!! other forms.....!!!!
  // now look for other XForms
  TopNode:=FindDataNodeById(SystemNodeTree,SystemRootName,'',true);
  for i:=0 to length(TopNode.ChildNodes)-1 do
  begin
    if (TopNode.ChildNodes[i].NodeType='TXForm')
    and (TopNode.NodeName<>'CodeEditForm')
    and (TopNode.NodeName<>'PasteDialog')
    and (TopNode.ChildNodes[i].IsDynamic)
    and (TopNode.ChildNodes[i].NameSpace='')
    then
    begin
      TargetString:=TargetString + ' ,#'+TopNode.ChildNodes[i].NodeName+StyleTargets;
    end;
  end;

  if (Stylepriority = '1;') or (Stylepriority='3;')
  then
    ExternalCSSText:=ExternalCSSText+TargetString+' {'+ StyleProperties +'}'
  else
    InlineCSSText:=InlineCSSText+TargetString+' {'+ StyleProperties +'} ';


end;

  Procedure GenerateStyleSheets;
  var CSSText,CurrentRule,PriorityOneStyleRules,PriorityTwoStyleRules,PriorityThreeStyleRules,PriorityFourStyleRules:string;
      i, numchildren:integer;
      rules:TStringArray;
  begin
    //!!!! change this use JSON parser on the underlying treedata????

    PriorityOneStyleRules:='';
    PriorityTwoStyleRules:='';
    PriorityThreeStyleRules:='';
    PriorityFourStyleRules:='';
    InlineCSSText:= '';
    ExternalCSSText:= '';

    setlength(rules,0);
   // rules:=XTree.TXTree(StylesNode).GetChildNodes('StyleSheetContentsScrollNode');
    rules:=XTree.TXTree(StylesNode).GetChildNodes('StyleSheetNode0');

    numchildren:= length(rules);
    if numchildren >0 then
    for i := 0 to  numchildren - 1
    do
    begin
      GenerateStyleRule(rules[i]);
      //GenerateStyleRule generates the text in  InlineCSSText and  ExternalCSSText
    end;

    setPropertyValue('GeneratedStyleSheetText','ItemValue',InlineCSSText + LineEnding + ExternalCSSText);
 //   setPropertyValue('ExternalCssText','ItemValue',ExternalCSSText);
    RefreshStyleSheet(InlineCSSText,ExternalCSSText);
end;

procedure InitialiseStyleResources;
begin
  EditAttributeValue('StyleResources','','TreeData','["StyleResources","StyleRule",' +
                    '["StyleTargets",["TargetTypes",' +
                    '["WidgetType",["All"],["Numeric","TXProgressBar","TXNumericSlider","TXNumberSpinner"],' +
                    '["Text","TXLabel","TXHyperlink","TXEditBox","TXMemo","TXTable"],' +
                    '["Selectors","TXButton","TXCheckBox","TXRadioBtns","TXComboBox","TXTree","TXDatePicker","TXColorPicker","TXMainMenu","TXMenuItem"],' +
                    '["LayoutWidgets","TXHBox","TXVBox","TXGroupBox","TXScrollBox","TXTabControl","TXTabSheet","TXForm"]],' +
                    '["Identifier","ClassName","WidgetID"]],' +
                    '["TargetGrouping","(And)","(Or)","Not(And)","Not(Or)"]],' +
                    '["StyleProperties",["Widget","WidgetPadding","WidgetBorder","WidgetCorners","WidgetMargin","WidgetBackground"],' +
                    '["Font","FontFamily","FontSize","FontColor","FontBackgroundColor","FontWeight","FontStyle","TextDecor"],' +
                    '["Transformations","Rotate","Scale","Skew"],["States","Selectable","Cursor","Visibility"],' +
                    '["Effects","Transition","Filter"]]]');
end;

{$endif}

procedure  UpdateOrCreateStyleSheet(StyleText,myStyleSheetTitle:string;pos:integer);
//https://www.w3.org/wiki/Dynamic_style_-_manipulating_CSS_with_JavaScript
begin
  {$ifdef JScript}
  asm
    var foundCSSSheet;
    foundCSSSheet = false;
    var x = document.getElementById(myStyleSheetTitle);
    if (x!=null) {x.innerHTML = StyleText;}
    else
    {
      var myStyleSheet = document.createElement('style');
      myStyleSheet.id=myStyleSheetTitle;
      myStyleSheet.innerHTML = StyleText;
      if (pos<0) {
        document.head.appendChild(myStyleSheet); }
      else {
        document.head.prepend(myStyleSheet); }
    }
  end;
  {$endif}
end;
 procedure  RemoveStyleSheet(myStyleSheetTitle:string);
 begin
   {$ifdef JScript}
   asm
     var x = document.getElementById(myStyleSheetTitle);
     if (x!=null) {x.parentNode.removeChild(x);}
   end;
   {$endif}
 end;

Function GetIdsOrClassNames(GetIDs:Boolean):TStringList;
var
   list:TStringList;
   TopNode:TdataNode;
   i:integer;
   function DrillDown(StartNode:TdataNode; GetIDs:Boolean):TStringList;
   var
     theList,cList:TStringList;
     classes,tmp:String;
     c:integer;
   begin
     theList:=TStringList.Create;
     theList.Sorted:=true;
     theList.Duplicates := dupIgnore;
     if StartNode.IsDynamic then
       if GetIds then
       begin
         theList.Add(StartNode.NameSpace+StartNode.nodeName);
       end
       else
       begin
         if StartNode.HasAttribute('HTMLClasses') then
         begin
           classes:=StartNode.GetAttribute('HTMLClasses',false).AttribValue;
           if classes<>'' then
           begin
             cList:=TStringList.Create;
             cList.StrictDelimiter:=true;
             cList.Delimiter:=' ';
             cList.LineBreak:=' ';
             cList.Text:=classes;
             cList.Sorted := True;
             cList.Duplicates := dupIgnore;
             for c := clist.count - 1 downto 0 do
             begin
               if Trim(clist[c]) = '' then
                 clist.Delete(c);
             end;
             theList.AddStrings(cList);
             cList.Free;
             //tmp:=theList.Text;
             //tmp:=cList.Text;
           end;
         end;
       end;

     for c:=0 to length(StartNode.childnodes)-1 do
     begin
       theList.AddStrings(DrillDown(StartNode.childnodes[c],GetIDs));
     end;
     result:=theList;
   end;
begin
  ClearInspectors;
  list := DrillDown( MainFormProjectRoot, GetIDs);
  list.StrictDelimiter:=true;
  list.Delimiter:='|';
  list.LineBreak:='|';
  list.SkipLastLineBreak:=true;
  // now look for other XForms
  TopNode:=FindDataNodeById(SystemNodeTree,SystemRootName,'',true);
  for i:=0 to length(TopNode.ChildNodes)-1 do
  begin
    if (TopNode.ChildNodes[i].NodeType='TXForm')
    and (TopNode.NodeName<>'CodeEditForm')
    and (TopNode.NodeName<>'PasteDialog')
    and (TopNode.ChildNodes[i].IsDynamic)
    then
    begin
      list.AddStrings(DrillDown(TopNode.ChildNodes[i],GetIDs));
    end;
  end;
  result:=list;
end;


Procedure SetCSSEditorStyles;
var WhiteSpaceRule:string;
begin
  WhiteSpaceRule:='.noChildren,.hasChildren { white-space: pre;}';
  // spoof:= '.noChildren,.hasChildren { white-space: normal;}';
  //indirectWhiteSpaceRule:='@import url("data:text/css;charset=''utf-8'','+WhiteSpaceRule +'") ';
  //UpdateOrCreateStyleSheet(' ','CSSEditorStyles');
  //UpdateOrCreateStyleSheet(spoof,'CSSEditorStyles1');
  UpdateOrCreateStyleSheet(WhiteSpaceRule,'CSSEditorStyles',-1);

end;

Procedure AddToOptionList(instring:string);
var numOptionTypes:integer;
begin
  numOptionTypes:=length(StyleOptions);
  setlength(StyleOptions,numOptionTypes+1);
  StyleOptions[numOptionTypes] := TStringList.Create();
  StyleOptions[numOptionTypes].Delimiter := '|';
  StyleOptions[numOptionTypes].StrictDelimiter := True;
  StyleOptions[numOptionTypes].Sorted := False;
  StyleOptions[numOptionTypes].DelimitedText:=instring;
  // the first element is the style type
  // the second element is the hint to be added to the editor popup
  // the third option is the property designator for CSS
  // following items are the options for that style type
  // an item described as "FreeFormatEdit" means the user simply types the string value with no other checking
end;

Procedure SetStyleOptions;
var
  theList:TStringList;
  begin
     setlength(StyleOptions,0);
 //    AddToOptionList( 'ClassName|hint|FreeFormatEdit|Class Name Not Set');
 //    AddToOptionList( 'WidgetID|hint|FreeFormatEdit|Widget ID not set');
    theList:=GetIdsOrClassNames(false);
    AddToOptionList( 'ClassName||class|'+ theList.Text);
    theList:=GetIdsOrClassNames(true);
    AddToOptionList( 'WidgetID||id|'+ theList.Text);
 //   theList.Free;
    //showmessage( 'WidgetID||id'+ GetIdsOrClassNames(true));

    AddToOptionList('Priority|StylePriorityare:-1)!important in external file 2)!important inline 3)inline 4)external file |priority:|4;|3;|2;|1;');
    AddToOptionList('State||State:|Normal;|:focus|:hover');
    AddToOptionList('Group||Relationship:|Self;|SiblingsOf;|ChildrenOf;|DescendentsOf;');

    AddToOptionList('FontFamily|Asubsetofwebsafefonts|font-family:|CourierNew,Courier,monospace;|Arial,Helvetica,sans-serif;|TimesNewRoman,Times,serif;|inherit;');
    AddToOptionList('FontSize||font-size:|xx-small;|x-small;|small;|medium;|large;|x-large;|xx-large;|inherit;');
    AddToOptionList('FontColor||color:|white;|silver;|gray;|black;|red;|maroon;|yellow;|olive;|lime;|green;|aqua;|teal;|blue;|navy;|fuchsia;|purple;|inherit;');
    AddToOptionList('FontBackgroundColor||background-color:|white;|silver;|gray;|black;|red;|maroon;|yellow;|olive;|lime;|green;|aqua;|teal;|blue;|navy;|fuchsia;|purple;|inherit;');
    AddToOptionList('FontWeight||font-weight:|lighter;|normal;|bold;|bolder;|inherit;');
    AddToOptionList('FontStyle||font-style:|normal;|italic;|oblique;|inherit;');
    AddToOptionList('TextDecor||text-decoration:|line-through;|underline;|overline;');

    AddToOptionList( 'WidgetBorder|a selection of border colors and thicknesses| border: |0.1em red solid;|0.3em red solid;|0.5em red solid;|0.1em green solid;|0.3em green solid;|0.5em green solid;|0.1em blue solid;|0.3em blue solid;|0.5em blue solid;|0.1em black solid;|0.3em black solid;|0.5em black solid;|0.1em white solid;|0.3em white solid;|0.5em white solid;|inherit; ') ;
    // margin is the space around the widget
    AddToOptionList( 'WidgetMargin|Some options for the width of the margin between the widget and its border| margin: | 0.1em; | 0.3em; | 0.5em ; |  0.7em; |  0.9em; | inherit; ') ;
    // padding is the space around the widget and inside the border
    AddToOptionList( 'WidgetPadding|Some options for the width of the padding between widget border and other components|padding:|0.1em;|0.3em;|0.5em;|0.7em;|0.9em;|inherit;');
    AddToOptionList( 'WidgetCorners|choose one of the first four options to have one corner rounded or the finally one to round them all |border-radius:|20px 0 0 0;|0 20px 0 0;|0 0 20px0 ;|0 0 0 20px;|20px 20px 20px 20px;|inherit;') ;
    AddToOptionList('WidgetBackground||background-color:|white;|silver;|gray;|black;|red;|maroon;|yellow;|olive;|lime;|green;|aqua;|teal;|blue;|navy;|fuchsia;|purple;|inherit;');

    AddToOptionList('Scale||transform:|scale(0.5);|scale(0.7);|scale(1.0);|scale(1.3);|scale(1.6);|scale(2.0);|inherit;');
    AddToOptionList('Rotate||transform:|rotate(45deg);|rotate(90deg);|rotate(135deg);|rotate(180deg);|rotate(225deg);|rotate(270deg);|rotate(315deg);|inherit;');
    AddToOptionList( 'Skew|  |transform:| skew(10deg 10deg);|skew(5deg 5deg);|skew(5deg 15deg);|skew(15deg 5deg);|inherit;') ;

    AddToOptionList('Cursor||cursor:|default;|pointer;|crosshair;|move;|text;|wait;|progress;|help;|n-resize;|ne-resize;|e-resize;|se-resize;|inherit;');
    AddToOptionList('Visibility||visibility:|visible;|hidden;|inherit;');
    AddToOptionList( 'Selectable|>all< selects text with one click instead of a double click|user-select: |auto;|none;|text;|all;|inherit;');//all selects text with one click intstead of a doubleclick

    AddToOptionList( 'Transition| |transition:|1s linear;|1s ease; |0.5s linear;|0.5s ease;|1.5s linear;|1.5s ease;| inherit;') ;
    AddToOptionList('Filter||filter:|none|blur(3px);|brightness(0.5);|contrast(2);|grayscale(0.5);|hue-rotate(180deg);|opacity(0.5);|saturate(1.5);|sepia(1);|inherit;');

  end;


Procedure SetCSSNodeTypes;
begin
  AllowChildren          :=[ StyleSheet, StyleTargets, StyleProperties,_And_,_Or_,Not_And_,Not_Or_];
  StyleSheetNodes        :=[StyleSheet];
  StyleRuleNodes         :=[StyleRule];
  ClassificationNodes    :=[Identifier,StyleResources,TargetGrouping,TargetTypes,WidgetType,Numeric,Text,Selectors,LayoutWidgets,Relationship,State,StyleProperties,Widget,Font,Transformations,States,Effects];
  TargetGroupingNodes    :=[_And_,_Or_,Not_And_,Not_Or_];
  RelationshipNodes      :=[ChildrenOf,DescendentsOf,SiblingsOf];
  StylePropertyNodes     :=[WidgetCorners,WidgetPadding,WidgetMargin,WidgetBorder,WidgetBackground,FontFamily,FontSize,FontColor,FontBackgroundColor,FontWeight,FontStyle,TextDecor,Rotate,Scale,Skew,Selectable,Cursor,Visibility,Transition,Filter];
  StyletargetNodes       :=[All,TXProgressBar,TXNumericSlider,TXNumberSpinner,TXLabel,TXHyperlink,TXEditBox,TXMemo,TXTable,TXButton,TXCheckBox,TXRadioBtns,TXComboBox,TXTree,TXDatePicker,TXColorPicker,TXMainMenu,TXMenuItem,TXHBox,TXVBox,TXGroupBox,TXScrollBox,TXTabControl,TXTabSheet,TXForm,ClassName,WidgetID,Hover,Visited,Focused];
  TargetNodesWithOptions :=[ClassName,WidgetID];
  EditableTargetNodes    :=[];
  StyleRuleSelectors     :=[Priority,State,Group];

end;

procedure InitStyleTreeDisplay;
 begin
//   str:=RecursivelyGenerateTheTreeDescriptionString(StyleTree);
   EditAttributeValue('StyleSheet','', 'TreeData' ,'["StyleSheet"]');
 end;

 procedure InitialiseStyleDesigner;
 begin
   Setlength(StyleOptions,0);
   Setlength(CSSTermList,1);
   CSSTermList[0] := TStringList.Create;
   CSSTermList[0].Add('*');
   SetCSSNodeTypes;
   SetStyleOptions;
   SetCSSEditorStyles;
   EditAttributeValue('QualifierEditBox','','IsVisible','false');
   EditAttributeValue('StyleNodeQualifier','','IsVisible','false');
   EditAttributeValue('StyleTreeEditBtn','','IsVisible','false');
   EditAttributeValue('StyleTreeApplyBtn','','IsVisible','false');
   InitStyleTreeDisplay;

   StylesNode:=FindDataNodeById(systemnodetree,'StyleSheet','',true);

 end;

end.


