XIDE Project
============

Overview
--------
This Lazarus project provides a RAD IDE for Free Pascal in the browser (Chrome) or the desktop.

It is not intended for general purpose web page development, but for Line Of Business or Embedded System apps where 
pascal is needed to provide extensive client side functionality and/or you need to share a code base between browser 
and desktop versions. 

It is built using XComponents, Pas2js and GPUJS and allows full WYSIWYG drag and drop development for HTML widgets CSS rules and 
SVG components. The UI widget event handlers and simple GPU code are then written in Pascal.

Installation
------------

The Lazarus package XComponents must be installed (see separate docs provided with XComponents).

Load the XIDE project into Lazarus IDE.

XIDE can be used either with or without components which use the ‘Chromium Embedded Framework’.  
The compiler directive ‘-dChromium’ must be set in ‘Project Options’ to include these components.
Also see cef installation notes provided with XComponents.
If the ‘-dChromium’ directive is set, then the CEF4Delphi_Lazarus package must also have been installed.
In XIDE.lpr, set the string CEFLibDir to the location of your installed cef framework files (eg. 'C:\cef4Master\FrameworkDir');

Compile the project to create a .exe file.

Run the project either within the Lazarus IDE framework, or stand-alone using XIDE.exe.

On the main menu, select System>Run Settings.  
Set the full path to your local Lazarus-installed pascal compiler, fpc.exe.

To run XIDE on a browser, execute XIDE.exe, then on main menu select Web>Run in Browser.  This will generate a file
XIDEMain.html which can be used stand-alone.

Using XIDE
----------

The interface consists of an object inspector tree structure (UI Designer), and a components list (UI Resources).  
A system is built by dragging (or pasting) resource components into the UI Designer tree.  They will then appear on
the screen in the User Interface section.

Use the UI Designer to select components and set their properties.

Position components using the vertical and horizontal layout container panels TXHBox and TXVBox.  
There is no absolute positioning.  

Component heights and widths can be expressed in pixels (20, 20px) or as a percentage of the parent dimension (20%).
You can also set the 'Alignment' property for positioning of a component within an HBox or VBox.

Events
------

To add event handling code, select a component and look at the available events on the 'Events' tab in the properties panel.
Click the 'Edit Event Code' button (...) to pop up an editor, where a block of pascal code can be written.
Note there is no procedure header - this is handled automatically by the system builder.

See the main menu Help>Overview screen for notes on available functions within events code.

To test your code, use main menu 'Run Mode'.  This will compile all your event code, and if there are
no errors, it will hide the design interface.  The system will now respond to user events as coded.
Press 'Design Mode' to return to the design interface.

Code Designer
-------------

This provides a tree view of all of the event code that has been added by the user in the current system.  
It also allows complete pascal code units to be added, to provide any common functions that my be required.

Style Designer
--------------

This provides a tree structure to apply CSS styling to system components.  The available objects for building
this tree are provided in the 'Style Resources' tree.
NOTE AT PRESENT THIS FACILITY IS FUNCTIONAL ONLY IN THE BROWSER ENVIRONMENT.

Saved Systems
-------------

At any time in design mode, you can save a system design (main menu System>Save to Local Storage).  
Systems are saved as text files.
In the desktop environment this will create a file in the subfolder SavedSystems.  
In the browser environment it will save to browser local storage.
The same formatted text can be copied to or loaded from the clipboard.


If you make changes to the XIDE framework project
-------------------------------------------------

There is a resource file xide.lrs, which must be in the project folder.
It is used during the process of transferring to browser execution, to provide the source code for the
pas2js transpiler to build the framework javascript in the XIDEMain.html file.
If any of the framework code is changed, this file would need to be rebuilt.
Edit the supplied file setupxidelrs.txt, for correct paths to the listed source files.
Rebuild the resource file using a command line like: c:/fpcupdeluxe/lazarus/tools/lazres xide.lrs @setupxidelrs.txt








