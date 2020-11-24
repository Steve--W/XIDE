XIDE Project
============

Overview
--------
This Lazarus project provides a RAD IDE for Free Pascal in the browser (Chrome) or desktop.
(Lazarus downloads:   https://wiki.lazarus.freepascal.org/fpcupdeluxe)
XIDE is developed and tested on Lazarus version 2.1.0, FPC version 3.3.1.
At present, XIDE is tested only on Windows, and Chrome.

It is not intended for general purpose web page development, but for Line Of Business or Embedded System apps where 
pascal is needed to provide extensive client side functionality and/or you need to share a code base between browser 
and desktop versions. 

It is built using XComponents, Pas2js and GPUJS and allows full WYSIWYG drag and drop development for HTML widgets CSS rules and 
SVG components. The UI widget event handlers are then written in Pascal.  Python code scripts can also be created.

Installation
------------

The package XComponents must first be installed into the Lazarus IDE. 
XComponents can be downloaded from: https://github.com/Steve--W/XComponents

Load the XIDE project into Lazarus IDE.

XIDE can be used either with or without components which use the ‘Chromium Embedded Framework’.  
The compiler directive ‘-dChromium’ must be set in ‘Project Options’ to include these components (in both the project and the XComponents package).
Also see cef installation notes provided with XComponents (docs/XComponentsProjectNotes.md).
If the ‘-dChromium’ directive is set, then the CEF4Delphi_Lazarus package and cef4 runtime framework must also have been installed,
(see https://github.com/salvadordf/CEF4Delphi)
In XIDE.lpr, set the string CEFLibDir to the location of your installed cef4 framework files (eg. 'C:\cef4Master\FrameworkDir');

Compile the project to create a .exe file.

Run the project either within the Lazarus IDE framework, or stand-alone using XIDE.exe.

On the main menu, select System>Run Settings.  
Set the full path to your local Lazarus-installed pascal compiler, fpc.exe.

To run XIDE on a browser, run XIDE.exe, then on main menu select Web>Run in Browser.  This will generate a file
XIDEMain.html which will then be launched in the browser.  The html file can also be used stand-alone.

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

Python
------

If the compiler directive -dPython is set, there is also an option to create Python language scripts here.  
Python code is executed on entry to run mode.

Python code can also be called directly from a Pascal event handler, using the function RunPython('..script..');

Desktop - uses the Lazarus embedded python engine - users must first download and install the Python4Lazarus_package 
(https://github.com/Alexey-T/Python-for-Lazarus), and also install Python on their machine (https://www.python.org/downloads/).
After installing Python, make sure that the system PATH variable is updated to include the installed location of Python.exe.
If you want to use Python packages such as numpy or matplotlib, install these using the pip utility (https://pip.pypa.io/en/stable/installing/).
Pip requires the PATH variable to include the installed location of pip.exe (generally the Python location, in subfolder /Scripts).

Browser - Pyodide is included in XIDE to provide the Python interface.  
(ref: https://pyodide.readthedocs.io/en/latest/using_pyodide_from_javascript.html)
   Offline - If you need to use Pyodide offline, it is necessary to create a local folder (./pyodide_local) containing 
   the downloaded pyodide toolset (eg. from https://github.com/iodide-project/pyodide/releases/download/0.14.3/pyodide-build-0.14.3.tar.bz2)
   This folder must also contain the file loadlocal.js, which can be found at:
   https://github.com/iodide-project/pyodide/tree/6a2dd522f1eb4143f2630deae0a1fa9555546dfe/runlocal
   There is a pyodide_local folder containing minimum required files provided at: https://github.com/Steve--W/XIDE

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


Example Projects
----------------
<p><a href="https://steve--w.github.io/XIDEPages/">
            XIDE Project Examples
          </a></p>









