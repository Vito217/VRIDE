# PharoVRIDE's Channel Log

## V0.18.4 (12/11/2020)
* Added default icon
* Fixed some bugs

## V0.18.3 (09/11/2020)
* Code refactoring
* Better performance

## V0.18.2 (06/11/2020)
* Updated Unity + XR Plugins

## V0.18.1 (04/11/2020)
* Added window that shows the tasks that will be used during the test.
* Fixed some lightning issues.

## V0.18.0 (29/10/2020)
* Improved Keyboard layout. Now each window will have just one keyboard instead of two. The Do, Print, Inspect, Accept, Copy and Paste buttons are now part of PunchKeyboard as a UI Panel. Tab and Delete buttons where added.
* Fixed minor bugs.

## V0.17.0 (27/10/2020)
* Added buttons for resizing and scaling.
* Added support for Oculus Android.

## V0.16.0 (22/10/2020)
* Upgraded to Unity 2020.1.10f1
* Improved some UI elements.
* Fixed incorrect text parsing on server requests.
* Fixed bad keycode mapping on Punchkeyboard.

## V0.15.0 (15/10/2020)
* Fixed dragging for AFrames and their primitives,
* Reduced size of VIVE colliders.
* Fixed some bugs in the text editors. 

## V0.14.3 (08/10/2020)
* Fixed some playground and browser bugs.
* Fixed many AFrame layout issues.
* Using VIU ViveRig as the main camera.
* Improved some UI elements.
* Add new skyboxes (created by Richard Whitelock)

## V0.14.2 (05/10/2020)
* Fixed AFrame size.
* Reduced capsule collider radius.

## V0.14.1 (02/10/2020)
* AFrame lines working properly.
* Fixed AFrame dragging

## V0.14.0 (01/10/2020)
* Added AFrame exporter for Roassal3 (only primitives and text working by now)
* The execution order for every exporter is as follows: AFrame -> SVG -> PNG
* Using capsule colliders on hand controllers.
* Fixed some keyboard bugs.

## V0.13.2 (28/09/2020)
* Fixed some bugs

## V0.13.1 (22/09/2020)
* Using default ViveColliders

## V0.13.0 (21/09/2020)
* Added PunchKeyboard by Jonathan Ravasz.
* Added an About section.
* User can now change between VRIDE Keyboard or PunchKeyboard.
* Both keyboards have collision detection.

## V0.12.2 (19/09/2020)
* Using Vive Input Utility as the main Camera Rig.

## V0.12.1 (18/09/2020)
* Fixed collisions

## V0.12.0 (18/09/2020)
* Added Roassal 2 and 3 examples. You can see them on the menu.
* Now you can use "Transcript show:" to log into the VRIDE log window
* It is possible to create a Roassal view by just calling the "view" property (Roassal2) or "canvas" property (Roassal3). PNG will be the default format by now.
* Fixed some visual issues.

## V0.11.1 (10/09/2020)
* Fixed Vive Teleporting

## V0.11.0 (08/09/2020)
* Added a virtual keyboard.
* Fixed minor bugs.

## V0.10.2 (04/09/2020)
* Fixed player position.

## V0.10.1 (01/09/2020)
* Added package, class and method filters.

## V0.10.0 (29/08/2020)
* Added VIVE Input Utility plugin.
* VR controls optimized for HTC VIVE.
* Added Roassal3 compatibility (only for PNG images).
* Some code refactoring.

## V0.9.3 (25/08/2020)
* Improved performance while loading packages from the image.

## V0.9.2 (18/08/2020)
* Fixed bug that didn't show new methods with arguments.
* Improved title screen graphics.

## V0.9.1 (17/08/2020)
* Moved sysData to SaveAndLoadModule.cs
* Added vertical window movement.
* Fixed some Playground bugs related to Roassal2.

NOTE: Due to implementantion issues, all local variables must ALWAYS be declared before using them.

## V0.9.0 (13/08/2020)
* Now the User can specify the IP they will connect to.
* Fixed a bug that appeared when the User dragged a window.
* Fixed log file and session file locations.

## V0.8.2 (30/07/2020)
* Changed log.txt location.
* Turned off text highlighting due to malfunctions after recent Unity updates.
* Turned off internal Pharo server due to compatibility issues with other platforms.
* Fixed inspector's text editor bug.
* Made some code improvements.

## V0.8.1 (24/07/2020)
* Fixed package, class and method creation.
* Fixed UI layouts.
* Fixed text editor bugs.
* Fixed Esc button.

## V0.8.0 (07/07/2020)
* Added missing packages from Pharo image.
* Added Class-Side and Instance-Side
* Code refactoring

## V0.7.0 (28/06/2020)
* Pharo Image included.
* Added log file with timestamps
* Added Browser Packages section
* Added cache System.
* Fixed some visual issues
* Improved Mouse and Keyboard controls
* Improved window dragging

## V0.6.2 (01/06/2020)
* Fixed some inspector bugs.

## V0.6.1 (26/05/2020)
* Fixed bug where two or more classes and their methods are shown at the same time

## V0.6.0 (24/05/2020)
* Changed camera behaviour for desktop mode.
* Added window close button.
* Windows can now be dragged left and right.
* Added compatibility with Roassal2's Grapher.
* Now browsers contain the same information (classes and methods)
* Fixed some coding and minor bugs

## V0.5.0 (10/05/2020)
* Added SteamVR controlls.

## V0.4.0 (06/05/2020)
* Added a title screen
* Added support for future VR controls (still in development)
* Fixed some bugs that occur during method definition
* Some prefabs improvements

## V0.3.0 (02/05/2020)
* Now you can invoke as many instances of browsers and playgrounds as you want (see the Controls section in the README file)
* Text Highlighting was improved
* Prefabs now have colored borders

## V0.2.1 (29/04/2020)
* Improved Prefabs

## V0.2.0 (26/04/2020)
* Now the inspector is working.
* Fixed bug where a method's name could have linebreaks and blank spaces, leading to duplicated methods on update.

## V0.1.0 (21/04/2020)
* Added working browser
* Added working text editor
* Includes keyboard controls
