Purpose
=======

Distribute Windows Mixed Reality XR SDK providers for the Unity XR system.

Current set of providers in this package include:
* Display
* Input
* Anchor
* Experience
* Session


Getting Started
===============
### Create a new Unity project, or use an existing project

### Go into the Packages folder of your project
* Open up a terminal window
* cd to your Unity project folder
* cd into Packages 

### Update manifest.json to point to the version of the Windows Mixed Reality XR SDK package

If using a Production released version of the package, please use the official Package Manager UI in Unity to add and manage your version of the package.

If using staging for preview and experimental versions, add the following to your *dependencies* section:

		{
		  "registry": "https://staging-packages.unity.com",
		  "dependencies": {
		  		...,
				"com.unity.xr.windowsmr": "<staging version you wish to use>"
			}
		}

At this point you should be all set to use the package to develop your Windows Mixed Reality based Unity application.
