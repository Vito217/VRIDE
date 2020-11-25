# Changelog
All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [3.4.0] - 2020-09-15
* Fix binaries copying into builds when Windows Mixed Reality was not enabled in XR Management.
* Fix loader to only run on Windows Editor or Player.
* Fix invalid memory copying of missing meshing data.
* Fix null deref issue when accessing source state properties
* Fix rotation gimble lock at 120 degrees due to using LERP instead of SLERP for rotation interpolation.
* Implement support for Mirror View OcclusionMeshSideBySide.
* Fix culling clipping issue where clipping was overly aggressive on HMDs.
* Fix anchor issue where we were not taking into account any currently stored anchors the WMR may have.
* Fix anchor issue were we were not correctly determining the proper location for an anchor returned from the WMR anchor store on subsequent app starts.
* Fix anchor issue where we were not correctly setting tracking state on each anchor.
* Fix game view rendering when running MRP in simulator mode.
* Update management dependency to 3.2.15
* Update documentation to call out the fact that we only support SPI and there is no UI to change it as per design.
* Correctly clean up subsystems on failure in initialization.

## [3.3.1] - 2020-08-03
* Adjust line endings in scripts to all be Unix LF.

## [3.3.0] - 2020-07-12
* Update minor version due to added remoting scripting apis.
* Add input display name for Windows MR HMD.
* Allow for secondary UWP windows support.
* Fix Culling combined frustum FOV
* Multiple corrections for hologram stability and convergence.
* Update remoting window with message clarify which platform is used for Player Settings when entering play mode.

## [3.2.0] - 2020-06-04
* Update remoting binaries to 2.1.3.
* Prepare for 3.1.6 release.
  
## [3.1.6-preview.3] - 2020-05-15
* Add Eye gaze support through EyeGazeRotation and EyeGazePosition input feature usages.
* Fix build processor to not copy settings to unsupported targets.

## [3.1.6-preview.2] - 2020-04-20
* Support use of background UWP window for WSA XR applications.
* Stop copying binary files from package when the provider is not assigned to the build target.

## [3.1.6-preview.1] - 2020-04-08
* Add support for game view eye rendering.
* Arm32 support
* Apply frame settings to all cameras and not just the first stereo camera.
* Add access to Simulation and Render Holographic Frames.
  
## [3.1.5] - 2020-04-08
*  Back management dependency down to 3.0.6.

## [3.1.4] - 2020-04-08
*  Fix package to be usable by downlevel management.

## [3.1.3] - 2020-04-03
* Release for management 3.2.4.

## [3.1.2] - 2020-04-03
* Release for management 3.2.2.

## [3.1.1] - 2020-03-23
* Release for management 3.2.1.

## [3.1.0] - 2020-03-13
* Release for management 3.2.0.

## [3.1.0-preview.2] - 2020-02-26
* Update management dependency to 3.2.0 series.

## [3.1.0-preview.1] - 2020-02-20
* Update management dependency to 3.2.0 series.
* Replace package initialization code with the new package metadata information.

## [3.0.1] - 2019-02-07
* Update management dependency for verification.
* Package release for verification.

## [3.0.1-preview.4] - 2020-01-30
* Add airtap feature usage for HoloLens
* Update Remoting Plugins with x64 UWP versions.
* Add build settings toggle to copy remoting plugins to final build.
* Fix positional tracking at app launch
* Fix coordinate system reset after tracking is lost and regained


## [3.0.1-preview.3] - 2019-12-17
* Fix camera tracking and camera tracking state reporting through XRInput.
* Fix license text.
* Fix camera disconnect to actually call the disconnect API and not re-connect.
* Add Pointer pose and grasp to HoloLens v2 hand tracking.
* Fix documentation to remove markdown that doesn't work with package management renderer.
* Fix build processor to only set boot config items if the target has a WindowsMRLoader instance in the list.
* Fix null deref exception in Gesture provider.

## [3.0.1-preview.2] - 2019-12-02
* Fix input provider missing feature usage for source loss and pointer position.
* Add pose validation to display subsystem so that rendering system correctly skips trying to render frames that are invalid.
* Fix logging to handle wide char strings correctly.

## [3.0.1-preview.1] - 2019-11-14
* Update XR Management dependency to 3.0.5.
* Add build target filtering.
* Fix Occlusion Mesh creating each frame.
* Add version define to allow us to use uplevel management features while also allowing us to run on down level management.

## [3.0.0] - 2019-11-15
* 2020.1 Release package for verification.

## [3.0.0-preview.2] - 2019-11-14
* Change AR Subsystems version to 3.0.0 and modify the RP API to reflect name changes.
* Update XR Management dependency to 3.0.4.

## [3.0.0-preview.1] - 2019-08-28
* Pick up header API changes that lock the package to 2020.1 only.
* Update to new preview XR Management 3.0.5 and add build target filtering.

## [2.0.4-preview.2] - 2019-10-25
* Update headers to reflect changes from Unity core.

## [2.0.4-preview.1] - 2019-10-18
* Fix an issue that was preventing builds of the package outside of Unity.

## [2.0.3] - 2019-10-15
* Update package version for official release

## [2.0.3-preview.2] - 2019-10-01
* Fixed depth buffer issue where we assume reverse Z is always on when converting projection matrix.
* Remove Windows Mixed Reality legacy package dependency.
* Add AudioPluginMsHRTF spatializer to package.
* Add Remoting plugins to package.

## [2.0.3-preview.1] - 2019-08-28
* Input System device layouts for Windows MR devices when Input System package is included.
* Add Hand tracking support for HoloLens v2. We now expose bones for each hand when an app is using a device capable of hand tracking.
* WindowsMR HMDs now have a user presence input feature that will inform the developer if the HMD is currently being used.  Use UnityEngine.XR.CommonUsages.UserPresence to retrieve it.
* Fix input issue where we were using the wrong frame for input data.
* Fix depth buffer issue where we were not handling Unity rendering depth bufer in reverse Z correctly.
* Update dependencies to handle changes in Subsystem definition.

## [2.0.2-preview.1] - 2019-08-23
* Implement recenter functionality. Implementation matches legacy VR functionality.
* Renamed HMD tracking input features to be suffixed with Rotation or Position where appropriate.
* Fix testing dll meta files.
* Update XR Management dependency to 3.0.2-preview.2

## [2.0.1] - 2019-07-22
* Remove errant subsystems from json file.

## [2.0.0] - 2019-07-22
* Update package version ahead of release.
* Added reference point support for native pointer access.
* Added support for getting native mesh data for a meshing subsystem mesh.
* Added support to get all current native SpatialSourceState instances from the input subsystem.
* Update display subsystem to handle promotion out of experimental.
* Add hand tracking for HoloLens devices.
* Add remoting support for HoloLens v2 devices.

## [1.99.0-preview.2] - 2019-07-03
* Update to use standalone Session and Reference Point subsystems.
* Update XR Management dependency to handle Experimental namespace removal in trunk.
* Update metro package dependency.

## [1.99.0-preview.1] - 2019-06-18
* Pick up 2019.2 preview changes that are applicable to 2019.3.

## [1.99.0-preview] - 2019-06-13
* Update package to support 2019.3+ only.
* Rev version to almost 2. This is to make space for 2019.2 preview versions and in acknowledgement of the breaking changes that will happen soon.
* Add documentation for gestures.

## [1.0.0-preview.9] - 2019-06-13
* Move documentation revision history out of documentation. Replaced with change log tracking of changes.
* Add XmlDocs to the appropriate APIs.
* Strip out session subsystem.
* Add IsTracked property to HMD tracked device (though currently defaults to true all the time).
* Remap Touchpad Axes to primary 2d axis and thumbstick axis to secondary to make grouping correct.
* Move tumbstick pressed from Thumbrest to Secondary 2D Axis clicked.
* Add package specific external APIs for getting the Holographic Space, The Spatial Coordinate Sysem and the Windows MR tracking state.

## [1.0.0-preview.8] - 2019-06-11
* Update package promotion pipeline to latest CI scripts.

## [1.0.0-preview.7] - 2019-05-29
* Fix build issue when the user has decided to not create an instance of settings for WindowsMR.
* Integrate latest standalone gesture manager subsystem.
* Update XR Managmeent to 2.0.0-preview.19

## [1.0.0-preview.6] - 2019-05-28
* Update package naming.
* Update namespaces to follow expected naming conventions.
* Ensure input devices are disconnected when input subsystem stops.
* Add standalone gesture subsystem.

## [1.0.0-preview.5] - 2019-05-13
* Fix for fog generation issue due to using D3D projection matrix instead of OpenGL.
* Split render pose out of tracking pose to separate logical concerns.

## [1.0.0-preview.4] - 2019-05-10
* Update management version to point at Yamato build management.
* Change package over to using Yamato for build/publish tasks. We'll still keep Jenkins for unit test verification for now.
* Fix crash due to new streamlined management.
* Fix culling frustum to correctly calculate the combined bounds of the eye projections.
* Fix issue in projection creation to convert D3D depth bounds to OGL depth bounds.
* Take dependency on new version of Metro package to handle a build break due to ugui.

## [1.0.0-preview.3] - 2019-03-26
* Update package to point to latest management release verison.

## [1.0.0-preview.2] - 2019-03-26
* Updated min Unity version.

## [1.0.0-preview.1] - 2019-03-26
* Update version to show preliminary v1 release.
* Remove all shim code. We no longer shim Legacy VR API support.
* Bring Display Subsystem support up to current XR SDK version (Stabilization Plane/Focus Point, Content Protection, Reprojection mode)
* Implement support for shared depth buffer.
* Add Gesture Subsystem.
* Add Meshing Subsystem implementation.
* Add Experience Subsystem.
* Add support for Historic Queries for controller/input subsystem data.
* Add initial haptics support.
* Number of bugs and QoL issues resolved.

## [0.2.0-preview.11] - 2018-12-05
* Support shim interface to allow for interoperation with UnityEngine.XR.WSA classes.
* Fix input so that we report Head node through Tracked Pose Driver.
* Add dependency on Windows Metro package to support shim implementation.

## [0.2.0-preview.10] - 2018-11-27
* Update XR Management dependency
* Added dependency on Windows MR Metro package
* QA Added some play mode testing
* Added shim interface for interoperation with legacy VR WSA Classes
* Fixed issues with updated Microsoft SDK 17763
* Update bee and changed build to add Arm64 support
* Fixed build to correctly create DLLs on CI. Removed DLLs from source repository.

Known Issues:

* Controllers will not work in Editor. Working with Microsoft on that currently.
* Stability of integration shim is not great.
* Integration has bugs that are beng tracked and dealt with.

## [0.2.0-preview.9] - 2018-10-31
* Update XR Management dependency

## [0.2.0-preview.8] - 2018-10-29
* Resolve CI issues.

## [0.2.0-preview.7] - 2018-10-29
* Test CI/CD for GitLab -> GitHub  move
* Test pickup of XR Managmenet 0.2.0-preview.6 post GitLab -> GitHub move
* Add copy of settings from VR Device for Windows MR to XR SDK Windows MR.


## [0.2.0-preview.3] - 2018-10-24
* Updated to pick up dependencies from XR Management
* Support for splash screen merged in.


## [0.2.0-preview.2] - 2018-10-16
* Fix memory leaks
* Rename generated managed DLLs to not conflict with the com.unity.xr.windowsmr.metro package.


## [0.2.0-preview.1] - 2018-09-10
* Updated XR Management dependency to 0.2.0-preview.1
* Package initialization support added.
* Support for more than one build target added for settings.

## [0.1.0-preview.13] - 2018-01-01
* Now supports HoloLens and Standalone builds
* Supports Reference Points subsystem to allow for ephemeral world anchors.
* Test project cleaned up and pointed to the correct location in the repo for the package.


## [0.1.0-preview.12] - 2018-08-20
* Update the package description and name.
* Fixed a crash issue revealed when compositor layers were enabled
* Several changes to attempt to get compisitor layers rendering

## [0.1.0-preview.11] - 2018-08-20
* Implemented XR Management API. We now have an XRLoader instance, a build processeor, configuration and data passing to native land.

## [0.1.0-preview.10] - 2018-08-16
* Rebuilt dll (hadn't been rebuilt since .7)

## [0.1.0-preview.9] - 2018-08-16
* Fixed incorrect library name in UnitySubsystemsManifest.json

## [0.1.0-preview.8] - 2018-08-16
* Merged local changes to master

## [0.1.0-preview.7] - 2018-08-15
* Fixed a crashing bug caused by uninitialized pointer that only shows up in Release builds.
* Updated the bee.exe with a version that supports some custom work to handle C++/WinRT.
* Updated README to be a bit more useful.
* Add Packages folder for TestProject back into the repo.

## [0.1.0-preview.6] - 2018-08-15

* Correct naming of provider in native registration code.
* Modify bee script to make sure PDB and lib files are copied as well as DLL
* Modified yaml file to strip the PDBs and libs from the generated bits on package creation.

## [0.1.0-preview.5] - 2018-08-14

### Remove build folder and replace with the dll checked into the proto folder. When we can build on the runners we need to delete that checked in dll.

## [0.1.0-preview.4] - 2018-08-14

### More meta files changes

## [0.1.0-preview.3] - 2018-08-14

### Fix package layout to conform to expected XR SDK package layout.

## [0.1.0-preview.2] - 2018-08-14

### Add missing meta files to stop Unity from complaining

## [0.1.0-preview.1] - 2018-08-13

### This is the first release of *Unity Windows Mixed Reality XR SDK Provider*.
