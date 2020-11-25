# Changelog
All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [3.1.6] - 2020-10-06
### Fixes
- Fix [XRReferenceImageLibraries](xref:UnityEngine.XR.ARSubsystems.XRReferenceImageLibrary) when duplicated from an existing reference image library. Reference image libraries are assigned unique GUIDs on creation, so if you created one by duplicating an existing library, they would have identical GUIDs. The actual reference image library used at runtime was not well defined in this case. 

## [3.1.5] - 2020-09-08
### Fixes
- Fixed an issue which could throw an exception when subsystems were run in the Editor (e.g., for simulation or remoting). This could happen when a trackable (e.g., a plane or anchor) was removed. This did not affect Player builds (i.e., on device). This is an example of the exception and associated callstack:
```
NullReferenceException: Object reference not set to an instance of an object
Unity.Collections.LowLevel.Unsafe.AtomicSafetyHandle.CheckReadAndThrow (Unity.Collections.LowLevel.Unsafe.AtomicSafetyHandle handle)
Unity.Collections.NativeArray`1[T].Copy (Unity.Collections.NativeArray`1[T] src, Unity.Collections.NativeArray`1[T] dst)
Unity.Collections.NativeArray`1[T].CopyFrom (Unity.Collections.NativeArray`1[T] array)
UnityEngine.XR.ARSubsystems.TrackableChanges`1[T]..ctor (System.Void* addedPtr, System.Int32 addedCount, System.Void* updatedPtr, System.Int32 updatedCount, System.Void* removedPtr, System.Int32 removedCount, UnityEngine.XR.ARSubsystems.XRReferencePoint defaultT, System.Int32 stride, Unity.Collections.Allocator allocator)
...
```

## [3.1.3] - 2020-04-13
### New
- Adding the API updater files required to update from `XRReferencePoint*` to `XRAnchor*`.
- Adding new methods to the `XRCameraSubsystem` to query the list of enabled & disabled material keywords for the shader.

## [3.0.2] - 2020-03-18
### Fixes
- Fixed "X" (remove reference image) icon in the `XRReferenceImageLibrary` inspector.

## [3.0.0] - 2019-11-05
### Breaking changes
- Renaming the concept of `Reference Points` to `Anchors`. The Unity script updater should convert any references to `XRReferencePointSubsystem`, `XRReferencePoint`, and `XRReferencePointSubsystemDescriptor` the next time the project is loaded into the Editor.

## [3.0.0-preview.4] - 2019-10-22
### New
- Change `TrySetFocusMode` to a property called `focusMode`, enabling providers to implement a getter as well as a setter.
- Added `classification` on `BoundedPlane` which provides
contextual information such as `Floor`, `Wall`, `Ceiling`.

### Fixes
- [`XRCameraImagePlane.ToString()`](https://docs.unity3d.com/Packages/com.unity.xr.arsubsystems@3.0/api/UnityEngine.XR.ARSubsystems.XRCameraImagePlane.html#UnityEngine_XR_ARSubsystems_XRCameraImagePlane_ToString) invoked `NativeArray.ToString()` to build its string, which produced a string with a lot of irrelevant information. Now, it just displays the length of the array, row stride, and pixel stride of the plane.

## [3.0.0-preview.3] - 2019-09-26
### New
- Added an input device layout for the [Input System](https://github.com/Unity-Technologies/InputSystem), for use by individual providers.

### Breaking Changes
- Updated the interface for all subsystems to apply a consistent pattern across all subsystems. This affects subsystem implementations (such as ARCore and ARKit) but should not affect consumers of those subsystems.
- Removed `supported` property on the `XRFaceSubsystem`. Providers (such as ARKit) are expected not to register themselves if they are not supported.
- Removed `supported` property on the `XREnvironmentProbeSubsystem`. Providers (such as ARKit) are expected not to register themselves if they are not supported.

## [3.0.0-preview.2] - 2019-09-05
- Updated package version to 3.0.0-preview.2 for consistency with other AR-related packages.

## [3.0.0-preview.1] - 2019-08-21
### New
- Add support for reference image libraries that are modifiable at runtime. For platforms that support it, this allows you to add new reference images at runtime.

## [2.2.0-preview.4] - 2019-07-30
### New
- Add support for eye tracking.
- Added an [XRParticipantSubsystem](../manual/participant-subsystem.html) which can track users in a multi-user collaborative session.
- Add support for exposure duration
- Add support for exposure offset
- Add support for Lightweight Render Pipeline and Universal Render Pipeline.
- Add support for height scale estimatation for the 3D human body subsystem.

## [2.2.0-preview.3] - 2019-07-16
### New
- Add support for `NotTrackingReason`.
- Add support for getting the ambient light intensity in lumens.
- Add functionality to the `XRSessionSubsystem` to enable synchronizing the Unity frame with the AR session update. See `XRSessionSubsystem.matchFrameRate` and `XRSessionSubsystem.frameRate`.

### Fixes
- Fix `CameraFocusMode` handling in `XRCameraSubsystem`. This fixes an issue when running on a provider that does not set the default focus mode to `CameraFocusMode.Fixed`.

## [2.2.0-preview.2] - 2019-06-05
- Adding support for ARKit 3 functionality: Human pose estimation, human segmentation images, session collaboration, multiple face tracking, and tracking a face (with front camera) while in world tracking (with rear camera).

## [2.1.0-preview.2] - 2019-05-16
### Fixes
- Fix documentation links.

## [2.1.0-preview.1] - 2019-05-06
### New
- Add an image tracking subsystem.
- Add an environment probe subsystem.
- Add a face tracking subystem.
- Add an object tracking subsystem for detecting previously scanned 3D objects.

## [2.0.1] - 2019-03-12
- 2019.2 verified release

## [1.0.0-preview.1] - 2019-01-14
- This is the first release of *Unity Package com.unity.xr.subsystems.
