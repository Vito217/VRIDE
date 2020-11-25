# Changelog
All notable changes to this package will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [0.10.0-preview.7] - 2020-11-03
- Added multi-object editing support to all Editors
- Fixed Inspector foldouts to keep expanded state when clicking between GameObjects

## [0.10.0-preview.6] - 2020-10-30
- Added support for haptic impulses in XR Controller (Action-based)
- Fixed issue with actions not being considered pressed the frame after triggered
- Fixed issue where an AR test would fail due to the size of the Game view
- Fixed exception when adding an Input Action Manager while playing

## [0.10.0-preview.5] - 2020-10-23
- Added sample containing default set of input actions and presets
- Fixed issue with PrimaryAxis2D input from mouse not moving the scrollbars on UI as expected. (1278162)
- Fixed issue where Bezier Curve did not take into account controller tilt. (1245614)
- Fixed issue where a socket's hover mesh was offset. (1285693)
- Fixed issue where disabling parent before XRGrabInteractable child was causing an error in OnSelectCanceling()

## [0.10.0-preview.4] - 2020-10-14
- Fixed migration of a renamed field in interactors

## [0.10.0-preview.3] - 2020-10-14
- Added ability to control whether the line will always be cut short at the first raycast hit, even when invalid, to the Interactor Line Visual (1252532)
- Fixed Tracked Device Graphic Raycaster not respecting the Raycast Target property of UGUI Graphic when unchecked (1221300)
- Fixed XR Ray Interactor flooding the console with assertion errors when sphere cast is used (1259554) (1266781)
- Fixed foldouts in the Inspector to expand or collapse when clicking the label, not just the icon (1259683)
- Fixed created objects having a duplicate name of a sibling (1259702)
- Fixed created objects not being selected automatically (1259682)
- Fixed XRUI Input Module component being duplicated in EventSystem GameObject after creating it from UI Canvas menu option (1218216)
- Fixed missing AudioListener on created XR Rig Camera (1241970)
- Fixed several issues related to creating objects from the GameObject menu, such as broken undo/redo and proper use of context object
- Fixed issue where GameObjects parented under an XRGrabInteractable did not retain their local position and rotation when drawn as a Socket Interactor Hover Mesh (1256693)
- Fixed issue where Interaction callbacks (OnSelectEnter, OnSelectExit, OnHoverEnter, and OnHoverExit) are triggered before interactor and interactable objects are updated.(1231662, 1228907, 1231482)
- Renamed OnSelectEnter, OnSelectExit, OnSelectCancel, OnHoverEnter, OnHoverExit, OnFirstHoverEnter, and OnLastHoverExit to OnSelectEntered, OnSelectExited, OnSelectCanceled, OnHoverEntered, OnHoverExited, OnFirstHoverEntered, and OnLastHoverExited respectively.
- Upgrade Guide: ILineRenderable method signatures replaced some ref parameters with out parameters; callers should replace ref with out

## [0.10.0-preview.2] - 2020-08-26
- Added XR Device Simulator and sample assets for simulating an XR HMD and controllers using keyboard & mouse

## [0.10.0-preview.1] - 2020-08-10
- Added continuous move and turn locomotion
- Fixed compilation issue when AR Foundation package is also installed
- Fixed the Interactor Line Visual lagging behind the controller (1264748)
- Fixed Socket Interactor not creating default hover materials, and backwards usage of the materials (1225734)
- Fixed Tint Interactable Visual to allow it to work with objects that have multiple materials
- Improved Tint Interactable Visual to not create a material instance when Emission is enabled on the material
- Changed accesibility levels to avoid protected fields, instead exposed through properties
- Upgrade Guide: Components that use Input System actions no longer automatically enable or disable them. Add the InputActionManager component to a GameObject in a scene and use the Inspector to reference the InputActionAsset you want to automatically enable at startup.
- Upgrade Guide: Some properties have been renamed from PascalCase to camelCase to conform with coding standard; the API Updator should update usage automatically in most cases

## [0.9.9-preview.3] - 2020-06-24
- In progress changes to visibilty

## [0.9.9-preview.2] - 2020-06-22
- Hack week version push.

## [0.9.9-preview.1] - 2020-06-04
- Fixes controller recording not working
- Swaps axis for feature API anchor manipulation
- Starts controller recording at 0 time so you dont have to wait for the recording to start playing.

## [0.9.9-preview] - 2020-06-04
- Adds new input system support
- Makes a number of members and properties protected rather than private
- Removes sealed from a number of classes.
- Adds abiltiy to query the controller from the interactor

## [0.9.4-preview] - 2020-04-01
- Fix to allow 1.3.X or 2.X versions of legacy input helpers to work with the XR Interaction toolkit.

## [0.9.3-preview] - 2020-01-23
- Lots of fixes based on user feedback from the blog post / forums
    - Adds pose provider support to XR Controller MonoBehaviour
    - Fixes minor documentation issues
    - Fixed passing from hand to hand of objects using direct interactors
    - Removes need for box colliders behind UI to stop line visuals from drawing through them
    - Adds abiilty to put objects back to their original hierarchy position when dropping them.
    - Fixes null ref in controller states clear
    - Fixes no "OnRelease" even for Activate on Grabbable
    - Makes teleport configurable to use either activate or select

## [0.9.2-preview] - 2019-12-17
- Rolled LIH version back until 1.3.9 is on production.

## [0.9.1-preview] - 2019-12-12
- Documentation image fix

## [0.9.0-preview] - 2019-12-06
- Release candidate

## [0.0.9-preview] - 2019-12-06
- Further release prep

## [0.0.8-preview] - 2019-12-05
- Pre-release release.

## [0.0.6-preview] - 2019-10-15
- Further CI/CD fixes.
- Changes to readme.md file

## [0.0.5-preview] - 2019-10-03
- Renamed everything to com.unity.xr.interaction.toolkit / XR Interaction Toolkit
- Setup CI correctly.

## [0.0.4-preview] - 2019-05-08
- Bump package version for CI tests.

## [0.0.3-preview] - 2019-05-07

### This is the first preview of XR Interaction.
Initial preview release of the XR Interaction framework.
