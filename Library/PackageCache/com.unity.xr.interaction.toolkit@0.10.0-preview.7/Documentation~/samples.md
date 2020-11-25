# Samples

To install samples included with this package, follow the instructions for [Installing samples](index.md#installing-samples) using the Package Manager.

|**Sample**|**Description**|
|---|---|
|[Default Input Actions](#default-input-actions)|Default set of input actions and presets for use with XR Interaction Toolkit behaviors that use the Input System.|
|[XR Device Simulator](#xr-device-simulator)|Assets related to the simulation of XR HMD and controllers.|

## Default Input Actions

This sample is installed into the default location for package samples, in the `Assets\Samples\XR Interaction Toolkit\[version]\Default Input Actions` folder. You can move these Assets to a different location.

This sample contains an [Input Action Asset](https://docs.unity3d.com/Packages/com.unity.inputsystem@1.0/manual/ActionAssets.html) that contains [Actions](https://docs.unity3d.com/Packages/com.unity.inputsystem@1.0/manual/Actions.html) with typical [Input Bindings](https://docs.unity3d.com/Packages/com.unity.inputsystem@1.0/manual/ActionBindings.html) for use with behaviors in the XR Interaction Toolkit that read input.

This sample also contains [Presets](https://docs.unity3d.com/Manual/Presets.html) for behaviors that use actions to streamline their configuration.

|**Asset**|**Description**|
|---|---|
|`XRI Default Continuous Move.preset`|Preset for [Continuous Move Provider](locomotion.md#continuous-move-provider).|
|`XRI Default Continuous Turn.preset`|Preset for [Continuous Turn Provider](locomotion.md#continuous-turn-provider).|
|`XRI Default Input Actions.inputactions`|Asset that contains actions with typical bindings and several [Control Schemes](https://docs.unity3d.com/Packages/com.unity.inputsystem@1.0/manual/ActionBindings.html#control-schemes) for use in XR experiences.|
|`XRI Default Left Controller.preset`|Preset for left hand [Controllers](index.md#controllers).|
|`XRI Default Right Controller.preset`|Preset for right hand [Controllers](index.md#controllers).|
|`XRI Default Snap Turn.preset`|Preset for [Snap Turn Provider](locomotion.md#snap-turn-provider).|

### Input Actions Asset

The following image shows the [Action editor](https://docs.unity3d.com/Packages/com.unity.inputsystem@1.0/manual/ActionAssets.html#editing-input-action-assets) of the Asset included in the sample, while editing one of the bindings of an action in one of the maps.

![sample-default-input-actions-asset](images/sample-default-input-actions-asset.png)

The Asset contains several Action Maps, separated based on the bound device. Having different sets of actions based on the controller handedness, rather than one set of actions with input bindings for multiple controllers, allows for easier fine-grained management of the allowed actions per-hand. For example, the `XRI RightHand\Turn` action can be disabled while the right hand has picked something up, while turning is still allowed by having the `XRI LeftHand\Turn` action enabled.

|**Action Map**|**Description**|
|---|---|
|XRI HMD|Actions with input bindings to a head-mounted display (that is, `<XRHMD>`).|
|XRI LeftHand|Actions with input bindings to a left hand controller (that is, `<XRController>{LeftHand}`)|
|XRI RightHand|Actions with input bindings to a right hand controller (that is, `<XRController>{RightHand}`)|

There are also several Input Control Schemes to group different input controls. You can use these to selectively enable or disable some of the bindings based on the locomotion movement control scheme in use.

|**Control Scheme**|**Description**|
|---|---|
|Generic XR Controller|Bindings that should remain enabled when applying any movement control scheme.|
|Continuous Move|Bindings that should remain enabled when applying the continuous movement control scheme.|
|Noncontinuous Move|Bindings that should remain enabled when applying the noncontinuous movement control scheme.|

For a complete example of configuring input actions for each controller, and using control schemes, see the [XR Interaction Toolkit Examples](https://github.com/Unity-Technologies/XR-Interaction-Toolkit-Examples) project.

### Configuring Preset Manager defaults

After importing the sample into your Project, if you want to use the Assets, it is recommended to use the [Preset Manager](https://docs.unity3d.com/Manual/class-PresetManager.html) to change the default Presets to those included in this sample. This will allow the objects you create from the **GameObject &gt; XR** menu to automatically populate the action properties of the behavior, as configured in each preset.

To easily set a preset as the default for its associated behavior, select the Asset in the Project window, then click the **Add to [behavior] default** button in the Inspector.

![sample-default-input-actions-preset](images/sample-default-input-actions-preset.png)

Access the Preset Manager from Unity's main menu (go to **Edit &gt; Project Settings**, then select **Preset Manager**).

The following image shows the Preset Manager with the included presets set as default for their associated behavior. For the presets which depend on the hand of the controller, a Filter value of **Left** and **Right** is set to allow the appropriate preset to be chosen based on the name of the GameObject.

![preset-manager](images/preset-manager.png)

## XR Device Simulator

This sample is installed into the default location for package samples, in the `Assets\Samples\XR Interaction Toolkit\[version]\XR Device Simulator` folder. You can move these Assets to a different location.

The XR Interaction Toolkit package provides an example implementation of an XR Device Simulator to allow for manipulating an HMD and a pair of controllers using mouse and keyboard input. This sample contains example bindings for use with that simulator, and a Prefab which you can add to your Scene to quickly start using the simulator.

|**Asset**|**Description**|
|---|---|
|`XR Device Simulator Controls.inputactions`|Asset that contains actions with default bindings for use with the XR Device Simulator.|
|`XR Device Simulator.prefab`|Prefab with the XR Device Simulator component with references to actions configured, and an Input Action Manager component to enable the actions.|

### Input Actions Asset

The following image shows the [Action editor](https://docs.unity3d.com/Packages/com.unity.inputsystem@1.0/manual/ActionAssets.html#editing-input-action-assets) of the Asset included in the sample, while editing one of the bindings of an action in one of the maps.

![sample-device-simulator-controls-asset](images/sample-device-simulator-controls-asset.png)

## Document revision history

|Date|Reason|
|---|---|
|October 20, 2020|Document created. Matches package version 0.10.0.|
