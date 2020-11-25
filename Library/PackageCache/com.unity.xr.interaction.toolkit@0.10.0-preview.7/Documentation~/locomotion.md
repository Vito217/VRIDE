
# Locomotion

The XR Interaction Toolkit package provides a set of locomotion primitives that offer the means to move about a Scene during an XR experience. These components are:
- An XR Rig that represents the user
- A Locomotion System that control access to the XR Rig
- A teleportation system with teleportation destinations
- A Snap Turn Provider that rotates the rig by fixed angles
- A Continuous Turn Provider that smoothly rotates the rig over time
- A Continuous Move Provider that smoothly moves the rig over time

This documentation outlines how to use and extend these components.

## Glossary

| **Term** | **Meaning** |
|-|-|
| **XR Rig** | `MonoBehaviour` that specifies a Rig, a Camera Floor Offset Object, or a Camera. Also provides Stationary or Room-Scale options of tracking space to configure the XR Rig. |
| **Rig** | The base GameObject of the XR Rig. This is the GameObject that the application will manipulate via locomotion. By default, the Rig is the GameObject that the XR Rig is attached to.|
| **Camera Floor Offset Object** | GameObject to move the Camera to the desired height off the floor. |
| **Camera Object** | GameObject that contains a Camera component. This is usually the main Camera that renders what the user sees, and is usually the "head" of XR rigs. |
| **Room-Scale** | A floor-relative tracking mode. When the Scene starts, the origin is the floor. |
| **Stationary** | A device-relative tracking mode. When the Scene starts, the origin is the device, and the Camera moves to the height set by Camera Floor Offset Object. |
| **Locomotion System** | `MonoBehaviour` that controls which Locomotion Provider can move the Rig. |
| **Locomotion Provider** | Base class for various locomotion implementations. |
| **Teleportation** | A type of locomotion that teleports the rig from one position to another position. |
| **Snap Turn** | A type of locomotion that rotates the rig by a fixed angle. |
| **Continuous Turn** | A type of locomotion that smoothly rotates the rig by an amount over time. |
| **Continuous Move** | A type of locomotion that smoothly moves the rig by an amount over time. |

## Set up a basic Scene for Teleportation and Snap Turn

Before you follow the steps below, to streamline setup of Action-based behaviors, it is recommended that you install the [Default Input Actions](samples.md#default-input-actions) sample and follow the steps for [configuring Preset Manager defaults](samples.md#configuring-preset-manager-defaults) to reduce the burden of configuring the Actions when using Action-based behaviors. Using Action-based behaviors is recommended; for more information, see [Action-based vs. Device-based behaviors](index.md#action-based-vs.-device-based-behaviors).

### 1. Set up the XR Rig 

To set up an XR Rig, use one of the helper commands (**Room-Scale XR Rig** or **Stationary XR Rig**) from the **GameObject &gt; XR** menu. This creates an XR Rig GameObject in the Scene and sets the **Tracking Origin Mode** to **Floor** or **Device**, respectively. The command also creates an Interaction Manager if there isn't one in the Scene.

This also creates two child GameObjects with an XR Controller on each to represent the motion controllers. For Action-based controllers, the Actions that you assign should use either the **XR Controller (LeftHand)** or **XR Controller (RightHand)** binding paths. For Device-based controllers, the **Controller Node** is set to **Left Hand** and **Right Hand** automatically.

![gameobject-xr-menu](images/gameobject-xr-menu.png)

### 2. Add a Locomotion System with teleportation and snap turning

On the **XR Rig** GameObject, add a **Locomotion System**, a **Teleportation Provider**, and a **Snap Turn Provider**.

To set up snap turn, you need to configure the [Snap Turn Provider](#snap-turn-provider) in the Inspector.

For Action-based, set **Left Hand Snap Turn Action** and/or **Right Hand Snap Turn Action** to Vector 2 Control Type Actions with Bindings for your desired inputs.

For Device-based, set the **Controllers** list to contain one or both XR Controller objects created in the previous step for the controllers whose input should trigger a snap turn. For a controller with a joystick, the **Turn Input Source** field should be **Primary 2D Axis**.

![locomotion-setup-xr-rig-components](images/locomotion-setup-xr-rig-components.png)

### 3. Create teleportation Interactables

From Unity's main menu, click **GameObject &gt; XR &gt; Teleportation Area** or **GameObject &gt; XR &gt; Teleportation Anchor** to create a plane that can be teleported to. Teleportation Area is a teleportation destination which teleports the user to their pointed location on a surface, whereas a Teleportation Anchor is a teleportation destination which teleports the user to a pre-determined specific position and/or rotation.

If you followed steps 1-3, you should have a basic Scene with the ability to perform teleportation and snap turn with your controllers. The following steps offer additional configuration details for changing the appearance of the line of the XR Ray Interactor.

### 4. XR Ray Interactor line types

The XR Ray Interactor provides three default line types that can be used to select interactables in the Scene:

* **Straight Line**
* **Projectile Curve**
* **Bezier Curve**

These options are described below.

#### Straight Line

![raycast-configuration-straight-line](images/raycast-configuration-straight-line.png)

If you select the **Straight Line** option, the XR Ray Interactor performs a single raycast into the Scene with a ray length set by the **Max Raycast Distance** property. The image above shows the configuration options.

| **Property** | **Description** |
|-|-|
| **Max Raycast Distance** | The distance to be raycast into the Scene. |

#### Projectile Curve

If you select the **Projectile Curve** option, the XR Ray Interactor samples the trajectory of a projectile to generate a projectile curve. You can use the angle of the Controller to control the distance of the landing point. When you lift your Controller, the landing point first goes further away, then comes closer if you keep lifting the Controller.

The **Projectile Curve** option is recommended for use in teleportation scenarios.

![raycast-configuration-projectile-curve](images/raycast-configuration-projectile-curve.png)

| **Property** | **Description** |
|-|-|
| **Reference Frame** | The reference frame of the projectile. If you don't set this, the XR Ray Interactor attempts to use the local XR Rig, which makes the curve always go up then down in the tracking space. If the XR Rig doesn't exist, the curve rotates with the Controller. |
| **Velocity** | Initial velocity of the projectile. Increase this value to make the curve reach further. |
| **Acceleration** | Gravity of the projectile in the reference frame. |
| **Additional FlightTime** | Additional flight time after the projectile lands. Increase this value to make the endpoint drop lower in height. |
| **Sample Frequency** | The number of sample points of the curve. Higher numbers offer better better quality. |

#### Bezier Curve

![raycast-configuration-bezier-curve](images/raycast-configuration-bezier-curve.png)

In addition to its start point, the **Bezier Curve** uses a control point and an end point. The start point is the position of the **Attach Transform** of the **XR Ray Interactor**. Both the **Projectile Curve** and the **Bezier Curve** use the reference frame of the Rig unless otherwise set by the user.

| **Property** | **Description** |
|-|-|
| **End Point Distance** | Define how far away the end point is from the start point. Increase the value to increase the distance. |
| **End Point Height** | Define how high the end point is in relation to the start point. Increase this value to increase the height. |
| **Control Point Distance** | Define how far away the peak of the curve is from the start point. Increase this value to increase the distance. |
| **Control Point Height** | Define how high the peak of the curve is in relation to the start point. Increase this value to increase the height. |
| **Sample Frequency** | Define the number of sample points the curve has. Higher numbers offer better quality. |

### 5. Set line visual

The XR Interactor Line Visual gives you additional options to customize the appearance of your line for teleportation. It requires the [Line Renderer](https://docs.unity3d.com/Manual/class-LineRenderer.html) component and gets line points from the XR Ray Interactor.

![xr-interactor-line-visual](images/xr-interactor-line-visual.png)

| **Property** | **Description** |
|-|-|
| **Line Width** | The width of the line, in centimeters. |
| **Width Curve** | The relative width of the line from start to end. |
| **Valid Color Gradient** | When the line hits any collider of a valid target, the line changes to this color gradient. |
| **Invalid Color Gradient** | When there is no valid place to teleport, the line changes to this color gradient. |
| **Override Line Length** | If you enable this option, the line visual can have a different length from the underlying raycast. |
| **Line Length** | If you enable the **Override Line Length option**, this field controls the visual line length. The visual line length can't be longer than the raycast distance. |
| **Smooth Movement** | If enabled, the rendered line is delayed from and smoothly follows the raycast line. |
| **Reticle** | GameObject to visualize the destination of Teleportation. |

## Architecture

The Locomotion System is responsible for managing one XR Rig. The XR Rig handles the user's position in Unity world space. The Locomotion System can restrict access to the XR Rig while Locomotion Providers are moving it.

For example, at the request of the Teleportation Provider, the Locomotion System locks the XR Rig while in a Teleport action. This ensures that the user can't do another action, such as snap turning or teleporting again, while the current action is active.

After the Teleport has finished, the Teleportation Provider relinquishes the exclusive lock on the system and allows other Locomotion Providers to influence the XR Rig.

Locomotion Providers can modify the XR Rig without taking exclusive access if necessary. However, before you give a Locomotion Provider non-exclusive access to the XR Rig, you should always check to see if the Locomotion System is busy before it makes any changes to the XR Rig.

The overall flow of a Locomotion request is as follows:

1. The Locomotion Provider checks to see if the Locomotion System is currently busy.
2. If not, the Locomotion Provider requests exclusive access to the Locomotion System.
3. If the request is successful, the Locomotion Provider moves the XR Rig.
4. When the Locomotion Provider has finished modifying the user's position and/or rotation, the Locomotion Provider relinquishes exclusive access to the Locomotion System.

If the Locomotion System is busy, or the Locomotion Provider is unable to gain exclusive access to the Locomotion System, the Locomotion Provider shouldn't modify the Locomotion System's XR Rig. 

### XR Rig

The Locomotion System uses the XR Rig as the anchor for the user. 

Before detailing the options on the XR Rig component, it's important to understand the recommended hierarchy of GameObjects to support Interaction.

The image below shows the XR Rig component. 

![xr-rig](images/xr-rig.png)

| **Property** | **Description** |
|-|-|
|**Rig Base Game Object**|Indicates which GameObject acts as the Transform from tracking space into world space. In the recommended hierarchy, this is the "XR Rig" GameObject.|
|**Camera Floor Offset Object**|Sets which GameObject has a vertical offset applied if the device tracking origin doesn't contain the user's height.|
|**Camera Game Object**|Indicates which GameObject holds the user's camera. This is important because the user's camera might not be at the origin of the tracking volume. In the suggested hierarchy, this is the "Camera" GameObject.|
|**Tracking Space**|Sets the desired tracking space used by the application.|
|**Camera Y Offset**| Number of world space units by which the GameObject specified by the **Camera Floor Offset Object** is moved up vertically if the device tracking origin doesn't contain the user's height.|

### Locomotion System

The Locomotion System is a `MonoBehaviour` that acts as the arbitrator for Locomotion Provider access to an XR Rig. 

The following is an image of the Locomotion System component:

![locomotion-system](images/locomotion-system.png)

| **Property** | **Description** |
|-|-|
|**Timeout**|Controls the maximum amount of time a single Locomotion Provider can keep exclusive access of the Locomotion System. By default, the value is set to 10 seconds.|
|**XR Rig**|Select which XR Rig this Locomotion System will control. You can have as many Locomotion Systems and XR Rigs in your Scene as necessary. By default, it will find the object of type XR Rig in the Scene.|

As a best practice, the Locomotion System should be located on the XR Rig GameObject. For more information, see the recommended hierarchy setup for interaction.

### Locomotion Providers

Locomotion Providers implement different types of locomotion. The package supplies multiple Locomotion Providers: the [Teleportation Provider](#teleportation-provider), the [Snap Turn Provider](#snap-turn-provider), the [Continuous Turn Provider](#continuous-turn-provider), and the [Continuous Move Provider](#continuous-move-provider), all of which implement the `LocomotionProvider` abstract class. These are discussed in more detail in the sections below.

The `LocomotionProvider` class provides a simple interface to request and relinquish exclusive access to the configured Locomotion System. If no `LocomotionSystem` class is configured, the Locomotion Provider attempts to find a Locomotion System in the current Scene(s).

To request exclusive access to the Locomotion System, use the `BeginLocomotion` method. To relinquish access to the Locomotion System, use the `EndLocomotion` method. The implementation of Locomotion Provider must call these methods as appropriate, and relinquish its access when it has finished interacting with the Locomotion System. 

Use the `CanBeginLocomotion` method to check if the Locomotion System is currently in exclusive mode before attempting to call `BeginLocomotion` to acquire it. 

The `LocomotionProvider` abstract class also providers two events:
* `startLocomotion` is invoked on a successful call to `BeginLocomotion`.
* `endLocomotion` is invoked on a successful call to `EndLocomotion`.

### Teleportation

The package provides a simple implementation of teleportation that also demonstrates how to implement complex locomotion scenarios using the `LocomotionProvider` and `LocomotionSystem` interfaces.

The Teleportation Provider inherits from the `LocomotionProvider` abstract class. The Teleportation Provider is responsible for moving the Rig to the desired location on the user's request.

This implementation has two types of teleportation destinations: an 'Anchor'-based teleportation destination, and an 'Area'-based teleportation destination. These are discussed in more detail below. In short:

- Anchors teleport the user to a pre-determined specific position and/or rotation that they specify.
- Areas allow the player to choose a location on a surface that they wish to teleport to.

Both types of teleportation destinations are implemented on top of the XR Interaction system using the `BaseTeleportationInteractable` as the starting point for shared code.

The XR Interaction system also provides various line rendering options. For more information, see documentation for the [Interaction Package](index.md).

#### Teleportation Provider

The Teleportation Provider Component implements the `LocomotionProvider` abstract class. You can have as many instances of the Teleportation Provider Component in your Scene as you need. However, in most cases, a single instance is enough. As a best practice, place this instance on the XR Rig GameObject.

The following image shows the Teleportation Provider MonoBehaviour.

![teleportation-provider](images/teleportation-provider.png)

The **System** field should reference the Locomotion System MonoBehaviour that you want the teleportation provider to interact with. If you don't specify a Locomotion System, the provider attempts to find one in the current Scene.

#### Teleportation Area Interactable

The Teleportation Area Interactable is a specialization of the `BaseTeleportInteractable` class. It allows the user to select any location on the surface as their destination.

The Teleportation Area Interactable is intended to be used by the Ray Interactor or any of its specializations. It uses the intersection point of the ray and the area's collision volume to determine the location that the user wants to teleport to. The Teleportation Area Interactable has a specialized implementation of the `GenerateTeleportRequest` method, which generates a teleportation request that is queued with the Teleportation Provider.

The following image shows an example of a portion of the Teleportation Area Interactable as it appears in the Inspector:

![teleportation-area](images/teleportation-area.png)

The properties on the Teleportation Area Interactable are similar to other Interactables. The table below only covers the elements that support teleportation.

| **Property** | **Description** |
|-|-|
|**Match Orientation** |Specifies how to orient the rig after teleportation. You can choose from the following options:<br/><ul><li>**World Space Up** to stay oriented according to the world space up vector.</li><li>**Target Up** to orient according to the target `BaseTeleportationInteractable` Transform's up vector.</li><li>**Target Up And Forward** to orient according to the target `BaseTeleportationInteractable` Transform's rotation.</li><li>**None** to maintain the same orientation before and after teleporting.</li></ul>|
|**Teleport Trigger**|Specifies whether the teleportation triggers when the user enters or exits the selection.
|**Teleportation Provider** |Indicates which Teleportation Provider this Interactable communicates with. If a Teleportation Provider is not configured, the Interactable attempts to find a Teleportation Provider in the current Scene(s).|

**Match Orientation** is used to specify how the rotation of the rig changes when teleporting.
- If your application does not rotate the rig in any way, and you always want the rig's up vector to match World Space's Up vector, use the **World Space Up** option. 
- If you want the user to be able to stand on a ceiling, wall, or other tilted surface, and have them rotate to match so that the ceiling or wall feels like their new floor, select **Target Up** instead. The rig will match the up vector of the Transform that the Teleportation Area component is attached to.
- If you want to point the user in a very specific direction when they arrive at a target, select **Target Up And Forward**. This will match the rig's rotation to the exact rotation of the Transform that a Teleportation Area is attached to.
- If you do not want a teleport to change the rotation in any way, and you want the user to retain the same rotation before and after a teleport, select **None**.  If your entire application is oriented at a 45 degree angle, for instance, you can rotate the Rig's root Transform and set all teleport targets to `MatchOrientation.None`.

#### Teleportation Anchor Interactable

The Teleportation Anchor is a specialization of the `BaseTeleportInteractable` class that allows the user to teleport to an anchor location by selecting the anchor or an area around it.

The Teleportation Anchor Interactable is intended to be used by the Ray Interactor or any of its specializations. It uses the intersection point of the ray and the area's collision volume to determine the location that the user wants to teleport to. The Teleportation Anchor Interactable has a specialized implementation of the `GenerateTeleportRequest` method, which generates a teleportation request that is queued with the Teleportation Provider.

The following image shows an example of a portion of the Teleportation Anchor Interactable as it appears in the Inspector:

![teleportation-anchor](images/teleportation-anchor.png)

The properties on the **Teleportation Anchor** Interactable are similar to the **Teleportation Area** Interactable. This documentation only covers new elements.

The **Teleport Anchor Transform** field defines the transform that the Rig teleports to when the user teleports to this anchor. It uses both the position and the rotation of the anchor, depending on which **Match Orientation** is selected.

### Snap Turn Provider

The package provides an example implementation of a Snap Turn Provider. A snap turn means the Rig rotates by a fixed amount when the application receives a configured input (for example, a joystick is moved to the left, or a D-pad is pressed to the right).

It is recommended that you use the Action-based variant instead of the Device-based variant to take advantage of the benefits that the Input System package provides.

#### Action-based

The following image shows an example of the Snap Turn Provider (Action-based).

![snap-turn-provider-action-based](images/snap-turn-provider-action-based.png)

|**Property**|**Description**|
|---|---|
|**System**|The Locomotion System that this locomotion provider will communicate with for exclusive access to an XR Rig. If one is not provided, the system will attempt to locate one during its `Awake` call.|
|**Turn Amount**|Specify by how many degrees the Rig will rotate around the Y axis during each snap turn.|
|**Debounce Time**|Specify how much time must pass after a successful snap turn before the user can trigger a second snap turn.|
|**Enable Turn Left Right**|Controls whether to enable left and right snap turns.|
|**Enable Turn Around**|Controls whether to enable 180° snap turns.|
|**Left Hand Snap Turn Action**|The Action that will be used to read input from the left hand controller.|
|**Right Hand Snap Turn Action**|The Action that will be used to read input from the right hand controller.|

#### Device-based

The following image shows an example of the Snap Turn Provider (Device-based).

![snap-turn-provider-device-based](images/snap-turn-provider-device-based.png)

|**Property**|**Description**|
|---|---|
|**System**|The Locomotion System that this locomotion provider communicates with for exclusive access to an XR Rig. If none is provided, the behavior will attempt to locate one during its `Awake` call.|
|**Turn Input Source**|The 2D Input Axis on the controller devices that will be used to trigger a snap turn.|
|**Controllers**|Each element in the controllers list is a reference to an XR Controller that provides device inputs to trigger snap turning.|
|**Turn Amount**|Specify by how many degrees the Rig will rotate around the Y axis during each snap turn.|
|**Dead Zone**| The controller needs to move more than the amount you specify in this field to be able to trigger a snap turn.|
|**Enable Turn Left & Right**|Controls whether to enable left and right snap turns.|
|**Enable Turn Around**|Controls whether to enable 180° snap turns.|
|**Activation Timeout**|Specify how much time must pass after a successful snap turn before the user can trigger a second snap turn.|

### Continuous Turn Provider

The package provides an example implementation of a Continuous Turn Provider. Continuous turning, as opposed to snap turning by discrete angles, smoothly rotates the Rig by an amount over time when the application receives a configured input (for example, a joystick is tilted to the right).

It is recommended that you use the Action-based variant instead of the Device-based variant to take advantage of the benefits that the Input System package provides.

#### Action-based

The following image shows an example of the Continuous Turn Provider (Action-based).

![continuous-turn-provider-action-based](images/continuous-turn-provider-action-based.png)

|**Property**|**Description**|
|---|---|
|**System**|The Locomotion System that this locomotion provider will communicate with for exclusive access to an XR Rig. If one is not provided, the system will attempt to locate one during its `Awake` call.|
|**Turn Speed**|The number of degrees/second clockwise to rotate when turning clockwise.|
|**Left Hand Turn Action**|The Action used to read input from the left hand controller.|
|**Right Hand Turn Action**|The Action used to read input from the right hand controller.|

#### Device-based

The following image shows an example of the Continuous Turn Provider (Device-based).

![continuous-turn-provider-device-based](images/continuous-turn-provider-device-based.png)

|**Property**|**Description**|
|---|---|
|**System**|The Locomotion System that this locomotion provider will communicate with for exclusive access to an XR Rig. If one is not provided, the system will attempt to locate one during its `Awake` call.|
|**Turn Speed**|The number of degrees/second clockwise to rotate when turning clockwise.|
|**Input Binding**|The 2D Input Axis on the controller devices that will be used to trigger turning.|
|**Controllers**|Each element in the controllers list is a reference to an XR Controller that provides device inputs to trigger turning.|
|**Deadzone Min**|Value below which input values will be clamped. After clamping, values will be renormalized to [0, 1] between min and max.|
|**Deadzone Max**|Value above which input values will be clamped. After clamping, values will be renormalized to [0, 1] between min and max.|

### Continuous Move Provider

The package provides an example implementation of a Continuous Move Provider. Continuous moving, as opposed to teleporting, smoothly translates the Rig by an amount over time when the application receives a configured input (for example, a joystick is tilted forward).

The **Forward Source** can be used to define which direction the Rig should move when, for example, pushing forward on a joystick. By default, it will use the Camera Object, meaning the user will move forward in the direction they are facing. An example of how this property can be used is to set it to a Transform that tracks the pose of a motion controller to allow the user to move forward in the direction they are holding the controller.

If a [Character Controller](https://docs.unity3d.com/Manual/class-CharacterController.html) is present on the Rig, this Continuous Move Provider will move the Rig using [`CharacterController.Move`](https://docs.unity3d.com/ScriptReference/CharacterController.Move.html) rather than directly translating the Transform of the Rig.

It is recommended that you use the Action-based variant instead of the Device-based variant to take advantage of the benefits that the Input System package provides.

#### Action-based

The following image shows an example of the Continuous Move Provider (Action-based).

![continuous-move-provider-action-based](images/continuous-move-provider-action-based.png)

|**Property**|**Description**|
|---|---|
|**System**|The Locomotion System that this locomotion provider will communicate with for exclusive access to an XR Rig. If one is not provided, the system will attempt to locate one during its `Awake` call.|
|**Move Speed**|The speed, in units per second, to move forward.|
|**Enable Strafe**|Controls whether to enable strafing (sideways movement).|
|**Use Gravity**|Controls whether gravity affects this provider when a Character Controller is used.|
|**Gravity Application Mode**|Controls when gravity begins to take effect.|
|**Forward Source**|The source Transform to define the forward direction.|
|**Left Hand Move Action**|The Action that will be used to read input from the left hand controller.|
|**Right Hand Move Action**|The Action that will be used to read input from the right hand controller.|

#### Device-based

The following image shows an example of the Continuous Move Provider (Device-based).

![continuous-move-provider-device-based](images/continuous-move-provider-device-based.png)

|**Property**|**Description**|
|---|---|
|**System**|The Locomotion System that this locomotion provider will communicate with for exclusive access to an XR Rig. If one is not provided, the system will attempt to locate one during its `Awake` call.|
|**Move Speed**|The speed, in units per second, to move forward.|
|**Enable Strafe**|Controls whether to enable strafing (sideways movement).|
|**Use Gravity**|Controls whether gravity affects this provider when a Character Controller is used.|
|**Gravity Application Mode**|Controls when gravity begins to take effect.|
|**Forward Source**|The source Transform to define the forward direction.|
|**Input Binding**|The 2D Input Axis on the controller devices that will be used to trigger moving.|
|**Controllers**|Each element in the controllers list is a reference to an XR Controller that provides device inputs to trigger moving.|
|**Deadzone Min**|Value below which input values will be clamped. After clamping, values will be renormalized to [0, 1] between min and max.|
|**Deadzone Max**|Value above which input values will be clamped. After clamping, values will be renormalized to [0, 1] between min and max.|

#### Character Controller Driver

You can use the Character Controller Driver to drive the height of a Character Controller on the Rig upon locomotion events emitted by, for example, a Continuous Move Provider. This can allow for the capsule collider of the Rig (that is, the user) to be automatically resized when the user crouches down or stands up and tries to move with a joystick. This can be useful, together with other Collider objects, to constrain the user from moving forward unless their head would be lower than an obstacle, for instance.

Use the **Min Height** and **Max Height** properties to clamp the height of the Character Controller that this behavior sets, in order to prevent unwanted extremes. It might be useful to set an upper limit as a quality of life improvement for extremely tall users so the Character Controller can fit through the Scene at a typical standing height without forcing the user to lower their head.

The following image shows an example of the Character Controller Driver.

![character-controller-driver](images/character-controller-driver.png)

|**Property**|**Description**|
|---|---|
|**Locomotion Provider**|The Locomotion Provider object to listen to.|
|**Min Height**|The minimum height of the character's capsule that this behavior will set.|
|**Max Height**|The maximum height of the character's capsule that this behavior will set.|

## Document revision history

|Date|Reason|
|---|---|
|October 20, 2020|Added continuous locomotion and updated for Inspector changes. Matches package version 0.10.0.|
|January 10, 2020|Documentation fixes, adds revision history.|
