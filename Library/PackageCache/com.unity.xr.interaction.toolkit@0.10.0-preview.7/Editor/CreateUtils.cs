using System;
using System.ComponentModel;
using JetBrains.Annotations;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using UnityEngine.SpatialTracking;
using UnityEngine.XR;
using UnityEditor.SceneManagement;
using UnityEngine.InputSystem;
using UnityEngine.Rendering;
using UnityEngine.XR.Interaction.Toolkit;
using UnityEngine.XR.Interaction.Toolkit.UI;
using Object = UnityEngine.Object;

#if AR_FOUNDATION_PRESENT
using UnityEngine.XR.ARFoundation;
using UnityEngine.XR.Interaction.Toolkit.AR;
#endif

namespace UnityEditor.XR.Interaction.Toolkit
{
    static class CreateUtils
    {
        enum InputType
        {
            ActionBased,
            DeviceBased,
        }

        const string k_LineMaterial = "Default-Line.mat";
        const string k_UILayerName = "UI";

        [MenuItem("GameObject/XR/Ray Interactor (Action-based)", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateRayInteractorActionBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            CreateRayInteractor(menuCommand.GetContextTransform(), InputType.ActionBased);
        }

        [MenuItem("GameObject/XR/Device-based/Ray Interactor", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateRayInteractorDeviceBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            CreateRayInteractor(menuCommand.GetContextTransform(), InputType.DeviceBased);
        }

        [MenuItem("GameObject/XR/Direct Interactor (Action-based)", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateDirectInteractorActionBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            CreateDirectInteractor(menuCommand.GetContextTransform(), InputType.ActionBased);
        }

        [MenuItem("GameObject/XR/Device-based/Direct Interactor", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateDirectInteractorDeviceBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            CreateDirectInteractor(menuCommand.GetContextTransform(), InputType.DeviceBased);
        }

        [MenuItem("GameObject/XR/Socket Interactor", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateSocketInteractor(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            var socketInteractableGO = CreateAndPlaceGameObject("Socket Interactor", menuCommand.GetContextTransform(),
                typeof(SphereCollider),
                typeof(XRSocketInteractor));

            var sphereCollider = socketInteractableGO.GetComponent<SphereCollider>();
            Undo.RecordObject(sphereCollider, "Configure Sphere Collider");
            sphereCollider.isTrigger = true;
            sphereCollider.radius = 0.1f;
        }

        [MenuItem("GameObject/XR/Grab Interactable", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateGrabInteractable(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            var grabInteractableGO = CreateAndPlaceGameObject("Grab Interactable", menuCommand.GetContextTransform(),
                typeof(XRGrabInteractable),
                typeof(SphereCollider));

            var sphereCollider = grabInteractableGO.GetComponent<SphereCollider>();
            Undo.RecordObject(sphereCollider, "Configure Sphere Collider");
            sphereCollider.isTrigger = false;
            sphereCollider.radius = 0.1f;
        }

        [MenuItem("GameObject/XR/Interaction Manager", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateInteractionManager(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager(menuCommand.GetContextTransform());
        }

        [MenuItem("GameObject/XR/Room-Scale XR Rig (Action-based)", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateRoomScaleXRRigActionBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateRoomScaleXRRig(menuCommand.GetContextTransform(), InputType.ActionBased);
        }

        [MenuItem("GameObject/XR/Device-based/Room-Scale XR Rig", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateRoomScaleXRRigDeviceBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateRoomScaleXRRig(menuCommand.GetContextTransform(), InputType.DeviceBased);
        }

        [MenuItem("GameObject/XR/Stationary XR Rig (Action-based)", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateStationaryXRRigActionBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateStationaryXRRig(menuCommand.GetContextTransform(), InputType.ActionBased);
        }

        [MenuItem("GameObject/XR/Device-based/Stationary XR Rig", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateStationaryXRRigDeviceBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateStationaryXRRig(menuCommand.GetContextTransform(), InputType.DeviceBased);
        }

        [MenuItem("GameObject/XR/Locomotion System (Action-based)", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateLocomotionSystemActionBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateLocomotionSystem(menuCommand.GetContextTransform(), InputType.ActionBased);
        }

        [MenuItem("GameObject/XR/Device-based/Locomotion System", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateLocomotionSystemDeviceBased(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateLocomotionSystem(menuCommand.GetContextTransform(), InputType.DeviceBased);
        }

        [MenuItem("GameObject/XR/Teleportation Area", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateTeleportationArea(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            CreateAndPlacePrimitive("Teleportation Area", menuCommand.GetContextTransform(),
                PrimitiveType.Plane,
                typeof(TeleportationArea));
        }

        [MenuItem("GameObject/XR/Teleportation Anchor", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateTeleportationAnchor(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            var anchorGO = CreateAndPlacePrimitive("Teleportation Anchor", menuCommand.GetContextTransform(),
                PrimitiveType.Plane,
                typeof(TeleportationAnchor));

            var destinationGO = ObjectFactory.CreateGameObject("Anchor");
            Place(destinationGO, anchorGO.transform);

            var teleportationAnchor = anchorGO.GetComponent<TeleportationAnchor>();
            Undo.RecordObject(teleportationAnchor, "Configure Teleportation Anchor");
            teleportationAnchor.teleportAnchorTransform = destinationGO.transform;
        }

        [MenuItem("GameObject/XR/UI Canvas", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateXRUICanvas(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            var parentOfNewGameObject = menuCommand.GetContextTransform();

            var editingPrefabStage = (StageUtility.GetCurrentStageHandle() != StageUtility.GetMainStageHandle());

            var canvasGO = CreateAndPlaceGameObject("Canvas", parentOfNewGameObject,
                typeof(Canvas),
                typeof(CanvasScaler),
                typeof(GraphicRaycaster),
                typeof(TrackedDeviceGraphicRaycaster));

            // Either inherit the layer of the parent object, or use the same default that GameObject/UI/Canvas uses.
            if (parentOfNewGameObject == null)
            {
                Undo.RegisterCompleteObjectUndo(canvasGO, "Change Layer");
                canvasGO.layer = LayerMask.NameToLayer(k_UILayerName);
            }

            var canvas = canvasGO.GetComponent<Canvas>();
            Undo.RecordObject(canvas, "Configure Canvas");
            canvas.renderMode = RenderMode.WorldSpace;

            if (!editingPrefabStage)
                canvas.worldCamera = Camera.main;
            else
                Debug.LogWarning("You have just added an XR UI Canvas to a prefab." +
                    $" To function properly with an {nameof(XRRayInteractor)}, you must also set the Canvas component's worldCamera field in your scene.",
                    canvasGO);

            // Ensure there is at least one EventSystem setup properly
            var inputModule = Object.FindObjectOfType<XRUIInputModule>();
            if (inputModule == null)
            {
                if (!editingPrefabStage)
                    CreateXRUIEventSystem(menuCommand);
                else
                    Debug.LogWarning("You have just added an XR UI Canvas to a prefab." +
                        $" To function properly with an {nameof(XRRayInteractor)}, you must also add an XR UI EventSystem to your scene.",
                        canvasGO);
            }

            // May need to set this again since creating the XR UI EventSystem would have overwritten this
            Undo.SetCurrentGroupName("Create " + canvasGO.name);
            Selection.activeGameObject = canvasGO;
        }

        [MenuItem("GameObject/XR/UI EventSystem", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateXRUIEventSystem(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            var currentStage = StageUtility.GetCurrentStageHandle();

            // Ensure there is at least one EventSystem setup properly
            var inputModule = currentStage.FindComponentOfType<XRUIInputModule>();
            if (inputModule == null)
            {
                var eventSystem = currentStage.FindComponentOfType<EventSystem>();
                GameObject eventSystemGO;
                if (eventSystem == null)
                {
                    eventSystemGO = CreateAndPlaceGameObject("EventSystem", menuCommand.GetContextTransform(),
                        typeof(EventSystem),
                        typeof(XRUIInputModule));
                }
                else
                {
                    eventSystemGO = eventSystem.gameObject;

                    // Remove the Standalone Input Module if already implemented, since it will block the XRUIInputModule
                    var standaloneInputModule = eventSystemGO.GetComponent<StandaloneInputModule>();
                    if (standaloneInputModule != null)
                        Undo.DestroyObjectImmediate(standaloneInputModule);

                    Undo.AddComponent<XRUIInputModule>(eventSystemGO);
                }

                inputModule = eventSystemGO.GetComponent<XRUIInputModule>();
            }

            Selection.activeGameObject = inputModule.gameObject;
        }

        static XRRig CreateXRRig(Transform parentOfNewGameObject, InputType inputType)
        {
            var xrCamera = Camera.main;

            // Don't use if the MainCamera is not part of the current stage being edited.
            if (xrCamera != null && !StageUtility.GetCurrentStageHandle().Contains(xrCamera.gameObject))
                xrCamera = null;

            // If the existing MainCamera is already part of an XR Rig,
            // create a new camera instead of trying to reuse it.
            if (xrCamera != null && xrCamera.GetComponentInParent<XRRig>() != null)
                xrCamera = null;

            // If the existing MainCamera is selected, the hierarchy would be invalid
            // since the camera should be a child of the rig.
            if (xrCamera != null && xrCamera.transform == parentOfNewGameObject)
                parentOfNewGameObject = parentOfNewGameObject.parent;

            CreateInteractionManager();

            var xrRigGO = CreateAndPlaceGameObject("XR Rig", parentOfNewGameObject, typeof(XRRig));

            var cameraOffsetGO = ObjectFactory.CreateGameObject("Camera Offset");
            Place(cameraOffsetGO, xrRigGO.transform);

            if (xrCamera == null)
            {
                var xrCameraGO = ObjectFactory.CreateGameObject("Main Camera",
                    typeof(Camera),
                    typeof(AudioListener),
                    GetTrackedPoseDriverType(inputType));
                xrCamera = xrCameraGO.GetComponent<Camera>();
            }

            Undo.RecordObject(xrCamera, "Configure Camera");
            xrCamera.tag = "MainCamera";
            xrCamera.nearClipPlane = 0.01f;
            Place(xrCamera.gameObject, cameraOffsetGO.transform);

            switch (inputType)
            {
                case InputType.ActionBased:
                {
                    var trackedPoseDriver = xrCamera.GetComponent<UnityEngine.InputSystem.XR.TrackedPoseDriver>();
                    if (trackedPoseDriver == null)
                        trackedPoseDriver = Undo.AddComponent<UnityEngine.InputSystem.XR.TrackedPoseDriver>(xrCamera.gameObject);

                    Undo.RecordObject(trackedPoseDriver, "Configure Tracked Pose Driver");
                    trackedPoseDriver.positionAction = new InputAction(null, InputActionType.Value, "<XRHMD>/centerEyePosition", null, null, "Vector3");
                    trackedPoseDriver.rotationAction = new InputAction(null, InputActionType.Value, "<XRHMD>/centerEyeRotation", null, null, "Quaternion");
                    break;
                }
                case InputType.DeviceBased:
                {
                    var trackedPoseDriver = xrCamera.GetComponent<TrackedPoseDriver>();
                    if (trackedPoseDriver == null)
                        trackedPoseDriver = Undo.AddComponent<TrackedPoseDriver>(xrCamera.gameObject);

                    Undo.RecordObject(trackedPoseDriver, "Configure Tracked Pose Driver");
                    trackedPoseDriver.SetPoseSource(TrackedPoseDriver.DeviceType.GenericXRDevice, TrackedPoseDriver.TrackedPose.Center);
                    break;
                }
                default:
                    throw new InvalidEnumArgumentException(nameof(inputType), (int)inputType, typeof(InputType));
            }

            var xrRig = xrRigGO.GetComponent<XRRig>();
            Undo.RecordObject(xrRig, "Configure XR Rig");
            xrRig.cameraFloorOffsetObject = cameraOffsetGO;
            xrRig.cameraGameObject = xrCamera.gameObject;
            return xrRig;
        }

        static XRRig CreateXRRigWithHandControllers(Transform parentOfNewGameObject, InputType inputType)
        {
            var xrRig = CreateXRRig(parentOfNewGameObject, inputType);
            var cameraOffsetTransform = xrRig.cameraFloorOffsetObject.transform;

            var leftHandRayInteractorGO = CreateRayInteractor(cameraOffsetTransform, inputType, "LeftHand Controller");
            var leftHandController = leftHandRayInteractorGO.GetComponent<XRController>();
            if (leftHandController != null)
            {
                Undo.RecordObject(leftHandController, "Configure LeftHand XR Controller");
                leftHandController.controllerNode = XRNode.LeftHand;
            }

            var rightHandRayInteractorGO = CreateRayInteractor(cameraOffsetTransform, inputType, "RightHand Controller");
            var rightHandController = rightHandRayInteractorGO.GetComponent<XRController>();
            if (rightHandController != null)
            {
                Undo.RecordObject(rightHandController, "Configure RightHand XR Controller");
                rightHandController.controllerNode = XRNode.RightHand;
            }

            Place(leftHandRayInteractorGO, cameraOffsetTransform);
            Place(rightHandRayInteractorGO, cameraOffsetTransform);

            // Need to set this again since creating the ray interactors would have overwritten this
            Undo.SetCurrentGroupName("Create " + xrRig.gameObject.name);
            Selection.activeGameObject = xrRig.gameObject;

            return xrRig;
        }

        static void CreateRoomScaleXRRig(Transform parentOfNewGameObject, InputType inputType)
        {
            var xrRig = CreateXRRigWithHandControllers(parentOfNewGameObject, inputType);

            Undo.RecordObject(xrRig, "Configure XR Rig");
            xrRig.trackingOriginMode = TrackingOriginModeFlags.Floor;
        }

        static void CreateStationaryXRRig(Transform parentOfNewGameObject, InputType inputType)
        {
            var xrRig = CreateXRRigWithHandControllers(parentOfNewGameObject, inputType);

            Undo.RecordObject(xrRig, "Configure XR Rig");
            xrRig.trackingOriginMode = TrackingOriginModeFlags.Device;
        }

        /// <summary>
        /// Create the <see cref="XRInteractionManager"/> if necessary, and select it in the Hierarchy.
        /// </summary>
        /// <param name="parent">The parent <see cref="Transform"/> to use.</param>
        static void CreateInteractionManager(Transform parent = null)
        {
            var currentStage = StageUtility.GetCurrentStageHandle();

            var interactionManager = currentStage.FindComponentOfType<XRInteractionManager>();
            if (interactionManager == null)
                CreateAndPlaceGameObject("XR Interaction Manager", parent, typeof(XRInteractionManager));
            else
                Selection.activeGameObject = interactionManager.gameObject;
        }

        static void CreateLocomotionSystem(Transform parent, InputType inputType, string name = "Locomotion System")
        {
            var locomotionSystemGO = CreateAndPlaceGameObject(name, parent,
                typeof(LocomotionSystem),
                typeof(TeleportationProvider),
                GetSnapTurnType(inputType));

            var locomotionSystem = locomotionSystemGO.GetComponent<LocomotionSystem>();

            var teleportationProvider = locomotionSystemGO.GetComponent<TeleportationProvider>();
            Undo.RecordObject(teleportationProvider, "Configure Teleportation Provider");
            teleportationProvider.system = locomotionSystem;

            var snapTurnProvider = locomotionSystemGO.GetComponent<SnapTurnProviderBase>();
            Undo.RecordObject(snapTurnProvider, "Configure Snap Turn Provider");
            snapTurnProvider.system = locomotionSystem;
        }

        static GameObject CreateRayInteractor(Transform parent, InputType inputType, string name = "Ray Interactor")
        {
            var rayInteractableGO = CreateAndPlaceGameObject(name, parent,
                GetControllerType(inputType),
                typeof(XRRayInteractor),
                typeof(LineRenderer),
                typeof(XRInteractorLineVisual));

            SetupLineRenderer(rayInteractableGO.GetComponent<LineRenderer>());

            return rayInteractableGO;
        }

        static GameObject CreateDirectInteractor(Transform parent, InputType inputType, string name = "Direct Interactor")
        {
            var directInteractorGO = CreateAndPlaceGameObject(name, parent,
                GetControllerType(inputType),
                typeof(SphereCollider),
                typeof(XRDirectInteractor));

            var sphereCollider = directInteractorGO.GetComponent<SphereCollider>();
            Undo.RecordObject(sphereCollider, "Configure Sphere Collider");
            sphereCollider.isTrigger = true;
            sphereCollider.radius = 0.1f;

            return directInteractorGO;
        }

        static void SetupLineRenderer(LineRenderer lineRenderer)
        {
            Undo.RecordObject(lineRenderer, "Configure Line Renderer");
            var materials = new Material[1];
            materials[0] = AssetDatabase.GetBuiltinExtraResource<Material>(k_LineMaterial);
            lineRenderer.materials = materials;
            lineRenderer.loop = false;
            lineRenderer.widthMultiplier = 0.005f;
            lineRenderer.startColor = Color.blue;
            lineRenderer.endColor = Color.blue;
            lineRenderer.numCornerVertices = 4;
            lineRenderer.numCapVertices = 4;
            lineRenderer.shadowCastingMode = ShadowCastingMode.Off;
            lineRenderer.receiveShadows = false;
            lineRenderer.useWorldSpace = true;
            lineRenderer.sortingOrder = 5;
        }

        static Type GetControllerType(InputType inputType)
        {
            switch (inputType)
            {
                case InputType.ActionBased:
                    return typeof(ActionBasedController);
                case InputType.DeviceBased:
                    return typeof(XRController);
                default:
                    throw new InvalidEnumArgumentException(nameof(inputType), (int)inputType, typeof(InputType));
            }
        }

        static Type GetSnapTurnType(InputType inputType)
        {
            switch (inputType)
            {
                case InputType.ActionBased:
                    return typeof(ActionBasedSnapTurnProvider);
                case InputType.DeviceBased:
                    return typeof(DeviceBasedSnapTurnProvider);
                default:
                    throw new InvalidEnumArgumentException(nameof(inputType), (int)inputType, typeof(InputType));
            }
        }

        static Type GetTrackedPoseDriverType(InputType inputType)
        {
            switch (inputType)
            {
                case InputType.ActionBased:
                    return typeof(UnityEngine.InputSystem.XR.TrackedPoseDriver);
                case InputType.DeviceBased:
                    return typeof(TrackedPoseDriver);
                default:
                    throw new InvalidEnumArgumentException(nameof(inputType), (int)inputType, typeof(InputType));
            }
        }

        /// <summary>
        /// Gets the <see cref="Transform"/> associated with the <see cref="MenuCommand.context"/>.
        /// </summary>
        /// <param name="menuCommand">The object passed to custom menu item functions to operate on.</param>
        /// <returns>Returns the <see cref="Transform"/> of the object that is the target of a menu command,
        /// or <see langword="null"/> if there is no context.</returns>
        static Transform GetContextTransform(this MenuCommand menuCommand)
        {
            var context = menuCommand.context as GameObject;
#pragma warning disable IDE0031 // Use null propagation -- Do not use for UnityEngine.Object types
            return context != null ? context.transform : null;
#pragma warning restore IDE0031
        }

        static GameObject CreateAndPlaceGameObject(string name, Transform parent, params Type[] types)
        {
            var go = ObjectFactory.CreateGameObject(name, types);

            Place(go, parent);
            Undo.SetCurrentGroupName("Create " + go.name);
            Selection.activeGameObject = go;

            return go;
        }

        static GameObject CreateAndPlacePrimitive(string name, Transform parent, PrimitiveType primitiveType, params Type[] types)
        {
            var go = ObjectFactory.CreatePrimitive(primitiveType);
            go.name = name;
            go.SetActive(false);
            foreach (var type in types)
                ObjectFactory.AddComponent(go, type);
            go.SetActive(true);

            Place(go, parent);
            Undo.SetCurrentGroupName("Create " + go.name);
            Selection.activeGameObject = go;

            return go;
        }

        static void Place(GameObject go, Transform parent)
        {
            var transform = go.transform;

            if (parent != null)
            {
                // Must call RecordObject and reset values before SetTransformParent to ensure the
                // Transform values are correct upon undo in the case that it's a root object.
                // Undo.SetTransformParent did not have parameter worldPositionStays to be able
                // to set false until 2020.2.0a17 (fb# 1247086).
                Undo.RecordObject(transform, "Reset Transform");
                ResetTransform(transform);
                Undo.SetTransformParent(transform, parent, "Reparenting");
                ResetTransform(transform);
                Undo.RegisterCompleteObjectUndo(go, "Change Layer");
                go.layer = parent.gameObject.layer;
            }
            else
            {
                // Puts it at the scene pivot, and otherwise world origin if there is no Scene view.
                var view = SceneView.lastActiveSceneView;
                if (view != null)
                    view.MoveToView(transform);
                else
                    transform.position = Vector3.zero;

                StageUtility.PlaceGameObjectInCurrentStage(go);
            }

            // Only at this point do we know the actual parent of the object and can modify its name accordingly.
            GameObjectUtility.EnsureUniqueNameForSibling(go);
        }

        static void ResetTransform(Transform transform)
        {
            transform.localPosition = Vector3.zero;
            transform.localRotation = Quaternion.identity;
            transform.localScale = Vector3.one;

            if (transform.parent is RectTransform)
            {
                var rectTransform = transform as RectTransform;
                if (rectTransform != null)
                {
                    rectTransform.anchorMin = Vector2.zero;
                    rectTransform.anchorMax = Vector2.one;
                    rectTransform.anchoredPosition = Vector2.zero;
                    rectTransform.sizeDelta = Vector2.zero;
                }
            }
        }

#if AR_FOUNDATION_PRESENT
        [MenuItem("GameObject/XR/AR Gesture Interactor", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateARGestureInteractor(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();

            var originGO = CreateAndPlaceGameObject("AR Session Origin", menuCommand.GetContextTransform(),
                typeof(ARSessionOrigin));
            var cameraGO = CreateAndPlaceGameObject("AR Camera", originGO.transform,
                typeof(Camera),
                typeof(TrackedPoseDriver),
                typeof(ARCameraManager),
                typeof(ARCameraBackground),
                typeof(ARGestureInteractor));

            var camera = cameraGO.GetComponent<Camera>();
            Undo.RecordObject(camera, "Configure AR Camera");
            camera.clearFlags = CameraClearFlags.Color;
            camera.backgroundColor = Color.black;
            camera.nearClipPlane = 0.1f;
            camera.farClipPlane = 20f;

            var origin = originGO.GetComponent<ARSessionOrigin>();
            Undo.RecordObject(origin, "Configure AR Session Origin");
            origin.camera = camera;

            var trackedPoseDriver = cameraGO.GetComponent<TrackedPoseDriver>();
            Undo.RecordObject(trackedPoseDriver, "Configure Tracked Pose Driver");
            trackedPoseDriver.SetPoseSource(TrackedPoseDriver.DeviceType.GenericXRDevice, TrackedPoseDriver.TrackedPose.ColorCamera);
        }
        
        [MenuItem("GameObject/XR/AR Placement Interactable", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateARPlacementInteractable(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();
            
            CreateAndPlaceGameObject("AR Placement Interactable", menuCommand.GetContextTransform(), typeof(ARPlacementInteractable));
        }
        
        [MenuItem("GameObject/XR/AR Selection Interactable", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateARSelectionInteractable(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();
            
            CreateAndPlaceGameObject("AR Selection Interactable", menuCommand.GetContextTransform(), typeof(ARSelectionInteractable));
        }
        
        [MenuItem("GameObject/XR/AR Translation Interactable", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateARTranslationInteractable(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();
            
            CreateAndPlaceGameObject("AR Translation Interactable", menuCommand.GetContextTransform(), typeof(ARTranslationInteractable));
        }
        
        [MenuItem("GameObject/XR/AR Scale Interactable", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateARScaleInteractable(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();
            
            CreateAndPlaceGameObject("AR Scale Interactable", menuCommand.GetContextTransform(), typeof(ARScaleInteractable));
        }
        
        [MenuItem("GameObject/XR/AR Rotation Interactable", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateARRotationInteractable(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();
            
            CreateAndPlaceGameObject("AR Rotation Interactable", menuCommand.GetContextTransform(), typeof(ARRotationInteractable));
        }
        
        [MenuItem("GameObject/XR/AR Annotation Interactable", false, 10), UsedImplicitly]
#pragma warning disable IDE0051 // Remove unused private members -- Editor Menu Item
        static void CreateARAnnotationInteractable(MenuCommand menuCommand)
#pragma warning restore IDE0051
        {
            CreateInteractionManager();
            
            CreateAndPlaceGameObject("AR Annotation Interactable", menuCommand.GetContextTransform(), typeof(ARRotationInteractable));
        }
#endif // AR_FOUNDATION_PRESENT
    }
}
