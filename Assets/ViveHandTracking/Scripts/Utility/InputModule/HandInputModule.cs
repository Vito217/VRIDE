using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

namespace ViveHandTracking {

[HelpURL("https://hub.vive.com/storage/tracking/unity/advanced.html#inputmoduleunity")]
[DisallowMultipleComponent]
class HandInputModule : PointerInputModule {
  public const int kHandID = -100;

  public BaseHandInput handInput = null;
  [Tooltip("Threshold in meters for drag, estimated by hit point distance between current frame and just pressed")]
  public float dragThreshold = 0.2f;

  private List<PointerInputModule> disabledModules = new List<PointerInputModule>();
  private List<RaycastResult> raycastResults = new List<RaycastResult>();
  private bool lastClicking = false;
  private Vector3 lastHitPoint = Vector3.zero;
  private Vector3 lastDownPoint = Vector3.zero;

  public override bool IsModuleSupported() {
    return GestureProvider.Current != null && handInput != null;
  }

  public override bool ShouldActivateModule() {
    return base.ShouldActivateModule() && GestureProvider.Status == GestureStatus.Running;
  }

  // modified from StandaloneInputModule
  public override void ActivateModule() {
    var toSelect = eventSystem.currentSelectedGameObject;
    if (toSelect == null)
      toSelect = eventSystem.firstSelectedGameObject;
    eventSystem.SetSelectedGameObject(toSelect, GetBaseEventData());

    // disable all other modules to avoid conflict
    foreach (var module in transform.GetComponents<PointerInputModule>()) {
      if (!module.enabled || module == this)
        continue;
      module.enabled = false;
      disabledModules.Add(module);
    }
  }

  public override void DeactivateModule() {
    base.DeactivateModule();
    ClearSelection();
    foreach (var module in disabledModules)
      module.enabled = true;
    disabledModules.Clear();
  }

  public override void Process() {
    PointerEventData eventData;
    var created = GetPointerData(kHandID, out eventData, true);
    eventData.Reset();
    eventData.position = handInput.centerOfScreen;
    eventData.delta = Vector2.one;
    eventData.scrollDelta = Vector2.zero;
    eventData.button = PointerEventData.InputButton.Left;
    eventData.pointerCurrentRaycast = new RaycastResult();

    if (handInput.visible) {
      GraphicRaycast(eventData);
      PhysicsRaycast(eventData);
    }

    if (eventData.pointerCurrentRaycast.gameObject != null)
      handInput.SetHit(eventData.pointerCurrentRaycast.distance);
    else
      handInput.SetHit(null);

    Vector3 hitPoint = handInput.eventCamera.transform.position + handInput.eventCamera.transform.forward *
                       eventData.pointerCurrentRaycast.distance;
    if (!created)
      eventData.delta = new Vector2(Vector3.Distance(lastHitPoint, hitPoint), 0);
    lastHitPoint = hitPoint;

    if (eventSystem.currentSelectedGameObject != null)
      ExecuteEvents.Execute(eventSystem.currentSelectedGameObject, GetBaseEventData(), ExecuteEvents.updateSelectedHandler);
    ProcessPress(eventData);
    ProcessMove(eventData);
    ProcessDrag(eventData);
  }

  // modified from StandaloneInputModule
  private void ProcessPress(PointerEventData eventData) {
    var currentHover = eventData.pointerCurrentRaycast.gameObject;
    var clicking = handInput.clicking && handInput.visible;

    // press down
    if (clicking && !lastClicking) {
      eventData.eligibleForClick = true;
      eventData.delta = Vector2.zero;
      eventData.dragging = false;
      eventData.useDragThreshold = true;
      eventData.pressPosition = eventData.position;
      eventData.pointerPressRaycast = eventData.pointerCurrentRaycast;
      lastDownPoint = lastHitPoint;

      DeselectIfSelectionChanged(currentHover, eventData);

      var newPressed = ExecuteEvents.ExecuteHierarchy(currentHover, eventData, ExecuteEvents.pointerDownHandler);
      if (newPressed == null)
        newPressed = ExecuteEvents.GetEventHandler<IPointerClickHandler>(currentHover);

      float time = Time.unscaledTime;
      if (newPressed == eventData.lastPress) {
        var diffTime = time - eventData.clickTime;
        if (diffTime < 0.3f)
          ++eventData.clickCount;
        else
          eventData.clickCount = 1;
      } else
        eventData.clickCount = 1;

      eventData.pointerPress = newPressed;
      eventData.rawPointerPress = currentHover;
      eventData.clickTime = time;
      eventData.pointerDrag = ExecuteEvents.GetEventHandler<IDragHandler>(currentHover);
      if (eventData.pointerDrag != null)
        ExecuteEvents.Execute(eventData.pointerDrag, eventData, ExecuteEvents.initializePotentialDrag);
    }

    // press up
    if (!clicking && lastClicking) {
      ExecuteEvents.Execute(eventData.pointerPress, eventData, ExecuteEvents.pointerUpHandler);

      var pointerUpHandler = ExecuteEvents.GetEventHandler<IPointerClickHandler>(currentHover);
      if (eventData.pointerPress == pointerUpHandler && eventData.eligibleForClick)
        ExecuteEvents.Execute(eventData.pointerPress, eventData, ExecuteEvents.pointerClickHandler);
      else if (eventData.pointerDrag != null && eventData.dragging)
        ExecuteEvents.ExecuteHierarchy(currentHover, eventData, ExecuteEvents.dropHandler);

      eventData.eligibleForClick = false;
      eventData.pointerPress = null;
      eventData.rawPointerPress = null;
      if (eventData.pointerDrag != null && eventData.dragging)
        ExecuteEvents.Execute(eventData.pointerDrag, eventData, ExecuteEvents.endDragHandler);

      eventData.dragging = false;
      eventData.pointerDrag = null;
      if (currentHover != eventData.pointerEnter) {
        HandlePointerExitAndEnter(eventData, null);
        HandlePointerExitAndEnter(eventData, currentHover);
      }
    }

    lastClicking = clicking;
  }

  protected override void ProcessDrag(PointerEventData pointerEvent) {
    // set useDragThreshold to false only if exceeds drag threshold
    // this starts dragging in base.ProcessDrag
    if (pointerEvent.pointerDrag != null && pointerEvent.useDragThreshold)
      pointerEvent.useDragThreshold = Vector3.Distance(lastHitPoint, lastDownPoint) < dragThreshold;
    base.ProcessDrag(pointerEvent);
  }

  private void GraphicRaycast(PointerEventData eventData) {
    foreach (var raycaster in GameObject.FindObjectsOfType<GraphicRaycaster>()) {
      var canvas = raycaster.GetComponent<Canvas>();
      if (canvas == null)
        continue;
      if (canvas.worldCamera != null && canvas.worldCamera != handInput.eventCamera)
        continue;
      // canvas must use event camera as world camera
      canvas.worldCamera = handInput.eventCamera;

      raycastResults.Clear();
      raycaster.Raycast(eventData, raycastResults);
      if (raycastResults.Count == 0)
        continue;

      var firstRaycastResult = FindFirstRaycast(raycastResults);
      if (firstRaycastResult.gameObject == null)
        continue;
      if (eventData.pointerCurrentRaycast.gameObject == null ||
          eventData.pointerCurrentRaycast.distance > firstRaycastResult.distance ||
          (eventData.pointerCurrentRaycast.distance == firstRaycastResult.distance &&
           eventData.pointerCurrentRaycast.sortingOrder < firstRaycastResult.sortingOrder))
        eventData.pointerCurrentRaycast = firstRaycastResult;
    }
  }

  private void PhysicsRaycast(PointerEventData eventData) {
    raycastResults.Clear();
    handInput.pointerPhysicsRaycaster.Raycast(eventData, raycastResults);
    if (raycastResults.Count == 0)
      return;

    var firstRaycastResult = FindFirstRaycast(raycastResults);
    if (firstRaycastResult.gameObject == null)
      return;
    if (eventData.pointerCurrentRaycast.gameObject == null ||
        eventData.pointerCurrentRaycast.distance > firstRaycastResult.distance)
      eventData.pointerCurrentRaycast = firstRaycastResult;
  }
}

}
