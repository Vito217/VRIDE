using UnityEngine;
using UnityEngine.EventSystems;

namespace ViveHandTracking {

enum HandInputType { Auto, Left, Right }

[RequireComponent(typeof(Camera))]
[RequireComponent(typeof(PhysicsRaycaster))]
abstract class BaseHandInput : MonoBehaviour {
  [Tooltip("Select which hand to use as raycast input. Auto selects first hand visible.")]
  public HandInputType inputType;
  [Tooltip("Hand state for left hand, state 1 means clicking. Optional, use hand.isPinching if null.")]
  public HandStateChecker leftHandState = null;
  [Tooltip("Hand state for right hand, state 1 means clicking. Optional, use hand.isPinching if null.")]
  public HandStateChecker rightHandState = null;

  public Camera eventCamera {
    get;
    private set;
  }
  public PhysicsRaycaster pointerPhysicsRaycaster {
    get;
    private set;
  }
  public Vector2 centerOfScreen {
    get;
    private set;
  }
  public bool visible {
    get;
    private set;
  }
  public bool clicking {
    get;
    private set;
  }
  private bool useRightHand = true;

  protected virtual void Awake() {
    eventCamera = gameObject.GetComponent<Camera>();
    eventCamera.stereoTargetEye = StereoTargetEyeMask.None;
    eventCamera.enabled = false;
    centerOfScreen = new Vector2(eventCamera.pixelWidth, eventCamera.pixelHeight) / 2;

    pointerPhysicsRaycaster = gameObject.GetComponent<PhysicsRaycaster>();

    // validation
    var module = Object.FindObjectOfType<HandInputModule>();
    if (module == null) {
      Debug.Log("Using hand input without HandInputModule, creating...");
      if (EventSystem.current == null) {
        var go = new GameObject("EventSystem");
        go.AddComponent<EventSystem>();
        Debug.Log("Added EventSystem game object");
      }
      module = EventSystem.current.gameObject.AddComponent<HandInputModule>();
    }
    if (module.handInput == null) {
      Debug.Log("Add " + name + " to HandInputModule");
      module.handInput = this;
    }
  }

  protected void Update() {
    var hand = GetHand();
    if (hand == null) {
      visible = false;
      return;
    }
    visible = true;
    eventCamera.transform.position = hand.pinch.pinchStart;
    eventCamera.transform.rotation = hand.pinch.pinchRotation;
    var state = useRightHand ? rightHandState : leftHandState;
    clicking = state != null ? state.state == 1 : hand.pinch.isPinching;
  }

  // This is called every frame to display graphics.
  // The start point of ray is transform.position, direction of ray is transform.forward
  // Parameter: distance from ray start point to hit point, null if no hit.
  public abstract void SetHit(float? distance);

  private GestureResult GetHand() {
    if (inputType == HandInputType.Left) {
      useRightHand = false;
      return GestureProvider.LeftHand;
    } else if (inputType == HandInputType.Right) {
      useRightHand = true;
      return GestureProvider.RightHand;
    }
    var hand = useRightHand ? GestureProvider.RightHand : GestureProvider.LeftHand;
    if (hand == null) {
      hand = useRightHand ? GestureProvider.LeftHand : GestureProvider.RightHand;
      if (hand != null)
        useRightHand = !useRightHand;
    }
    return hand;
  }
}

}
