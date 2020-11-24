using System.Collections;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.UI;

namespace ViveHandTracking {

[HelpURL("https://hub.vive.com/storage/tracking/unity/advanced.html#inputmoduleunity")]
[RequireComponent(typeof(LineRenderer))]
class HandInputRayPointer : BaseHandInput {
  public bool showRay = true;
  public float defaultRayLength = 5;
  public GameObject pointer = null;
  public float pointerSize = 0.003f;
  public Color pointerNormalColor = new Color32(83, 255, 0, 130);
  public Color pointerHighlighColor = new Color32(0, 181, 255, 130);

  private LineRenderer rayRenderer = null;
  private Image image = null;

  protected override void Awake() {
    base.Awake();

    rayRenderer = gameObject.GetComponent<LineRenderer>();
    rayRenderer.useWorldSpace = false;
    if (rayRenderer.positionCount != 2) {
      rayRenderer.positionCount = 2;
      rayRenderer.SetPosition(0, Vector3.zero);
    }
    rayRenderer.enabled = false;
    if (pointer != null) {
      pointer.SetActive(false);
      image = pointer.GetComponentInChildren<Image>();
    }
  }

  IEnumerator Start() {
    while (GestureProvider.Status == GestureStatus.NotStarted)
      yield return null;
    if (!GestureProvider.HaveSkeleton)
      showRay = false;
  }

  public override void SetHit(float? distance) {
    if (distance != null) {
      var position = Vector3.forward * distance.Value;
      rayRenderer.SetPosition(1, position);
      rayRenderer.enabled = showRay;
      if (pointer != null) {
        pointer.transform.localScale = Vector3.one * distance.Value * pointerSize;
        pointer.transform.localPosition = position;
        pointer.SetActive(true);
        if (image != null)
          image.color = clicking ? pointerHighlighColor : pointerNormalColor;
      }
    } else {
      rayRenderer.SetPosition(1, Vector3.forward * defaultRayLength);
      rayRenderer.enabled = showRay && visible;
      if (pointer != null)
        pointer.SetActive(false);
    }
  }
}

}
