using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking.Sample {

class Grab : MonoBehaviour {
  private static Color enterColor = new Color(0, 0, 0.3f, 1);
  private static Color moveColor = new Color(0, 0.3f, 0, 1);

  private Rigidbody target = null;
  private Transform anchor = null;
  private int state = 0;
  private bool exit = true;

  void Awake() {
    var go = new GameObject("Anchor");
    anchor = go.transform;
    anchor.parent = transform;
  }

  void Update() {
    if (state == 1 && target != null) {
      anchor.position = transform.position;
      if (GestureProvider.HaveSkeleton)
        anchor.rotation = transform.rotation;
    }
  }

  void OnTriggerEnter(Collider other) {
    if (!other.gameObject.name.StartsWith("Cube"))
      return;

    var newTarget = other.GetComponent<Rigidbody>();
    if (newTarget == target) {
      exit = false;
      return;
    }
    if (target != null && state == 1)
      StopMove();
    target = other.GetComponent<Rigidbody>();
    if (target != null) {
      exit = false;
      if (state == 1)
        StartMove();
      else
        SetColor(false);
    }
  }

  void OnTriggerExit(Collider other) {
    if (other.GetComponent<Rigidbody>() != target)
      return;
    if (state == 1)
      exit = true;
    else {
      SetColor(null);
      target = null;
    }
  }

  public void OnStateChanged(int state) {
    this.state = state;
    if (target == null)
      return;
    if (state == 1)
      StartMove();
    else if (state == 0) {
      StopMove();
      if (exit)
        target = null;
      else
        SetColor(false);
    }
  }

  void StartMove() {
    target.useGravity = false;
    target.isKinematic = true;
    anchor.SetParent(target.transform.parent, true);
    target.transform.SetParent(anchor, true);
    SetColor(true);
  }

  void StopMove() {
    target.transform.SetParent(anchor.parent, true);
    anchor.parent = transform;
    target.useGravity = true;
    target.isKinematic = false;
    SetColor(null);
  }

  // true: moving, false: touching, null: not touched
  void SetColor(bool? moving) {
    if (target == null)
      return;
    var renderer = target.GetComponent<Renderer>();
    if (renderer == null)
      return;
    var material = renderer.material;
    if (moving == null) {
      material.DisableKeyword("_EMISSION");
      return;
    }
    material.EnableKeyword("_EMISSION");
    material.SetColor ("_EmissionColor", moving.Value ? moveColor : enterColor);
  }
}

}
