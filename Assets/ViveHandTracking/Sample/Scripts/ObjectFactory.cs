using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking.Sample {

// This script is used for adding/reseting objects
class ObjectFactory: MonoBehaviour {
  public GameObject DefaultPrefab = null;
  public GameObject BoxPrefab = null;

  private Transform Camera = null;

  void Start() {
    Reset();
    Camera = GestureProvider.Current.transform;
  }

  public void AddObject() {
    var go = GameObject.Instantiate(BoxPrefab, transform, false);
    go.name = "Cube";
    go.transform.position = Camera.position + Camera.rotation * new Vector3(0, 1, 1.3f);
    go.transform.rotation = Quaternion.identity;
  }

  public void Reset() {
    foreach (Transform child in transform)
      GameObject.Destroy(child.gameObject);
    var go = GameObject.Instantiate(DefaultPrefab, transform, false);
    go.name = "Default Objects";
    go.transform.localPosition = Vector3.zero;
    go.transform.localRotation = Quaternion.identity;
  }
}

}
