using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking.Sample {

// This script is used to set emission color when right hand pushes the box
class PushColor : MonoBehaviour {
  private static Color color = new Color(0.3f, 0, 0, 1);

  private Material material = null;

  IEnumerator Start() {
    while (true) {
      while (material == null)
        yield return null;
      var currentMat = material;
      currentMat.EnableKeyword("_EMISSION");
      currentMat.SetColor ("_EmissionColor", color);
      while (material != null)
        yield return null;
      yield return new WaitForSeconds(0.3f);
      currentMat.DisableKeyword("_EMISSION");
    }
  }

  void OnCollisionEnter(Collision other) {
    if (other.gameObject.name.StartsWith("Cube"))
      material = other.transform.GetComponent<Renderer>().material;
  }

  void OnCollisionExit(Collision other) {
    if (other.gameObject.name.StartsWith("Cube"))
      material = null;
  }
}

}
