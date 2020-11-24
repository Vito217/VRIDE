using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace ViveHandTracking {

[HelpURL("https://hub.vive.com/storage/tracking/unity/model.html")]
public class ModelRenderer : MonoBehaviour {
  private const float minAlpha = 0.2f;

  [Tooltip("Initial rotation for model to point thumb finger upward and index finger forward when fingers open")]
  public Vector3 initialRotation = Vector3.zero;
  [Tooltip("Draw left hand if true, right hand otherwise")]
  public bool IsLeft = false;
  [Tooltip("Root object of skinned mesh")]
  public GameObject Hand = null;
  [Tooltip("Nodes of skinned mesh, must be size of 21 in same order as skeleton definition")]
  public Transform[] Nodes = new Transform[21];
  [Tooltip("Collider type created with hand. The layer of the object is same as this object.")]
  public HandColliderType colliderType = HandColliderType.None;
  [Tooltip("Use hand confidence as alpha, low confidence hand becomes transparent")]
  public bool showConficenceAsAlpha = false;

  private List<Transform> colliders = null;
  private Vector3[] modelFingerVec;
  private Vector3 localNoraml;

  void Awake() {
    InitializeModel();
    Hand.SetActive(false);

    // create colliders
    if (colliderType != HandColliderType.None) {
      colliders = new List<Transform>();
      var go = new GameObject("Collider");
      go.transform.parent = Hand.transform;
      go.layer = gameObject.layer;
      var collider = go.AddComponent<BoxCollider>();
      collider.isTrigger = colliderType == HandColliderType.Trigger;
      colliders.Add(go.transform);
    }
    if (colliderType == HandColliderType.Collider) {
      // add bones colliders
      for (int i = 0; i < Bones.Length; i += 2) {
        var go = new GameObject("Bone" + i);
        go.transform.parent = Hand.transform;
        go.layer = gameObject.layer;
        go.AddComponent<CapsuleCollider>();
        colliders.Add(go.transform);
      }
    }
  }

  IEnumerator Start() {
    while (GestureProvider.Status == GestureStatus.NotStarted)
      yield return null;
    if (!GestureProvider.HaveSkeleton)
      this.enabled = false;
  }

  void Update() {
    GestureResult result = IsLeft ? GestureProvider.LeftHand : GestureProvider.RightHand;
    if (result == null) {
      Hand.SetActive(false);
      return;
    }
    Hand.SetActive(true);

    transform.position = result.points[0];
    transform.rotation = result.rotation * Quaternion.Euler(initialRotation);

    Vector3 indexDir = (result.points[5] - result.points[0]).normalized;
    Vector3 midDir = (result.points[9] - result.points[0]).normalized;
    Vector3 palmDir = IsLeft ? Vector3.Cross(indexDir, midDir) : Vector3.Cross(midDir, indexDir);
    Vector3 thumbAxis = (result.points[17] - result.points[1]).normalized;

    int nodeIndex = 1;
    int vecIndex = 0;
    for (int i = 0; i < 5; ++i, nodeIndex += 4, vecIndex += 3) {
      Vector3 root = result.points[nodeIndex];
      Vector3 joint1 = result.points[nodeIndex + 1];
      Vector3 vec1 = (joint1 - root).normalized * modelFingerVec[vecIndex].magnitude;
      Vector3 joint2 = result.points[nodeIndex + 2];
      Vector3 vec2 = (joint2 - joint1).normalized * modelFingerVec[vecIndex + 1].magnitude;
      Vector3 top = result.points[nodeIndex + 3];
      Vector3 vec3 = (top - joint2).normalized * modelFingerVec[vecIndex + 2].magnitude;

      Vector3 fingerNormal;
      if (i == 0)
        fingerNormal = CalculateFingerNormal(vec1, vec2, thumbAxis, IsLeft ? palmDir : -palmDir);
      else
        fingerNormal = CalculateFingerNormal(vec1, vec2, palmDir, Vector3.Cross(indexDir, palmDir));

      SetNodeRotation(nodeIndex, modelFingerVec[vecIndex], vec1, fingerNormal);
      SetNodeRotation(nodeIndex + 1, modelFingerVec[vecIndex + 1], vec2, fingerNormal);
      SetNodeRotation(nodeIndex + 2, modelFingerVec[vecIndex + 2], vec3, fingerNormal);
    }

    if (showConficenceAsAlpha) {
      var color = Hand.GetComponent<Renderer>().material.color;
      color.a = result.confidence > minAlpha ? result.confidence : minAlpha;
      Hand.GetComponent<Renderer>().material.color = color;
    }
    if (colliderType == HandColliderType.Trigger)
      UpdateTigger();
    else if (colliderType == HandColliderType.Collider)
      UpdateCollider();
  }

  private Vector3 CalculateFingerNormal(Vector3 vec1, Vector3 vec2, Vector3 forward, Vector3 right) {
    Vector3 vec1p = vec1 - Vector3.Dot(vec1, right) * right / right.sqrMagnitude;
    Vector3 vec2p = vec2 - Vector3.Dot(vec2, right) * right / right.sqrMagnitude;
    float angle0 = Vector3.Angle(vec1p, vec2p);
    float angle1 = Vector3.Angle(vec1p, forward);
    float angle2 = Vector3.Angle(vec2p, forward);

    Vector3 normal;
    if (angle0 > angle1 && angle0 > angle2)
      normal = Vector3.Cross(vec1, vec2) * AngleSign(vec1p, vec2p, right);
    else if (angle1 > angle2)
      normal = Vector3.Cross(vec1, forward) * AngleSign(vec1p, forward, right);
    else
      normal = Vector3.Cross(vec2, forward) * AngleSign(vec2p, forward, right);
    return normal.normalized;
  }

  private void SetNodeRotation(int nodeIndex, Vector3 modelVec, Vector3 axis, Vector3 fingerNormal) {
    Nodes[nodeIndex].rotation = Quaternion.FromToRotation(modelVec, axis);
    var angle = SignedAngle(Nodes[nodeIndex].rotation * localNoraml, fingerNormal, axis);
    Nodes[nodeIndex].rotation = Quaternion.AngleAxis(angle, axis) * Nodes[nodeIndex].rotation;
  }

  private void InitializeModel() {
    // find local normal vector in node local axis, assuming all the finger nodes have same local axis
    localNoraml = FindLocalNormal(Nodes[9]);
    // get initial finger direction and length in local axis
    modelFingerVec = new Vector3[15];
    int vecIndex = 0;
    int nodeIndex = 1;
    for (int i = 0; i < 5; i++, nodeIndex += 4) {
      modelFingerVec[vecIndex] = Quaternion.Inverse(Nodes[nodeIndex].rotation) *
                                 (Nodes[nodeIndex + 1].position - Nodes[nodeIndex].position);
      vecIndex++;
      modelFingerVec[vecIndex] = Quaternion.Inverse(Nodes[nodeIndex + 1].rotation) *
                                 (Nodes[nodeIndex + 2].position - Nodes[nodeIndex + 1].position);
      vecIndex++;
      modelFingerVec[vecIndex] = Quaternion.Inverse(Nodes[nodeIndex + 2].rotation) *
                                 (Nodes[nodeIndex + 3].position - Nodes[nodeIndex + 2].position);
      vecIndex++;
    }
  }

  private Vector3 FindLocalNormal(Transform transform) {
    var axis = Vector3.zero;
    var minDistance = 0f;
    var dot = Vector3.Dot(transform.forward, Vector3.right);
    if (dot > minDistance) {
      minDistance = dot;
      axis = Vector3.forward;
    } else if (-dot > minDistance) {
      minDistance = -dot;
      axis = Vector3.back;
    }

    dot = Vector3.Dot(transform.right, Vector3.right);
    if (dot > minDistance) {
      minDistance = dot;
      axis = Vector3.right;
    } else if (-dot > minDistance) {
      minDistance = -dot;
      axis = Vector3.left;
    }

    dot = Vector3.Dot(transform.up, Vector3.right);
    if (dot > minDistance) {
      minDistance = dot;
      axis = Vector3.up;
    } else if (-dot > minDistance) {
      minDistance = -dot;
      axis = Vector3.down;
    }
    return axis;
  }

  #region Colliders

  // Links between keypoints, 2*i & 2*i+1 forms a link.
  private static int[] Bones = new int[] {
    2, 3, 3, 4, // thumb
    5, 6, 6, 7, 7, 8, // index
    9, 10, 10, 11, 11, 12, // middle
    13, 14, 14, 15, 15, 16, // ring
    17, 18, 18, 19, 19, 20, // pinky
  };
  private static readonly Vector3 OneCM = Vector3.one * 0.01f;

  void UpdateTigger() {
    var bounds = new Bounds(Nodes[0].position, Vector3.zero);
    for (int i = 1; i < Nodes.Length; i++)
      bounds.Encapsulate(Nodes[i].position);
    SetBounds(colliders[0], bounds);
  }

  void UpdateCollider() {
    // palm bounds
    var bounds = new Bounds(Nodes[0].position, Vector3.zero);
    bounds.Encapsulate(Nodes[1].position);
    bounds.Encapsulate(Nodes[2].position);
    bounds.Encapsulate(Nodes[5].position);
    bounds.Encapsulate(Nodes[9].position);
    bounds.Encapsulate(Nodes[13].position);
    bounds.Encapsulate(Nodes[17].position);
    SetBounds(colliders[0], bounds);

    int index = 0;
    for (int i = 1; i < colliders.Count; i++) {
      var start = Nodes[Bones[index++]].position;
      var end = Nodes[Bones[index++]].position;
      colliders[i].position = (start + end) / 2;
      var direction = end - start;
      colliders[i].rotation = Quaternion.FromToRotation(Vector3.forward, direction);
      colliders[i].localScale = new Vector3(0.01f, 0.01f, direction.magnitude);
    }
  }

  void SetBounds(Transform t, Bounds b) {
    t.position = b.center;
    t.rotation = Quaternion.identity;
    t.localScale = Vector3.Max(b.size, OneCM);
  }

  #endregion

  private static float AngleSign(Vector3 v1, Vector3 v2, Vector3 axis) {
    return Mathf.Sign(Vector3.Dot(axis, Vector3.Cross(v1, v2)));
  }

  private static float SignedAngle(Vector3 v1, Vector3 v2, Vector3 axis) {
#if UNITY_2017_1_OR_NEWER
    return Vector3.SignedAngle(v1, v2, axis);
#else
    return  Vector3.Angle(v1, v2) * AngleSign(v1, v2, axis);
#endif
  }
}
}
