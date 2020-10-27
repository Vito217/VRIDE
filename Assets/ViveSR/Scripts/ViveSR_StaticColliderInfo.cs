//The region of Deprecation period API will remove in the future.
#region Deprecation period API
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    /**
    * @warning The class will remove in the future.
    */
    [ExecuteInEditMode]
    public class ViveSR_StaticColliderInfo : ViveSR_RigidReconstructionCollider
    {
        [SerializeField]
        private ColliderShapeType _shape_type;
        [SerializeField]
        private PlaneOrientation _orientation;
        public new float ApproxArea = 0.0f;
        public new Vector3 GroupNormal = Vector3.zero;
        public new float RectWidth = 0.0f;
        public new float RectHeight = 0.0f;
        public new Vector3 RectRightAxis = Vector3.right;
        [HideInInspector]
        [SerializeField]
        private uint _prop_bits;

        private int _plane_id = -1;
        private int _scene_obj_id = -1;
        private SceneUnderstandingObjectType _semantic_type = SceneUnderstandingObjectType.NONE;

        [SerializeField]
        private ViveSR_StaticColliderInfo _corres_convex_cld = null;
        [SerializeField]
        private ViveSR_StaticColliderInfo _corres_bounding_rect_cld = null;
        [SerializeField]
        private ViveSR_StaticColliderInfo _corres_mesh_cld = null;

        public new int PlaneID
        {
            get { return _plane_id; }
            set { _plane_id = value; }
        }

        public new int SceneObjectID
        {
            get { return _scene_obj_id; }
            set { _scene_obj_id = value; }
        }

        public new SceneUnderstandingObjectType SemanticType
        {
            get { return _semantic_type; }
            set { _semantic_type = value; }
        }

        public new PlaneOrientation Orientation { get { return _orientation; } }

        private float new_width, new_height;

        void Awake()
        {
            _prop_bits = (uint)_shape_type | (uint)_orientation;
        }

        public new void SetBit(uint bit)
        {
            if (bit == (uint)ColliderShapeType.CONVEX_SHAPE || bit == (uint)ColliderShapeType.BOUND_RECT_SHAPE || bit == (uint)ColliderShapeType.MESH_SHAPE)
                _shape_type = (ColliderShapeType)bit;
            else if (bit == (uint)PlaneOrientation.HORIZONTAL || bit == (uint)PlaneOrientation.VERTICAL || bit == (uint)PlaneOrientation.OBLIQUE || bit == (uint)PlaneOrientation.FRAGMENT)
                _orientation = (PlaneOrientation)bit;

            _prop_bits = (uint)_shape_type | (uint)_orientation;
        }

        public new bool CheckHasAllBit(uint bit)
        {
            return ((_prop_bits & bit) == bit);
        }

        public void SetCorrespondingColliderOfType(ColliderShapeType type, ViveSR_StaticColliderInfo info)
        {
            if (type == ColliderShapeType.CONVEX_SHAPE)
            {
                _corres_convex_cld = info;
            }
            else if (type == ColliderShapeType.BOUND_RECT_SHAPE)
            {
                _corres_bounding_rect_cld = info;
            }
            else if (type == ColliderShapeType.MESH_SHAPE)
            {
                _corres_mesh_cld = info;
            }
        }

        public new ViveSR_StaticColliderInfo GetCorrespondingColliderOfType(ColliderShapeType type)
        {
            if (type == ColliderShapeType.CONVEX_SHAPE)
            {
                return _corres_convex_cld;
            }
            else if (type == ColliderShapeType.BOUND_RECT_SHAPE)
            {
                return _corres_bounding_rect_cld;
            }
            else if (type == ColliderShapeType.MESH_SHAPE)
            {
                return _corres_mesh_cld;
            }
            else
            {
                return null;
            }
        }

        // To-Do: return raycastPositions ??
        public new void GetColliderUsableLocations(float interval_w, float interval_h, float surf_shift, List<Vector3> out_locations, out Quaternion rotation)
        {
            out_locations.Clear();
            rotation = new Quaternion();

            ViveSR_StaticColliderInfo bb_cldInfo = GetCorrespondingColliderOfType(ColliderShapeType.BOUND_RECT_SHAPE);
            if (bb_cldInfo != null)
            {
                // Get collider axes
                Vector3 bb_center = bb_cldInfo.GetComponent<MeshRenderer>().bounds.center;
                Vector3 right = bb_cldInfo.RectRightAxis;
                Vector3 up = bb_cldInfo.GroupNormal;
                Vector3 forward = Vector3.Cross(right, up);
                forward.Normalize();
                right = Vector3.Cross(up, forward);
                right.Normalize();
                float bb_width = bb_cldInfo.RectWidth;
                float bb_height = bb_cldInfo.RectHeight;
                // return rotation
                rotation.SetLookRotation(forward, up);

                // Check if collider exists in each position by Raycast
                for (float j = -bb_height / 2 + interval_h / 2; j <= bb_height / 2; j += interval_h)
                {
                    for (float i = -bb_width / 2 + interval_w / 2; i <= bb_width / 2; i += interval_w)
                    {
                        Vector3 pos = bb_center + right * i + forward * j;
                        Vector3 pos_raycast = pos + up * 0.1f;
                        RaycastHit hit;
                        Physics.Raycast(pos_raycast, -up, out hit);
                        if (hit.collider != null && hit.collider.gameObject.name == this.gameObject.name)
                            out_locations.Add(pos + up * surf_shift);
                    }
                }
            }
        }

        public new void GetColliderUsableLocationsWithRightAxis(float interval_w, float interval_h, float surf_shift, List<Vector3> outLocations, out Quaternion rotation, ref Vector3 rightVec)
        {
            outLocations.Clear();
            rotation = new Quaternion();

            ViveSR_StaticColliderInfo bb_cldInfo = GetCorrespondingColliderOfType(ColliderShapeType.BOUND_RECT_SHAPE);
            if (bb_cldInfo != null)
            {
                // Get collider axes
                Vector3 bb_center = bb_cldInfo.GetComponent<MeshRenderer>().bounds.center;
                Vector3 up = bb_cldInfo.GroupNormal;
                Vector3 forward = Vector3.Cross(rightVec, up);
                forward.Normalize();
                rightVec = Vector3.Cross(up, forward);
                rightVec.Normalize();
                float bb_width = bb_cldInfo.RectWidth;
                float bb_height = bb_cldInfo.RectHeight;

                // return rotation
                rotation.SetLookRotation(forward, up);

                // Calculate new range
                new_height = new_width = Mathf.Sqrt(Mathf.Pow(bb_height, 2) + Mathf.Pow(bb_width, 2));
                // Check if collider exists in each position by Raycast
                for (float j = -new_height / 2 + interval_h / 2; j <= new_height / 2; j += interval_h)
                {
                    for (float i = -new_width / 2 + interval_w / 2; i <= new_width / 2; i += interval_w)
                    {
                        Vector3 pos = bb_center + rightVec * i + forward * j;
                        Vector3 pos_raycast = pos + up * 0.1f;
                        RaycastHit hit;
                        Physics.Raycast(pos_raycast, -up, out hit);
                        if (hit.collider != null && hit.collider.gameObject.name == this.gameObject.name)
                            outLocations.Add(pos + up * surf_shift);
                    }
                }
            }
        }

    }
}
#endregion