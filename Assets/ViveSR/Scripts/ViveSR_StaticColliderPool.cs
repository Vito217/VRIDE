//The region of Deprecation period API will remove in the future.
#region Deprecation period API
using System.Linq;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    /**
    * @warning The class will remove in the future.
    */
    [ExecuteInEditMode]
    public class ViveSR_StaticColliderPool : ViveSR_RigidReconstructionColliderManager
    {
        private List<ViveSR_StaticColliderInfo> all_colliders = new List<ViveSR_StaticColliderInfo>();
        private int num_clds;

        //
        private List<ViveSR_StaticColliderInfo> _temp_list = new List<ViveSR_StaticColliderInfo>();
        private List<Vector3> placer_locations = new List<Vector3>();
        private List<GameObject> placer_list = new List<GameObject>();
        private GameObject _default_placer;
        public new float placer_interval_w = 0.1f;
        public new float placer_interval_h = 0.1f;
        public new float placer_up_shift = 0.03f;
        public new float placer_show_scale = 0.03f;

#if UNITY_EDITOR
        public new ColliderShapeType queried_shape;
        public new PlaneOrientation queried_orient;
        public new ColliderCondition queried_condition;
        public new bool doing_query = false;
        private bool last_doing_query = false;
        public new bool doing_extract_pos = false;
        private bool last_doing_extract_pos = false;
#endif        

        // data pre-proc
        static public new bool ProcessDataAndGenColliderInfo(GameObject go)
        {
            // organize and category collider type
            bool has_collider = false;
            MeshFilter[] mesh_filters = go.GetComponentsInChildren<MeshFilter>();
            int num_rnds = mesh_filters.Length;
            for (int id = 0; id < num_rnds; ++id)
            {
                ViveSR_StaticColliderInfo cld_info = mesh_filters[id].gameObject.AddComponent<ViveSR_StaticColliderInfo>();
                SceneUnderstandingObjectType semantic_type = ViveSR_SceneUnderstanding.GetSemanticTypeFromObjName(go.name);
                cld_info.SemanticType = semantic_type;
                if (semantic_type != SceneUnderstandingObjectType.NONE)
                {
                    string s_idx = go.name.Replace(ViveSR_SceneUnderstanding.SemanticTypeToString(semantic_type) + "_", "").Replace("_cld", "");
                    cld_info.SceneObjectID = int.Parse(s_idx);
                }

                string mesh_name = mesh_filters[id].name;
                string new_name = "";
                bool this_is_cld = false;

                if (mesh_name.Contains("PlaneConvexCollider"))
                {
                    new_name = "PlaneConvexCollider";
                    cld_info.SetBit((int)ColliderShapeType.CONVEX_SHAPE);
                    this_is_cld = true;
                }
                else if (mesh_name.Contains("PlaneBBCollider"))
                {
                    new_name = "PlaneBBCollider";
                    cld_info.SetBit((int)ColliderShapeType.BOUND_RECT_SHAPE);
                    this_is_cld = true;
                }
                else if (mesh_name.Contains("PlaneMeshCollider"))
                {
                    new_name = "PlaneMeshCollider";
                    cld_info.SetBit((int)ColliderShapeType.MESH_SHAPE);
                    this_is_cld = true;
                }

                if (mesh_name.Contains("Horizontal")) cld_info.SetBit((int)PlaneOrientation.HORIZONTAL);
                else if (mesh_name.Contains("Vertical")) cld_info.SetBit((int)PlaneOrientation.VERTICAL);
                else cld_info.SetBit((int)PlaneOrientation.OBLIQUE);

                has_collider = (has_collider || this_is_cld);
                if (!this_is_cld)
                {
                    Component.DestroyImmediate(cld_info);
                }
                else
                {
                    // parse area
                    int area_string_start_idx = mesh_name.LastIndexOf("Area_");
                    if (area_string_start_idx != -1)
                    {
                        area_string_start_idx = area_string_start_idx + 5;
                        string cur_string = mesh_name.Substring(area_string_start_idx);
                        int area_string_end_idx = cur_string.IndexOf("_");
                        cld_info.ApproxArea = float.Parse(cur_string.Substring(0, area_string_end_idx));
                    }
                    else
                    {
                        cld_info.SetBit((int)PlaneOrientation.FRAGMENT);
                    }

                    // parse normal
                    int normal_string_start_idx = mesh_name.LastIndexOf("Normal_");
                    if (normal_string_start_idx != -1)
                    {
                        normal_string_start_idx = normal_string_start_idx + 7;
                        string cur_string = mesh_name.Substring(normal_string_start_idx);
                        int normal_x_end_idx = cur_string.IndexOf("_");
                        cld_info.GroupNormal.x = float.Parse(cur_string.Substring(0, normal_x_end_idx));

                        cur_string = cur_string.Substring(normal_x_end_idx + 1);
                        int normal_y_end_idx = cur_string.IndexOf("_");
                        cld_info.GroupNormal.y = float.Parse(cur_string.Substring(0, normal_y_end_idx));

                        cur_string = cur_string.Substring(normal_y_end_idx + 1);
                        int normal_z_end_idx = cur_string.IndexOf("_");
                        cld_info.GroupNormal.z = float.Parse(cur_string.Substring(0, normal_z_end_idx));
                    }

                    // parse right axis
                    int right_string_start_idx = mesh_name.LastIndexOf("Right_");
                    if (right_string_start_idx != -1)
                    {
                        right_string_start_idx = right_string_start_idx + 6;
                        string cur_string = mesh_name.Substring(right_string_start_idx);
                        int right_x_end_idx = cur_string.IndexOf("_");
                        cld_info.RectRightAxis.x = float.Parse(cur_string.Substring(0, right_x_end_idx));

                        cur_string = cur_string.Substring(right_x_end_idx + 1);
                        int right_y_endidx = cur_string.IndexOf("_");
                        cld_info.RectRightAxis.y = float.Parse(cur_string.Substring(0, right_y_endidx));

                        cur_string = cur_string.Substring(right_y_endidx + 1);
                        int right_z_end_idx = cur_string.IndexOf("_");
                        cld_info.RectRightAxis.z = float.Parse(cur_string.Substring(0, right_z_end_idx));
                    }

                    // parse width height
                    int wh_string_start_idx = mesh_name.LastIndexOf("WH_");
                    if (wh_string_start_idx != -1)
                    {
                        wh_string_start_idx = wh_string_start_idx + 3;
                        string cur_string = mesh_name.Substring(wh_string_start_idx);
                        int width_end_idx = cur_string.IndexOf("_");
                        cld_info.RectWidth = float.Parse(cur_string.Substring(0, width_end_idx));

                        cur_string = cur_string.Substring(width_end_idx + 1);
                        int heightEndIdx = cur_string.IndexOf("_");
                        cld_info.RectHeight = float.Parse(cur_string.Substring(0, heightEndIdx));
                    }

                    // parse id
                    int idxStringStartIdx = mesh_name.LastIndexOf("_");
                    cld_info.PlaneID = (int.Parse(mesh_name.Substring(idxStringStartIdx + 1)));
                    new_name = new_name + "_" + mesh_name.Substring(idxStringStartIdx + 1);
                    mesh_filters[id].gameObject.name = new_name;
                }
            }

            return has_collider;
        }

        public new void OrganizeHierarchy()
        {
            ViveSR_StaticColliderInfo[] infos = GetComponentsInChildren<ViveSR_StaticColliderInfo>(true);
            int len = infos.Length;

            // init list with length * null
            List<ViveSR_StaticColliderInfo> brInfo_list = new List<ViveSR_StaticColliderInfo>(len);
            List<ViveSR_StaticColliderInfo> cInfo_list = new List<ViveSR_StaticColliderInfo>(len);
            List<ViveSR_StaticColliderInfo> mInfo_list = new List<ViveSR_StaticColliderInfo>(len);
            brInfo_list.AddRange(Enumerable.Repeat((ViveSR_StaticColliderInfo)null, len));
            cInfo_list.AddRange(Enumerable.Repeat((ViveSR_StaticColliderInfo)null, len));
            mInfo_list.AddRange(Enumerable.Repeat((ViveSR_StaticColliderInfo)null, len));

            GameObject mesh_cld_group = new GameObject("PlaneMeshColliderGroup");
            {
                mesh_cld_group.transform.SetParent(transform);
                GameObject horizontal_group = new GameObject("Horizontal");
                GameObject vertical_group = new GameObject("Vertical");
                GameObject oblique_group = new GameObject("Oblique");
                GameObject fragment_group = new GameObject("Fragment");
                {
                    horizontal_group.transform.SetParent(mesh_cld_group.transform);
                    vertical_group.transform.SetParent(mesh_cld_group.transform);
                    oblique_group.transform.SetParent(mesh_cld_group.transform);
                    fragment_group.transform.SetParent(mesh_cld_group.transform);
                }
            }
            mesh_cld_group.SetActive(true);

            GameObject convex_cld_group = new GameObject("PlaneConvexColliderGroup");
            {
                convex_cld_group.transform.SetParent(transform);
                GameObject horizontal_group = new GameObject("Horizontal");
                GameObject vertical_group = new GameObject("Vertical");
                GameObject oblique_group = new GameObject("Oblique");
                {
                    horizontal_group.transform.SetParent(convex_cld_group.transform);
                    vertical_group.transform.SetParent(convex_cld_group.transform);
                    oblique_group.transform.SetParent(convex_cld_group.transform);
                }
            }
            convex_cld_group.SetActive(false);

            GameObject bb_cld_group = new GameObject("PlaneBoundingRectColliderGroup");
            {
                bb_cld_group.transform.SetParent(transform);
                GameObject horizontal_group = new GameObject("Horizontal");
                GameObject vertical_group = new GameObject("Vertical");
                GameObject oblique_group = new GameObject("Oblique");
                {
                    horizontal_group.transform.SetParent(bb_cld_group.transform);
                    vertical_group.transform.SetParent(bb_cld_group.transform);
                    oblique_group.transform.SetParent(bb_cld_group.transform);
                }
            }
            bb_cld_group.SetActive(false);


            for (int i = 0; i < len; ++i)
            {
                Transform parent = transform;
                ViveSR_StaticColliderInfo cldInfo = infos[i];
                if (cldInfo.CheckHasAllBit((uint)ColliderShapeType.MESH_SHAPE))
                {
                    parent = parent.Find("PlaneMeshColliderGroup");
                    cldInfo.SetCorrespondingColliderOfType(ColliderShapeType.MESH_SHAPE, cldInfo);
                    mInfo_list[cldInfo.PlaneID] = cldInfo;
                }
                else if (cldInfo.CheckHasAllBit((uint)ColliderShapeType.CONVEX_SHAPE))
                {
                    parent = parent.Find("PlaneConvexColliderGroup");
                    cldInfo.SetCorrespondingColliderOfType(ColliderShapeType.CONVEX_SHAPE, cldInfo);
                    cInfo_list[cldInfo.PlaneID] = cldInfo;
                }
                else if (cldInfo.CheckHasAllBit((uint)ColliderShapeType.BOUND_RECT_SHAPE))
                {
                    parent = parent.Find("PlaneBoundingRectColliderGroup");
                    cldInfo.SetCorrespondingColliderOfType(ColliderShapeType.BOUND_RECT_SHAPE, cldInfo);
                    brInfo_list[cldInfo.PlaneID] = cldInfo;
                }

                if (cldInfo.CheckHasAllBit((uint)PlaneOrientation.HORIZONTAL)) parent = parent.Find("Horizontal");
                else if (cldInfo.CheckHasAllBit((uint)PlaneOrientation.VERTICAL)) parent = parent.Find("Vertical");
                else if (cldInfo.CheckHasAllBit((uint)PlaneOrientation.OBLIQUE)) parent = parent.Find("Oblique");
                else parent = parent.Find("Fragment"); // this should only appear in PlaneMesh

                cldInfo.transform.SetParent(parent, true);
                cldInfo.gameObject.AddComponent<MeshCollider>();

                MeshRenderer rnd = cldInfo.gameObject.GetComponent<MeshRenderer>();
                if (rnd)
                {
                    Material wireframe = new Material(Shader.Find("ViveSR/Wireframe"));
                    wireframe.SetFloat("_ZTest", 0);
                    wireframe.SetFloat("_Thickness", 0);
                    rnd.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
                    rnd.receiveShadows = false;
                    rnd.sharedMaterial = wireframe;
                    rnd.enabled = false;
                }

            }

            // get collider of other groups
            _LinkColliderInfo(ColliderShapeType.BOUND_RECT_SHAPE, brInfo_list, new List<ViveSR_StaticColliderInfo>[] { cInfo_list, mInfo_list });
            _LinkColliderInfo(ColliderShapeType.CONVEX_SHAPE, cInfo_list, new List<ViveSR_StaticColliderInfo>[] { brInfo_list, mInfo_list });
            _LinkColliderInfo(ColliderShapeType.MESH_SHAPE, mInfo_list, new List<ViveSR_StaticColliderInfo>[] { brInfo_list, cInfo_list });
        }

        private void _LinkColliderInfo(ColliderShapeType type, List<ViveSR_StaticColliderInfo> srcList, List<ViveSR_StaticColliderInfo>[] dstListArray)
        {
            for (int listID = 0; listID < dstListArray.Length; ++listID)
            {
                List<ViveSR_StaticColliderInfo> dstList = dstListArray[listID];
                for (int i = 0; i < dstList.Count; ++i)
                {
                    if (dstList[i] != null && srcList[i] != null)
                        dstList[i].SetCorrespondingColliderOfType(type, srcList[i]);
                }
            }
        }

        // Unity 
        void Awake()
        {
            if (Application.isPlaying)
            {
                _default_placer = GameObject.CreatePrimitive(PrimitiveType.Cube);
                _default_placer.hideFlags = HideFlags.HideInHierarchy;
                Destroy(_default_placer.GetComponent<Collider>());
                MeshRenderer rend = _default_placer.GetComponent<MeshRenderer>();
                rend.material.shader = Shader.Find("Unlit/Color");
                rend.material.SetColor("_Color", Color.red);
                rend.enabled = false;
            }

            ViveSR_StaticColliderInfo[] infoArray = GetComponentsInChildren<ViveSR_StaticColliderInfo>(true);
            for (int i = 0; i < infoArray.Length; ++i)
                this._AddColliderInfo(infoArray[i]);
        }

        void OnApplicationQuit()
        {
            if (_default_placer)
                Destroy(_default_placer);
        }

        private void _AddColliderInfo(ViveSR_StaticColliderInfo info)
        {
            if (!all_colliders.Contains(info))
                all_colliders.Add(info);

            num_clds = all_colliders.Count;
        }

        private void _GetAllColliderHasProps_Internal(uint[] props)
        {
            int numProps = props.Length;
            _temp_list.Clear();

            uint bits = 0;
            for (int i = 0; i < numProps; ++i)
                bits |= props[i];

            for (int j = 0; j < num_clds; ++j)
            {
                if (all_colliders[j].CheckHasAllBit(bits))
                    _temp_list.Add(all_colliders[j]);
            }
        }

        public new ViveSR_StaticColliderInfo GetClosestColliderWithProps(Vector3 testPos, uint[] props)
        {
            _GetAllColliderHasProps_Internal(props);    // get filtered info in tempList
            int found_id = -1;
            float min_dist = float.MaxValue;
            for (int i = 0; i < _temp_list.Count; i++)
            {
                ViveSR_StaticColliderInfo info = _temp_list[i];
                float dist = Vector3.Distance(info.GetComponent<MeshRenderer>().bounds.center, testPos);
                if (dist < min_dist)
                {
                    min_dist = dist;
                    found_id = i;
                }
            }

            return (found_id == -1) ? null : _temp_list[found_id];
        }

        public new ViveSR_StaticColliderInfo GetFurthestColliderWithProps(Vector3 testPos, uint[] props)
        {
            _GetAllColliderHasProps_Internal(props);    // get filtered info in tempList
            int found_id = -1;
            float max_dist = float.MinValue;
            for (int i = 0; i < _temp_list.Count; i++)
            {
                ViveSR_StaticColliderInfo info = _temp_list[i];
                float dist = Vector3.Distance(info.GetComponent<MeshRenderer>().bounds.center, testPos);
                if (dist > max_dist)
                {
                    max_dist = dist;
                    found_id = i;
                }
            }

            return (found_id == -1) ? null : _temp_list[found_id];
        }

        public new ViveSR_StaticColliderInfo GetLargestCollider(uint[] props)
        {
            _GetAllColliderHasProps_Internal(props);    // get filtered info in tempList
            int found_id = -1;
            float max_Area = float.MinValue;
            for (int i = 0; i < _temp_list.Count; i++)
            {
                ViveSR_StaticColliderInfo info = _temp_list[i];
                if (info.ApproxArea > max_Area)
                {
                    max_Area = info.ApproxArea;
                    found_id = i;
                }
            }

            return (found_id == -1) ? null : _temp_list[found_id];
        }

        // get colliders within customized height range
        public new ViveSR_StaticColliderInfo[] GetColliderByHeightRange(ColliderShapeType shapeType, float lowest_height, float highest_height)
        {
            _temp_list.Clear();

            if (lowest_height <= highest_height)
            {
                for (int i = 0; i < all_colliders.Count; i++)
                {
                    ViveSR_StaticColliderInfo info = all_colliders[i];
                    if (info.CheckHasAllBit((uint)shapeType))
                    {
                        Vector3 center = info.GetComponent<MeshRenderer>().bounds.center;
                        if (center.y >= lowest_height && center.y <= highest_height)
                            _temp_list.Add(info);
                    }
                }
            }
            return _temp_list.ToArray();
        }

        public new ViveSR_StaticColliderInfo[] GetAllColliderHasProps(uint[] props)
        {
            _GetAllColliderHasProps_Internal(props);
            return _temp_list.ToArray();
        }

        public new ViveSR_StaticColliderInfo[] GetColliderWithPropsAndCondition(uint[] props, ColliderCondition condition, Vector3 testPos = new Vector3())
        {
            if (condition == ColliderCondition.NONE)
                return GetAllColliderHasProps(props);

            ViveSR_StaticColliderInfo info = null;
            if (condition == ColliderCondition.LARGEST)
                info = GetLargestCollider(props);
            else if (condition == ColliderCondition.CLOSEST)
                info = GetClosestColliderWithProps(testPos, props);
            else if (condition == ColliderCondition.FURTHEST)
                info = GetFurthestColliderWithProps(testPos, props);

            _temp_list.Clear();
            if (info) _temp_list.Add(info);

            return _temp_list.ToArray();
        }

        public new SceneUnderstandingObjectType GetSemanticType()
        {
            SceneUnderstandingObjectType type = SceneUnderstandingObjectType.NONE;

            if (num_clds > 0 && all_colliders[0].SemanticType != type)
                type = all_colliders[0].SemanticType;

            return type;
        }

#if UNITY_EDITOR
        void Update()
        {
            if (doing_query && !last_doing_query)
            {
                ShowAllColliderWithPropsAndCondition(new uint[] { (uint)queried_shape, (uint)queried_orient }, queried_condition, Camera.main.transform.position);
                if (doing_extract_pos)
                {
                    ClearPlacerList();
                    DrawAllExtractedPlacerLocations(_temp_list.ToArray());
                }
            }
            else if (!doing_query && last_doing_query)
            {
                HideAllColliderRenderers();
                ClearPlacerList();
            }
            last_doing_query = doing_query;

            // object placer
            if (doing_extract_pos && !last_doing_extract_pos)
            {
                DrawAllExtractedPlacerLocations(_temp_list.ToArray());
            }
            else if (!doing_extract_pos && last_doing_extract_pos)
            {
                ClearPlacerList();
            }
            last_doing_extract_pos = doing_extract_pos;
        }
#endif
        public new void ShowAllColliderWithPropsAndCondition(uint[] props, ColliderCondition condition = ColliderCondition.NONE, Vector3 test_pos = new Vector3())
        {
            _temp_list.Clear();
            HideAllColliderRenderers();

            GetColliderWithPropsAndCondition(props, condition, test_pos);

            // draw
            int num = _temp_list.Count;
            for (int i = 0; i < num; ++i)
            {
                MeshRenderer rnd = _temp_list[i].GetComponent<MeshRenderer>();
                if (rnd == null)
                    rnd = _temp_list[i].gameObject.AddComponent<MeshRenderer>();

                Material wireframe = new Material(Shader.Find("ViveSR/Wireframe"));
                wireframe.SetFloat("_ZTest", 0);
                wireframe.SetFloat("_Thickness", 0);
                rnd.sharedMaterial = wireframe;
                rnd.enabled = true;
            }
        }

        public void DrawAllExtractedPlacerLocations(ViveSR_StaticColliderInfo[] info_array)
        {
            for (int infoIdx = 0; infoIdx < info_array.Length; ++infoIdx)
            {
                //Vector3[] raycastPositions;
                Quaternion outRot;
                ViveSR_StaticColliderInfo info = info_array[infoIdx];
                info.GetColliderUsableLocations(placer_interval_w, placer_interval_h, placer_up_shift, placer_locations, out outRot);
                for (int i = 0; i < placer_locations.Count; i++)
                {
                    GameObject placer = GameObject.Instantiate(_default_placer);
                    placer.GetComponent<MeshRenderer>().enabled = true;
                    placer.transform.localScale = new Vector3(placer_show_scale, placer_show_scale, placer_show_scale);
                    placer.transform.position = placer_locations[i];
                    placer.transform.rotation = outRot;
                    placer_list.Add(placer);
                }
            }
        }

        public new void HideAllColliderRenderers()
        {
            for (int i = 0; i < all_colliders.Count; i++)
                all_colliders[i].GetComponent<MeshRenderer>().enabled = false;
        }

        public new void ClearPlacerList()
        {
            for (int i = 0; i < placer_list.Count; i++)
                Destroy(placer_list[i]);

            placer_list.Clear();
        }
    }
}
#endregion