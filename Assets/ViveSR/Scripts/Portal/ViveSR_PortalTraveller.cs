using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    [RequireComponent(typeof(Collider))]
    public class ViveSR_PortalTraveller : MonoBehaviour
    {
        public bool isController = false;
        public bool isPlayer = false;

        [SerializeField] bool isInRealWorld = true;
        public bool IsInRealWorld
        {
            get
            {
                return isInRealWorld;
            }
            set
            {
                isInRealWorld = value;
            }
        }
        [SerializeField] bool isInVirtualWorld = false;
        public bool IsInVirtualWorld
        {
            get
            {
                return isInVirtualWorld;
            }
            set
            {
                isInVirtualWorld = value;
            }
        }
        [SerializeField] bool isTransitioning = false;
        public bool IsTransitioning
        {
            get
            {
                return isTransitioning;
            }
            set
            {
                isTransitioning = value;
            }
        }

        bool prevIsTransitioning = false;
        private WorldMode currentWorld;

        public WorldMode CurrentWorld
        {
            get
            {
                return currentWorld;
            }
            set
            {
                currentWorld = value;
            }
        }

        private ViveSR_PortalManager portalMgr;
        private MeshRenderer[] renderers = null;
        public MeshRenderer[] Renderers
        {
            get
            {
                return renderers;
            }
            set
            {
                renderers = value;
            }
        }

        private MeshRenderer[] dupRenderers = null;

        private Collider selfCld;
        private bool originIsTrigger;
        
        private MeshRenderer hitPortalRnd;
        private Vector3 originalScale;
        private ViveSR_Portal hitPortal;

        private TransitionMatUpdateCB materialCB = null;

        void Start()
        {
            portalMgr = FindObjectOfType<ViveSR_PortalManager>();
            if (!isController && !isPlayer && portalMgr != null)
            {
                if (portalMgr.controllerInWorld == WorldMode.RealWorld)
                {
                    gameObject.layer = LayerMask.NameToLayer("Default");
                    isInRealWorld = true;
                }
                else
                {
                    gameObject.layer = LayerMask.NameToLayer("VirtualWorldLayer");
                    isInRealWorld = false;
                }

                if (renderers == null) renderers = gameObject.GetComponentsInChildren<MeshRenderer>(true);
                SwitchMaterials( renderers, (isInRealWorld? WorldMode.RealWorld : WorldMode.VRWorld ) );
            }

            isInVirtualWorld = !isInRealWorld;
            selfCld = GetComponent<Collider>();
            originIsTrigger = selfCld.isTrigger;
        }

        void InitDuplicatedRenderer()
        {
            if (renderers == null) renderers = gameObject.GetComponentsInChildren<MeshRenderer>(true);
            int numRnds = renderers.Length;
            List<MeshRenderer> dupRndList = new List<MeshRenderer>();
            for (int i = 0; i < numRnds; ++i )
            {
                MeshRenderer rnd = renderers[i];
                GameObject dupGO = new GameObject(rnd.name + "_dup", typeof(MeshFilter), typeof(MeshRenderer));
                dupGO.transform.SetParent(rnd.transform, false);
                dupGO.GetComponent<MeshFilter>().mesh = rnd.GetComponent<MeshFilter>().mesh;

                MeshRenderer dupRnd = dupGO.GetComponent<MeshRenderer>();
                dupRnd.materials = rnd.materials;
                dupRnd.enabled = false;
                dupRndList.Add(dupRnd);
            }

            dupRenderers = dupRndList.ToArray();
        }

        void SwitchTransitioningMaterials()
        {
            if (materialCB == null) return;

            foreach (MeshRenderer dupRnd in dupRenderers )
            {
                foreach (Material mat in dupRnd.materials) materialCB(mat);
            }            
        }

        public void SwitchMaterials( MeshRenderer[] targetRnds, WorldMode toWorld )
        {
            try
            {
                if (toWorld == WorldMode.RealWorld)
                {
                    foreach (Renderer rnd in targetRnds)
                    {
                        rnd.gameObject.layer = LayerMask.NameToLayer("Default");
                        foreach (Material mat in rnd.materials)
                        {
                            mat.SetFloat("_StencilValue", portalMgr.realWorldStencilValue);
                            mat.SetFloat("_StencilComp", (float)UnityEngine.Rendering.CompareFunction.Equal);
                        }
                    }
                }
                else // ( VRWorld )
                {
                    foreach (Renderer rnd in targetRnds)
                    {
                        rnd.gameObject.layer = LayerMask.NameToLayer("VirtualWorldLayer");
                        foreach (Material mat in rnd.materials)
                        {
                            mat.SetFloat("_StencilValue", portalMgr.virtualWorldStencilValue);
                            //mat.SetFloat("_StencilComp", (float)UnityEngine.Rendering.CompareFunction.Equal);
                            // both is virtual world, the v-world layer is rendered after portal stencil is written but we don't want traveler to be culled by stencil
                            if (portalMgr.viewerInWorld == WorldMode.VRWorld)
                                mat.SetFloat("_StencilComp", (float)UnityEngine.Rendering.CompareFunction.Disabled);
                            else
                                mat.SetFloat("_StencilComp", (float)UnityEngine.Rendering.CompareFunction.Equal);
                        }
                    }
                }
            }
            catch (System.Exception e)
            {
                Debug.LogWarning("Null Exception on PortalTraveller:" + gameObject.name + "," + e.Message);
            }         
            
        }

        public void SetClippingPlaneEnable(MeshRenderer[] targetRnds, bool flag, WorldMode clipInWorld)
        {
            try
            {
                if (flag == false)
                {
                    foreach (Renderer rnd in targetRnds)
                    {
                        foreach (Material mat in rnd.materials)
                            mat.DisableKeyword("CLIP_PLANE");
                    }
                    return;
                }

                Vector4 planeEquation = (clipInWorld == WorldMode.RealWorld) ? hitPortal.planeEquation : -hitPortal.planeEquation;
                foreach (Renderer rnd in targetRnds)
                {
                    foreach (Material mat in rnd.materials)
                    {
                        mat.EnableKeyword("CLIP_PLANE");
                        mat.SetVector("_ClipPlane", planeEquation);
                    }
                }
            }
            catch (System.Exception e)
            {
                Debug.LogWarning("Null Exception on PortalTraveller:" + gameObject.name + "," + e.Message);
            }              
        }

        private void CheckWorldSide(ViveSR_Portal portal)
        {
            Vector3 selfPos = transform.position;
            Vector4 testPoint = new Vector4(selfPos.x, selfPos.y, selfPos.z, 1.0f);

            isInRealWorld = (Vector4.Dot(portal.planeEquation, testPoint) >= 0);
            isInVirtualWorld = !isInRealWorld;

            currentWorld = (isInRealWorld ? WorldMode.RealWorld : WorldMode.VRWorld);
        }

        public void OnTriggerEnter(Collider other)
        {
            ViveSR_Portal otherPortal = other.transform.root.GetComponent<ViveSR_Portal>();
            if (otherPortal)
            {
                hitPortal = otherPortal;
                hitPortalRnd = hitPortal.GetComponentInChildren<MeshRenderer>();
                CheckWorldSide(hitPortal);  // check which world is now
                isTransitioning = true;
                selfCld.isTrigger = true;
                materialCB = otherPortal.TransitionMaterialUpdateCB;    // set the material behaviours when transitioning

                if (renderers == null) renderers = gameObject.GetComponentsInChildren<MeshRenderer>(true);
                SetClippingPlaneEnable(renderers, true, currentWorld);                
                CheckTransitioningBehavious();
            }
        }

        public void OnTriggerExit(Collider other)
        {
            ViveSR_Portal otherPortal = other.transform.root.GetComponent<ViveSR_Portal>();
            if (otherPortal)
            {
                hitPortal = otherPortal;
                hitPortalRnd = hitPortal.GetComponentInChildren<MeshRenderer>();

                CheckWorldSide(hitPortal);  // check which world is now
                isTransitioning = false;
                selfCld.isTrigger = originIsTrigger;

                if (isPlayer)
                {
                    portalMgr.viewerInWorld = currentWorld;
                    portalMgr.UpdateViewerWorld();
                    gameObject.layer = LayerMask.NameToLayer( (currentWorld == WorldMode.RealWorld)? "Default" : "VirtualWorldLayer");
                }
                else
                {
                    if (isController) portalMgr.controllerInWorld = currentWorld;
                    if (renderers == null) renderers = gameObject.GetComponentsInChildren<MeshRenderer>(true);
                    SwitchMaterials(renderers, currentWorld);
                    SetClippingPlaneEnable(renderers, false, currentWorld);
                }

                CheckTransitioningBehavious();
            }
        }

        private void CheckTransitioningBehavious()
        {
            if (isTransitioning != prevIsTransitioning)
            {
                prevIsTransitioning = isTransitioning;

                // viewer is changing transitioning....this fixes flickering issue
                if (isPlayer)
                {
                    if (isTransitioning)
                    {
                        originalScale = hitPortalRnd.transform.localScale;
                        hitPortalRnd.transform.localPosition = new Vector3(0, 0, isInRealWorld ? -4 : 4);
                        hitPortalRnd.transform.localScale = new Vector3(30, 3.9f, 30);
                    }
                    else
                    {
                        hitPortalRnd.transform.localPosition = Vector3.zero;
                        hitPortalRnd.transform.localScale = originalScale;
                    }
                }
                // item is changing transitioning....
                else
                {
                    // rendering duplicated go: 
                    if (dupRenderers == null || dupRenderers.Length == 0) InitDuplicatedRenderer();

                    // when become transitioning, render duplicated item in other world
                    if (isTransitioning)
                    {
                        WorldMode dupItemWorld = (isInVirtualWorld ? WorldMode.RealWorld : WorldMode.VRWorld);
                        SwitchMaterials(dupRenderers, dupItemWorld);
                        SwitchTransitioningMaterials();
                        SetClippingPlaneEnable(dupRenderers, true, dupItemWorld);
                    }
                }
                RenderTransitioningMesh(isTransitioning);
            }
            // end if status is switched
        }

        private void RenderTransitioningMesh(bool enable)
        {
            if (dupRenderers == null)
                return;

            foreach (MeshRenderer dupRnd in dupRenderers)
                dupRnd.enabled = enable;
        }

        public void ResetTransitionState()
        {
            prevIsTransitioning = isTransitioning = false;
            RenderTransitioningMesh(isTransitioning);
        }

    }
}


