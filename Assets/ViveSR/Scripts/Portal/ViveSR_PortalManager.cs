using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    public enum WorldMode
    {
        RealWorld = 0,
        VRWorld = 1
    }

    public class ViveSR_PortalManager : MonoBehaviour
    {
        public int realWorldStencilValue = 0;
        public int virtualWorldStencilValue = 1;

        public MeshRenderer stencilCleaner;
        public MeshRenderer depthCleaner;        

        public WorldMode viewerInWorld = WorldMode.RealWorld;
        public WorldMode controllerInWorld = WorldMode.RealWorld;

        public Material controllerMaterial;

        private List<ViveSR_Portal> portals = new List<ViveSR_Portal>();
        private Shader ctrllerOriShader;
        private Camera cam;

        // Use this for initialization
        void Awake()
        {
            cam = GetComponentInChildren<Camera>(true);
            if (cam == null)
            {
                Debug.LogError("No portal camera found!");
                return;
            }
            Physics.IgnoreLayerCollision(LayerMask.NameToLayer("Default"), LayerMask.NameToLayer("VirtualWorldLayer"));
        }

        void Start()
        {
            if (controllerMaterial != null)
            {
                ctrllerOriShader = controllerMaterial.shader;
                controllerMaterial.shader = Shader.Find("ViveSR/Standard, Stencil");
            }
        }

        void OnDisable()
        {
            if (controllerMaterial != null) controllerMaterial.shader = ctrllerOriShader;
        }

        public void TurnOnCamera()
        {
            // The camera is a registered virtual camera in ViveSR_DualCameraRig.
            // Activate the virtual camera.
            ViveSR_DualCameraRig.Instance.ActivateRegisteredVirtualCamera(cam, true);
        }
        public void TurnOffCamera()
        {
            // The camera is a registered virtual camera in ViveSR_DualCameraRig.
            // Deactivate the virtual camera.
            ViveSR_DualCameraRig.Instance.ActivateRegisteredVirtualCamera(cam, false);
        }

        public void AddPortal(GameObject portalGO)
        {
            ViveSR_Portal portal = portalGO.GetComponent<ViveSR_Portal>();
            if ( portal )
            {
                portals.Add(portal);
                portal.SetRenderRule(viewerInWorld, realWorldStencilValue, virtualWorldStencilValue);
                portal.UpdatePlaneNormal();
            }
        }

        public void ClearPortal(GameObject portalGO)
        {
            ViveSR_Portal portal = portalGO.GetComponent<ViveSR_Portal>();
            if (portal)
            {
                // if it is in the list
                if (portals.Remove(portal))
                {
                    Destroy(portal.gameObject);
                }
            }
        }

        public void ClearAllPortals()
        {
            foreach (ViveSR_Portal portal in portals)
            {
                Destroy(portal.gameObject);
            }
            portals.Clear();
        }


        public void UpdateViewerWorld()
        {
            // we don't change the depth of pass-thru and virtual world cameras now (in testing)
            if (viewerInWorld == WorldMode.VRWorld)
            {
                // clear the screen stencil to the virtual world stencil
                stencilCleaner.material.SetFloat("_StencilValue", virtualWorldStencilValue);
                stencilCleaner.enabled = true;
                //depthCleaner.material.SetFloat("_StencilValue", realWorldStencilValue);
            }
            else
            {
                // clear the screen stencil to the real world stencil ( 0 = exactly the default value)
                //stencilCleaner.material.SetFloat("_StencilValue", realWorldStencilValue);
                stencilCleaner.enabled = false;                
                depthCleaner.material.SetFloat("_StencilValue", virtualWorldStencilValue);
                depthCleaner.material.renderQueue = 999;
            }

            foreach (ViveSR_Portal portal in portals)
                portal.SetRenderRule(viewerInWorld, realWorldStencilValue, virtualWorldStencilValue);
        }
    }
}

