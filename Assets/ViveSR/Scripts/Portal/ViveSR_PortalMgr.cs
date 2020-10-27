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
    public class ViveSR_PortalMgr : MonoBehaviour
    {
        public int realWorldStencilValue = 0;
        public int virtualWorldStencilValue = 1;

        public MeshRenderer stencilCleaner;
        public MeshRenderer depthCleaner;

        public WorldMode viewerInWorld = WorldMode.RealWorld;
        public WorldMode controllerInWorld = WorldMode.RealWorld;

        public Material controllerMaterial;

        private ViveSR_PortalManager new_interface_portal_manager;
        private Shader ctrllerOriShader;
        private Camera cam;
        void Awake()
        {
            new_interface_portal_manager = new ViveSR_PortalManager();
            new_interface_portal_manager.realWorldStencilValue = realWorldStencilValue;
            new_interface_portal_manager.virtualWorldStencilValue = virtualWorldStencilValue;
            new_interface_portal_manager.stencilCleaner = stencilCleaner;
            new_interface_portal_manager.depthCleaner = depthCleaner;
            new_interface_portal_manager.viewerInWorld = viewerInWorld;
            new_interface_portal_manager.controllerInWorld = controllerInWorld;
            new_interface_portal_manager.controllerMaterial = controllerMaterial;

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
            if (new_interface_portal_manager.controllerMaterial != null)
            {
                ctrllerOriShader = new_interface_portal_manager.controllerMaterial.shader;
                new_interface_portal_manager.controllerMaterial.shader = Shader.Find("ViveSR/Standard, Stencil");
            }
        }

        void OnDisable()
        {
            if (new_interface_portal_manager.controllerMaterial != null) new_interface_portal_manager.controllerMaterial.shader = ctrllerOriShader;
        }

        public void TurnOnCamera()
        {
            new_interface_portal_manager.TurnOnCamera();
        }
        public void TurnOffCamera()
        {
            new_interface_portal_manager.TurnOffCamera();
        }

        public void AddPortal(GameObject portalGO)
        {
            new_interface_portal_manager.AddPortal(portalGO);
        }

        public void ClearPortal(GameObject portalGO)
        {
            new_interface_portal_manager.ClearAllPortals();
        }

        public void ClearAllPortals()
        {
            new_interface_portal_manager.ClearAllPortals();
        }


        public void UpdateViewerWorld()
        {
            new_interface_portal_manager.UpdateViewerWorld();
        }
    }
}
#endregion