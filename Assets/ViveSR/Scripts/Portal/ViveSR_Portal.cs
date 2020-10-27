using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Vive.Plugin.SR
{
    public delegate void TransitionMatUpdateCB(Material mat);

    public class ViveSR_Portal : MonoBehaviour
    {
        public Vector4 planeEquation = new Vector4();
        private List<MeshRenderer> renderers = new List<MeshRenderer>();
        private List<ParticleSystemRenderer> effectRnds = new List<ParticleSystemRenderer>();

        private TransitionMatUpdateCB materialCB;
        public TransitionMatUpdateCB TransitionMaterialUpdateCB
        {
            get { return materialCB; }
            set { materialCB = value; }
        }

        private static void DefaultTransitionMatCB(Material mat)
        {
            mat.shader = Shader.Find("ViveSR/Wireframe");
            mat.SetFloat("_Thickness", 0.0f);
            mat.SetColor("_Color", Color.white);
            mat.renderQueue = 2001;
        }

        // Use this for initialization
        void Awake()
        {
            materialCB = DefaultTransitionMatCB;

            MeshRenderer[] rnds = GetComponentsInChildren<MeshRenderer>();
            foreach (MeshRenderer rnd in rnds)
            {
                // is mesh renderer
                if (rnd.sharedMaterial.shader.name.Equals("ViveSR/PortalMeshShader"))
                    renderers.Add(rnd);
            }

            ParticleSystemRenderer[] particleRnds = GetComponentsInChildren<ParticleSystemRenderer>();
            foreach (ParticleSystemRenderer particle in particleRnds)
                effectRnds.Add(particle);
        }

        public void UpdatePlaneNormal()
        {
            Vector3 fwd = transform.forward;
            Vector3 pos = transform.position;
            float planeD = -Vector3.Dot(fwd, pos);
            planeEquation.Set(fwd.x, fwd.y, fwd.z, planeD);     // plane(A,B,C,D) => Ax + By + Cz + D = 0;
        }

        public void SetRenderRule(WorldMode viewerInWorld, int realWStencil, int virtualWStencil)
        {
            if (viewerInWorld == WorldMode.VRWorld)
            {
                foreach (MeshRenderer rnd in renderers)
                {
                    // now we don't change the pass-thru camera and virtual world camera depth order, so this is not needed
                    rnd.material.SetFloat("_StencilValue", realWStencil);
                }

                foreach (ParticleSystemRenderer particle in effectRnds)
                    particle.gameObject.layer = LayerMask.NameToLayer("Default");   // move to the default camera (make sure it's rendered later)
            }
            else
            {
                foreach (MeshRenderer rnd in renderers)
                {
                    // now we don't change the pass-thru camera and virtual world camera depth order, so this is not needed
                    rnd.material.SetFloat("_StencilValue", virtualWStencil);
                }

                foreach (ParticleSystemRenderer particle in effectRnds)
                    particle.gameObject.layer = LayerMask.NameToLayer("VirtualWorldLayer");
            }
        }

        // Update is called once per frame
        void Update()
        {

        }
    }
}
