using System;
using System.Collections.Generic;
using UnityEngine.Rendering;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Interactor used for holding interactables via a socket. This component is not designed to be attached to a controller
    /// (thus does not derive from <see cref="XRBaseControllerInteractor"/>) and instead will always attempt to select an interactable that it is
    /// hovering over (though will not perform exclusive selection of that interactable).
    /// </summary>
    [DisallowMultipleComponent]
    [AddComponentMenu("XR/XR Socket Interactor")]
    public class XRSocketInteractor : XRBaseInteractor
    {
        [Header("Socket")]

        [SerializeField]
        bool m_ShowInteractableHoverMeshes = true;
        /// <summary>
        /// Whether this socket should show a mesh at socket's attach point for interactables that it is hovering over.
        /// </summary>
        public bool showInteractableHoverMeshes
        {
            get => m_ShowInteractableHoverMeshes;
            set => m_ShowInteractableHoverMeshes = value;
        }

        [SerializeField]
        Material m_InteractableHoverMeshMaterial;
        /// <summary>
        /// Material used for rendering interactable meshes on hover
        /// (a default material will be created if none is supplied).
        /// </summary>
        public Material interactableHoverMeshMaterial
        {
            get => m_InteractableHoverMeshMaterial;
            set => m_InteractableHoverMeshMaterial = value;
        }

        [SerializeField]
        Material m_InteractableCantHoverMeshMaterial;
        /// <summary>
        /// Material used for rendering interactable meshes on hover when there is already a selected object in the socket
        /// (a default material will be created if none is supplied).
        /// </summary>
        public Material interactableCantHoverMeshMaterial
        {
            get => m_InteractableCantHoverMeshMaterial;
            set => m_InteractableCantHoverMeshMaterial = value;
        }

        [SerializeField]
        bool m_SocketActive = true;
        /// <summary>
        /// Whether socket interaction is enabled.
        /// </summary>
        public bool socketActive
        {
            get => m_SocketActive;
            set => m_SocketActive = value;
        }

        [SerializeField]
        float m_InteractableHoverScale = 1f;
        /// <summary>
        /// Scale at which to render hovered interactable.
        /// </summary>
        public float interactableHoverScale
        {
            get => m_InteractableHoverScale;
            set => m_InteractableHoverScale = value;
        }

        [SerializeField]
        float m_RecycleDelayTime = 1f;
        /// <summary>
        /// Sets the amount of time the socket will refuse hovers after an object is removed.
        /// </summary>
        public float recycleDelayTime
        {
            get => m_RecycleDelayTime;
            set => m_RecycleDelayTime = value;
        }

        float m_LastRemoveTime = -100f;

        /// <summary>
        /// Reusable list of valid targets.
        /// </summary>
        readonly List<XRBaseInteractable> m_ValidTargets = new List<XRBaseInteractable>();

        /// <summary>
        /// Reusable map of interactables to their distance squared from this interactor (used for sort).
        /// </summary>
        readonly Dictionary<XRBaseInteractable, float> m_InteractableDistanceSqrMap = new Dictionary<XRBaseInteractable, float>();

        readonly Dictionary<XRBaseInteractable, MeshFilter[]> m_MeshFilterCache = new Dictionary<XRBaseInteractable, MeshFilter[]>();

        /// <summary>
        /// Sort comparison function used by <see cref="GetValidTargets"/>.
        /// </summary>
        Comparison<XRBaseInteractable> m_InteractableSortComparison;

        int InteractableSortComparison(XRBaseInteractable x, XRBaseInteractable y)
        {
            var xDistance = m_InteractableDistanceSqrMap[x];
            var yDistance = m_InteractableDistanceSqrMap[y];
            if (xDistance > yDistance)
                return 1;
            if (xDistance < yDistance)
                return -1;
            return 0;
        }

        /// <inheritdoc />
        protected override void Awake()
        {
            base.Awake();

            m_InteractableSortComparison = InteractableSortComparison;
            CreateDefaultHoverMaterials();
        }

        protected void OnTriggerEnter(Collider col)
        {
            var interactable = interactionManager.TryGetInteractableForCollider(col);
            if (interactable && !m_ValidTargets.Contains(interactable) && selectTarget != interactable)
                m_ValidTargets.Add(interactable);
        }

        protected void OnTriggerExit(Collider col)
        {
            var interactable = interactionManager.TryGetInteractableForCollider(col);
            if (interactable && m_ValidTargets.Contains(interactable) && selectTarget != interactable)
                m_ValidTargets.Remove(interactable);
        }

        /// <summary>
        /// Create the default hover materials
        /// for <see cref="interactableHoverMeshMaterial"/> and <see cref="interactableCantHoverMeshMaterial"/> if necessary.
        /// </summary>
        protected virtual void CreateDefaultHoverMaterials()
        {
            if (m_InteractableHoverMeshMaterial != null && m_InteractableCantHoverMeshMaterial != null)
                return;

            var shaderName = GraphicsSettings.currentRenderPipeline ? "Universal Render Pipeline/Lit" : "Standard";
            var defaultShader = Shader.Find(shaderName);

            if (defaultShader == null)
            {
                Debug.LogWarning("Failed to create default materials for Socket Interactor," +
                    $" was unable to find \"{shaderName}\" Shader. Make sure the shader is included into the game build.", this);
                return;
            }

            if (m_InteractableHoverMeshMaterial == null)
            {
                m_InteractableHoverMeshMaterial = new Material(defaultShader);
                SetMaterialFade(m_InteractableHoverMeshMaterial, new Color(0f, 0f, 1f, 0.6f));
            }

            if (m_InteractableCantHoverMeshMaterial == null)
            {
                m_InteractableCantHoverMeshMaterial = new Material(defaultShader);
                SetMaterialFade(m_InteractableCantHoverMeshMaterial, new Color(1f, 0f, 0f, 0.6f));
            }
        }

        /// <summary>
        /// Set Standard <paramref name="material"/> with Fade rendering mode
        /// and set <paramref name="color"/> as the main color.
        /// </summary>
        /// <param name="material">The <see cref="Material"/> whose properties will be set.</param>
        /// <param name="color">The main color to set.</param>
        static void SetMaterialFade(Material material, Color color)
        {
            material.SetOverrideTag("RenderType", "Transparent");
            material.SetFloat(ShaderPropertyLookup.mode, 2f);
            material.SetInt(ShaderPropertyLookup.srcBlend, (int)BlendMode.SrcAlpha);
            material.SetInt(ShaderPropertyLookup.dstBlend, (int)BlendMode.OneMinusSrcAlpha);
            material.SetInt(ShaderPropertyLookup.zWrite, 0);
            // ReSharper disable StringLiteralTypo
            material.DisableKeyword("_ALPHATEST_ON");
            material.EnableKeyword("_ALPHABLEND_ON");
            material.DisableKeyword("_ALPHAPREMULTIPLY_ON");
            // ReSharper restore StringLiteralTypo
            material.renderQueue = (int)RenderQueue.Transparent;
            material.SetColor(GraphicsSettings.currentRenderPipeline ? ShaderPropertyLookup.baseColor : ShaderPropertyLookup.color, color);
        }

        /// <inheritdoc />
        protected internal override void OnHoverEntering(XRBaseInteractable interactable)
        {
            base.OnHoverEntering(interactable);
            MeshFilter[] interactableMeshFilters = interactable.GetComponentsInChildren<MeshFilter>();
            if (interactableMeshFilters.Length > 0)
            {
                m_MeshFilterCache.Add(interactable, interactableMeshFilters);
            }
        }

        /// <inheritdoc />
        protected internal override void OnHoverExiting(XRBaseInteractable interactable)
        {
            base.OnHoverExiting(interactable);
            m_MeshFilterCache.Remove(interactable);
        }

         /// <inheritdoc />
        protected internal override void OnSelectExiting(XRBaseInteractable interactable)
        {
            base.OnSelectExiting(interactable);
            m_LastRemoveTime = Time.time;
        }

        /// <inheritdoc />
        public override void ProcessInteractor(XRInteractionUpdateOrder.UpdatePhase updatePhase)
        {
            base.ProcessInteractor(updatePhase);

            if (updatePhase == XRInteractionUpdateOrder.UpdatePhase.Dynamic && m_ShowInteractableHoverMeshes && hoverTargets.Count > 0)
                DrawHoveredInteractables();
        }

        Matrix4x4 GetInteractableAttachMatrix(XRGrabInteractable interactable, MeshFilter meshFilter, Vector3 scale)
        {
            var interactableLocalPosition = Vector3.zero;
            var interactableLocalRotation = Quaternion.identity;

            if (interactable.attachTransform != null)
            {
                // localPosition doesn't take into account scaling of parent objects, so scale attachpoint by lossyScale which is the global scale.
                interactableLocalPosition =  Vector3.Scale(interactable.attachTransform.localPosition, interactable.attachTransform.lossyScale);
                interactableLocalRotation = interactable.attachTransform.localRotation;
            }

            var finalPosition = attachTransform.position - interactableLocalPosition;
            var finalRotation = attachTransform.rotation * interactableLocalRotation;

            if(interactable.transform != meshFilter.transform)
            {
                finalPosition += Vector3.Scale(interactable.transform.InverseTransformPoint(meshFilter.transform.position), interactable.transform.lossyScale);
                finalRotation *= Quaternion.Inverse(Quaternion.Inverse(meshFilter.transform.rotation) * interactable.transform.rotation);
            }

            return Matrix4x4.TRS(finalPosition, finalRotation, scale);
        }

        protected virtual void DrawHoveredInteractables()
        {
            var materialToDrawWith = selectTarget == null ? m_InteractableHoverMeshMaterial : m_InteractableCantHoverMeshMaterial;
            if (materialToDrawWith == null)
                return;

            var cam = Camera.main;
            if (cam == null)
                return;

            var cullingMask = cam.cullingMask;

            var hoveredScale = Mathf.Max(0f, m_InteractableHoverScale);

            foreach (var hoverTarget in hoverTargets)
            {
                var grabTarget = hoverTarget as XRGrabInteractable;
                if (grabTarget == null || grabTarget == selectTarget)
                    continue;

                if (!m_MeshFilterCache.TryGetValue(grabTarget, out var interactableMeshFilters))
                    continue;

                if (interactableMeshFilters == null || interactableMeshFilters.Length == 0)
                    continue;

                foreach (var meshFilter in interactableMeshFilters)
                {
                    // TODO By only checking the main camera culling flags, but drawing the mesh in all cameras,
                    // aren't we ignoring the culling mask of non-main cameras? Or does DrawMesh handle culling
                    // automatically, making this early continue unnecessary?
                    if (meshFilter == null || (cullingMask & (1 << meshFilter.gameObject.layer)) == 0)
                        continue;

                    for (var submeshIndex = 0; submeshIndex < meshFilter.sharedMesh.subMeshCount; ++submeshIndex)
                    {
                        Graphics.DrawMesh(
                            meshFilter.sharedMesh,
                            GetInteractableAttachMatrix(grabTarget, meshFilter, meshFilter.transform.lossyScale * hoveredScale),
                            materialToDrawWith,
                            gameObject.layer, // TODO Why use this Interactor layer instead of the Interactable layer?
                            null, // Draw mesh in all cameras (default value)
                            submeshIndex);
                    }
                }
            }
        }

        /// <summary>
        /// Retrieve the list of interactables that this interactor could possibly interact with this frame.
        /// This list is sorted by priority (in this case distance).
        /// </summary>
        /// <param name="validTargets">Populated List of interactables that are valid for selection or hover.</param>
        public override void GetValidTargets(List<XRBaseInteractable> validTargets)
        {
            validTargets.Clear();
            m_InteractableDistanceSqrMap.Clear();

            // Calculate distance squared to interactor's attach transform and add to validTargets (which is sorted before returning)
            foreach (var interactable in m_ValidTargets)
            {
                if (interactable != selectTarget)
                {
                    m_InteractableDistanceSqrMap[interactable] = interactable.GetDistanceSqrToInteractor(this);
                    validTargets.Add(interactable);
                }
            }

            validTargets.Sort(m_InteractableSortComparison);
        }

        /// <summary>
        /// (Read Only) Indicates whether this interactor is in a state where it could hover (always <see langword="true"/> for sockets if active).
        /// </summary>
        public override bool isHoverActive => m_SocketActive;

        /// <summary>
        /// (Read Only) Indicates whether this interactor is in a state where it could select (always <see langword="true"/> for sockets if active).
        /// </summary>
        public override bool isSelectActive => m_SocketActive;

        /// <summary>
        /// (Read Only) Indicates whether this interactor requires exclusive selection of an interactable (always <see langword="true"/> for sockets).
        /// </summary>
        public override bool requireSelectExclusive => true;

        /// <summary>
        /// (Read Only) Overriding movement type of the selected interactable's movement (always <see cref="XRBaseInteractable.MovementType.Kinematic"/> for sockets).
        /// </summary>
        public override XRBaseInteractable.MovementType? selectedInteractableMovementTypeOverride => XRBaseInteractable.MovementType.Kinematic;

        /// <inheritdoc />
        public override bool CanSelect(XRBaseInteractable interactable)
        {
            return base.CanSelect(interactable) && (selectTarget == null || selectTarget == interactable);
        }

        /// <inheritdoc />
        public override bool CanHover(XRBaseInteractable interactable)
        {
            return base.CanHover(interactable) && Time.time > m_LastRemoveTime + m_RecycleDelayTime;
        }

        struct ShaderPropertyLookup
        {
            public static readonly int mode = Shader.PropertyToID("_Mode");
            public static readonly int srcBlend = Shader.PropertyToID("_SrcBlend");
            public static readonly int dstBlend = Shader.PropertyToID("_DstBlend");
            public static readonly int zWrite = Shader.PropertyToID("_ZWrite");
            public static readonly int baseColor = Shader.PropertyToID("_BaseColor");
            public static readonly int color = Shader.PropertyToID("_Color"); // Legacy
        }
    }
}
