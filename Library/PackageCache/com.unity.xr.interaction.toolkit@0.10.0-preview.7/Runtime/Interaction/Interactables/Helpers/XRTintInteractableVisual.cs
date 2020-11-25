using System.Collections.Generic;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Simple Interactable Visual component that demonstrates hover or selection state with emissive tinting.
    /// Note: requires use of a shader that supports emission (such as Standard shader) with the variant included in the game build.
    /// </summary>
    [AddComponentMenu("XR/Helpers/XR Tint Interactable Visual")]
    [DisallowMultipleComponent]
    public class XRTintInteractableVisual : MonoBehaviour
    {
        [SerializeField, Tooltip("Tint color for interactable.")]
        Color m_TintColor = Color.yellow;

        /// <summary>
        /// The tint color for interactable.
        /// </summary>
        public Color tintColor
        {
            get => m_TintColor;
            set => m_TintColor = value;
        }

        [SerializeField, Tooltip("Tint on hover.")]
        bool m_TintOnHover = true;

        /// <summary>
        /// Whether this should tint on hover.
        /// </summary>
        public bool tintOnHover
        {
            get => m_TintOnHover;
            set => m_TintOnHover = value;
        }

        [SerializeField, Tooltip("Tint on selection.")]
        bool m_TintOnSelection = true;

        /// <summary>
        /// Whether this should tint on selection.
        /// </summary>
        public bool tintOnSelection
        {
            get => m_TintOnSelection;
            set => m_TintOnSelection = value;
        }

        [SerializeField, Tooltip("Renderer(s) to use for tinting (will default to any Renderer on the GameObject if not specified).")]
        List<Renderer> m_TintRenderers = new List<Renderer>();

        /// <summary>
        /// The <see cref="Renderer"/>(s) to use for tinting (will default to any <see cref="Renderer"/> on the <see cref="GameObject"/> if not specified).
        /// </summary>
        public List<Renderer> tintRenderers
        {
            get => m_TintRenderers;
            set => m_TintRenderers = value;
        }

        XRBaseInteractable m_Interactable;

        MaterialPropertyBlock m_TintPropertyBlock;

        bool m_EmissionEnabled;

        bool m_HasLoggedMaterialInstance;

        /// <summary>
        /// Reusable list of type <see cref="Material"/> to reduce allocations.
        /// </summary>
        static readonly List<Material> s_Materials = new List<Material>();

        protected void Awake()
        {
            m_Interactable = GetComponent<XRBaseInteractable>();
            if (m_Interactable != null)
            {
                m_Interactable.onFirstHoverEntered.AddListener(OnFirstHoverEntered);
                m_Interactable.onLastHoverExited.AddListener(OnLastHoverExited);
                m_Interactable.onSelectEntered.AddListener(OnSelectEntered);
                m_Interactable.onSelectExited.AddListener(OnSelectExited);
            }
            else
                Debug.LogWarning($"Could not find required interactable component on {gameObject} for tint visual." +
                    " Cannot respond to hover or selection.", this);

            if (m_TintRenderers.Count == 0)
            {
                GetComponents(m_TintRenderers);
                if (m_TintRenderers.Count == 0)
                    Debug.LogWarning($"Could not find required Renderer component on {gameObject} for tint visual.", this);
            }

            // Determine if Emission is enabled on the material, or if material instances will need
            // to be created to enable it.
            m_EmissionEnabled = GetEmissionEnabled();

            m_TintPropertyBlock = new MaterialPropertyBlock();
        }

        protected void OnDestroy()
        {
            if (m_Interactable != null)
            {
                m_Interactable.onFirstHoverEntered.RemoveListener(OnFirstHoverEntered);
                m_Interactable.onLastHoverExited.RemoveListener(OnLastHoverExited);
                m_Interactable.onSelectEntered.RemoveListener(OnSelectEntered);
                m_Interactable.onSelectExited.RemoveListener(OnSelectExited);
            }
        }

        /// <summary>
        /// Apply or remove a tint to all Renderers used for tinting.
        /// </summary>
        /// <param name="on">Whether to apply a tint when <see langword="true"/>, or remove the tint when <see langword="false"/>.</param>
        protected virtual void SetTint(bool on)
        {
            var emissionColor = on ? m_TintColor * Mathf.LinearToGammaSpace(1f) : Color.black;

            if (!m_EmissionEnabled && !m_HasLoggedMaterialInstance)
            {
                Debug.LogWarning("Emission is not enabled on a Material used by a tint visual, a Material instance will need to be created.", this);
                m_HasLoggedMaterialInstance = true;
            }

            foreach (var render in m_TintRenderers)
            {
                if (render == null)
                    continue;

                // Create material instances to enable Emission
                if (!m_EmissionEnabled)
                {
                    render.GetMaterials(s_Materials);
                    foreach (var material in s_Materials)
                    {
                        if (on)
                            material.EnableKeyword("_EMISSION");
                        else
                            material.DisableKeyword("_EMISSION");
                    }
                    s_Materials.Clear();
                }

                render.GetPropertyBlock(m_TintPropertyBlock);
                m_TintPropertyBlock.SetColor(ShaderPropertyLookup.emissionColor, emissionColor);
                render.SetPropertyBlock(m_TintPropertyBlock);
            }
        }

        /// <summary>
        /// Gets whether all shared materials on the Renderers used for tinting have emission enabled.
        /// </summary>
        /// <returns>Returns <see langword="true"/> if all materials used for tinting have emission enabled. Returns <see langword="false"/> otherwise.</returns>
        protected virtual bool GetEmissionEnabled()
        {
            foreach (var render in m_TintRenderers)
            {
                if (render == null)
                    continue;

                render.GetSharedMaterials(s_Materials);
                foreach (var sharedMaterial in s_Materials)
                {
                    if (!sharedMaterial.IsKeywordEnabled("_EMISSION"))
                    {
                        s_Materials.Clear();
                        return false;
                    }
                }
            }

            s_Materials.Clear();
            return true;
        }

        void OnFirstHoverEntered(XRBaseInteractor interactor)
        {
            if (m_TintOnHover)
                SetTint(true);
        }

        void OnLastHoverExited(XRBaseInteractor interactor)
        {
            if (m_TintOnHover)
                SetTint(false);
        }

        void OnSelectEntered(XRBaseInteractor interactor)
        {
            if (m_TintOnSelection)
                SetTint(true);
        }

        void OnSelectExited(XRBaseInteractor interactor)
        {
            if (m_TintOnSelection)
                SetTint(false);
        }

        struct ShaderPropertyLookup
        {
            public static readonly int emissionColor = Shader.PropertyToID("_EmissionColor");
        }
    }
}
