using System;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Get line points and hit point info for rendering.
    /// </summary>
    public interface ILineRenderable
    {
        /// <summary>
        /// Performs the computation to convert the sample points from their local positions to world space
        /// and outputs an array of those points to form a line.
        /// </summary>
        /// <param name="linePoints">When this method returns, contains the sample points if successful.</param>
        /// <param name="numPoints">When this method returns, contains the number of sample points if successful.</param>
        /// <returns>Returns <see langword="true"/> if the sample points form a valid line, such as by having at least two points.
        /// Returns <see langword="false"/> otherwise.</returns>
        bool GetLinePoints(ref Vector3[] linePoints, out int numPoints);

        /// <summary>
        /// Gets the current raycast hit information., it will return the world position and the normal vector
        /// of the hit point, and its position in linePoints.
        /// </summary>
        /// <param name="position">When this method returns, contains the world position of the ray impact point if the raycast result is valid.</param>
        /// <param name="normal">When this method returns, contains the world normal of the surface the ray hit if the raycast result is valid.</param>
        /// <param name="positionInLine">When this method returns, contains the index within the list of raycast points returned by <see cref="GetLinePoints"/>.</param>
        /// <param name="isValidTarget">When this method returns, contains whether both the raycast result is valid and a valid target for interaction.</param>
        /// <returns>Returns <see langword="true"/> if the raycast result is valid. Returns <see langword="false"/> otherwise.</returns>
        bool TryGetHitInfo(out Vector3 position, out Vector3 normal, out int positionInLine, out bool isValidTarget);
    }

    /// <summary>
    /// Interactor helper object aligns a <see cref="LineRenderer"/> with the Interactor.
    /// </summary>
    [AddComponentMenu("XR/Helpers/XR Interactor Line Visual")]
    [DisallowMultipleComponent]
    [RequireComponent(typeof(LineRenderer))]
    [DefaultExecutionOrder(XRInteractionUpdateOrder.k_LineVisual)]
    public class XRInteractorLineVisual : MonoBehaviour, IXRCustomReticleProvider
    {
        const float k_MinLineWidth = 0.0001f;
        const float k_MaxLineWidth = 0.05f;

        [SerializeField, Range(k_MinLineWidth, k_MaxLineWidth)]
        [Tooltip("Controls the width of the line.")]
        float m_LineWidth = 0.02f;
        /// <summary>
        /// Controls the width of the line.
        /// </summary>
        public float lineWidth
        {
            get => m_LineWidth;
            set
            {
                m_LineWidth = value;
                m_PerformSetup = true;
            }
        }

        [SerializeField]
        [Tooltip("Controls which source is used to determine the length of the line. Set to true to use the Line Length set by this behavior. Set to false have the length of the line determined by the interactor.")]
        bool m_OverrideInteractorLineLength = true;
        /// <summary>
        /// A boolean value that controls which source is used to determine the length of the line.
        /// Set to <see langword="true"/> to use the Line Length set by this behavior.
        /// Set to <see langword="false"/> have the length of the line determined by the interactor.
        /// </summary>
        /// <seealso cref="lineLength"/>
        public bool overrideInteractorLineLength
        {
            get => m_OverrideInteractorLineLength;
            set => m_OverrideInteractorLineLength = value;
        }

        [SerializeField]
        [Tooltip("Controls the length of the line when overriding.")]
        float m_LineLength = 10f;
        /// <summary>
        /// Controls the length of the line when overriding.
        /// </summary>
        /// <seealso cref="overrideInteractorLineLength"/>
        public float lineLength
        {
            get => m_LineLength;
            set => m_LineLength = value;
        }

        [SerializeField]
        [Tooltip("Controls the relative width of the line from start to end.")]
        AnimationCurve m_WidthCurve = AnimationCurve.Linear(0f, 1f, 1f, 1f);
        /// <summary>
        /// Controls the relative width of the line from start to end.
        /// </summary>
        public AnimationCurve widthCurve
        {
            get => m_WidthCurve;
            set
            {
                m_WidthCurve = value;
                m_PerformSetup = true;
            }
        }

        [SerializeField]
        [Tooltip("Controls the color of the line as a gradient from start to end to indicate a valid state.")]
        Gradient m_ValidColorGradient = new Gradient
        {
            colorKeys = new[] { new GradientColorKey(Color.white, 0f), new GradientColorKey(Color.white, 1f) },
            alphaKeys = new[] { new GradientAlphaKey(1f, 0f), new GradientAlphaKey(1f, 1f) },
        };
        /// <summary>
        /// Controls the color of the line as a gradient from start to end to indicate a valid state.
        /// </summary>
        public Gradient validColorGradient
        {
            get => m_ValidColorGradient;
            set => m_ValidColorGradient = value;
        }

        [SerializeField]
        [Tooltip("Controls the color of the line as a gradient from start to end to indicate an invalid state.")]
        Gradient m_InvalidColorGradient = new Gradient
        {
            colorKeys = new[] { new GradientColorKey(Color.red, 0f), new GradientColorKey(Color.red, 1f) },
            alphaKeys = new[] { new GradientAlphaKey(1f, 0f), new GradientAlphaKey(1f, 1f) },
        };
        /// <summary>
        /// Controls the color of the line as a gradient from start to end to indicate an invalid state.
        /// </summary>
        public Gradient invalidColorGradient
        {
            get => m_InvalidColorGradient;
            set => m_InvalidColorGradient = value;
        }

        [SerializeField]
        [Tooltip("Controls whether the rendered segments will be delayed from and smoothly follow the target segments.")]
        bool m_SmoothMovement;
        /// <summary>
        /// Controls whether the rendered segments will be delayed from and smoothly follow the target segments.
        /// </summary>
        /// <seealso cref="followTightness"/>
        /// <seealso cref="snapThresholdDistance"/>
        public bool smoothMovement
        {
            get => m_SmoothMovement;
            set => m_SmoothMovement = value;
        }

        [SerializeField]
        [Tooltip("Controls the speed that the rendered segments will follow the target segments when Smooth Movement is enabled.")]
        float m_FollowTightness = 10f;
        /// <summary>
        /// Controls the speed that the rendered segments will follow the target segments when Smooth Movement is enabled.
        /// </summary>
        /// <seealso cref="smoothMovement"/>
        /// <seealso cref="snapThresholdDistance"/>
        public float followTightness
        {
            get => m_FollowTightness;
            set => m_FollowTightness = value;
        }

        [SerializeField]
        [Tooltip("Controls the threshold distance between line points at two consecutive frames to snap rendered segments to target segments when Smooth Movement is enabled.")]
        float m_SnapThresholdDistance = 10f;
        /// <summary>
        /// Controls the threshold distance between line points at two consecutive frames to snap rendered segments to target segments when Smooth Movement is enabled.
        /// </summary>
        /// <seealso cref="smoothMovement"/>
        /// <seealso cref="followTightness"/>
        public float snapThresholdDistance
        {
            get => m_SnapThresholdDistance;
            set => m_SnapThresholdDistance = value;
        }

        [SerializeField]
        [Tooltip("Stores the reticle that will appear at the end of the line when it is valid.")]
        GameObject m_Reticle;
        /// <summary>
        /// Stores the reticle that will appear at the end of the line when it is valid.
        /// </summary>
        public GameObject reticle
        {
            get => m_Reticle;
            set => m_Reticle = value;
        }

        [SerializeField]
        [Tooltip("Controls whether the line will always be cut short by this behavior at the first raycast hit, even when invalid.")]
        bool m_StopLineAtFirstRaycastHit = true;
        /// <summary>
        /// Controls whether the line will always be cut short by this behavior at the first raycast hit, even when invalid.
        /// </summary>
        /// <remarks>
        /// The line will always be cut short by this behavior when pointing at a valid target.
        /// <see langword="true"/> means to do the same even when pointing at an invalid target.
        /// <see langword="false"/> means the line will continue to the configured line length.
        /// </remarks>
        public bool stopLineAtFirstRaycastHit
        {
            get => m_StopLineAtFirstRaycastHit;
            set => m_StopLineAtFirstRaycastHit = value;
        }

        Vector3 m_ReticlePos;
        Vector3 m_ReticleNormal;
        int m_EndPositionInLine;

        bool m_SnapCurve = true;
        bool m_PerformSetup;
        GameObject m_ReticleToUse;

        LineRenderer m_LineRenderer;

        // interface to get target point
        ILineRenderable m_LineRenderable;

        // reusable lists of target points
        Vector3[] m_TargetPoints;
        int m_NoTargetPoints = -1;

        // reusable lists of rendered points
        Vector3[] m_RenderPoints;
        int m_NoRenderPoints = -1;

        // reusable lists of rendered points to smooth movement
        Vector3[] m_PreviousRenderPoints;
        int m_NoPreviousRenderPoints = -1;

        readonly Vector3[] m_ClearArray = { Vector3.zero, Vector3.zero };

        GameObject m_CustomReticle;
        bool m_CustomReticleAttached;

        protected void Reset()
        {
            if (TryFindLineRenderer())
            {
                ClearLineRenderer();
                UpdateSettings();
            }
        }

        protected void OnValidate()
        {
            UpdateSettings();
        }

        protected void Awake()
        {
            m_LineRenderable = GetComponent<ILineRenderable>();

            if (m_Reticle != null)
                m_Reticle.SetActive(false);

            UpdateSettings();
        }

        protected void OnEnable()
        {
            m_SnapCurve = true;
            m_ReticleToUse = null;

            Reset();

            Application.onBeforeRender += OnBeforeRenderLineVisual;
        }

        protected void OnDisable()
        {
            if (m_LineRenderer != null)
                m_LineRenderer.enabled = false;
            m_ReticleToUse = null;

            Application.onBeforeRender -= OnBeforeRenderLineVisual;
        }

        void ClearLineRenderer()
        {
            if (TryFindLineRenderer())
            {
                m_LineRenderer.SetPositions(m_ClearArray);
                m_LineRenderer.positionCount = 0;
            }
        }

        [BeforeRenderOrder(XRInteractionUpdateOrder.k_BeforeRenderLineVisual)]
        void OnBeforeRenderLineVisual()
        {
            UpdateLineVisual();
        }

        void UpdateLineVisual()
        {
            if (m_PerformSetup)
            {
                UpdateSettings();
                m_PerformSetup = false;
            }
            if (m_LineRenderer == null)
                return;
            if (m_LineRenderable == null)
            {
                m_LineRenderer.enabled = false;
                return;
            }

            m_NoRenderPoints = 0;

            // Get all the line sample points from the ILineRenderable interface
            if (!m_LineRenderable.GetLinePoints(ref m_TargetPoints, out m_NoTargetPoints))
            {
                m_LineRenderer.enabled = false;
                ClearLineRenderer();
                return;
            }

            // Sanity check.
            if (m_TargetPoints == null ||
                m_TargetPoints.Length == 0 ||
                m_NoTargetPoints == 0 ||
                m_NoTargetPoints > m_TargetPoints.Length)
            {
                m_LineRenderer.enabled = false;
                ClearLineRenderer();
                return;
            }

            // Make sure we have the correct sized arrays for everything.
            if (m_RenderPoints == null || m_RenderPoints.Length < m_NoTargetPoints)
            {
                m_RenderPoints = new Vector3[m_NoTargetPoints];
                m_NoRenderPoints = 0;
            }
            if (m_PreviousRenderPoints == null)
            {
                m_PreviousRenderPoints = new Vector3[m_NoTargetPoints];
                m_NoPreviousRenderPoints = 0;
            }

            // If there is a big movement (snap turn, teleportation), snap the curve
            if (m_PreviousRenderPoints.Length != m_NoTargetPoints)
            {
                m_SnapCurve = true;
            }
            else
            {
                // Compare the two endpoints of the curve, as that will have the largest delta.
                if (m_PreviousRenderPoints != null &&
                    m_NoPreviousRenderPoints > 0 &&
                    m_NoPreviousRenderPoints <= m_PreviousRenderPoints.Length &&
                    m_TargetPoints != null &&
                    m_NoTargetPoints > 0 &&
                    m_NoTargetPoints <= m_TargetPoints.Length)
                {
                    var prevPointIndex = m_NoPreviousRenderPoints - 1;
                    var currPointIndex = m_NoTargetPoints - 1;
                    if (Vector3.Distance(m_PreviousRenderPoints[prevPointIndex], m_TargetPoints[currPointIndex]) > m_SnapThresholdDistance)
                    {
                        m_SnapCurve = true;
                    }
                }
            }

            // If the line hits, insert reticle position into the list for smoothing.
            // Remove the last point in the list to keep the number of points consistent.
            if (m_LineRenderable.TryGetHitInfo(out m_ReticlePos, out m_ReticleNormal, out m_EndPositionInLine, out var isValidTarget))
            {
                // End the line at the current hit point.
                if ((isValidTarget || m_StopLineAtFirstRaycastHit) && m_EndPositionInLine > 0 && m_EndPositionInLine <= m_NoTargetPoints)
                {
                    m_TargetPoints[m_EndPositionInLine] = m_ReticlePos;
                    m_NoTargetPoints = m_EndPositionInLine + 1;
                }
            }

            if (m_SmoothMovement && (m_NoPreviousRenderPoints == m_NoTargetPoints) && !m_SnapCurve)
            {
                // Smooth movement by having render points follow target points
                var length = 0f;
                var maxRenderPoints = m_RenderPoints.Length;
                for (var i = 0; i < m_NoTargetPoints && m_NoRenderPoints < maxRenderPoints; ++i)
                {
                    var smoothPoint = Vector3.Lerp(m_PreviousRenderPoints[i], m_TargetPoints[i], m_FollowTightness * Time.deltaTime);

                    if (m_OverrideInteractorLineLength)
                    {
                        if (m_NoRenderPoints > 0 && m_RenderPoints.Length > 0)
                        {
                            var segLength = Vector3.Distance(m_RenderPoints[m_NoRenderPoints - 1], smoothPoint);
                            length += segLength;
                            if (length > m_LineLength)
                            {
                                var delta = length - m_LineLength;
                                // Re-project final point to match the desired length
                                smoothPoint = Vector3.Lerp(m_RenderPoints[m_NoRenderPoints - 1], smoothPoint, delta / segLength);
                                m_RenderPoints[m_NoRenderPoints] = smoothPoint;
                                m_NoRenderPoints++;
                                break;
                            }
                        }

                        m_RenderPoints[m_NoRenderPoints] = smoothPoint;
                        m_NoRenderPoints++;
                    }
                    else
                    {
                        m_RenderPoints[m_NoRenderPoints] = smoothPoint;
                        m_NoRenderPoints++;
                    }
                }
            }
            else
            {
                if (m_OverrideInteractorLineLength)
                {
                    var length = 0f;
                    var maxRenderPoints = m_RenderPoints.Length;
                    for (var i = 0; i < m_NoTargetPoints && m_NoRenderPoints < maxRenderPoints; ++i)
                    {
                        var newPoint = m_TargetPoints[i];
                        if (m_NoRenderPoints > 0 && m_RenderPoints.Length > 0)
                        {
                            var segLength = Vector3.Distance(m_RenderPoints[m_NoRenderPoints - 1], newPoint);
                            length += segLength;
                            if (length > m_LineLength)
                            {
                                var delta = length - m_LineLength;
                                // Re-project final point to match the desired length
                                var resolvedPoint = Vector3.Lerp(m_RenderPoints[m_NoRenderPoints - 1], newPoint, 1-(delta / segLength));
                                m_RenderPoints[m_NoRenderPoints] = resolvedPoint;
                                m_NoRenderPoints++;
                                break;
                            }
                        }

                        m_RenderPoints[m_NoRenderPoints] = newPoint;
                        m_NoRenderPoints++;
                    }
                }
                else
                {
                    Array.Copy(m_TargetPoints, m_RenderPoints, m_NoTargetPoints);
                    m_NoRenderPoints = m_NoTargetPoints;
                }
            }

            // When a straight line has only two points and color gradients have more than two keys,
            // interpolate points between the two points to enable better color gradient effects.
            if (isValidTarget)
            {
                m_LineRenderer.colorGradient = m_ValidColorGradient;
                // Set reticle position and show reticle
                m_ReticleToUse = m_CustomReticleAttached ? m_CustomReticle : m_Reticle;
                if (m_ReticleToUse != null)
                {
                    m_ReticleToUse.transform.position = m_ReticlePos;
                    m_ReticleToUse.transform.up = m_ReticleNormal;
                    m_ReticleToUse.SetActive(true);
                }
            }
            else
            {
                m_LineRenderer.colorGradient = m_InvalidColorGradient;
                m_ReticleToUse = m_CustomReticleAttached ? m_CustomReticle : m_Reticle;
                if (m_ReticleToUse != null)
                {
                    m_ReticleToUse.SetActive(false);
                }
            }

            if (m_NoRenderPoints >= 2)
            {
                m_LineRenderer.enabled = true;
                m_LineRenderer.positionCount = m_NoRenderPoints;
                m_LineRenderer.SetPositions(m_RenderPoints);
            }
            else
            {
                m_LineRenderer.enabled = false;
                ClearLineRenderer();
                return;
            }

            // Update previous points
            Array.Copy(m_RenderPoints, m_PreviousRenderPoints, m_NoRenderPoints);
            m_NoPreviousRenderPoints = m_NoRenderPoints;
            m_SnapCurve = false;
            m_ReticleToUse = null;
        }

        void UpdateSettings()
        {
            if (TryFindLineRenderer())
            {
                m_LineRenderer.widthMultiplier =  Mathf.Clamp(m_LineWidth, k_MinLineWidth, k_MaxLineWidth);
                m_LineRenderer.widthCurve = m_WidthCurve;
                m_SnapCurve = true;
            }
        }

        bool TryFindLineRenderer()
        {
            m_LineRenderer = GetComponent<LineRenderer>();
            if (m_LineRenderer == null)
            {
                Debug.LogWarning("No Line Renderer found for Interactor Line Visual.", this);
                enabled = false;
                return false;
            }
            return true;
        }

        public bool AttachCustomReticle(GameObject reticleInstance)
        {
            if (!m_CustomReticleAttached)
            {
                if (m_Reticle != null)
                {
                    m_Reticle.SetActive(false);
                }
            }
            else
            {
                if (m_CustomReticle != null)
                {
                    m_CustomReticle.SetActive(false);
                }
            }

            m_CustomReticle = reticleInstance;
            if (m_CustomReticle != null)
            {
                m_CustomReticle.SetActive(true);
            }

            m_CustomReticleAttached = true;
            return false;
        }

        public bool RemoveCustomReticle()
        {
            if (m_CustomReticle != null)
            {
                m_CustomReticle.SetActive(false);
            }

            if (m_Reticle != null)
            {
                m_Reticle.SetActive(true);
            }

            m_CustomReticle = null;
            m_CustomReticleAttached = false;
            return false;
        }
    }

}
