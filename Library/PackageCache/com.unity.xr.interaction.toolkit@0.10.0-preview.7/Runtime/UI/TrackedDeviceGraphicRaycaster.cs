using System;
using System.Collections.Generic;
using UnityEngine.EventSystems;
using UnityEngine.UI;

namespace UnityEngine.XR.Interaction.Toolkit.UI
{
    public class TrackedDeviceGraphicRaycaster : BaseRaycaster
    {
        const int k_MaxRaycastHits = 10;

        readonly struct RaycastHitData
        {
            public RaycastHitData(Graphic graphic, Vector3 worldHitPosition, Vector2 screenPosition, float distance)
            {
                this.graphic = graphic;
                this.worldHitPosition = worldHitPosition;
                this.screenPosition = screenPosition;
                this.distance = distance;
            }

            public Graphic graphic { get; }
            public Vector3 worldHitPosition { get; }
            public Vector2 screenPosition { get; }
            public float distance { get; }
        }

        class RaycastHitComparer : IComparer<RaycastHitData>
        {
            public int Compare(RaycastHitData a, RaycastHitData b)
                => b.graphic.depth.CompareTo(a.graphic.depth);
        }

        [SerializeField]
        bool m_IgnoreReversedGraphics;

        public bool ignoreReversedGraphics
        {
            get => m_IgnoreReversedGraphics;
            set => m_IgnoreReversedGraphics = value;
        }

        [SerializeField]
        bool m_CheckFor2DOcclusion;

        public bool checkFor2DOcclusion
        {
            get => m_CheckFor2DOcclusion;
            set => m_CheckFor2DOcclusion = value;
        }

        [SerializeField]
        bool m_CheckFor3DOcclusion;

        public bool checkFor3DOcclusion
        {
            get => m_CheckFor3DOcclusion;
            set => m_CheckFor3DOcclusion = value;
        }

        [SerializeField]
        LayerMask m_BlockingMask = int.MaxValue;

        public LayerMask blockingMask
        {
            get => m_BlockingMask;
            set => m_BlockingMask = value;
        }

        /// <inheritdoc />
#pragma warning disable IDE0031 // Use null propagation -- Do not use for UnityEngine.Object types
        public override Camera eventCamera => canvas != null ? canvas.worldCamera : null;
#pragma warning restore IDE0031

        /// <summary>Perform a raycast against objects within this Raycaster's domain.</summary>
        /// <param name="eventData">Data containing where and how to raycast.</param>
        /// <param name="resultAppendList">The resultant hits from the raycast.</param>
        public override void Raycast(PointerEventData eventData, List<RaycastResult> resultAppendList)
        {
            if (eventData is TrackedDeviceEventData trackedEventData)
            {
                PerformRaycasts(trackedEventData, resultAppendList);
            }
        }

        Canvas m_Canvas;

        Canvas canvas
        {
            get
            {
                if (m_Canvas != null)
                    return m_Canvas;

                m_Canvas = GetComponent<Canvas>();
                return m_Canvas;
            }
        }

        readonly RaycastHit[] m_OcclusionHits3D = new RaycastHit[k_MaxRaycastHits];
        readonly RaycastHit2D[] m_OcclusionHits2D = new RaycastHit2D[k_MaxRaycastHits];
        readonly RaycastHitComparer m_RaycastHitComparer = new RaycastHitComparer();

        static readonly Vector3[] s_Corners = new Vector3[4];

        // Use this list on each raycast to avoid continually allocating.
        readonly List<RaycastHitData> m_RaycastResultsCache = new List<RaycastHitData>();

        [NonSerialized]
        static readonly List<RaycastHitData> s_SortedGraphics = new List<RaycastHitData>();

        static RaycastHit FindClosestHit(RaycastHit[] hits, int count)
        {
            var index = 0;
            var distance = float.MaxValue;
            for (var i = 0; i < count; i++)
            {
                if (hits[i].distance < distance)
                {
                    distance = hits[i].distance;
                    index = i;
                }
            }

            return hits[index];
        }

        static RaycastHit2D FindClosestHit(RaycastHit2D[] hits, int count)
        {
            var index = 0;
            var distance = float.MaxValue;
            for (var i = 0; i < count; i++)
            {
                if (hits[i].distance < distance)
                {
                    distance = hits[i].distance;
                    index = i;
                }
            }

            return hits[index];
        }

        void PerformRaycasts(TrackedDeviceEventData eventData, List<RaycastResult> resultAppendList)
        {
            if (canvas == null)
                return;

            if (eventCamera == null)
                return;

            var rayPoints = eventData.rayPoints;
            var layerMask = eventData.layerMask;
            for(var i = 1; i < rayPoints.Count; i++)
            {
                var from = rayPoints[i - 1];
                var to = rayPoints[i];
                if (PerformRaycast(from, to, layerMask, resultAppendList))
                {
                    eventData.rayHitIndex = i;
                    break;
                }
            }
        }

        bool PerformRaycast(Vector3 from, Vector3 to, LayerMask layerMask, List<RaycastResult> resultAppendList)
        {
            var hitSomething = false;

            var rayDistance = Vector3.Distance(to, from);
            var ray = new Ray(from, (to - from).normalized * rayDistance);

            var hitDistance = rayDistance;
            if (m_CheckFor3DOcclusion)
            {
                var hitCount = Physics.RaycastNonAlloc(ray, m_OcclusionHits3D, hitDistance, m_BlockingMask);

                if (hitCount > 0)
                {
                    var hit = FindClosestHit(m_OcclusionHits3D, hitCount);
                    hitDistance = hit.distance;
                    hitSomething = true;
                }
            }

            if (m_CheckFor2DOcclusion)
            {
                var hitCount = Physics2D.RaycastNonAlloc(ray.origin, ray.direction, m_OcclusionHits2D, m_BlockingMask);

                if (hitCount > 0)
                {
                    var hit = FindClosestHit(m_OcclusionHits2D, hitCount);
                    hitDistance = hit.distance > hitDistance ? hitDistance : hit.distance;
                    hitSomething = true;
                }
            }

            m_RaycastResultsCache.Clear();
            SortedRaycastGraphics(canvas, ray, hitDistance, layerMask, m_RaycastResultsCache);

            // Now that we have a list of sorted hits, process any extra settings and filters.
            foreach (var hitData in m_RaycastResultsCache)
            {
                var validHit = true;

                var go = hitData.graphic.gameObject;
                if (m_IgnoreReversedGraphics)
                {
                    var forward = ray.direction;
                    var goDirection = go.transform.rotation * Vector3.forward;
                    validHit = Vector3.Dot(forward, goDirection) > 0;
                }

                validHit &= hitData.distance < hitDistance;

                if (validHit)
                {
                    var trans = go.transform;
                    var transForward = trans.forward;
                    var castResult = new RaycastResult
                    {
                        gameObject = go,
                        module = this,
                        index = resultAppendList.Count,
                        distance = hitData.distance,
                        depth = hitData.graphic.depth,
                        sortingLayer = canvas.sortingLayerID,
                        sortingOrder = canvas.sortingOrder,
                        worldPosition = hitData.worldHitPosition,
                        worldNormal = -transForward
                    };
                    resultAppendList.Add(castResult);

                    hitSomething = true;
                }
            }

            return hitSomething;
        }

        void SortedRaycastGraphics(Canvas canvas, Ray ray, float maxDistance, LayerMask layerMask, List<RaycastHitData> results)
        {
            var graphics = GraphicRegistry.GetGraphicsForCanvas(canvas);

            s_SortedGraphics.Clear();
            for (int i = 0; i < graphics.Count; ++i)
            {
                var graphic = graphics[i];

                // -1 means it hasn't been processed by the canvas, which means it isn't actually drawn
                if (graphic.depth == -1 || !graphic.raycastTarget || graphic.canvasRenderer.cull)
                    continue;

                if (((1 << graphic.gameObject.layer) & layerMask) == 0)
                    continue;

                if (RayIntersectsRectTransform(graphic.rectTransform, ray, out var worldPos, out var distance))
                {
                    if (distance <= maxDistance)
                    {
                        Vector2 screenPos = eventCamera.WorldToScreenPoint(worldPos);
                        // mask/image intersection - See Unity docs on eventAlphaThreshold for when this does anything
                        if (graphic.Raycast(screenPos, eventCamera))
                        {
                            s_SortedGraphics.Add(new RaycastHitData(graphic, worldPos, screenPos, distance));
                        }
                    }
                }
            }

            SortingHelpers.Sort(s_SortedGraphics, m_RaycastHitComparer);
            results.AddRange(s_SortedGraphics);
        }

        static bool RayIntersectsRectTransform(RectTransform transform, Ray ray, out Vector3 worldPosition, out float distance)
        {
            transform.GetWorldCorners(s_Corners);
            var plane = new Plane(s_Corners[0], s_Corners[1], s_Corners[2]);

            if (plane.Raycast(ray, out var enter))
            {
                var intersection = ray.GetPoint(enter);

                var bottomEdge = s_Corners[3] - s_Corners[0];
                var leftEdge = s_Corners[1] - s_Corners[0];
                var bottomDot = Vector3.Dot(intersection - s_Corners[0], bottomEdge);
                var leftDot = Vector3.Dot(intersection - s_Corners[0], leftEdge);

                // If the intersection is right of the left edge and above the bottom edge.
                if (leftDot >= 0f && bottomDot >= 0f)
                {
                    var topEdge = s_Corners[1] - s_Corners[2];
                    var rightEdge = s_Corners[3] - s_Corners[2];
                    var topDot = Vector3.Dot(intersection - s_Corners[2], topEdge);
                    var rightDot = Vector3.Dot(intersection - s_Corners[2], rightEdge);

                    //If the intersection is left of the right edge, and below the top edge
                    if (topDot >= 0f && rightDot >= 0f)
                    {
                        worldPosition = intersection;
                        distance = enter;
                        return true;
                    }
                }
            }

            worldPosition = Vector3.zero;
            distance = 0f;
            return false;
        }
    }
}
