namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// The update order for <see cref="MonoBehaviour"/>s in XR Interaction.
    /// </summary>
    /// <remarks>
    /// This is primarily used to control initialization order as the update of interactors / interaction manager / interactables is handled by the
    /// Interaction managers themselves.
    /// </remarks>
    public static class XRInteractionUpdateOrder
    {
        public const int k_ControllerRecorder = -30000;
        public const int k_DeviceSimulator = k_Controllers - 1;
        public const int k_Controllers = k_ControllerRecorder + 10;
        public const int k_InteractionManager = -100;
        public const int k_Interactors = k_InteractionManager + 1;
        public const int k_Interactables = k_Interactors + 1;
        public const int k_LineVisual = 100;
        public const int k_BeforeRenderOrder = 100;
        public const int k_BeforeRenderLineVisual = k_BeforeRenderOrder + 1;

        public enum UpdatePhase
        {
            Fixed,
            Dynamic,
            Late,
            OnBeforeRender,
        }
    }
}
