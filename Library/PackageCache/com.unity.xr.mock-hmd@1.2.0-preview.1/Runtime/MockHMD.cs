using System.Runtime.InteropServices;

namespace Unity.XR.MockHMD
{
    /// <summary>
    /// Runtime scripting API for Mock HMD provider.
    /// </summary>
    public static class MockHMD
    {
        private const string LibraryName = "UnityMockHMD";

        /// <summary>
        /// Set the stereo rendering mode.
        /// </summary>
        /// <param name="renderMode">rendering mode</param>
        /// <returns>true if render mode successfully set</returns>
        [DllImport(LibraryName, EntryPoint = "NativeConfig_SetRenderMode")]
        public static extern bool SetRenderMode(MockHMDBuildSettings.RenderMode renderMode);

        /// <summary>
        /// Set the resolution of the eye textures.
        /// </summary>
        [DllImport(LibraryName, EntryPoint = "NativeConfig_SetEyeResolution")]
        public static extern bool SetEyeResolution(int width, int height);

        /// <summary>
        /// Set the crop value applied when rendering the mirror view.
        /// This is useful to remove the peripheral distorted part of the image.
        /// </summary>
        /// <param name="crop">the amount to remove from the image, valid range is 0.0 to 0.5</param>
        [DllImport(LibraryName, EntryPoint = "NativeConfig_SetMirrorViewCrop")]
        public static extern bool SetMirrorViewCrop(float crop);
    }
}