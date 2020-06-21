using System;
using System.Reflection;
using UnityEditor.Build.Pipeline.Interfaces;

namespace UnityEditor.Build.Pipeline.Utilities
{
    /// <summary>
    /// Internal interface so switch platform build task can initialize editor build callbacks
    /// </summary>
    internal interface IEditorBuildCallbacks : IContextObject
    {
        /// <summary>
        /// Callbacks need to be Initialized after platform switch
        /// </summary>
        void InitializeCallbacks();
    }

    /// <summary>
    /// Manages initialization and cleanup of Unity Editor IPreprocessShaders, IProcessScene, & IProcessSceneWithReport build callbacks.
    /// </summary>
    public class BuildInterfacesWrapper : IDisposable, IEditorBuildCallbacks
    {
        Type m_Type = null;

        bool m_Disposed = false;

        /// <summary>
        /// Default constructor, initializes properties to defaults
        /// </summary>
        public BuildInterfacesWrapper()
        {
            m_Type = Type.GetType("UnityEditor.Build.BuildPipelineInterfaces, UnityEditor");
            InitializeCallbacks();
        }

        /// <summary>
        /// Public dispose function when instance is not in a using statement and manual dispose is required
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (m_Disposed)
                return;

            CleanupCallbacks();
            m_Disposed = true;
        }

        /// <summary>
        /// Initializes Unity Editor IPreprocessShaders, IProcessScene, & IProcessSceneWithReport build callbacks.
        /// </summary>
        public void InitializeCallbacks()
        {
            var init = m_Type.GetMethod("InitializeBuildCallbacks", BindingFlags.NonPublic | BindingFlags.Static);
            init.Invoke(null, new object[] { 18 }); // 18 = BuildCallbacks.SceneProcessors | BuildCallbacks.ShaderProcessors
        }

        /// <summary>
        /// Cleanup Unity Editor IPreprocessShaders, IProcessScene, & IProcessSceneWithReport build callbacks.
        /// </summary>
        public void CleanupCallbacks()
        {
            var clean = m_Type.GetMethod("CleanupBuildCallbacks", BindingFlags.NonPublic | BindingFlags.Static);
            clean.Invoke(null, null);
        }
    }
}
