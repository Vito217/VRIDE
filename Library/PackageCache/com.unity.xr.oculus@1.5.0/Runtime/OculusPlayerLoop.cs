#if UNITY_STANDALONE_WIN && !UNITY_EDITOR
using System.Runtime.InteropServices;
using UnityEngine;
using UnityEngine.LowLevel;
using UnityEngine.PlayerLoop;

namespace Unity.XR.Oculus
{
    public static partial class NativeMethods
    {
        [DllImport("OculusXRPlugin", CharSet = CharSet.Auto)]
        internal static extern bool GetAppShouldQuit();
    }

    internal static class OculusPlayerLoop
    {
        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
        static void Setup()
        {
            var loop = PlayerLoop.GetCurrentPlayerLoop();
            for (var i = 0; i < loop.subSystemList.Length; ++i)
            {
                if ( loop.subSystemList[i].type == typeof(PreUpdate))
                {
                    loop.subSystemList[i].updateDelegate += Update;
                }
            }
            PlayerLoop.SetPlayerLoop(loop);
        }

        static void Update()
        {
            if (NativeMethods.GetAppShouldQuit())
            {
                Application.Quit();
            }
        }
    }
}
#endif
