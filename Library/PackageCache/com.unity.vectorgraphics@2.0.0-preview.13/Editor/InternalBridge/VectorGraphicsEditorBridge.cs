using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using UnityEditor;
using UnityEditor.U2D;
using UnityEditor.U2D.Interface;
#if !(UNITY_2019_3_OR_NEWER)
using UnityEditor.Experimental.U2D;
#endif
using UnityEngine.U2D.Interface;

namespace Unity.VectorGraphics.Editor
{
    internal static class InternalEditorBridge
    {
        internal static void ShowSpriteEditorWindow()
        {
            SpriteUtilityWindow.ShowSpriteEditorWindow();
        }

        internal static List<Vector2[]> GenerateOutline(UnityEngine.Texture2D texture, Rect rect, float detail, byte alphaTolerance, bool holeDetection)
        {
            Vector2[][] paths;
            UnityEditor.Sprites.SpriteUtility.GenerateOutline(texture, rect, detail, alphaTolerance, holeDetection, out paths);
            if (paths == null || paths.Length == 0)
                return null;
            return paths.ToList();
        }

        internal static string GetTextureInfoString(UnityEngine.Texture2D tex)
        {
            var format = TextureUtil.GetTextureFormat(tex);
            return "" + tex.width + "x" + tex.height + " " + 
                TextureUtil.GetTextureFormatString(format) + " " +
                EditorUtility.FormatBytes(TextureUtil.GetStorageMemorySizeLong(tex));
        }
    }
}