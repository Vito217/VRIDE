using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using Unity.VectorGraphics;
using UnityEngine;

namespace ImageUtils
{
    public static class ImageModule
    {
        public static Image LoadFromByteArray(byte[] array)
        {
            return (Bitmap)((new ImageConverter()).ConvertFrom(array));
        }

        public static void SaveImage(byte[] array, string path)
        {
            Image img = LoadFromByteArray(array);
            img.Save(path);
        }

        public static byte[] ReadImage(string path)
        {
            Image img = Image.FromFile(path);
            byte[] arr;
            using (MemoryStream ms = new MemoryStream())
            {
                img.Save(ms, System.Drawing.Imaging.ImageFormat.Png);
                arr = ms.ToArray();
            }
            return arr;
        }

        public static Sprite ImportSVG(string path)
        {
            var tessOptions = new VectorUtils.TessellationOptions()
            {
                StepDistance = 100.0f,
                MaxCordDeviation = 0.5f,
                MaxTanAngleDeviation = 0.1f,
                SamplingStepSize = 0.01f
            };
            var sceneInfo = SVGParser.ImportSVG(new StreamReader(path));
            var geoms = VectorUtils.TessellateScene(sceneInfo.Scene, tessOptions);
            Sprite sprite = VectorUtils.BuildSprite(geoms, 100.0f, VectorUtils.Alignment.Center, Vector2.zero, 128, true);
            return sprite;
        }
    }

}