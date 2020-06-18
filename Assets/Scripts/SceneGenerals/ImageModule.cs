using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
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

        private static byte[] toByteArray(string responseString, string pattern)
        {
            responseString = Regex.Replace(responseString, pattern, "");
            return responseString.Split(' ').Select(x => Byte.Parse(x, NumberStyles.Integer, null)).ToArray();
        }

        public static Sprite ImportSVG(string responseString)
        {
            string path = Application.persistentDataPath + @"\temp";
            File.WriteAllBytes(
                path,
                toByteArray(responseString, @"#|\[|\]|\n|( 0)*")
            );
            var tessOptions = new VectorUtils.TessellationOptions()
            {
                StepDistance = 100.0f,
                MaxCordDeviation = 0.5f,
                MaxTanAngleDeviation = 0.1f,
                SamplingStepSize = 0.01f
            };
            var sceneInfo = SVGParser.ImportSVG(new StreamReader(path));
            var geoms = VectorUtils.TessellateScene(sceneInfo.Scene, tessOptions);
            return VectorUtils.BuildSprite(geoms, 100.0f, VectorUtils.Alignment.Center, Vector2.zero, 128, true);
        }

        public static Sprite ImportPNG(string responseString)
        {
            Texture2D tex = new Texture2D(2, 2);
            tex.LoadImage(toByteArray(responseString, @"#|\[|\]|\n"));
            return Sprite.Create(
                tex,
                new Rect(0, 0, tex.width, tex.height),
                Vector2.zero
            );
        }
    }
}