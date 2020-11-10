using System;
using System.Drawing;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Unity.VectorGraphics;
using UnityEngine;

/// <summary>
/// Tools for image processing
/// </summary>
namespace ImageUtils
{
    /// <summary>
    /// Processes PNG and SVG images
    /// </summary>
    public static class ImageModule
    {
        /// <summary>
        /// Convert bytearray to Iamge
        /// </summary>
        /// <param name="array">the bytearray</param>
        /// <returns>An Image object</returns>
        public static Image LoadFromByteArray(byte[] array)
        {
            return (Bitmap)((new ImageConverter()).ConvertFrom(array));
        }

        /// <summary>
        /// Stores bytearray as image file.
        /// </summary>
        /// <param name="array">The bytearray</param>
        /// <param name="path">Dir. path</param>
        public static void SaveImage(byte[] array, string path)
        {
            Image img = LoadFromByteArray(array);
            img.Save(path);
        }

        /// <summary>
        /// Loads image from path
        /// </summary>
        /// <param name="path">Dir. path</param>
        /// <returns>Image bytes</returns>
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

        /// <summary>
        /// Convert string-formated bytearray to bytearray
        /// </summary>
        /// <param name="responseString">Formated string</param>
        /// <param name="pattern">How the string is formated</param>
        /// <returns>A byte array</returns>
        public static byte[] toByteArray(string responseString, string pattern)
        {
            responseString = Regex.Replace(responseString, pattern, "");
            return responseString.Split(' ').Select(x => Byte.Parse(x, NumberStyles.Integer, null)).ToArray();
        }

        /// <summary>
        /// Converts a string-formated SVG bytearray to a Sprite.
        /// </summary>
        /// <param name="responseString">Formated bytearray</param>
        /// <returns>A Sprite</returns>
        public static Sprite ImportSVG(string responseString)
        {
            string path = Application.persistentDataPath + @"\temp.svg";
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

        /// <summary>
        /// Converts a string-formated PNG bytearray to a Sprite.
        /// </summary>
        /// <param name="responseString">Formated bytearray</param>
        /// <returns>A Sprite</returns>
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