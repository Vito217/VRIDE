using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Unity.Android.Logcat
{
    internal static class Paths
    {
        public static string Combine(params string[] components)
        {
            var path = components.Where(_ => !string.IsNullOrEmpty(_)).Aggregate(Path.Combine);
            if (string.IsNullOrEmpty(path))
                throw new ArgumentException("At least one component must be provided!");
            return path;
        }

        public static string[] Split(string path)
        {
            return Split(path, Path.DirectorySeparatorChar);
        }

        public static string[] Split(string path, char separator)
        {
            var result = new List<string>(path.Split(separator));

            for (var i = 0; i < result.Count;)
            {
                result[i] = result[i].Trim();
                if (result[i].Equals(""))
                {
                    result.RemoveAt(i);
                }
                else
                {
                    i++;
                }
            }

            return result.ToArray();
        }

        public static string CreateTempDirectory()
        {
            for (int i = 0; i < 32; ++i)
            {
                string tempDirectory = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());

                if (File.Exists(tempDirectory) || Directory.Exists(tempDirectory))
                    continue;

                Directory.CreateDirectory(tempDirectory);

                return tempDirectory;
            }
            throw new IOException("CreateTempDirectory failed");
        }

        public static string UnifyDirectorySeparator(string path)
        {
            return path.Replace('\\', Path.DirectorySeparatorChar).Replace('/', Path.DirectorySeparatorChar);
        }
    }
}
