using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Security.Cryptography;
using UnityEditor.Build.Pipeline.Utilities;

namespace UnityEditor.Build.Pipeline.Tests
{
    [TestFixture]
    class HashingPerformanceTests
    {
        const int k_PerfRunCount = 1000;
        const int k_PerfByteSize = 1 * 1024 * 1024;

        static byte[] GetFixedRandomBytes(int size)
        {
            var rnd = new Random(1);
            var b = new byte[size];
            rnd.NextBytes(b);
            return b;
        }

        static void GetRawHashes<T>(byte[] b, List<RawHash> hashes) where T : HashAlgorithm
        {
            for (int i = 0; i < k_PerfRunCount; i++)
                hashes.Add(HashingMethods.Calculate<T>(b));
        }

        [Test]
        public void MD4Performance()
        {
            var b = GetFixedRandomBytes(k_PerfByteSize);

            var timer = new Stopwatch();
            var hashes = new List<RawHash>(k_PerfRunCount);

            timer.Start();
            GetRawHashes<MD4>(b, hashes);
            timer.Stop();

            var first = hashes[0];
            foreach (var hash in hashes)
                Assert.AreEqual(first, hash);
            UnityEngine.Debug.Log($"MD4 Hash Time {timer.Elapsed}");
        }

        [Test]
        public void MD5Performance()
        {
            var b = GetFixedRandomBytes(k_PerfByteSize);

            var timer = new Stopwatch();
            var hashes = new List<RawHash>(k_PerfRunCount);

            timer.Start();
            GetRawHashes<MD5>(b, hashes);
            timer.Stop();

            var first = hashes[0];
            foreach (var hash in hashes)
                Assert.AreEqual(first, hash);
            UnityEngine.Debug.Log($"MD5 Hash Time {timer.Elapsed}");
        }

#if UNITY_2019_3_OR_NEWER
        [Test]
        public void SpookyHashPerformance()
        {
            var b = GetFixedRandomBytes(k_PerfByteSize);

            var timer = new Stopwatch();
            var hashes = new List<RawHash>(k_PerfRunCount);

            timer.Start();
            GetRawHashes<SpookyHash>(b, hashes);
            timer.Stop();

            var first = hashes[0];
            foreach (var hash in hashes)
                Assert.AreEqual(first, hash);
            UnityEngine.Debug.Log($"SpookyHash Hash Time {timer.Elapsed}");
        }
#endif
    }
}
