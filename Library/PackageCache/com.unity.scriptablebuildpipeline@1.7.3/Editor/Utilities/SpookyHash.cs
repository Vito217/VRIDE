#if UNITY_2019_3_OR_NEWER
using System.Security.Cryptography;
using UnityEngine;

namespace UnityEditor.Build.Pipeline.Utilities
{
    public unsafe sealed class SpookyHash : HashAlgorithm
    {
        byte[] m_Hash;

        SpookyHash()
        {
            Initialize();
        }

        public new static SpookyHash Create()
        {
            return new SpookyHash();
        }

        public override void Initialize()
        {
            m_Hash = new byte[16];
        }

        protected override void HashCore(byte[] array, int ibStart, int cbSize)
        {
            fixed (byte* data = &array[ibStart])
            fixed (byte* hash = &m_Hash[0])
            {
                var dataSize = (ulong)cbSize;
                var lower = (ulong*)&hash[0];
                var upper = (ulong*)&hash[8];
                HashUnsafeUtilities.ComputeHash128(data, dataSize, lower, upper);
            }
        }

        protected override byte[] HashFinal()
        {
            return m_Hash;
        }
    }
}
#endif