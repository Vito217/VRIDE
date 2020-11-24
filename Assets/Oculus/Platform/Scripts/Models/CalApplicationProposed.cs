// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class CalApplicationProposed
  {
    public readonly UInt64 ID;


    public CalApplicationProposed(IntPtr o)
    {
      ID = CAPI.ovr_CalApplicationProposed_GetID(o);
    }
  }

}
