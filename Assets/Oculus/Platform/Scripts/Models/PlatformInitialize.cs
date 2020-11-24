// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class PlatformInitialize
  {
    public readonly PlatformInitializeResult Result;


    public PlatformInitialize(IntPtr o)
    {
      Result = CAPI.ovr_PlatformInitialize_GetResult(o);
    }
  }

}
