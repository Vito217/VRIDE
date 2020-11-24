// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class ShareMediaResult
  {
    public readonly ShareMediaStatus Status;


    public ShareMediaResult(IntPtr o)
    {
      Status = CAPI.ovr_ShareMediaResult_GetStatus(o);
    }
  }

}
