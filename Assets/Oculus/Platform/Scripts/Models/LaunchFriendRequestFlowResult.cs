// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class LaunchFriendRequestFlowResult
  {
    public readonly bool DidCancel;
    public readonly bool DidSendRequest;


    public LaunchFriendRequestFlowResult(IntPtr o)
    {
      DidCancel = CAPI.ovr_LaunchFriendRequestFlowResult_GetDidCancel(o);
      DidSendRequest = CAPI.ovr_LaunchFriendRequestFlowResult_GetDidSendRequest(o);
    }
  }

}
