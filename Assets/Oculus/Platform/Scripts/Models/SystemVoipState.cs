// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class SystemVoipState
  {
    public readonly VoipMuteState MicrophoneMuted;
    public readonly SystemVoipStatus Status;


    public SystemVoipState(IntPtr o)
    {
      MicrophoneMuted = CAPI.ovr_SystemVoipState_GetMicrophoneMuted(o);
      Status = CAPI.ovr_SystemVoipState_GetStatus(o);
    }
  }

}
