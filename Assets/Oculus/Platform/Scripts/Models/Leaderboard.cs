// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class Leaderboard
  {
    public readonly string ApiName;


    public Leaderboard(IntPtr o)
    {
      ApiName = CAPI.ovr_Leaderboard_GetApiName(o);
    }
  }

}
