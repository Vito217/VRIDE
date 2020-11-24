// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class PartyID
  {
    public readonly UInt64 ID;


    public PartyID(IntPtr o)
    {
      ID = CAPI.ovr_PartyID_GetID(o);
    }
  }

}
