// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class OrgScopedID
  {
    public readonly UInt64 ID;


    public OrgScopedID(IntPtr o)
    {
      ID = CAPI.ovr_OrgScopedID_GetID(o);
    }
  }

}
