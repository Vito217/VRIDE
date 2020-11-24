// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;
    using System.Collections.Generic;

    public class NetSyncVoipAttenuationValue
  {
    public readonly float Decibels;
    public readonly float Distance;


    public NetSyncVoipAttenuationValue(IntPtr o)
    {
      Decibels = CAPI.ovr_NetSyncVoipAttenuationValue_GetDecibels(o);
      Distance = CAPI.ovr_NetSyncVoipAttenuationValue_GetDistance(o);
    }
  }

  public class NetSyncVoipAttenuationValueList : DeserializableList<NetSyncVoipAttenuationValue> {
    public NetSyncVoipAttenuationValueList(IntPtr a) {
      var count = (int)CAPI.ovr_NetSyncVoipAttenuationValueArray_GetSize(a);
      _Data = new List<NetSyncVoipAttenuationValue>(count);
      for (int i = 0; i < count; i++) {
        _Data.Add(new NetSyncVoipAttenuationValue(CAPI.ovr_NetSyncVoipAttenuationValueArray_GetElement(a, (UIntPtr)i)));
      }

    }

  }
}
