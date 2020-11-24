// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class AbuseReportRecording
  {
    public readonly string RecordingUuid;


    public AbuseReportRecording(IntPtr o)
    {
      RecordingUuid = CAPI.ovr_AbuseReportRecording_GetRecordingUuid(o);
    }
  }

}
