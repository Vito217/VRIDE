// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class AssetFileDownloadResult
  {
    public readonly UInt64 AssetId;
    public readonly string Filepath;


    public AssetFileDownloadResult(IntPtr o)
    {
      AssetId = CAPI.ovr_AssetFileDownloadResult_GetAssetId(o);
      Filepath = CAPI.ovr_AssetFileDownloadResult_GetFilepath(o);
    }
  }

}
