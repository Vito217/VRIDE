// This file was @generated with LibOVRPlatform/codegen/main. Do not modify it!

namespace Oculus.Platform.Models
{
    using System;

    public class LivestreamingVideoStats
  {
    public readonly int CommentCount;
    public readonly int ReactionCount;
    public readonly string TotalViews;


    public LivestreamingVideoStats(IntPtr o)
    {
      CommentCount = CAPI.ovr_LivestreamingVideoStats_GetCommentCount(o);
      ReactionCount = CAPI.ovr_LivestreamingVideoStats_GetReactionCount(o);
      TotalViews = CAPI.ovr_LivestreamingVideoStats_GetTotalViews(o);
    }
  }

}
