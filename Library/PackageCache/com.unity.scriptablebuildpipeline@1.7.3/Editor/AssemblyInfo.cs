using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("Unity.ScriptableBuildPipeline.Editor.Tests")]
[assembly: InternalsVisibleTo("Unity.ScriptableBuildPipeline.Test.Fixtures")]
[assembly: InternalsVisibleTo("Unity.ScriptableBuildPipeline.Editor.PerformanceTests")]
[assembly: InternalsVisibleTo("SBPDebug.Editor")]
[assembly: InternalsVisibleTo("SBPDebug.Editor.Tests")]

// These are temporary while the IBuildLogger API is being solidified.
[assembly: InternalsVisibleTo("Unity.Addressables.Tests")]
[assembly: InternalsVisibleTo("Unity.Addressables")]
[assembly: InternalsVisibleTo("Unity.Addressables.Editor")]
[assembly: InternalsVisibleTo("Unity.Addressables.Editor.Tests")]
