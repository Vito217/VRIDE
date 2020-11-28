using NUnit.Framework;

public class AndroidLogcatNetTests
{
    // Ensure we're running tests with .NET 3.5, because Unity 2018.3 and older don't have .NET 3.5 deprecated
    [Test]
    public void EnsureDotNET35IsUsed()
    {
#if !NET_2_0 && !UNITY_2019_2_OR_NEWER
        Assert.Fail("Tests project should be using .NET 3.5, did you modify Scripting Runtime Version?");
#endif
    }
}
