using NUnit.Framework;
using UnityEditor;
using UnityEditor.Android;
using Unity.Android.Logcat;
using System.IO;
using System.Text.RegularExpressions;

public class AndroidLogcatAddr2LineTests
{
    string GetSymbolAddressUsingNM(AndroidTools tools, string symbolFilePath, string symbolName)
    {
        // With NDK 16, calling nm.exe shows an  error
        // System.Exception : L:\UnityInstalls\2019.1.7f1\Editor\Data\PlaybackEngines\AndroidPlayer\NDK\toolchains\aarch64-linux-android-4.9\prebuilt\windows-x86_64\bin\aarch64-linux-android-nm.exe -extern-only "L:\UnityInstalls\2019.1.7f1\Editor\Data\PlaybackEngines\AndroidPlayer\Variations\il2cpp\Development\Symbols\arm64-v8a\libmain.sym.so"
        // returned with exit code 1
        // L:\UnityInstalls\2019.1.7f1\Editor\Data\PlaybackEngines\AndroidPlayer\NDK\toolchains\aarch64-linux-android-4.9\prebuilt\windows-x86_64\bin\aarch64-linux-android-nm.exe: invalid option -- x
        var symbols = tools.RunNM(symbolFilePath);
        foreach (var s in symbols)
        {
            if (s.Contains(symbolName))
            {
                return "0x" + s.Split(' ')[0];
            }
        }
        return string.Empty;
    }

    string GetSymbolAddressUsingReadElf(AndroidTools tools, string symbolFilePath, string symbolName)
    {
        // Regex for
        //     63: 000000000000083c   144 FUNC    GLOBAL DEFAULT   10 JNI_OnLoad

        var regex = new Regex(@".*:\s*(?<address>\S*).*");
        var symbols = tools.RunReadElf(symbolFilePath);
        foreach (var s in symbols)
        {
            if (s.Contains(symbolName))
            {
                var result = regex.Match(s);
                if (result.Success == false)
                    throw new System.Exception("Failed to regex " + s);
                return "0x" + result.Groups["address"];
            }
        }
        return string.Empty;
    }

    private void CanResolveStacktraces(string abi)
    {
        if (!AndroidLogcatTestsSetup.AndroidSDKAndNDKAvailable())
        {
            System.Console.WriteLine("Test ignored");
            return;
        }
        var tools = new AndroidTools();
        const string symbolName = "JNI_OnLoad";
        var playerPackage = BuildPipeline.GetPlaybackEngineDirectory(BuildTarget.Android, BuildOptions.None);
        var expectedOutput = symbolName + " at ??:?";
        var symbolPath = Path.GetFullPath(Path.Combine(playerPackage, "Variations/il2cpp/Development/Symbols/" + abi + "/libmain.sym.so"));
#if UNITY_2019_3_OR_NEWER
        var targetAddress = GetSymbolAddressUsingNM(tools, symbolPath, symbolName);
#else
        var targetAddress = GetSymbolAddressUsingReadElf(tools, symbolPath, symbolName);
#endif

        Assert.IsNotEmpty(targetAddress, "Failed to find address for " + symbolName);
        var resolvedSymbols = tools.RunAddr2Line(symbolPath, new[] { targetAddress });
        Assert.IsTrue(resolvedSymbols.Length == 1, "Expected to resolve one symbol");
        Assert.AreEqual(expectedOutput, resolvedSymbols[0],
            string.Format("Failed to resolve symbol '{0}' for address '{1}'", symbolName, targetAddress));
    }

    [Test]
    public void CanResolveStacktracesARM64()
    {
        CanResolveStacktraces("arm64-v8a");
    }

    [Test]
    public void CanResolveStacktracesARMv7()
    {
        CanResolveStacktraces("armeabi-v7a");
    }
}
