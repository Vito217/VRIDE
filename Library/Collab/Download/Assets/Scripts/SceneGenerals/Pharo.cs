using System.Threading.Tasks;
using System.Net.Http;
using System.Text;
using System.Diagnostics;
using System.IO;
using UnityEngine;

namespace PharoModule
{
    public static class Pharo
    {
        public static string IP = "http://localhost:1701/repl";
        public static readonly HttpClient client = new HttpClient();

        public static async Task Start()
        {
            await Task.Run(() => {
                try
                {
                    string enginePath = Path.Combine(Application.streamingAssetsPath, "PharoEngine");
                    string executable = "";

                    if(Application.platform == RuntimePlatform.WindowsPlayer ||
                        Application.platform == RuntimePlatform.WindowsEditor)
                    {
                        executable = Path.Combine(".", "pharo-vm", "win", "PharoConsole");
                    }
                    else if (Application.platform == RuntimePlatform.OSXPlayer ||
                        Application.platform == RuntimePlatform.OSXEditor)
                    {
                        executable = Path.Combine(".", "pharo-vm", "macos", "Pharo.app", "Contents", 
                            "MacOS", "Pharo");
                    }
                    else
                    {
                        executable = Path.Combine(".", "pharo-vm", "linux", "pharo");
                    }

                    var process = new Process()
                    {
                        StartInfo = new ProcessStartInfo
                        {
                            WorkingDirectory = enginePath,
                            FileName = executable,
                            Arguments = $"vride.image st server.st",
                            RedirectStandardOutput = false,
                            UseShellExecute = true,
                            CreateNoWindow = true,
                            WindowStyle = ProcessWindowStyle.Hidden,
                        }
                    };
                    process.Start();
                }
                catch
                {
                    UnityEngine.Debug.Log("Local server not found. Make sure you have " +
                        "Pharo Launcher running.");
                }
            });
        }

        public static async Task<string> Execute(string code)
        {
            var request = await client.PostAsync(IP, new StringContent(code, Encoding.UTF8));
            return await request.Content.ReadAsStringAsync(); ;
        }

        public static async Task<string> Print(string code)
        {
            if (!code.Contains("compile"))
                code = "self class compiler evaluate: '" + code.Replace("'", "''") + "'";
            return await Execute(
                "[" + code + "]\n" +
                    "\ton: Error\n" +
                    "\tdo: [:e | '[Error] ' , (e message lookupClass name), ': ' , (e messageText)]."
            );
        }

        public static async Task<string> Inspect(string code)
        {
            return await Print(
                "res := [" + code + "] value .\n" +
                "tuples := OrderedCollection new.\n" +
                "tuples addLast: 'self=',(res value asString).\n" +
                "res class instVarNames do: [ :each |\n" +
                    "\ttuples addLast: (each asString),'=', ((res instVarNamed: each value) asString).\n" +
                "].\n" +
                "tuples ."
            );
        }
    }
}
