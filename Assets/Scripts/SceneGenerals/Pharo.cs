using System;
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
                //client.Timeout = TimeSpan.FromSeconds(20);
                /**try
                {
                    string enginePath = Path.Combine(Application.streamingAssetsPath, "PharoEngine");
                    string arguments = $"Pharo.image st server.st";
                    string executable = "";

                    if (Application.platform == RuntimePlatform.WindowsPlayer ||
                        Application.platform == RuntimePlatform.WindowsEditor)
                    {
                        if (File.Exists("c:/Windows/System32/bash.exe"))
                        {
                            executable = "c:/Windows/System32/bash.exe";
                            arguments = $"-c \"./pharo Pharo.image st server.st\"";
                        }
                        else
                        {
                            executable = Path.Combine(".", "pharo-vm-win10", "Pharo");
                        }
                    }
                    else if (Application.platform == RuntimePlatform.OSXPlayer ||
                        Application.platform == RuntimePlatform.OSXEditor)
                    {
                        executable = Path.Combine(".", "pharo-vm-macosx", "Pharo.app", "Contents", 
                            "MacOS", "Pharo");
                    }
                    else
                    {
                        executable = Path.Combine(".", "pharo");
                    }

                    var process = new Process()
                    {
                        StartInfo = new ProcessStartInfo
                        {
                            FileName = executable,
                            WorkingDirectory = enginePath,
                            Arguments = arguments,
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
                }**/
            });
        }

        public static async Task<string> Execute(string code)
        {
            var request = await client.PostAsync(IP, new StringContent(code, Encoding.UTF8));
            return await request.Content.ReadAsStringAsync();
        }

        public static async Task<string> Print(string code)
        {
            if (!code.Contains("compile"))
                code = "self class compiler evaluate: '" + code.Replace("'", "''") + "'";

            return await Execute(
                "[" + code + "]\n" +
                    "\ton: Exception\n" +
                    "\tdo: [:e | e traceCr]."
            );
        }

        public static async Task<string> Inspect(string code)
        {
            return await Print
            (
                "| res tuples |\n" +
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
