using System.Threading.Tasks;
using System.Net.Http;
using System.Text;
using System.Diagnostics;
using UnityEngine;

namespace PharoModule
{
    public static class Pharo
    {
        public static string IP = "http://localhost:1701/repl";
        public static readonly HttpClient client = new HttpClient();

        public static async Task Start()
        {
            string enginePath = Application.streamingAssetsPath + "/PharoEngine";
            string bashFile = (Application.platform == RuntimePlatform.WindowsPlayer ||
                               Application.platform == RuntimePlatform.WindowsEditor) ?
                                   "c:/Windows/System32/bash.exe" :
                                   "/bin/bash";

            await Task.Run(() => {
                var process = new Process()
                {
                    StartInfo = new ProcessStartInfo
                    {
                        FileName = bashFile,
                        WorkingDirectory = enginePath,
                        Arguments = $"-c \"./pharo vride.image st server.st\"",
                        RedirectStandardOutput = false,
                        UseShellExecute = true,
                        CreateNoWindow = true,
                        WindowStyle = ProcessWindowStyle.Hidden,
                    }
                };
                process.Start();
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
