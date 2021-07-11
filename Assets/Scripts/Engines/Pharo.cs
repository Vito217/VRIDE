using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Net.Http;
using System.Text;

/// <summary>
/// Tools for Pharo requests
/// </summary>
namespace PharoModule
{
    /// <summary>
    /// Sends Pharo requests to the server
    /// </summary>
    public static class Pharo
    {
        public static string pharoIP;
        public static readonly HttpClient pharoClient = new HttpClient();

        /// <summary>
        /// Sends a pice of code to execute
        /// </summary>
        /// <param name="code">The code</param>
        /// <returns>Response string</returns>
        public static async Task<string> Execute(string code)
        {
            var request = await pharoClient.PostAsync
            (
                pharoIP, 
                new StringContent
                (
                    "[" + code + "]\n" +
                        "\ton: Exception\n" +
                        "\tdo: [:e | e traceCr].", 
                    Encoding.UTF8
                )
            );
            return await request.Content.ReadAsStringAsync();
        }

        /// <summary>
        /// Sends a pice of code to execute and print
        /// </summary>
        /// <param name="code">The code</param>
        /// <returns>Response string</returns>
        public static async Task<string> Print(string code)
        {
            if (!code.Contains("compile"))
                //code = "self class compiler evaluate: '" + code.Replace("'", "''") + "'";
                //code = "self class compiler evaluate: '" + code.Replace("'", "''").Replace("''''", "''") + "'";
                code = "self class compiler evaluate: '" + Regex.Replace(code, @"'|''''", "''") + "'";

            return await Execute(code);
        }

        /// <summary>
        /// Sends a pice of code to execute and inspect
        /// </summary>
        /// <param name="code">The code</param>
        /// <returns>Response string</returns>
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
