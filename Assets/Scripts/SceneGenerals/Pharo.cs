using System.Collections;
using System.Collections.Generic;
using System.Threading.Tasks;
using System.Net.Http;
using System.Text;

namespace PharoModule
{
    public static class Pharo
    {
        public static string IP = "http://localhost:1701/repl";
        public static readonly HttpClient client = new HttpClient();

        public static async Task<string> Execute(string code)
        {
            var request = await client.PostAsync(IP, new StringContent(code, Encoding.UTF8));
            return await request.Content.ReadAsStringAsync(); ;
        }

        public static async Task<string> Print(string code)
        {
            return await Execute(
                "[" + code + "]\n" +
                    "\ton: Error\n" +
                    "\tdo: [:e | '[Error] ' , (e message lookupClass name), ': ' , (e messageText)]."
           );
        }

        public static async Task<string> Inspect(string code, string var)
        {
            return await Print(
                code + ".\n" +
                "tuples := OrderedCollection new.\n" +
                "tuples addLast: 'self=',(" + var + " value asString).\n" +
                var + " class instVarNames do: [ :each |\n" +
                    "\ttuples addLast: (each asString),'=', ((" + var + " instVarNamed: each value) asString).\n" +
                "].\n" +
                "tuples ."
            );
        }
    }
}
