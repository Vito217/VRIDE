using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Net.Http;
using System.Text;

public class PharoRequests : MonoBehaviour
{
    private static readonly HttpClient client = new HttpClient();

    // Start is called before the first frame update
    void Start()
    {
        PharoCall();
    }

    // Update is called once per frame
    void Update()
    {
        
    }

    async void PharoCall()
    {   
        var content = new StringContent("42 factorial", Encoding.UTF8);

        var response = await client.PostAsync("http://localhost:1701/repl", content);

        var responseString = await response.Content.ReadAsStringAsync();

        Debug.Log(responseString);
    }
}
