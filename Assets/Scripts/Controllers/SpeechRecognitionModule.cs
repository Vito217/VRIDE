using System;
using System.Collections;
using System.Speech.Recognition;
using UnityEngine;

public class SpeechRecognitionModule : MonoBehaviour
{
    static bool completed;

    // Start is called before the first frame update
    void Start()
    {
        StartCoroutine(SpeechTest());
    }

    // Update is called once per frame
    void Update()
    {
        
    }

    IEnumerator SpeechTest()
    {
        using (SpeechRecognizer recognizer = new SpeechRecognizer())
        {

            // Create and load a sample grammar.  
            Grammar testGrammar =
              new Grammar(new GrammarBuilder("hola hola"));
            testGrammar.Name = "Test Grammar";
            recognizer.LoadGrammar(testGrammar);

            // Attach event handlers for recognition events.  
            recognizer.SpeechRecognized +=
              new EventHandler<SpeechRecognizedEventArgs>(
                SpeechRecognizedHandler);
            recognizer.EmulateRecognizeCompleted +=
              new EventHandler<EmulateRecognizeCompletedEventArgs>(
                EmulateRecognizeCompletedHandler);

            completed = false;

            // Start asynchronous emulated recognition.   
            // This matches the grammar and generates a SpeechRecognized event.  
            recognizer.EmulateRecognizeAsync("testing testing");

            // Wait for the asynchronous operation to complete.  
            while (!completed)
            {
                yield return new WaitForSeconds(0.333f);
            }

            completed = false;

            // Start asynchronous emulated recognition.  
            // This does not match the grammar or generate a SpeechRecognized event.  
            recognizer.EmulateRecognizeAsync("testing one two three");

            // Wait for the asynchronous operation to complete.  
            while (!completed)
            {
                yield return new WaitForSeconds(0.333f);
            }
        }
    }

    static void SpeechRecognizedHandler(
      object sender, SpeechRecognizedEventArgs e)
    {
        if (e.Result != null)
        {
            Debug.Log("Recognition result = {0}" + e.Result.Text ?? "<no text>");
        }
        else
        {
            Debug.Log("No recognition result");
        }
    }

    // Handle the SpeechRecognizeCompleted event.  
    static void EmulateRecognizeCompletedHandler(
      object sender, EmulateRecognizeCompletedEventArgs e)
    {
        if (e.Result == null)
        {
            Debug.Log("No result generated.");
        }

        // Indicate the asynchronous operation is complete.  
        completed = true;
    }
}
