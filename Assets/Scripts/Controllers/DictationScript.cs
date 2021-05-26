using UnityEditor;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.Windows.Speech;

public class DictationScript : MonoBehaviour
{
    [SerializeField]
    private Text m_Hypotheses;

    [SerializeField]
    private Text m_Recognitions;

    private DictationRecognizer m_DictationRecognizer;

    void Start()
    {
        m_DictationRecognizer = new DictationRecognizer();

        m_DictationRecognizer.DictationResult += (text, confidence) =>
        {
            try
            {
                if(VRIDEMenu.enableSpeechRecognition)
                    Debug.LogFormat("Dictation result: {0}", text);
                m_Recognitions.text += text + "\n";
            }
            catch { }
        };

        m_DictationRecognizer.DictationHypothesis += (text) =>
        {
            try
            {
                Debug.LogFormat("Dictation hypothesis: {0}", text);
                m_Hypotheses.text += text;
            }
            catch { }
        };

        m_DictationRecognizer.DictationComplete += (completionCause) =>
        {
            try
            {
                if (completionCause != DictationCompletionCause.Complete)
                    Debug.LogErrorFormat("Dictation completed unsuccessfully: {0}.", completionCause);
            }
            catch { }
        };

        m_DictationRecognizer.DictationError += (error, hresult) =>
        {
            try
            {
                Debug.LogErrorFormat("Dictation error: {0}; HResult = {1}.", error, hresult);
            }
            catch { }
        };

        m_DictationRecognizer.Start();
    }
}