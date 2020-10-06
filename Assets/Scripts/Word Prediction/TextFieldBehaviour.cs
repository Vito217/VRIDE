using UnityEngine;
using UnityEngine.Events;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using System.Collections;
using System.Linq;
using TMPro;
//using WindowsInput;
//using NewtonVR;

public class TextFieldBehaviour : MonoBehaviour, ISelectHandler
{
	public NGramGenerator NGramHandler;
	//public NVRButton Space;

	private TMP_InputField inputField;

	void Start()
	{
		inputField = gameObject.GetComponent<TMP_InputField>();
	}

	public void OnSelect(BaseEventData eventData)
	{
		StartCoroutine(DisableHighlight());
	}

	public void MoveCaretToEnd()
	{
		StartCoroutine(DisableHighlight());
	}

	IEnumerator DisableHighlight()
	{
		Color originalTextColor = inputField.selectionColor;
		originalTextColor.a = 0f;

		inputField.selectionColor = originalTextColor;

		//Wait for one frame
		yield return null;

		//Scroll the view with the last character
		//inputField.MoveTextEnd(true);
		//Change the caret pos to the end of the text
		//inputField.caretPosition = inputField.text.Length;

		originalTextColor.a = 1f;
		inputField.selectionColor = originalTextColor;
	}

	void Update()
	{
		//if(Input.GetKeyUp(KeyCode.Space) || Space.ButtonUp && inputField.isFocused)
		if(Input.GetKeyUp(KeyCode.Space))
		{
			string inputText = inputField.text.TrimEnd();
			string lastWord = inputText.Split(' ').Last ();
			NGramHandler.PredictNextWords(lastWord);
		}
	}
}