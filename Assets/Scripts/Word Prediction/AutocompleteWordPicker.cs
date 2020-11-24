using System.Collections.Generic;
using UnityEngine;
using System.Linq;
using System.Text;
using System;
using TMPro;

public class AutocompleteWordPicker : MonoBehaviour
{
	public TMP_InputField TextField;
	public NGramGenerator WordPredictor;

	public void ReplaceWord(string correctWord)
	{
		List<string> inputText = new List<string>();
		StringBuilder builder = new StringBuilder();
		string input = TextField.text;
		string[] parts = input.Split(' ');
		parts = parts.Take(parts.Length - 1).ToArray();

		for(int i = 0; i < parts.Length; i++)
		{
			inputText.Add(parts[i]);
		}

		inputText.Add(correctWord);

		foreach (string w in inputText)
		{
			builder.Append(w).Append(" ");
		}
		TextField.text = builder.ToString();
		TextField.ActivateInputField();

		WordPredictor.PredictNextWords(correctWord);
	}

	public static string ReverseString(string s)
	{
		char[] charArray = s.ToCharArray ();
		Array.Reverse (charArray);
		return new string (charArray);
	}
}