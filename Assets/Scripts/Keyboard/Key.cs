using System;
using System.Threading;
using System.Text.RegularExpressions;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using LoggingModule;
using UnityEngine.EventSystems;

public class Key : MonoBehaviour
{
	public delegate void OnKeyPressed();
	public static OnKeyPressed keyPressed;
	public string KeyCapChar;
	public string AlterateKeyCapChar;
	public Rigidbody Rigidbody;
	public bool KeyPressed = false;
	public Color PressedKeycapColor;
	public Color KeycapColor;
	public Color InitialKeycapColor;

	protected Transform initialPosition;
	//private KeycodeAdder keycodeAdder;
	private Text keyCapText;
	private Vector3 initialLocalPosition;
	private Quaternion initialLocalRotation;
	private Vector3 constrainedPosition;
	private Quaternion constrainedRotation;
	private bool uppercaseSwitch = true;
	private bool symbolSwitch = false;
	private bool checkForButton = true;
	private const float DistanceToBePressed = 0.01f;
	private const float KeyBounceBackMultiplier = 1500f;
	private KeySoundController keySoundController;
	private float currentDistance = -1;
	private bool isSpecialKey;

	private InitializeBehaviour window;

	void Start()
	{
		//keycodeAdder = this.gameObject.GetComponent<KeycodeAdder> ();

		keyCapText = this.gameObject.GetComponentInChildren<Text> ();
		KeycapColor = this.gameObject.GetComponent<Renderer> ().material.color;
		InitialKeycapColor = KeycapColor;

		initialPosition = new GameObject(string.Format("[{0}] initialPosition", this.gameObject.name)).transform;
		initialPosition.parent = this.transform.parent;
		initialPosition.localPosition = Vector3.zero;
		initialPosition.localRotation = Quaternion.identity;

		if(Rigidbody == null) Rigidbody = GetComponent<Rigidbody>();

		initialLocalPosition = this.transform.localPosition;
		initialLocalRotation = this.transform.localRotation;

		constrainedPosition = initialLocalPosition;
		constrainedRotation = initialLocalRotation;

		keySoundController = transform.root.Find("Keyboards/Punchkeyboard")
			.gameObject.GetComponent<KeySoundController> ();

		SwitchKeycapCharCase ();

		isSpecialKey = Regex.Match(name, "Shift|Symbol").Success;

		window = transform.parent.parent.parent.parent.parent
			.parent.parent.gameObject.GetComponent<InitializeBehaviour>();
	}

	void FixedUpdate()
	{
		ConstrainPosition ();
		currentDistance = Vector3.Distance(this.transform.position, initialPosition.position);

		Vector3 PositionDelta = initialPosition.position - this.transform.position;
		this.Rigidbody.velocity = PositionDelta * KeyBounceBackMultiplier * Time.deltaTime;
	}

	void Update()
	{
		if (checkForButton)
		{
			if (currentDistance > DistanceToBePressed)
			{
				ActivateKeyPress();
			}
		} 
		else if (!checkForButton)
		{
			if (currentDistance < DistanceToBePressed)
			{
				KeyPressed = false;
				checkForButton = true;
			}
		}

		ChangeKeyColorOnPress ();
	}

	void LateUpdate()
	{
		ConstrainPosition ();
	}

	void ChangeKeyColorOnPress()
	{
		if (KeyPressed)
			gameObject.GetComponent<Renderer> ().material.color = PressedKeycapColor;
		else
			gameObject.GetComponent<Renderer> ().material.color = KeycapColor;
	}

	void ConstrainPosition()
	{
		constrainedPosition.y = this.transform.localPosition.y;
		if (this.transform.localPosition.y > initialLocalPosition.y)
			constrainedPosition.y = initialLocalPosition.y;
		this.transform.localPosition = constrainedPosition;
		this.transform.localRotation = constrainedRotation;
	}

	public void SwitchKeycapCharCase()
	{
		if (uppercaseSwitch)
		{
			keyCapText.text = KeyCapChar.ToLower ();
			uppercaseSwitch = false;
		}
		else
		{
			keyCapText.text = KeyCapChar.ToUpper ();
			uppercaseSwitch = true;
		}
	}

	public void SwitchToSymbols()
	{
		if (!symbolSwitch)
		{
			keyCapText.text = AlterateKeyCapChar;
			symbolSwitch = true;
		}
		else
		{
			keyCapText.text = KeyCapChar;
			keyCapText.text = KeyCapChar.ToLower();
			symbolSwitch = false;
		}
	}

	IEnumerator WriteStringToTarget()
    {
		if (!InteractionLogger.isUsingVirtualKeyboard)
			InteractionLogger.RegisterVirtualKeyboard();

		if (window.loadingWheel == null || !window.loadingWheel.activeSelf)
		{
			int lcp = window.lastCaretPosition;
			int lap = window.lastAnchorPosition;

			window.keyboardTarget.ActivateInputField();
			yield return null;

			window.keyboardTarget.caretPosition = lcp;
			window.keyboardTarget.selectionAnchorPosition = lap;

			if (name.Contains("Backspace"))
            {
				if (lcp != lap)
					DeleteSelection(ref lcp, ref lap, window);
                else
                {
					try
					{
						window.keyboardTarget.text = window.keyboardTarget.text.Remove(lcp - 1, 1);
						window.keyboardTarget.caretPosition = lcp - 1;
						window.keyboardTarget.selectionAnchorPosition = lap - 1;
						window.lastCaretPosition = lcp - 1;
						window.lastAnchorPosition = lap - 1;
					} catch { }
				}
			}
			else if (name.Contains("Return"))
            {
				if (lcp != lap) DeleteSelection(ref lcp, ref lap, window);
				window.keyboardTarget.text = window.keyboardTarget.text.Insert(lcp, "\n");
				window.keyboardTarget.caretPosition = lcp + 1;
				window.keyboardTarget.selectionAnchorPosition = lap + 1;
				window.lastCaretPosition = lcp + 1;
				window.lastAnchorPosition = lap + 1;
			}
			else if (name.Contains("Del"))
            {
				if (lcp != lap)
					DeleteSelection(ref lcp, ref lap, window);
				else
					StartCoroutine(Supr());
			}
			else if (name.Contains("Tab"))
			{
				window.keyboardTarget.text = window.keyboardTarget.text.Insert(Math.Min(lcp, lap), "\t");
				window.keyboardTarget.caretPosition = lcp + 1;
				window.keyboardTarget.selectionAnchorPosition = lap + 1;
				window.lastCaretPosition = lcp + 1;
				window.lastAnchorPosition = lap + 1;
			}
			else if (name.Contains("ArrowKey"))
            {
				if(name.Contains("LeftArrowKey"))
                {
					int c = lcp > 0 ? lcp - 1 : 0;
					window.keyboardTarget.caretPosition = c;
					window.keyboardTarget.selectionAnchorPosition = c;
					window.lastCaretPosition = c;
					window.lastAnchorPosition = c;
				}
				else if(name.Contains("RightArrowKey"))
                {
					int l = window.keyboardTarget.text.Length;
					int c = lcp < l ? lcp + 1 : l;
					window.keyboardTarget.caretPosition = c;
					window.keyboardTarget.selectionAnchorPosition = c;
					window.lastCaretPosition = c;
					window.lastAnchorPosition = c;
				}
				else if (name.Contains("UpArrowKey"))
				{
					int leftDist = 0;
					int rightDist = 0;
					int leftInd, rightInd;

					for (leftInd = lcp - 1; leftInd >= 0 && window.keyboardTarget.text[leftInd] != '\n'; leftInd--)
						leftDist += 1;

					for (rightInd = lcp; rightInd < window.keyboardTarget.text.Length && window.keyboardTarget.text[rightInd] != '\n'; rightInd++)
						rightDist += 1;

					int finalInd = lcp - rightDist - leftDist;
					int c = finalInd >= 0 ? finalInd : 0;

					window.keyboardTarget.caretPosition = c;
					window.keyboardTarget.selectionAnchorPosition = c;
					window.lastCaretPosition = c;
					window.lastAnchorPosition = c;
				}
				else
				{
					int leftDist = 0;
					int rightDist = 0;
					int leftInd, rightInd;

					for (leftInd = lcp - 1; leftInd >= 0 && window.keyboardTarget.text[leftInd] != '\n'; leftInd--)
						leftDist += 1;

					for (rightInd = lcp; rightInd < window.keyboardTarget.text.Length && window.keyboardTarget.text[rightInd] != '\n'; rightInd++)
						rightDist += 1;

					int finalInd = lcp + rightDist + leftDist;
					int c = finalInd > window.keyboardTarget.text.Length ? window.keyboardTarget.text.Length : finalInd;

					window.keyboardTarget.caretPosition = c;
					window.keyboardTarget.selectionAnchorPosition = c;
					window.lastCaretPosition = c;
					window.lastAnchorPosition = c;
				}
			}
			else
            {
				if (lcp != lap) DeleteSelection(ref lcp, ref lap, window);
				window.keyboardTarget.text = window.keyboardTarget.text.Insert(lcp, keyCapText.text);
				window.keyboardTarget.caretPosition = lcp + 1;
				window.keyboardTarget.selectionAnchorPosition = lap + 1;
				window.lastCaretPosition = lcp + 1;
				window.lastAnchorPosition = lap + 1;
			}
		}
	}

	private void DeleteSelection(ref int lcp, ref int lap, InitializeBehaviour window)
    {
		if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
		window.keyboardTarget.text = window.keyboardTarget.text
			.Remove(Math.Min(lcp, lap), lcp - lap);
		window.keyboardTarget.caretPosition = Math.Min(lcp, lap);
		lcp = Math.Min(lcp, lap);
	}

	public void ActivateKeyPress()
    {
		KeyPressed = true;
		keyPressed();
		if (!isSpecialKey) StartCoroutine(WriteStringToTarget());
		keySoundController.StartKeySound(this.gameObject.transform);
		checkForButton = false;
	}

	IEnumerator Supr()
    {
		int lcp = window.lastCaretPosition;

		EventSystem.current.SetSelectedGameObject(null);
		window.keyboardTarget.ActivateInputField();

		if (lcp < window.keyboardTarget.text.Length)
			window.keyboardTarget.text = window.keyboardTarget.text.Remove(lcp, 1);

		yield return null;

		window.keyboardTarget.caretPosition = lcp;
		window.keyboardTarget.selectionAnchorPosition = lcp;
		window.lastCaretPosition = lcp;
		window.lastAnchorPosition = lcp;

		yield return null;
	}
}