using System;
using System.Threading;
using System.Text.RegularExpressions;
using UnityEngine;
using UnityEngine.UI;

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
				KeyPressed = true;
				keyPressed ();
				if (!isSpecialKey) WriteStringToTarget();
				keySoundController.StartKeySound (this.gameObject.transform);
				checkForButton = false;
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

	private void WriteStringToTarget()
    {
		InitializeBehaviour window = transform.root.gameObject.GetComponent<InitializeBehaviour>();
		if (!window.loadingWheel.activeSelf)
		{
			int lcp = window.lastCaretPosition;
			int lap = window.lastAnchorPosition;
			window.keyboardTarget.ActivateInputField();
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
				{
					try
					{
						window.keyboardTarget.text = window.keyboardTarget.text.Remove(lcp, 1);
					}
					catch { }
				}
			}
			else if (name.Contains("Tab"))
			{
				window.keyboardTarget.text = window.keyboardTarget.text.Insert(Math.Min(lcp, lap), "    ");
				window.keyboardTarget.caretPosition = lcp + 4;
				window.keyboardTarget.selectionAnchorPosition = lap + 4;
				window.lastCaretPosition = lcp + 4;
				window.lastAnchorPosition = lap + 4;
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
}