using System;
using System.Threading;
using System.Text.RegularExpressions;
using System.Collections;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;
using LoggingModule;
using TMPro;

public class Key : MonoBehaviour
{
	public delegate void OnKeyPressed();
	public static OnKeyPressed keyPressed;

	public string KeyCapChar;
	public int keyCode;
	public string AlterateKeyCapChar;
	public int alterateKeyCode;

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
		//keycodeAdder = GetComponent<KeycodeAdder> ();

		keyCapText = GetComponentInChildren<Text> ();
		KeycapColor = GetComponent<Renderer> ().material.color;
		InitialKeycapColor = KeycapColor;

		initialPosition = new GameObject(string.Format("[{0}] initialPosition", name)).transform;
		initialPosition.parent = transform.parent;
		initialPosition.localPosition = Vector3.zero;
		initialPosition.localRotation = Quaternion.identity;

		if(Rigidbody == null) 
			Rigidbody = GetComponent<Rigidbody>();

		initialLocalPosition = transform.localPosition;
		initialLocalRotation = transform.localRotation;

		constrainedPosition = initialLocalPosition;
		constrainedRotation = initialLocalRotation;

		keySoundController = transform.root.Find("Punchkeyboard").GetComponent<KeySoundController> ();

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
			GetComponent<Renderer> ().material.color = PressedKeycapColor;
		else
			GetComponent<Renderer> ().material.color = KeycapColor;
	}

	void ConstrainPosition()
	{
		constrainedPosition.y = transform.localPosition.y;
		if (transform.localPosition.y > initialLocalPosition.y)
			constrainedPosition.y = initialLocalPosition.y;
		transform.localPosition = constrainedPosition;
		transform.localRotation = constrainedRotation;
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

	void WriteStringToTarget()
    {
		if (!InteractionLogger.isUsingVirtualKeyboard)
			InteractionLogger.RegisterVirtualKeyboard();

        try
        {
			TMP_InputField target = EventSystem.current.currentSelectedGameObject.GetComponent<TMP_InputField>();

			if (target.interactable)
			{
				int lcp = target.caretPosition;
				int lap = target.selectionAnchorPosition;

				if (name.Contains("Backspace"))
				{
					if (lcp != lap)
						DeleteSelection(ref lcp, ref lap, target);
					else
					{
						try
						{
							target.text = target.text.Remove(lcp - 1, 1);
							target.caretPosition = lcp - 1;
							target.selectionAnchorPosition = lap - 1;
						}
						catch { }
					}
				}
				else if (name.Contains("Return"))
				{
					if (lcp != lap) DeleteSelection(ref lcp, ref lap, target);
					target.text = target.text.Insert(lcp, "\n");
					target.caretPosition = lcp + 1;
					target.selectionAnchorPosition = lap + 1;
				}
				else if (name.Contains("Del"))
				{
					if (lcp != lap)
						DeleteSelection(ref lcp, ref lap, target);
					else
					{
						if (lcp < target.text.Length)
							target.text = target.text.Remove(lcp, 1);

						target.caretPosition = lcp;
						target.selectionAnchorPosition = lcp;
					}
				}
				else if (name.Contains("Tab"))
				{
					target.text = target.text.Insert(Math.Min(lcp, lap), "\t");
					target.caretPosition = lcp + 1;
					target.selectionAnchorPosition = lap + 1;
				}
				else if (name.Contains("ArrowKey"))
				{
					if (name.Contains("LeftArrowKey"))
					{
						int c = lcp > 0 ? lcp - 1 : 0;
						target.caretPosition = c;
						target.selectionAnchorPosition = c;
					}
					else if (name.Contains("RightArrowKey"))
					{
						int l = target.text.Length;
						int c = lcp < l ? lcp + 1 : l;
						target.caretPosition = c;
						target.selectionAnchorPosition = c;
					}
					else if (name.Contains("UpArrowKey"))
					{
						int leftDist = 0;
						int rightDist = 0;
						int leftInd, rightInd;

						for (leftInd = lcp - 1; leftInd >= 0 && target.text[leftInd] != '\n'; leftInd--)
							leftDist += 1;

						for (rightInd = lcp; rightInd < target.text.Length && target.text[rightInd] != '\n'; rightInd++)
							rightDist += 1;

						int finalInd = lcp - rightDist - leftDist;
						int c = finalInd >= 0 ? finalInd : 0;

						target.caretPosition = c;
						target.selectionAnchorPosition = c;
					}
					else
					{
						int leftDist = 0;
						int rightDist = 0;
						int leftInd, rightInd;

						for (leftInd = lcp - 1; leftInd >= 0 && target.text[leftInd] != '\n'; leftInd--)
							leftDist += 1;

						for (rightInd = lcp; rightInd < target.text.Length && target.text[rightInd] != '\n'; rightInd++)
							rightDist += 1;

						int finalInd = lcp + rightDist + leftDist;
						int c = finalInd > target.text.Length ? target.text.Length : finalInd;

						target.caretPosition = c;
						target.selectionAnchorPosition = c;
					}
				}
				else if (name.Contains("Copy"))
                {
					int start = target.selectionAnchorPosition;
					int end = target.caretPosition;
					if (end < start) start = Interlocked.Exchange(ref end, start);
					string selection = target.text.Substring(start, end - start);
					GUIUtility.systemCopyBuffer = selection;
				}
				else if (name.Contains("Paste"))
                {
					string selection = GUIUtility.systemCopyBuffer;

					if (lcp != lap)
					{
						if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
						target.text = target.text.Remove(Mathf.Min(lcp, lap), lcp - lap);
						target.caretPosition = Mathf.Min(lcp, lap);
						lcp = Mathf.Min(lcp, lap);
					}

					target.text = target.text.Insert(lcp, selection);
					target.caretPosition = lcp + selection.Length;
					target.selectionAnchorPosition = lcp + selection.Length;
				}
				else if (name.Contains("SelectAll"))
                {
					StartCoroutine(SelectingAllText(target));
				}
				else
				{
					if (lcp != lap) DeleteSelection(ref lcp, ref lap, target);
					target.text = target.text.Insert(lcp, keyCapText.text);
					target.caretPosition = lcp + 1;
					target.selectionAnchorPosition = lap + 1;
				}
			}
		}
        catch { }
	}

	private void DeleteSelection(ref int lcp, ref int lap, TMP_InputField target)
    {
		if (lcp < lap) lap = Interlocked.Exchange(ref lcp, lap);
		target.text = target.text.Remove(Math.Min(lcp, lap), lcp - lap);
		target.caretPosition = Math.Min(lcp, lap);
		lcp = Math.Min(lcp, lap);
	}

	public void ActivateKeyPress()
    {
		KeyPressed = true;
		keyPressed();
		if (!isSpecialKey) WriteStringToTarget();
		keySoundController.StartKeySound(this.gameObject.transform);
		checkForButton = false;
	}

	IEnumerator SelectingAllText(TMP_InputField target)
    {
		EventSystem.current.SetSelectedGameObject(null);
		yield return null;
		target.onFocusSelectAll = true;
		yield return null;
		target.ActivateInputField();
		yield return null;
		target.onFocusSelectAll = false;
	}
}