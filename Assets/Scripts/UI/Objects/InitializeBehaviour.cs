using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using UnityEngine;
using System.Threading;
using UnityEngine.UI;
using SaveAndLoad;
using PharoModule;
using LoggingModule;
using TMPro;

public abstract class InitializeBehaviour : MonoBehaviour
{
    public float speed = 8.0f;
    public bool initializing = false;
    public Vector3 new_pos;

    public TextMeshProUGUI code;
    public TMP_InputField field;
    public VRIDEController player;
    public Image panel;

    public bool dragging = false;
    public Vector3 rel_pos;
    public Vector3 rel_fwd;

    StringBuilder sb = new StringBuilder();
    List<char> notAN = new List<char> { ' ', '\n', '\t', '\r' };

    void Start()
    {
        StartCoroutine(Coroutine());
    }

    public IEnumerator Coroutine()
    {
        panel.color = UnityEngine.Random.ColorHSV();
        yield return innerStart();
    }

    void Update()
    {
        if (initializing)
            initializeAnimation();
        else
        {
            if (dragging)
                dragAction();
            innerBehaviour();
        }
    }

    public void initializeAnimation()
    {
        transform.position = Vector3.MoveTowards(
            transform.position,
            new_pos,
            speed
        );
        if (transform.position == new_pos)
            initializing = false;
    }

    public async void onChangeInput()
    {
        try
        {
            /**
            string text = field.text;
            text = Regex.Replace(text, @"<color=#b32d00>|<color=#00ffffff>|</color>|<b>|</b>", "");
            text = Regex.Replace(text, @"\t", "".PadRight(4));

            if (Input.GetKeyDown(KeyCode.Space) || Input.GetKeyDown(KeyCode.Return))
            {
                int i;
                for (i = field.caretPosition - 1; i >= 0 && !notAN.Contains(text[i]); i--)
                    sb.Insert(0, text[i].ToString(), 1);
                string previous_word = sb.ToString();
                int pw_len = previous_word.Length;
                for (i = i; i >= 0 && notAN.Contains(text[i]); i--) { }
                if ((i == -1) || (i >= 0 && text[i] == '.') || (pw_len > 0 && previous_word[0] == '#'))
                    field.caretPosition += 1;
                sb.Clear();
            }

            if (Input.GetKeyDown(KeyCode.Tab))
            {
                field.caretPosition += 4;
                if (text[field.caretPosition - 1] != ' ')
                    field.caretPosition -= 1;
            }

            text = Regex.Replace(text, @"(\A|\.\s*\n*\s*)([a-zA-Z0-9]+)(\s|\n)", "$1<b>$2</b>$3");
            text = Regex.Replace(text, @"(\n?\s*)(#[a-zA-Z0-9]+)(\n?\s*)", "$1<color=#00ffffff>$2</color>$3");

            field.text = text;
            **/
        }
        catch
        {
            //await SaveAndLoadModule.Save(player);
            //await Pharo.Execute("SmalltalkImage current snapshot: true andQuit: true.");
            //InteractionLogger.SessionEnd();
            //Application.Quit();
        }
    }

    public string cleanCode(string code)
    {
        return Regex.Replace(code, @"<color=#b32d00>|<color=#00ffffff>|</color>|<b>|</b>", "");
    }

    public string getSelectedCode(string clean_code)
    {
        int start = field.selectionAnchorPosition;
        int end = field.caretPosition;
        if (end < start)
            start = Interlocked.Exchange(ref end, start);
        int selection_length = end - start;
        string selection = clean_code.Substring(start, selection_length);
        if (selection == "")
            return clean_code;
        return selection;
    }

    public string getLastLineOfCode(string clean_code)
    {
        string[] lines = clean_code.Split('.');
        int len = lines.Length;
        if (len == 1)
            return lines[0];
        else
        {
            string last = lines[len - 1];
            string penultimate = lines[len - 2];
            return String.IsNullOrWhiteSpace(last) || String.IsNullOrEmpty(last) ?
                penultimate : last;
        }
    }

    public void onDrag()
    {
        dragging = true;
        rel_pos = player.transform.InverseTransformPoint(transform.position);
        rel_fwd = player.transform.InverseTransformDirection(transform.forward);
        InteractionLogger.StartTimerFor("WindowDragging");
    }

    public void onEndDrag()
    {
        dragging = false;
        InteractionLogger.EndTimerFor("WindowDragging");
    }

    public void dragAction()
    {
        Vector3 new_pos = player.transform.TransformPoint(rel_pos);
        Vector3 new_forw = player.transform.TransformDirection(rel_fwd);
        transform.position = Vector3.MoveTowards(
            transform.position,
            new_pos,
            0.15f
        );
        transform.forward = new_forw;
    }

    public virtual void Initialize(Vector3 init_pos, Vector3 final_pos,
        Vector3 forward, VRIDEController p)
    {
        player = p;
        transform.position = init_pos;
        transform.forward = forward;
        new_pos = final_pos;
        initializing = true;
    }

    public virtual void onSelect()
    {
        player.can_move = false;
    }

    public virtual void onDeselect()
    {
        player.can_move = true;
    }

    public abstract void onClose();

    public virtual void innerBehaviour() { }

    public virtual IEnumerator innerStart() {
        yield return null;
    }
}
