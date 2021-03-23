using System;
using System.Collections;
using UnityEngine;
using System.Threading;
using UnityEngine.UI;
using UnityEngine.EventSystems;
using LoggingModule;
using TMPro;

public class InitializeBehaviour : MonoBehaviour
{
    public TMP_InputField logText;
    public TMP_InputField field;
    public TMP_InputField keyboardTarget;
    public GameObject loadingWheel;
    public GameObject keyboardsGameObject;
    public Image panel;
    public AutocompleteWordPicker wordPicker;
    public int lastCaretPosition = 0;
    public int lastAnchorPosition = 0;
    public float sizeVariance = 20;
    public float scaleVariance = .2f;
    public bool fromUIClick = false;
    public bool freezeRotation = true;

    //Transform baseParent;
    //Vector3 baseScale;

    void Start()
    {
        GetComponent<Canvas>().worldCamera = Camera.main;
        StartCoroutine(Coroutine());
    }

    public IEnumerator Coroutine()
    {
        if(panel != null) panel.color = UnityEngine.Random.ColorHSV();
        yield return innerStart();
    }

    void Update()
    {
        if(freezeRotation)
            transform.forward = new Vector3(
                transform.forward.x, 0f, transform.forward.z);

        innerBehaviour();
    }

    /**
    StringBuilder sb = new StringBuilder();
    List<char> notAN = new List<char> { ' ', '\n', '\t', '\r' };

    public async void onChangeInput()
    {
        try
        {
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
        }
        catch
        {
            await SaveAndLoadModule.Save(player);
            await Pharo.Execute("SmalltalkImage current snapshot: true andQuit: true.");
            InteractionLogger.SessionEnd();
            Application.Quit();
        }
    }

    public string cleanCode(string code)
    {
        return Regex.Replace(code, @"<color=#b32d00>|<color=#00ffffff>|</color>|<b>|</b>", "");
    }
    **/

    public string getSelectedCode(string clean_code, bool includesEmpty)
    {
        int start = field.selectionAnchorPosition;
        int end = field.caretPosition;
        if (end < start) start = Interlocked.Exchange(ref end, start);
        string selection = clean_code.Substring(start, end - start);
        return !includesEmpty && selection == "" ? clean_code : selection;
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

    public void OnDrag(BaseEventData data)
    {
        //baseParent = transform.parent;
        //baseScale = transform.localScale;

        Transform player = ((PointerEventData) data).enterEventCamera.transform.root;
        if (player.gameObject.GetComponent<VRIDEInputHandler>().LeftTrigger)
            transform.SetParent(player.Find("Camera Offset/LeftHand Controller"));
        else
            transform.SetParent(player.Find("Camera Offset/RightHand Controller"));

        InteractionLogger.RegisterWindowDraggingStart(
            transform.position.x, transform.position.y, transform.position.z,
            name.Replace("(Clone)", ""), GetInstanceID().ToString());
    }

    public void OnEndDrag(BaseEventData data)
    {
        Vector3 gp = transform.position;
        Quaternion r = transform.rotation;

        transform.SetParent(null);
        transform.position = gp;
        transform.rotation = r;

        GetComponent<Canvas>().enabled = false;
        GetComponent<Canvas>().enabled = true;

        InteractionLogger.RegisterWindowDraggingEnd(
            transform.position.x, transform.position.y, transform.position.z,
            name.Replace("(Clone)", ""), GetInstanceID().ToString());
    }

    public void DeactivateTemporarily()
    {
        loadingWheel.SetActive(true);
    }

    public void Reactivate()
    {
        loadingWheel.SetActive(false);
    }

    public void KeepActiveOnSlide()
    {
        keyboardTarget.ActivateInputField();
        keyboardTarget.caretPosition = lastCaretPosition;
        keyboardTarget.selectionAnchorPosition = lastAnchorPosition;
    }

    public virtual void Initialize()
    {
        Vector3 pos = Camera.main.transform.position;
        Vector3 forw = Camera.main.transform.forward;
        Vector3 newFinalPos = new Vector3(
            pos.x + forw.x * .8f,
            .9f * pos.y,
            pos.z + forw.z * .8f);
        Vector3 newForw = new Vector3(forw.x, 0, forw.z);

        transform.position = newFinalPos;
        transform.forward = newForw;
    }

    public virtual void OnSelect(BaseEventData data) {
        keyboardTarget = data.selectedObject.GetComponent<TMP_InputField>();
        wordPicker.TextField = keyboardTarget;
    }

    public virtual void OnDeselect(BaseEventData data) {}

    public virtual void HorizontalExpand()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().sizeDelta += new Vector2(sizeVariance, 0f);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().sizeDelta += new Vector2(sizeVariance, 0f);

        InteractionLogger.RegisterWindowChange("Increased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "width");
    }

    public virtual void VerticalExpand()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().sizeDelta += new Vector2(0f, sizeVariance);
        if (loadingWheel != null)    
            loadingWheel.GetComponent<RectTransform>().sizeDelta += new Vector2(0f, sizeVariance);
        if (keyboardsGameObject != null)
            keyboardsGameObject.transform.localPosition -= new Vector3(0f, sizeVariance, 0f);

        InteractionLogger.RegisterWindowChange("Increased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "height");
    }

    public virtual void HorizontalContract()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().sizeDelta -= new Vector2(sizeVariance, 0f);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().sizeDelta -= new Vector2(sizeVariance, 0f);

        InteractionLogger.RegisterWindowChange("Decreased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "width");
    }

    public virtual void VerticalContract()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().sizeDelta -= new Vector2(0f, sizeVariance);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().sizeDelta -= new Vector2(0f, sizeVariance);
        if (keyboardsGameObject != null)
            keyboardsGameObject.transform.localPosition += new Vector3(0f, sizeVariance, 0f);

        InteractionLogger.RegisterWindowChange("Decreased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "height");
    }

    public virtual void IncreaseScale()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().localScale += new Vector3(scaleVariance, scaleVariance, scaleVariance);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().localScale += new Vector3(scaleVariance, scaleVariance, scaleVariance);
        if (keyboardsGameObject != null)
            keyboardsGameObject.transform.localPosition -= new Vector3(0f, 50f, 0f);

        InteractionLogger.RegisterWindowChange("Increased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "scale");
    }

    public virtual void DecreaseScale()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().localScale -= new Vector3(scaleVariance, scaleVariance, scaleVariance);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().localScale -= new Vector3(scaleVariance, scaleVariance, scaleVariance);
        if (keyboardsGameObject != null)
            keyboardsGameObject.transform.localPosition += new Vector3(0f, 50f, 0f);

        InteractionLogger.RegisterWindowChange("Decreased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "scale");
    }

    public virtual void onClose() { Destroy(gameObject); }

    public virtual void innerBehaviour() {
        if (keyboardTarget != null && keyboardTarget.isFocused)
        {
            if (fromUIClick)
            {
                fromUIClick = false;
                keyboardTarget.caretPosition = lastCaretPosition;
                keyboardTarget.selectionAnchorPosition = lastAnchorPosition;
            }

            lastCaretPosition = keyboardTarget.caretPosition;
            lastAnchorPosition = keyboardTarget.selectionAnchorPosition;
        }
    }

    public virtual IEnumerator innerStart()
    {
        yield return null;
    }

    public void ToggleKeyboard()
    {
        Renderer[] lChildRenderers = keyboardsGameObject.GetComponentsInChildren<Renderer>();
        BoxCollider[] lChildColliders = keyboardsGameObject.GetComponentsInChildren<BoxCollider>();
        foreach (Renderer lRenderer in lChildRenderers) lRenderer.enabled = !lRenderer.enabled;
        foreach (BoxCollider lCollider in lChildColliders) lCollider.enabled = !lCollider.enabled;

        CanvasGroup cg = keyboardsGameObject.GetComponent<CanvasGroup>();
        cg.interactable = !cg.interactable;
        cg.blocksRaycasts = !cg.blocksRaycasts;
        cg.alpha = Mathf.Abs(1f - cg.alpha);
    }
}