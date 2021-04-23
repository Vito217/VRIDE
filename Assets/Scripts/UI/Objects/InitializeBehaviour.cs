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
    public Image panel;
    public TMP_InputField logText;
    public TMP_InputField field;
    public TMP_InputField keyboardTarget;
    public TextMeshProUGUI lineCounter;
    public GameObject loadingWheel;
    public GameObject keyboardsGameObject;
    public bool freezeRotation = true;
    
    float sizeVariance = 20;
    float scaleVariance = .2f;

    void Start()
    {
        GetComponent<Canvas>().worldCamera = Camera.main;
        if (panel != null) panel.color = UnityEngine.Random.ColorHSV();
        if (keyboardsGameObject != null && !VRIDEMenu.keyboardToggleState) ToggleKeyboard();

        StartCoroutine(Coroutine());
    }

    public IEnumerator Coroutine()
    {
        yield return innerStart();
    }

    void Update()
    {
        if(freezeRotation) transform.forward = new Vector3(transform.forward.x, 0f, transform.forward.z);
        innerBehaviour();
    }

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
        StartCoroutine(HandleDrag(((PointerEventData)data).enterEventCamera.transform.root));
    }

    IEnumerator HandleDrag(Transform player)
    {
        yield return null;
        if (player.gameObject.GetComponent<VRIDEInputHandler>().LeftTrigger)
            transform.SetParent(player.Find("Camera Offset/LeftHand Controller"));
        else if (player.gameObject.GetComponent<VRIDEInputHandler>().RightTrigger)
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

    public void DeactivateTemporarily() { loadingWheel.SetActive(true); }

    public void Reactivate() { loadingWheel.SetActive(false); }

    public void KeepActiveOnSlide()
    {
        keyboardTarget.ActivateInputField();
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
    }

    public virtual void OnDeselect(BaseEventData data) {}

    public virtual void HorizontalExpand()
    {
        GetComponent<RectTransform>().sizeDelta += new Vector2(sizeVariance, 0f);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().sizeDelta += new Vector2(sizeVariance, 0f);

        InteractionLogger.RegisterWindowChange("Increased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "width");
    }

    public virtual void VerticalExpand()
    {
        GetComponent<RectTransform>().sizeDelta += new Vector2(0f, sizeVariance);
        if (loadingWheel != null)    
            loadingWheel.GetComponent<RectTransform>().sizeDelta += new Vector2(0f, sizeVariance);

        InteractionLogger.RegisterWindowChange("Increased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "height");
    }

    public virtual void HorizontalContract()
    {
        GetComponent<RectTransform>().sizeDelta -= new Vector2(sizeVariance, 0f);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().sizeDelta -= new Vector2(sizeVariance, 0f);

        InteractionLogger.RegisterWindowChange("Decreased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "width");
    }

    public virtual void VerticalContract()
    {
        GetComponent<RectTransform>().sizeDelta -= new Vector2(0f, sizeVariance);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().sizeDelta -= new Vector2(0f, sizeVariance);

        InteractionLogger.RegisterWindowChange("Decreased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "height");
    }

    public virtual void IncreaseScale()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().localScale += new Vector3(scaleVariance, scaleVariance, scaleVariance);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().localScale += new Vector3(scaleVariance, scaleVariance, scaleVariance);

        InteractionLogger.RegisterWindowChange("Increased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "scale");
    }

    public virtual void DecreaseScale()
    {
        transform.Find("Panel").gameObject.GetComponent<RectTransform>().localScale -= new Vector3(scaleVariance, scaleVariance, scaleVariance);
        if (loadingWheel != null)
            loadingWheel.GetComponent<RectTransform>().localScale -= new Vector3(scaleVariance, scaleVariance, scaleVariance);

        InteractionLogger.RegisterWindowChange("Decreased", name.Replace("(Clone)", ""), GetInstanceID().ToString(), "scale");
    }

    public virtual void onClose() { Destroy(gameObject); }

    public virtual void innerBehaviour() { }

    public virtual IEnumerator innerStart() { yield return null; }

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

    public void CountLines()
    {
        string t = "1";
        int lines = 1;

        foreach (char c in field.text) 
            if (c == '\n') 
                t += "\n" + ++lines;

        lineCounter.text = t;
    }

    protected void PaintWord(TMP_TextInfo textInfo, TMP_WordInfo wordInfo, Color color)
    {
        for (int i = 0; i < wordInfo.characterCount; ++i)
        {
            int charIndex = wordInfo.firstCharacterIndex + i;
            int meshIndex = textInfo.characterInfo[charIndex].materialReferenceIndex;
            int vertexIndex = textInfo.characterInfo[charIndex].vertexIndex;

            Color32[] vertexColors = field.textComponent.textInfo.meshInfo[meshIndex].colors32;
            vertexColors[vertexIndex + 0] = color;
            vertexColors[vertexIndex + 1] = color;
            vertexColors[vertexIndex + 2] = color;
            vertexColors[vertexIndex + 3] = color;
        }
    }
}