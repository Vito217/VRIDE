using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.EventSystems;
using Valve.VR;

public class SteamVRInputModule : BaseInputModule
{
    public Camera m_Camera;
    public SteamVR_Input_Sources m_TargetSource;
    public SteamVR_Action_Boolean m_ClickAction;

    private GameObject m_CurrentObject = null;
    private PointerEventData m_Data = null;

    protected override void Awake() {

        base.Awake();
        m_Data = new PointerEventData(eventSystem);
        eventSystem.RaycastAll(m_Data, m_RaycastResultCache);
        m_Data.pointerCurrentRaycast = FindFirstRaycast(m_RaycastResultCache);
        m_CurrentObject = m_Data.pointerCurrentRaycast.gameObject;
        m_RaycastResultCache.Clear();
        HandlePointerExitAndEnter(m_Data, m_CurrentObject);

        if (m_ClickAction.GetStateDown(m_TargetSource))
            ProcessPress(m_Data);

        if (m_ClickAction.GetStateUp(m_TargetSource))
            ProcessRelease(m_Data);
    }

    public override void Process() 
    {
        m_Data.Reset();
        m_Data.position = new Vector2(m_Camera.pixelWidth, m_Camera.pixelHeight) * 0.5f;

    }

    public PointerEventData GetData()
    {
        return m_Data;
    }

    private void ProcessPress(PointerEventData data)
    {

    }

    private void ProcessRelease(PointerEventData data)
    {

    }
}
