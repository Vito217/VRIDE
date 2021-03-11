using UnityEngine;
using UnityEngine.XR.Interaction.Toolkit;

public class HandHider : MonoBehaviour
{
    public GameObject handObject = null;

    private HandPhysics handPhysics = null;
    private XRDirectInteractor interactor = null;

    private void Awake()
    {
        handPhysics = handObject.GetComponent<HandPhysics>();
        interactor = GetComponent<XRDirectInteractor>();
    }

    private void OnEnable()
    {
        interactor.onSelectEntered.AddListener(Hide);
        interactor.onSelectExited.AddListener(Show);
    }

    private void OnDisable()
    {
        interactor.onSelectEntered.RemoveListener(Hide);
        interactor.onSelectExited.RemoveListener(Show);
    }

    private void Show(XRBaseInteractable interactable)
    {
        handPhysics.TeleportToTarget();
        handObject.SetActive(true);
    }

    private void Hide(XRBaseInteractable interactable)
    {
        handObject.SetActive(false);
    }
}
