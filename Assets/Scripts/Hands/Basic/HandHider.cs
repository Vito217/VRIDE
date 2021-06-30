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
#pragma warning disable CS0618 // El tipo o el miembro están obsoletos
        interactor.onSelectEntered.AddListener(Hide);
#pragma warning restore CS0618 // El tipo o el miembro están obsoletos
#pragma warning disable CS0618 // El tipo o el miembro están obsoletos
        interactor.onSelectExited.AddListener(Show);
#pragma warning restore CS0618 // El tipo o el miembro están obsoletos
    }

    private void OnDisable()
    {
#pragma warning disable CS0618 // El tipo o el miembro están obsoletos
        interactor.onSelectEntered.RemoveListener(Hide);
#pragma warning restore CS0618 // El tipo o el miembro están obsoletos
#pragma warning disable CS0618 // El tipo o el miembro están obsoletos
        interactor.onSelectExited.RemoveListener(Show);
#pragma warning restore CS0618 // El tipo o el miembro están obsoletos
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
