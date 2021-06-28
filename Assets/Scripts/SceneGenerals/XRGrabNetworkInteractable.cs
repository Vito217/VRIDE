using UnityEngine.XR.Interaction.Toolkit;
using Photon.Pun;


public class XRGrabNetworkInteractable : XRGrabInteractable
{
    private PhotonView photonView;

    void Start()
    {
        photonView = GetComponent<PhotonView>();
    }

    protected override void OnSelectEntered(SelectEnterEventArgs interactor)
    {
        photonView.RequestOwnership();
        base.OnSelectEntered(interactor);
    }
}
