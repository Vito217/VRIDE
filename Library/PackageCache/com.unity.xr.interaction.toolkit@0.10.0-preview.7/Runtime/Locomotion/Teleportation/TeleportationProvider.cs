using UnityEngine.Assertions;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// The <see cref="TeleportationProvider"/> is responsible for moving the XR Rig
    /// to the desired location on the user's request.
    /// </summary>
    public class TeleportationProvider : LocomotionProvider
    {
        /// <summary>
        /// The current teleportation request.
        /// </summary>
        protected TeleportRequest currentRequest { get; set; }

        /// <summary>
        /// Whether the current teleportation request is valid.
        /// </summary>
        protected bool validRequest { get; set; }

        /// <summary>
        /// This function will queue a teleportation request within the provider.
        /// </summary>
        /// <param name="teleportRequest">The teleportation request to queue.</param>
        /// <returns>Returns <see langword="true"/> if successfully queued. Returns <see langword="false"/> otherwise.</returns>
        public virtual bool QueueTeleportRequest(TeleportRequest teleportRequest)
        {
            currentRequest = teleportRequest;
            validRequest = true;
            return true;
        }

        /// <summary>
        /// Update function for the <see cref="TeleportationProvider"/>.
        /// </summary>
        protected virtual void Update()
        {
            if (!validRequest || !BeginLocomotion())
                return;

            var xrRig = system.xrRig;
            if (xrRig != null)
            {
                switch (currentRequest.matchOrientation)
                {
                    case MatchOrientation.WorldSpaceUp:
                        xrRig.MatchRigUp(Vector3.up);
                        break;
                    case MatchOrientation.TargetUp:
                        xrRig.MatchRigUp(currentRequest.destinationRotation * Vector3.up);
                        break;
                    case MatchOrientation.TargetUpAndForward:
                        xrRig.MatchRigUpCameraForward(currentRequest.destinationRotation * Vector3.up, currentRequest.destinationRotation * Vector3.forward);
                        break;
                    case MatchOrientation.None:
                        // Change nothing. Maintain current rig rotation.
                        break;
                    default:
                        Assert.IsTrue(false, $"Unhandled {nameof(MatchOrientation)}={currentRequest.matchOrientation}.");
                        break;
                }

                var heightAdjustment = xrRig.rig.transform.up * xrRig.cameraInRigSpaceHeight;

                var cameraDestination = currentRequest.destinationPosition + heightAdjustment;

                xrRig.MoveCameraToWorldLocation(cameraDestination);
            }

            EndLocomotion();
            validRequest = false;
        }
    }
}
