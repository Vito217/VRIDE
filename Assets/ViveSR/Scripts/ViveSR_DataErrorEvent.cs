//========= Copyright 2020, HTC Corporation. All rights reserved. ===========

using System;
using System.Collections.Generic;

namespace Vive.Plugin.SR
{
    public class ViveSR_DataErrorEvent
    {
        private Dictionary<int, Action> Handlers = new Dictionary<int, Action>();

        /// <summary>
        /// Register a callback for an error code.
        /// Any previous registered callback will be unregistered.
        /// </summary>
        public void RegisterHandler(int errorCode, Action callback)
        {
            // Allow only one handler for a specific type of error.
            Handlers[errorCode] = callback;
        }

        /// <summary>
        /// Unregister the callback of an error code.
        /// </summary>
        public void UnregisterHandler(int errorCode)
        {
            if (Handlers.ContainsKey(errorCode))
            {
                Handlers.Remove(errorCode);
            }
        }

        /// <summary>
        /// Invoke the registered callback of an error code.
        /// </summary>
        public void Invoke(int errorCode)
        {
            if (Handlers.ContainsKey(errorCode))
            {
                Handlers[errorCode]();
            }
        }
    }
}
