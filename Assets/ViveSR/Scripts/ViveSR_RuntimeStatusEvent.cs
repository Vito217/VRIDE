//========= Copyright 2020, HTC Corporation. All rights reserved. ===========

using System;
using System.Collections.Generic;

namespace Vive.Plugin.SR
{
    public class ViveSR_RuntimeStatusEvent
    {
        private Dictionary<RuntimeStatusFlag, List<Action<bool>>> Handlers = new Dictionary<RuntimeStatusFlag, List<Action<bool>>>();

        /// <summary>
        /// Register a callback for a flag.
        /// </summary>
        public void RegisterHandler(RuntimeStatusFlag flag, Action<bool> callback)
        {
            if (Handlers.ContainsKey(flag))
            {
                Handlers[flag].Add(callback);
            }
            else
            {
                var callbacks = new List<Action<bool>>();
                callbacks.Add(callback);
                Handlers[flag] = callbacks;
            }
        }

        /// <summary>
        /// Unregister a callback of a flag.
        /// </summary>
        public void UnregisterHandler(RuntimeStatusFlag flag, Action<bool> callback)
        {
            if (Handlers.ContainsKey(flag))
            {
                Handlers[flag].Remove(callback);
            }
        }

        /// <summary>
        /// Invoke all registered callbacks of a flag.
        /// </summary>
        public void Invoke(RuntimeStatusFlag flag, bool value)
        {
            if (Handlers.ContainsKey(flag))
            {
                foreach (var callback in Handlers[flag])
                {
                    callback(value);
                }
            }
        }
    }
}
