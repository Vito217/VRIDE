//========= Copyright 2017, HTC Corporation. All rights reserved. ===========
using UnityEngine;

namespace Vive.Plugin.SR
{
    public abstract class ViveSR_Module : MonoBehaviour, IModule
    {
        public virtual bool Initial() { return false; }
        public virtual bool Release() { return false; }
        public virtual bool RightBeforeStartModule() { return false; }
    }

    public interface IModule
    {
        bool Initial();
        bool Release();
    }
}