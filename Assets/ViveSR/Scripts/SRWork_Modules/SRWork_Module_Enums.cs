//========= Copyright 2018, HTC Corporation. All rights reserved. ===========
namespace Vive
{
    namespace Plugin.SR
    {

        /** @enum ModuleType
        An enum type of SRWorks engine. used for Vive.Plugin.SR.SRWorkModule_API.Initial().
        */
        public enum ModuleType
        {
            PASSTHROUGH = 0,
            DEPTH,
            DEPTHMESH,
            RIGIDRECONSTRUCTION,
            PASSTHROUGH4K,
            CONTROLLER_POSE,
            AI_Scene,
            MAX,
        }
        /** @enum ModuleStatus
        An enum type of modules status.
        */
        public enum ModuleStatus
        {
            ERROR,
            IDLE,
            WORKING,
            BLOCKED
        }
    }
}