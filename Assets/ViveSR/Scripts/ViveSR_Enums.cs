using System.Runtime.InteropServices;

namespace Vive.Plugin.SR
{
    /** @enum FrameworkStatus
    An enum type of the flags of the SRWorks runtime.
    The status is a combination of the flags.
    */
    public enum RuntimeStatusFlag
    {
        HMD_IS_PLUGOUT = 0,
        COVER_IS_PLUGOUT = 1,
        CAM_NON_4K_NOT_ALIVE = 2,
        CAM_4K_NOT_ALIVE = 3,
        INITIIAL_FAIL
    }
    /** @enum FrameworkStatus
    An enum type of the status of the specific SRWorks engine.
    */
    public enum FrameworkStatus
    {
        INITIAL,
        WORKING,
        START,
        STOP,
        ERROR
    }
    /** @enum Error
    An enum type of the module error code of the specific SRWorks engine.
    */
    enum Error
    {
        FAILED                  = -1,
        WORK                    = 0,
        INVAILD_INPUT           = 1,
        FILE_NOT_FOUND          = 2,
        DATA_NOT_FOUND          = 13,
        INITIAL_FAILED          = 1001,
        NOT_IMPLEMENTED         = 1003,
        NULL_POINTER            = 1004,
        OVER_MAX_LENGTH         = 1005,
        FILE_INVALID            = 1006,
        UNINSTALL_STEAM         = 1007,
        MEMCPY_FAIL             = 1008,
        NOT_MATCH               = 1009,
        NODE_NOT_EXIST          = 1010,
        UNKONW_MODULE           = 1011,
        MODULE_FULL             = 1012,
        UNKNOW_TYPE             = 1013,
        INVALID_MODULE          = 1014,
        INVALID_TYPE            = 1015,
        MEMORY_NOT_ENOUGH       = 1016,
        BUZY                    = 1017,
        NOT_SUPPORT             = 1018,
        INVALID_VALUE           = 1019,
        COMING_SOON             = 1020,
        INVALID_CHANGE          = 1021,
        TIMEOUT                 = 1022,
        DEVICE_NOT_FOUND        = 1023,
        INVALID_DEVICE          = 1024,
        NOT_AUTHORIZED          = 1025,
        ALREADY                 = 1026,
        INTERNAL                = 1027,
        CONNECTION_FAILED       = 1028,
        ALLOCATION_FAILED       = 1029,
        OPERATION_FAILED        = 1030,
        NOT_AVAILABLE           = 1031,
        CALLBACK_IN_PROGRESS    = 1032,
        SERVICE_NOT_FOUND       = 1033,
        DISABLED_BY_USER        = 1034,
        EULA_NOT_ACCEPT         = 1035,
        NO_NEW_DATA             = 1036,
        OPENCL_NOT_SUPPORT      = 1037,
        // 1038 - 1050 reserved
        JSON_LOAD_FAILED        = 1051,
        CAMERA_NOT_ENABLED      = 1052,
        GPU_MEMORY_FULL         = 1053,
        CUDA_NOT_SUPPORT        = 1054,
        CUDA_RUNTIME_ERROR      = 1055,
        CUDA_DRIVER_ERROR       = 1056,
        UNKNOWN_ERROR           = 1057
    };
    /** @enum DualCameraIndex
    An enum type of the camera index of the unity.
    */
    public enum DualCameraIndex
    {
        LEFT,
        RIGHT
    }
    /** @enum UndistortionMethod
    An enum type of the undistorted Method of the specific SRWorks engine.
    */
    public enum UndistortionMethod
    {
        UNDISTORTED_BY_MESH,
        UNDISTORTED_BY_SRMODULE,
        DEFISH_BY_MESH = UNDISTORTED_BY_MESH,           //The enum will remove in the future.
        DEFISH_BY_SRMODULE = UNDISTORTED_BY_SRMODULE    //The enum will remove in the future.
    }
    /** @enum CalibrationType
    An enum type of the calibration type of the unity.
    */
    public enum CalibrationType
    {
        RELATIVE,
        ABSOLUTE
    }
    /** @enum CalibrationAxis
    An enum type of the calibration axis of the unity.
    */
    public enum CalibrationAxis
    {
        X, Y, Z
    }
    /** @enum DualCameraDisplayMode
    An enum type of the display mode of the unity.
    */
    public enum DualCameraDisplayMode
    {
        VIRTUAL,
        REAL,
        MIX
    }
    /** @enum DualCameraStatus
    An enum type of the camera status of the specific engine.
    */
    public enum DualCameraStatus
    {
        NOT_FOUND,
        IDLE,
        WORKING,
        ERROR
    }
    /** @enum DualCameraMode
    An enum type of the camera mode of the unity.
    */
    public enum DualCameraMode
    {
        VIRTUAL,
        REAL,
        MIX
    }

    /** @enum VRDevice
    An enum type of the VR device.
    */
    public enum VRDevice
    {
        None,
        VIVE_PRO,
        COSMOS,
    }

    #region PassThrough
    /** @enum CAMERA_Param
    An enum type for camera parameters.
    */
    enum CAMERA_Param
    {
        CAMERA_FCX_0 = 0,                       // DOUBLE   0 is left camera
        CAMERA_FCX_1,                           // DOUBLE   1 is right camera
        CAMERA_FCY_0,                           // DOUBLE   0 is left camera
        CAMERA_FCY_1,                           // DOUBLE   1 is right camera
        CAMERA_FLEN_0,                          // DOUBLE   0 is left camera
        CAMERA_FLEN_1,                          // DOUBLE   1 is right camera
        CAMERA_ROTATION_0,                      // DOUBLE   The rotation array is row order.
        CAMERA_ROTATION_1,                      // DOUBLE
        CAMERA_ROTATION_2,                      // DOUBLE
        CAMERA_ROTATION_3,                      // DOUBLE
        CAMERA_ROTATION_4,                      // DOUBLE
        CAMERA_ROTATION_5,                      // DOUBLE
        CAMERA_ROTATION_6,                      // DOUBLE
        CAMERA_ROTATION_7,                      // DOUBLE
        CAMERA_ROTATION_8,                      // DOUBLE
        CAMERA_TRANSLATION_0,                   // DOUBLE
        CAMERA_TRANSLATION_1,                   // DOUBLE
        CAMERA_TRANSLATION_2,                   // DOUBLE
        UNDISTORTION_CENTER_CX_L,               // DOUBLE
        UNDISTORTION_CENTER_CY_L,               // DOUBLE
        UNDISTORTION_CENTER_CX_R,               // DOUBLE
        UNDISTORTION_CENTER_CY_R,               // DOUBLE
        CAMERA_PARAMS_MAX
    };
    /** @enum PassThroughParam
    An enum type for pass-through parameters.
    */
    enum PassThroughParam
    {
        VR_INIT = 0,
        VR_INIT_TYPE,
        OUTPUT_DISTORTED_WIDTH,
        OUTPUT_DISTORTED_HEIGHT,
        OUTPUT_DISTORTED_CHANNEL,
        OUTPUT_UNDISTORTED_WIDTH,
        OUTPUT_UNDISTORTED_HEIGHT,
        OUTPUT_UNDISTORTED_CHANNEL,
        OUTPUT_FPS,
        OFFSET_HEAD_TO_CAMERA,          // float[6]  (x0,y0,z0,x1,y1,z1)
        PLAY_AREA_RECT,                 // float[12] (x0,y0,z0,x1,y1,z1,...)
        VIDEO_RES_NATIVE_PTR,
        VIDEO_RES_VIEW_NATIVE_PTR,
        IMAGE_NATIVE_TEXTURE_PTR_L,
        IMAGE_NATIVE_TEXTURE_PTR_R,
        CAMERA_BRIGHTNESS = 100,
        CAMERA_CONTRAST,
        CAMERA_HUE,
        CAMERA_SATURATION,
        CAMERA_SHARPNESS,
        CAMERA_GAMMA,
        CAMERA_COLOR_ENABLE,
        CAMERA_WHITE_BALANCE,
        CAMERA_BACKLIGHT_COMPENSATION,
        CAMERA_GAIN,
        CAMERA_PAN,
        CAMERA_TILT,
        CAMERA_ROLL,
        CAMERA_ZOOM,
        CAMERA_EXPOSURE,
        CAMERA_IRIS,
        CAMERA_FOCUS,
        DEVICE_SYSTEM_TYPE,
        UNDISTORTION_MODE = 200,
        UNDISTORTION_CX,
        UNDISTORTION_CY,
        UNDISTORTION_FOCAL_LENGTH,
        UNDISTORTION_FMAT_RM_L,
        UNDISTORTION_FMAT_RM_R,
        UNDISTORTION_INTRINSIC_L,
        UNDISTORTION_INTRINSIC_R,
        UNDISTORTION_R_RECTIFY_L,
        UNDISTORTION_R_RECTIFY_R,
        UNDISTORTION_COEFFS_L,
        UNDISTORTION_COEFFS_R,
        UNDISTORTION_P_NEWPROJ_L,
        UNDISTORTION_P_NEWPROJ_R,
        UNDISTORTION_MAP_SIZE,
        UNDISTORTION_MAP_L,
        UNDISTORTION_MAP_R,
        UNDISTORTION_CENTER,
        MESH_NATIVE_VERTEX_BUFFER_PTR_L,
        MESH_NATIVE_VERTEX_BUFFER_PTR_R,
        MESH_NATIVE_VERTEX_ARRAY_L,
        MESH_NATIVE_VERTEX_ARRAY_R,
        MESH_CALIB_NATIVE_VERTEX_BUFFER_PTR_L,
        MESH_CALIB_NATIVE_VERTEX_BUFFER_PTR_R,
        MESH_CALIB_NATIVE_VERTEX_ARRAY_L,
        MESH_CALIB_NATIVE_VERTEX_ARRAY_R,
        ENABLE_IMAGE_PLANE_MESH_L,
        ENABLE_IMAGE_PLANE_MESH_R,
        ENABLE_CALIB_IMAGE_PLANE_MESH_L,
        ENABLE_CALIB_IMAGE_PLANE_MESH_R,
        CAMERA_PARAMETERS = 300,                // void *
        OFFSET_HEAD_TO_CAMERA_x0,               // float[6]  (x0,y0,z0,x1,y1,z1)
        OFFSET_HEAD_TO_CAMERA_y0,
        OFFSET_HEAD_TO_CAMERA_z0,
        OFFSET_HEAD_TO_CAMERA_x1,
        OFFSET_HEAD_TO_CAMERA_y1,
        OFFSET_HEAD_TO_CAMERA_z1,
        TRACKING_POSE = 500,                    // void *
        OUTPUT_4K_READY,                        // bool
        D3D11_SHARED_HANDLE_UNDISTORTED_LEFT,
        D3D11_SHARED_HANDLE_UNDISTORTED_RIGHT,
        SKIP_VGA_PASS_THROUGH
    };
    /** @enum PassThrough4KParam
    An enum type for pass-through 4K parameters.
    */
    enum PassThrough4KParam
    {
        OUTPUT_DISTORTED_4K_WIDTH = 0,
        OUTPUT_DISTORTED_4K_HEIGHT,
        OUTPUT_DISTORTED_4K_CHANNEL,
        OUTPUT_UNDISTORTED_4K_WIDTH,
        OUTPUT_UNDISTORTED_4K_HEIGHT,
        OUTPUT_UNDISTORTED_4K_CHANNEL,
        OUTPUT_4K_OFFSET_HEAD_TO_CAMERA,        // float[6]  (x0,y0,z0,x1,y1,z1)
        CAMERA_4K_PARAMETERS,                   // void *
        OFFSET_HEAD_TO_4K_CAMERA_x0,            // float[6]  (x0,y0,z0,x1,y1,z1)
        OFFSET_HEAD_TO_4K_CAMERA_y0,
        OFFSET_HEAD_TO_4K_CAMERA_z0,
        OFFSET_HEAD_TO_4K_CAMERA_x1,
        OFFSET_HEAD_TO_4K_CAMERA_y1,
        OFFSET_HEAD_TO_4K_CAMERA_z1,
        OUTPUT_4K_READY,						// bool 
        D3D11_SHARED_HANDLE_4K_UNDISTORTED_LEFT,
        D3D11_SHARED_HANDLE_4K_UNDISTORTED_RIGHT
    };
    #endregion

    #region Depth
    /** @enum DepthParam
    An enum type for depth parameters.
    */
    enum DepthParam
    {
        OUTPUT_WIDTH,
        OUTPUT_HEIGHT,
        OUTPUT_CHAANEL_0,
        OUTPUT_CHAANEL_1,
        TYPE,
        FOCULENS,
        BASELINE,
        COLLIDER_QUALITY,
        MESH_NEAR_DISTANCE,
        MESH_FAR_DISTANCE,
        DENOISE_MEDIAN_FILTER,  // range : 1, 3, 5; (default: 3)
        CONFIDENCE_THRESHOLD,   // range : 0 ~ 5; (default: 0.05)
        DENOISE_GUIDED_FILTER,	// range : 1 ~ 7; (default: 3)
        DEPTH_USING_CASE,
        KEEP_ONFLY_RESULT_AT_END,
    };
    /** @enum DepthCase
    An enum type for depth type.
    */
    public enum DepthCase
    {
        DEFAULT,
        CLOSE_RANGE,
    };

    // The DetphCmd index start from 100.
    // It's define in SR_Module.
    /** @enum DepthCmd
    An enum type for control signal to change depth module's behavior.
    */
    enum DepthCmd
    {
        EXTRACT_DEPTH_MESH = 100,
        ENABLE_SELECT_MESH_DISTANCE_RANGE,
        ENABLE_DEPTH_MESH_HOLE_FILLING,
        ENABLE_REFINEMENT,
        ENABLE_EDGE_ENHANCE,
        CHANGE_DEPTH_CASE,
        CHANGE_DEPTH_CASE_2_RECONSTRUCT_USED,
        ENABLE_RECORDING_DATASET = 200,
    }
    #endregion

    #region Reconstruction
    /** @enum ReconstructionParam
    An enum type for RigidReconstruction parameters.
    */
    enum ReconstructionParam
    {
        VOXEL_SIZE = 0,
        COLOR_SIZE = 1,
        DATA_SOURCE = 2,
        DATASET_PATH = 3,
        RGB_IMAGE_EXT = 4,
        DATASET_FRAME = 5,
        MAX_DEPTH = 6,
        MIN_DEPTH = 7,
        POINTCLOUD_POINTSIZE = 9,
        EXPORT_ADAPTIVE_MODEL = 10,
        ADAPTIVE_MAX_GRID = 11,
        ADAPTIVE_MIN_GRID = 12,
        ADAPTIVE_ERROR_THRES = 13,

        SECTOR_SIZE = 15,           // float
        SECTOR_NUM_PER_SIDE = 16,	// int

        ENABLE_FRUSTUM_CULLING = 20,

        CONFIG_FILEPATH = 21,
        CONFIG_QUALITY,
        CONFIG_EXPORT_COLLIDER,
        CONFIG_EXPORT_TEXTURE,

        DATA_CURRENT_POS = 31,
        LITE_POINT_CLOUD_MODE,
        FULL_POINT_CLOUD_MODE,
        LIVE_ADAPTIVE_MODE,
        MESH_REFRESH_INTERVAL = 37,
        ENABLE_SECTOR_GROUPER = 38,

        SCENE_UNDERSTANDING_ENABLE = 40,
        SCENE_UNDERSTANDING_MACHINE_VISION = 41,
        SCENE_UNDERSTANDING_CONFIG = 42,
        SCENE_UNDERSTANDING_REFINEMENT = 43,
        SEMANTIC_FUSION_ALL_AFTER_SCANNING = 44,

        VERTEX_BUFFER_NATIVE_PTR = 99,
        INDEX_BUFFER_NATIVE_PTR = 100,
    };
    /** @enum ReconstructionCmd
    An enum type for control signal to change RigidReconstruction module's behavior.
    */
    enum ReconstructionCmd
    {
        START = 200,
        STOP = 201,
        SHOW_INFO = 202,
        EXTRACT_POINT_CLOUD = 203,
        EXTRACT_VERTEX_NORMAL = 204,
        EXPORT_MODEL_RIGHT_HAND = 205,
        EXPORT_MODEL_FOR_UNITY = 206,
        EXPORT_SCENE_UNDERSTANDING_RIGHT_HAND = 207,
        EXPORT_SCENE_UNDERSTANDING_FOR_UNITY = 208,
        MODEL_PREVIEW_START_RIGHT_HAND = 209,
        MODEL_PREVIEW_START_FOR_UNITY = 210,
        MODEL_PREVIEW_NEXT_CHUNK = 211,
        MODEL_PREVIEW_FINISH = 212,
        EXPORT_ROOT_FOLDER = 213,
        RESET_RECONSTRUCTION_ENGINE = 214,
    };
    /** @enum ReconstructionQuality
    An enum type for reconstruction mesh quality.
    */
    public enum ReconstructionQuality
    {
        LOW = 2,
        MID = 3,
        HIGH = 4,
    }
    /** @enum ReconstructionLiveMeshExtractMode
    An enum type for reconstruction live mesh extract mode.
    */
    public enum ReconstructionLiveMeshExtractMode
    {
        VERTEX_WITHOUT_NORMAL = 0,
        VERTEX_WITH_NORMAL = 1,
        FACE_NORMAL = 2,
    };
    /** @enum ReconstructionLiveColliderType
    An enum type for reconstruction live collider yype.
    */
    public enum ReconstructionLiveColliderType
    {
        CONVEX_SHAPE = 0,
        BOUNDING_BOX_SHAPE = 1,
    };
    /** @enum ReconstructionExportStage
    An enum type for reconstruction module stage of export object.
    */
    public enum ReconstructionExportStage
    {
        STAGE_EXTRACTING_MODEL = 0x0017,
        STAGE_COMPACTING_TEXTURE = 0x0018,
        STAGE_SAVING_MODEL_FILE = 0x0019,
        STAGE_EXTRACTING_COLLIDER = 0x001A,
        SCENE_UNDERSTANDING_PASS_1 = 0x0030,
        SCENE_UNDERSTANDING_PASS_2 = 0x0031,
    }
    /** @enum ReconstructionDisplayMode
    An enum type for reconstruction mesh display mode.
    */
    public enum ReconstructionDisplayMode
    {
        FULL_SCENE = 0,
        FIELD_OF_VIEW = 1,
        ADAPTIVE_MESH = 2,
    }
    /** @enum ReconstructionRenderMode
    An enum type for reconstruction mesh rendering mode.
    */
    public enum ReconstructionRenderMode
    {
        NONE,
        ALL, // default
        SECTOR,
        MODEL_PREVIEW
    }
    #endregion

    #region AI scene
    public enum AISceneParam
    {
        REFINE_TYPE = 1,
        Img_CH,
        Img_Crop_W,
        Img_Crop_H,
        Label_W,
        Label_H,
        Label_CH,
        Label_Number,
        Model
    }
    public enum AI_Model
    {
        SCENE_SEMANTIC,
        HUMAN
    }
    #endregion
}
