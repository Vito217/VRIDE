# About the Oculus XR Plugin

The Oculus XR Plugin enables you to build applications for a variety of Oculus devices including the Rift, Rift S, Quest, and Go.

## Supported XR plugin subsystems

### Display 

The display subsystem provides stereo rendering support for the XR Plugin. It supports the following graphics APIs:

* Windows (Rift, Rift S)
    * DX11
* Android (Quest, Go)
    * OpenGL ES 3.0
    * Vulkan (Experimental, Quest only)

### Input 

The input subsystem provides controller support, haptics, and tracking for the controllers and HMD.

## XR Management support

Integration with XR Management isn't required to use the Oculus XR Plugin, but it provides for a simpler and easier way of using this and other Providers within Unity. The Oculus XR Plugin package ships with built-in XR Management support. For more information, see [XR Management Documention](https://docs.unity3d.com/Packages/com.unity.xr.management@latest)

The Oculus XR Plugin integration with XR Management provides the following functionality:

* **Runtime Settings** - Configure runtime settings such as rendering modes, depth buffer sharing, Dash support, etc.
* **Lifecycle Management** - The Oculus XR Plugin ships with a default XR Plugin loader implementation that handles subsystem lifecycle such as application initialization, shutdown, pausing, and resuming.

### Windows standalone settings (Rift, Rift S)

* **Stereo Rendering Mode** - You can select *Multi Pass* or *Single Pass Instanced* stereo rendering mode.
	* *Multi Pass* - Unity renders each eye independently by making two passes across the scene graph. Each pass has its own eye matrices and render target. Unity draws everything twice, which includes setting the graphics state for each pass. This is a slow and simple rendering method which doesn't require any special modification to shaders.
	* *Single Pass Instanced* - Unity uses a texture array with two slices, and uses instanced draw calls (converting non-instanced draws call to instanced versions when necessary) to direct rendering to the appropriate texture slice.  Custom shaders need to be modified for rendering in this mode.  Use Unity's XR shader macros to simplify authoring custom shaders. 
* **Shared Depth Buffer** - Enable or disable support for using a shared depth buffer. This allows Unity and Oculus to use a common depth buffer, which enables Oculus to composite the Oculus Dash and other utilities over the Unity application.
* **Dash Support** - Enable or disable Dash support. This inintializes the Oculus Plugin with Dash support, which enables the Oculus Dash to composite over the Unity application.

### Android settings (Quest, Go)

* **Stereo Rendering Mode** - You can select *Multi Pass* or *Multiview* stereo rendering mode.
	* *Multi Pass* - Unity renders each eye independently by making two passes across the scene graph. Each pass has its own eye matrices and render target. Unity draws everything twice, which includes setting the graphics state for each pass. This is a slow and simple rendering method which doesn't require any special modification to shaders.
	* *Multiview* - Multiview is essentially the same as the *Single Pass Instanced* option described above, except the graphics driver does the draw call conversion, requiring less work from the Unity engine. As with *Single Pass Instanced*, shaders need to be authored to enable Multiview.  Using Unity's XR shader macros will simplify custom shader development.
* **Low Overhead Mode** - If enabled, the GLES graphics driver will bypass validation code, potentially running faster. Disable this if you experience graphics instabilities. GLES only.
* **Protected Context** - If enabled, the Oculus SDK will create a protected graphics context. This has a slight overhead, and should only be enabled if you know that you need a protected context. For example, if you display protected video content.
* **Optimize Buffer Discards** - If enabled, the depth buffer contents will be discarded rather than resolved and the MSAA color buffer will be resolved rather than stored after rendering. This is a performance optimization that can possibly break rendering effects that sample from the depth buffer, such as camera stacking. Vulkan only.
* **Focus Aware** - If enabled, the application will continue running when system overlays and menus are present.
* **V2 Signing (Quest)** - Enable this if you are building for Quest. This enables application signing with the Android Package (APK) Signature Scheme v2. Disable v2 signing if building for Oculus Go.

## Technical details

### Fixed-Foveated Rendering (FFR)

Both Quest and Go support [fixed-foveated rendering](https://developer.oculus.com/documentation/quest/latest/concepts/mobile-ffr/) to provide better performance for [pixel-fill limited](https://en.wikipedia.org/wiki/Fillrate) applications.  Controlling the level of foveation is made available through APIs in the Oculus XR Plugin.

FFR works best when rendering directly into the *eye textures* using the [foward rendering mode](https://docs.unity3d.com/Manual/RenderTech-ForwardRendering.html).  [*Deferred rendering* mode](https://docs.unity3d.com/Manual/RenderTech-DeferredShading.html), which is characterized by rendering into an intermediate render texture, is not recommended for use with FFR.  This situation arises often when using the default *Universal Rendering Pipeline*, which included a blit operation by default at the end of the frame. 

### Vulkan

Currently, using the Vulkan graphics API is supported in an experimental release and only for the Quest platform.  The implementation supports multiview rendering and fixed-foveated rendering.

To enable Vulkan, follow the steps below:

* Open the **Project Settings** window (menu: **Edit > Project Settings**), and select **Player**.
* Under the **Android** settings, add and move **Vulkan** to the top of the list of **Graphic APIs** so that it is selected ahead of others.

Note that unless otherwise modified, OpenGL ES 3.0 is the default graphics API used.
