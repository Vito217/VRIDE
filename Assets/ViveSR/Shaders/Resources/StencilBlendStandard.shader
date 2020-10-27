Shader "ViveSR/BlendStandard, Stencil" {
	Properties {
		_Color ("Color", Color) = (1,1,1,1)
		_MainTex ("Albedo (RGB)", 2D) = "white" {}
		_BumpScale("Bump", Range(0,1)) = 0.5
		_BumpMap("Normal Map", 2D) = "bump" {}
		_EmissionColor("EmissionColor", Color) = (0,0,0,1)
		_EmissionMap("Emission", 2D) = "white" {}
		_Glossiness("Smoothness", Range(0,1)) = 0.5
		_MetallicGlossMap("Metallic", 2D) = "white"
		//[Enum(UnityEngine.Rendering.BlendMode)]_BlendSrc("Blend Src", int) = 1
		//[Enum(UnityEngine.Rendering.BlendMode)]_BlendDst("Blend Dst", int) = 0
		_StencilValue ("StencilRefValue", float) = 1
		[Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0	// disable
		[Enum(UnityEngine.Rendering.CompareFunction)]_ZTestComp("ZTest Compare", int) = 4		// lequal
	}
	SubShader {
		Tags {"Queue" = "Transparent" "IgnoreProjector" = "True" "RenderType"="Transparent" }
		LOD 200
		//Blend [_BlendSrc] [_BlendDst]
		Blend SrcAlpha OneMinusSrcAlpha

		ZTest[_ZTestComp]
		Stencil{
			Ref [_StencilValue]
			Comp[_StencilComp]
		}

		CGPROGRAM

		#pragma surface surf Standard fullforwardshadows alpha:fade
		#pragma target 3.0
		#pragma multi_compile __  CLIP_PLANE
		#include "../ViveSRCG.cginc"

		sampler2D _MainTex;

		struct Input {
			float2 uv_MainTex;
			float2 uv_BumpMap;
			float2 uv_MetallicGlossMap;
			float2 uv_EmissionMap;
			WORLD_POS_FORCLIP_SURF
		};

		half _Glossiness;
		half _BumpScale;
		fixed4 _Color;
		fixed4 _EmissionColor;
		sampler2D _BumpMap;
		sampler2D _MetallicGlossMap;
		sampler2D _EmissionMap;
		DECLARE_CLIP_PLANE_VARIABLE

		void surf (Input IN, inout SurfaceOutputStandard o) 
		{
			CLIP_PLANE_TEST(IN)
			// Albedo comes from a texture tinted by color
			fixed4 c = tex2D (_MainTex, IN.uv_MainTex) * _Color;
			fixed4 m = tex2D(_MetallicGlossMap, IN.uv_MetallicGlossMap) * _Glossiness;
			fixed4 e = tex2D(_EmissionMap, IN.uv_EmissionMap) * _EmissionColor;
			o.Albedo = c.rgb;

			// Metallic and smoothness come from slider variables
			o.Metallic = m.rgb;
			o.Smoothness = _Glossiness * m.a;
			o.Alpha = c.a * _Color.a;

			fixed3 normal = UnpackNormal(tex2D(_BumpMap, IN.uv_BumpMap));
			normal.xy *= _BumpScale;
			o.Normal = normalize(normal);

			o.Emission = e.rgb;
		}
		ENDCG
	}
	FallBack "Diffuse"
}
