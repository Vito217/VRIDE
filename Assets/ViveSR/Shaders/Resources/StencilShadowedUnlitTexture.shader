Shader "ViveSR/Unlit, Textured, Shadowed, Stencil"
{
	Properties
	{
		_MainTex ("Texture", 2D) = "white" {}
		_StencilValue ("StencilRefValue", float) = 0
		[Enum(UnityEngine.Rendering.CompareFunction)]_StencilComp("Stencil Compare", int) = 0	// disable
	}

	CGINCLUDE
		#include "UnityCG.cginc"
		#include "AutoLight.cginc"

		struct appdata
		{
			float4 vertex : POSITION;
			float2 uv : TEXCOORD0;
		};

		struct v2f
		{
			float4 pos : SV_POSITION;
			float2 uv : TEXCOORD0;
			SHADOW_COORDS(2)				
		};

		sampler2D _MainTex;
		float4 _MainTex_ST;
			
		v2f vert (appdata v)
		{
			v2f o;
			o.pos = UnityObjectToClipPos(v.vertex);
			o.uv = TRANSFORM_TEX(v.uv, _MainTex);
			TRANSFER_SHADOW(o)
			return o;
		}
			
		fixed4 frag (v2f i) : SV_Target
		{
			fixed4 col = tex2D(_MainTex, i.uv);
			fixed shadow = SHADOW_ATTENUATION(i);
			col.rgb *= shadow;
		#ifdef FWDADD				
			col.a = (1-shadow);
		#endif
			return col;
		}
	ENDCG

	SubShader
	{
		Tags { "RenderType"="Opaque" "Queue" = "Geometry-2" }

		Stencil{
			Ref  [_StencilValue]
			Comp [_StencilComp]
		}

		Pass
		{
			Tags { "LightMode"="ForwardBase"}

			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma multi_compile_fwdbase
			ENDCG
		}

		Pass
		{
			Tags { "LightMode"="ForwardAdd"}
			
			Blend SrcAlpha OneMinusSrcAlpha
			ZWrite Off

			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma multi_compile_fwdadd_fullshadows
			#pragma multi_compile FWDADD
			ENDCG
		}
	}

	FallBack "Standard"
}
