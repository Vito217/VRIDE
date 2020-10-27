Shader "ViveSR/MeshCuller, Shadowed, Stencil"
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
				float3 cPos : TEXCOORD1;
				SHADOW_COORDS(2)				
			};

			sampler2D _MainTex;
			sampler2D _PassThroughBGTex;
			
			v2f vert (appdata v)
			{
				v2f o;
				o.pos = UnityObjectToClipPos(v.vertex);
				o.cPos = o.pos.xyw;
				TRANSFER_SHADOW(o)
				return o;
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				float2 screenPos = i.cPos.xy / i.cPos.z;
				screenPos.x = 0.5 + screenPos.x * 0.5;
				screenPos.y = 0.5 - screenPos.y * 0.5;
				fixed4 col = tex2D(_PassThroughBGTex, screenPos);
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
		Tags { "RenderType"="Opaque" "Queue" = "Geometry-2"}

		Stencil{
			Ref  [_StencilValue]
			Comp [_StencilComp]
		}

		GrabPass{ "_PassThroughBGTex" }

		Pass
		{
			Tags { "LightMode"="ForwardBase"}
			Cull Off

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
			Cull Off
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
