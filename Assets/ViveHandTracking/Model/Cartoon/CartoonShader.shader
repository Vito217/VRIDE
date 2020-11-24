// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X
Shader "ViveHandTracking/Cartoon"
{
	Properties
	{
		_TextureSample1("Texture Sample 1", 2D) = "white" {}
		_TextureSample2("Texture Sample 2", 2D) = "white" {}
		_TextureSample4("Texture Sample 4", 2D) = "white" {}
		_Texture0("Texture 0", 2D) = "white" {}
		_Color("Main Color", Color) = (1,1,1,1)
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
		[HideInInspector] __dirty( "", Int ) = 1
	}

	SubShader
	{
		Tags{ "RenderType" = "Transparent"  "Queue" = "Transparent" "IgnoreProjector" = "True" "IsEmissive" = "true"  }
		Cull Back
		Blend SrcAlpha OneMinusSrcAlpha
		CGINCLUDE
		#include "UnityShaderVariables.cginc"
		#include "UnityPBSLighting.cginc"
		#include "Lighting.cginc"
		#pragma target 3.0
		struct Input
		{
			float2 uv_texcoord;
			float3 worldPos;
			float3 worldNormal;
		};

		uniform sampler2D _Texture0;
		uniform float Float4;
		uniform sampler2D _TextureSample1;
		uniform sampler2D _TextureSample2;
		uniform sampler2D _TextureSample4;
		uniform fixed4 _Color;

		void surf( Input i , inout SurfaceOutputStandard o )
		{
			float4 color84 = IsGammaSpace() ? float4(0.002487027,0.1630622,0.3382353,0) : float4(0.0001924943,0.02273982,0.09361818,0);
			float2 temp_output_113_0 = ( i.uv_texcoord * 1.2 );
			float2 panner1 = ( 1.0 * _Time.y * float2( 0.01,0 ) + temp_output_113_0);
			float2 panner2 = ( 1.0 * _Time.y * float2( 0,0.01 ) + temp_output_113_0);
			float cos77 = cos( 0.03 * _Time.y );
			float sin77 = sin( 0.03 * _Time.y );
			float2 rotator77 = mul( temp_output_113_0 - float2( 0,0 ) , float2x2( cos77 , -sin77 , sin77 , cos77 )) + float2( 0,0 );
			float temp_output_79_0 = ( ( tex2D( _Texture0, panner1 ).r + tex2D( _Texture0, panner2 ).r ) * tex2D( _Texture0, rotator77 ).r );
			float ifLocalVar80 = 0;
			if( temp_output_79_0 <= 0.02 )
				ifLocalVar80 = Float4;
			else
				ifLocalVar80 = temp_output_79_0;
			float4 color90 = IsGammaSpace() ? float4(0,0.1306599,0.2720588,0) : float4(0,0.01545672,0.0601584,0);
			float4 temp_output_91_0 = ( ( ( color84 * 2.0 ) * ifLocalVar80 ) + ( ( 1.0 - ifLocalVar80 ) * color90 ) );
			o.Albedo = temp_output_91_0.rgb;
			float3 ase_worldPos = i.worldPos;
			float3 ase_worldViewDir = normalize( UnityWorldSpaceViewDir( ase_worldPos ) );
			float3 ase_worldNormal = i.worldNormal;
			float fresnelNdotV97 = dot( ase_worldNormal, ase_worldViewDir );
			float fresnelNode97 = ( 0.0 + 1.0 * pow( 1.0 - fresnelNdotV97, 1.5 ) );
			float2 temp_output_107_0 = ( i.uv_texcoord * 3.0 );
			float2 panner33 = ( 1.0 * _Time.y * float2( 0.04,0 ) + temp_output_107_0);
			float2 panner37 = ( 1.0 * _Time.y * float2( 0,0.04 ) + temp_output_107_0);
			float cos103 = cos( 0.03 * _Time.y );
			float sin103 = sin( 0.03 * _Time.y );
			float2 rotator103 = mul( temp_output_107_0 - float2( 0,0 ) , float2x2( cos103 , -sin103 , sin103 , cos103 )) + float2( 0,0 );
			float temp_output_105_0 = ( ( tex2D( _TextureSample1, panner33 ).r + tex2D( _TextureSample2, panner37 ).r ) * tex2D( _TextureSample4, rotator103 ).r );
			float ifLocalVar92 = 0;
			if( temp_output_105_0 <= 0.35 )
				ifLocalVar92 = 0.0;
			else
				ifLocalVar92 = temp_output_105_0;
			float4 color99 = IsGammaSpace() ? float4(0.2216981,0.7344617,1,0) : float4(0.04027253,0.4986419,1,0);
			o.Emission = ( temp_output_91_0 + ( ( fresnelNode97 + ifLocalVar92 ) * ( color99 * 2.0 ) ) ).rgb;
			o.Alpha = _Color.a;
		}

		ENDCG
		CGPROGRAM
		#pragma surface surf Standard keepalpha fullforwardshadows

		ENDCG

		Pass
		{
			Name "ShadowCaster"
			Tags{ "LightMode" = "ShadowCaster" }
			ZWrite Off
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma target 3.0
			#pragma multi_compile_shadowcaster
			#pragma multi_compile UNITY_PASS_SHADOWCASTER
			#pragma skip_variants FOG_LINEAR FOG_EXP FOG_EXP2
			#include "HLSLSupport.cginc"
			#if ( SHADER_API_D3D11 || SHADER_API_GLCORE || SHADER_API_GLES3 || SHADER_API_METAL || SHADER_API_VULKAN )
				#define CAN_SKIP_VPOS
			#endif
			#include "UnityCG.cginc"
			#include "Lighting.cginc"
			#include "UnityPBSLighting.cginc"
			struct v2f
			{
				V2F_SHADOW_CASTER;
				float2 customPack1 : TEXCOORD1;
				float3 worldPos : TEXCOORD2;
				float3 worldNormal : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};
			v2f vert( appdata_full v )
			{
				v2f o;
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_INITIALIZE_OUTPUT( v2f, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				Input customInputData;
				float3 worldPos = mul( unity_ObjectToWorld, v.vertex ).xyz;
				half3 worldNormal = UnityObjectToWorldNormal( v.normal );
				o.worldNormal = worldNormal;
				o.customPack1.xy = customInputData.uv_texcoord;
				o.customPack1.xy = v.texcoord;
				o.worldPos = worldPos;
				TRANSFER_SHADOW_CASTER_NORMALOFFSET( o )
				return o;
			}
			half4 frag( v2f IN
			#if !defined( CAN_SKIP_VPOS )
			, UNITY_VPOS_TYPE vpos : VPOS
			#endif
			) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				Input surfIN;
				UNITY_INITIALIZE_OUTPUT( Input, surfIN );
				surfIN.uv_texcoord = IN.customPack1.xy;
				float3 worldPos = IN.worldPos;
				half3 worldViewDir = normalize( UnityWorldSpaceViewDir( worldPos ) );
				surfIN.worldPos = worldPos;
				surfIN.worldNormal = IN.worldNormal;
				SurfaceOutputStandard o;
				UNITY_INITIALIZE_OUTPUT( SurfaceOutputStandard, o )
				surf( surfIN, o );
				#if defined( CAN_SKIP_VPOS )
				float2 vpos = IN.pos;
				#endif
				SHADOW_CASTER_FRAGMENT( IN )
			}
			ENDCG
		}
	}
	Fallback "Diffuse"
}
