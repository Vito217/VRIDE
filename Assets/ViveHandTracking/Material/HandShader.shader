Shader "ViveHandTracking/Hand" {
	Properties {
		_Color ("Color", Color) = (1,1,1,1)
	}
	SubShader {
		Tags { "RenderType"="Opaque" "Queue" = "Transparent" "IgnoreProjector" = "True"}
		LOD 200
		Blend SrcAlpha OneMinusSrcAlpha

		CGPROGRAM
		#pragma surface surf Standard fullforwardshadows keepalpha
		#pragma target 3.0

		struct Input {
			fixed4 color : COLOR;
		};

		fixed4 _Color;

		void surf (Input IN, inout SurfaceOutputStandard o) {
			o.Albedo = _Color.rgb;
			o.Alpha = _Color.a;
		}
		ENDCG
	}
	FallBack "Diffuse"
}
