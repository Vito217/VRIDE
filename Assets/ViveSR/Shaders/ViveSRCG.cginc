#ifndef VIVESR_CG_INCLUDE
#define VIVESR_CG_INCLUDE

float PointOverPlane(float4 plane, float3 worldPos) 
{
	return ( dot(plane.xyz, worldPos) + plane.w );
}	

// ---------------
// V/G/F shader struct declare
// ---------------
#ifdef CLIP_PLANE
	#define WORLD_POS_FORCLIP(idx) float3 worldPos:TEXCOORD##idx;
#else
	#define WORLD_POS_FORCLIP(idx)
#endif

// ---------------
// surface shader struct declare
// ---------------
#ifdef CLIP_PLANE
	#define WORLD_POS_FORCLIP_SURF float3 worldPos;
#else
	#define WORLD_POS_FORCLIP_SURF
#endif

// ----------
// Variables
// ----------
#ifdef CLIP_PLANE
	#define DECLARE_CLIP_PLANE_VARIABLE float4 _ClipPlane;
#else
	#define DECLARE_CLIP_PLANE_VARIABLE
#endif


// ----------
// Function Macro
// ----------
#ifdef CLIP_PLANE
	#define COMPUTE_CLIP_WORLD_POS(vertOut, vertInPos) vertOut.worldPos = mul(unity_ObjectToWorld, vertInPos).xyz;
#else
	#define COMPUTE_CLIP_WORLD_POS(vertOut, vertInPos)
#endif

#ifdef CLIP_PLANE
	#define ASSIGN_CLIP_WORLD_POS_GEOSHADER(geoIn, geoOut) geoOut.worldPos = geoIn.worldPos;
#else
	#define ASSIGN_CLIP_WORLD_POS_GEOSHADER(geoIn, geoOut)
#endif

#ifdef CLIP_PLANE
	#define CLIP_PLANE_TEST(fragIn) clip( PointOverPlane(_ClipPlane, fragIn.worldPos) );
#else
	#define CLIP_PLANE_TEST(fragIn)
#endif

#endif