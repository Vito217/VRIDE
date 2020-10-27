using UnityEngine;
using UnityEditor;
using System.Collections;
using System.Collections.Generic;

namespace Vive.Plugin.SR
{
    public class ReconstructedAssetImporter : AssetPostprocessor
    {
        void OnPreprocessModel()
        {
            // assetPath;
            // assetImporter;
            if (!assetPath.Contains("/Recons3DAsset/"))
                return;

            ModelImporter importer = assetImporter as ModelImporter;
            importer.meshCompression = ModelImporterMeshCompression.Off;
            importer.optimizeMesh = false;
            importer.importBlendShapes = false;
            importer.isReadable = false;

            if (assetPath.Contains("/VertexColor/"))        // not used
                importer.importMaterials = false;
            else
                importer.importMaterials = true;

            //importer.importNormals = ModelImporterNormals.None;
            importer.importNormals = ModelImporterNormals.Calculate;
            importer.normalSmoothingAngle = 60;
            importer.importTangents = ModelImporterTangents.None;

            if (assetPath.Contains("_cld.obj"))
                importer.importMaterials = false;
            else
                importer.materialSearch = ModelImporterMaterialSearch.Local;
        }

        void OnPreprocessTexture()
        {
            // assetPath;
            // assetImporter;
            if (!assetPath.Contains("/Recons3DAsset/"))
                return;

            TextureImporter importer = assetImporter as TextureImporter;
            importer.mipmapEnabled = false;
        }

        void OnPostprocessModel(GameObject curGO)
        {
            if (!assetPath.Contains("/Recons3DAsset/"))
                return;

            if (ViveSR_RigidReconstructionColliderManager.ProcessDataAndGenColliderInfo(curGO) == true)
            {
                ViveSR_RigidReconstructionColliderManager collider_manager = curGO.AddComponent<ViveSR_RigidReconstructionColliderManager>();
                collider_manager.OrganizeHierarchy();
            }
            else
            {
                MeshRenderer[] rnds = curGO.GetComponentsInChildren<MeshRenderer>(true);
                int len = rnds.Length;
                for (int i = 0; i < len; ++i)
                {
                    rnds[i].shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;
                    rnds[i].sharedMaterial.shader = Shader.Find("ViveSR/Unlit, Textured, Shadowed, Stencil");
                    rnds[i].gameObject.layer = LayerMask.NameToLayer(ViveSR_RigidReconstruction.ReconsMeshLayerName);
                    //rnds[i].GetComponent<MeshFilter>().sharedMesh.RecalculateNormals();
                }
            }
        }
    }
}
