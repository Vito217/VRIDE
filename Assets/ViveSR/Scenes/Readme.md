1.Open File/Build Settings.
2.Add Assets/ViveSR/Scenes/ViveSR_Sample and Assets/ViveSR/Scenes/ViveSR_Sample_duplicate in Scenes In Build.

How to switch ViveSR_Sample scene to ViveSR_Sample_duplicate scene?
Open ViveSR_Sample scene in the Assets/ViveSR/Scenes.
Choose GameViewDebugTool gameobject in the ViveSR_Sample scene.
Check Switch_Sample_Duplicate_Scene checkbox in the ViveSR_LoadSampleDuplicateScene.cs.
The scene will switch to ViveSR_Sample_duplicate.

How to switch ViveSR_Sample_duplicate scene to ViveSR_Sample scene?
Open ViveSR_Sample_duplicate scene in the Assets/ViveSR/Scenes.
Choose GameViewDebugTool gameobject in the ViveSR_Sample_duplicate scene.
Check Switch_Sample_Scene checkbox in the ViveSR_LoadSampleScene.cs.
The scene will switch to ViveSR_Sample.