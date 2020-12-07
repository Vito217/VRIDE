using UnityEditor;
using UnityEngine;
using System;
using System.Reflection;
using System.Collections.Generic;

class ComponentCopier : EditorWindow {
    
	Vector2 scrollPos;
	static bool[] foldout;
	static bool[] selectAll;
	static Component[] components;
	static Type[] t;
	static bool[] enabled;
	static List<FieldInfo>[] fields;
	static List<PropertyInfo>[] properties;
	static object[][] fieldVals;
	static object[][] propertyVals;

	static bool[] selectedComponents;
	static bool[][] selectedFields;
	static bool[][] selectedProperties;
	
	static Dictionary<Type, int> copyTypeCount;
	static Dictionary<Type, int> pasteTypeCount;
	static List<FieldInfo> fieldsToRemove;
	static List<PropertyInfo> propertiesToRemove;
	
	static int typeIndex;

    static void SelectDeselectAll(int componentIndex)
	{
		if(fields[componentIndex].Count > 0)
		{
			for(int j = 0; j < fields[componentIndex].Count; j++)
				selectedFields[componentIndex][j] = selectAll[componentIndex];
		}

		if(properties[componentIndex].Count > 0)
		{
         	for(int j = 0; j < properties[componentIndex].Count; j++)
				selectedProperties[componentIndex][j] = selectAll[componentIndex];
		}
	}
	
	void CopyData()
	{
		for(int i = 0; i < selectedComponents.Length; i++)
		{
			if(selectedComponents[i])
			{	
				if(copyTypeCount.ContainsKey(t[i]))
					copyTypeCount[t[i]] = copyTypeCount[t[i]] + 1;
				else
					copyTypeCount.Add(t[i], 1);
				
				for(int j = 0; j < selectedFields[i].Length; j++)
				{
					if(selectedFields[i][j])
						fieldVals[i][j] = fields[i][j].GetValue(components[i]);
				}
				
				for(int j = 0; j < selectedProperties[i].Length; j++)
				{
					if(selectedProperties[i][j])
					{
						if(properties[i][j].CanRead && properties[i][j].GetIndexParameters().Length == 0)
							propertyVals[i][j] = properties[i][j].GetValue(components[i], null); 
						else
							Debug.LogWarning(properties[i][j].Name + " could not be copied.");
					}
				}
			}
		}
	}
}
