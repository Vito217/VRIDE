using System.Collections.Generic;
using System.Linq;
using UnityEditor.IMGUI.Controls;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Multi-column <see cref="TreeView"/> that shows Interactables.
    /// </summary>
    class XRInteractablesTreeView : TreeView
    {
        public static XRInteractablesTreeView Create(XRInteractionManager interactionManager, ref TreeViewState treeState, ref MultiColumnHeaderState headerState)
        {
            if (treeState == null)
                treeState = new TreeViewState();

            var newHeaderState = CreateHeaderState();
            if (headerState != null)
                MultiColumnHeaderState.OverwriteSerializedFields(headerState, newHeaderState);
            headerState = newHeaderState;

            var header = new MultiColumnHeader(headerState);
            return new XRInteractablesTreeView(interactionManager, treeState, header);
        }

        const float k_RowHeight = 20f;

        class Item : TreeViewItem
        {
            public XRBaseInteractable interactable;
        }

        enum ColumnId
        {
            Name,
            Type,
            LayerMask,
            Colliders,
            Hover,
            Select,

            COUNT
        }

        readonly XRInteractionManager m_InteractionManager;

        static MultiColumnHeaderState CreateHeaderState()
        {
            var columns = new MultiColumnHeaderState.Column[(int)ColumnId.COUNT];

            columns[(int)ColumnId.Name]      = new MultiColumnHeaderState.Column { width = 180f, minWidth = 80f, headerContent = new GUIContent("Name") };
            columns[(int)ColumnId.Type]      = new MultiColumnHeaderState.Column { width = 120f, minWidth = 80f, headerContent = new GUIContent("Type") };
            columns[(int)ColumnId.LayerMask] = new MultiColumnHeaderState.Column { width = 120f, minWidth = 80f, headerContent = new GUIContent("Layer Mask") };
            columns[(int)ColumnId.Colliders] = new MultiColumnHeaderState.Column { width = 120f, minWidth = 80f, headerContent = new GUIContent("Colliders") };
            columns[(int)ColumnId.Hover]     = new MultiColumnHeaderState.Column { width = 80f, minWidth = 80f, headerContent = new GUIContent("Hover") };
            columns[(int)ColumnId.Select]    = new MultiColumnHeaderState.Column { width = 80f, minWidth = 80f, headerContent = new GUIContent("Select") };

            return new MultiColumnHeaderState(columns);
        }

        XRInteractablesTreeView(XRInteractionManager manager, TreeViewState state, MultiColumnHeader header)
            : base(state, header)
        {
            m_InteractionManager = manager;
            showBorder = false;
            rowHeight = k_RowHeight;
            Reload();
        }

        protected override TreeViewItem BuildRoot()
        {
            // Wrap root control in invisible item required by TreeView.
            return new Item
            {
                id = 0,
                children = new List<TreeViewItem> { BuildInteractableTree() },
                depth = -1,
            };
        }

        TreeViewItem BuildInteractableTree()
        {
            var rootTreeItem = new Item
            {
                id = m_InteractionManager != null ? m_InteractionManager.GetInstanceID() : 1,
                displayName = m_InteractionManager != null ? m_InteractionManager.name : "-",
                depth = 0,
            };

            // Build children.
            if (m_InteractionManager != null && m_InteractionManager.interactables.Count > 0)
            {
                var children = new List<TreeViewItem>();
                foreach (var interactable in m_InteractionManager.interactables)
                {
                    var childItem = new Item
                    {
                        id = interactable.GetInstanceID(),
                        displayName = interactable.name,
                        interactable = interactable,
                        depth = 1,
                        parent = rootTreeItem,
                    };
                    children.Add(childItem);
                }

                // Sort children by name.
                children.Sort((a, b) => string.Compare(a.displayName, b.displayName));
                rootTreeItem.children = children;
            }

            return rootTreeItem;
        }

        protected override void RowGUI(RowGUIArgs args)
        {
            var item = (Item)args.item;

            var columnCount = args.GetNumVisibleColumns();
            for (var i = 0; i < columnCount; ++i)
            {
                ColumnGUI(args.GetCellRect(i), item, args.GetColumn(i), ref args);
            }
        }

        void ColumnGUI(Rect cellRect, Item item, int column, ref RowGUIArgs args)
        {
            CenterRectUsingSingleLineHeight(ref cellRect);

            if (column == (int)ColumnId.Name)
            {
                args.rowRect = cellRect;
                base.RowGUI(args);
            }

            if (item.interactable != null)
            {
                switch (column)
                {
                    case (int)ColumnId.Type:
                        GUI.Label(cellRect, item.interactable.GetType().Name);
                        break;
                    case (int)ColumnId.LayerMask:
                        GUI.Label(cellRect, item.interactable.interactionLayerMask.value.ToString());
                        break;
                    case (int)ColumnId.Colliders:
                        var colliderNames = item.interactable.colliders.Select(x => x.gameObject.name).ToList();
                        GUI.Label(cellRect, string.Join(",", colliderNames.ToArray()));
                        break;
                    case (int)ColumnId.Hover:
                        GUI.Label(cellRect, item.interactable.isHovered ? "True" : "False");
                        break;
                    case (int)ColumnId.Select:
                        GUI.Label(cellRect, item.interactable.isSelected ? "True" : "False");
                        break;
                }
            }
        }
    }
}
