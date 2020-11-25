using System.Collections.Generic;
using UnityEditor.IMGUI.Controls;

namespace UnityEngine.XR.Interaction.Toolkit
{
    /// <summary>
    /// Multi-column <see cref="TreeView"/> that shows Interactors.
    /// </summary>
    class XRInteractorsTreeView : TreeView
    {
        public static XRInteractorsTreeView Create(XRInteractionManager interactionManager, ref TreeViewState treeState, ref MultiColumnHeaderState headerState)
        {
            if (treeState == null)
                treeState = new TreeViewState();

            var newHeaderState = CreateHeaderState();
            if (headerState != null)
                MultiColumnHeaderState.OverwriteSerializedFields(headerState, newHeaderState);
            headerState = newHeaderState;

            var header = new MultiColumnHeader(headerState);
            return new XRInteractorsTreeView(interactionManager, treeState, header);
        }

        const float k_RowHeight = 20f;

        class Item : TreeViewItem
        {
            public XRBaseInteractor interactor;
        }

        enum ColumnId
        {
            Name,
            Type,
            HoverActive,
            SelectActive,
            HoverInteractable,
            SelectInteractable,

            COUNT
        }

        readonly XRInteractionManager m_InteractionManager;

        readonly List<XRBaseInteractable> m_HoverTargetList = new List<XRBaseInteractable>();

        static MultiColumnHeaderState CreateHeaderState()
        {
            var columns = new MultiColumnHeaderState.Column[(int)ColumnId.COUNT];

            columns[(int)ColumnId.Name] = new MultiColumnHeaderState.Column
            {
                width = 180f,
                minWidth = 60f,
                headerContent = new GUIContent("Name"),
            };
            columns[(int)ColumnId.Type] = new MultiColumnHeaderState.Column
            {
                width = 120f,
                minWidth = 60f,
                headerContent = new GUIContent("Type"),
            };
            columns[(int)ColumnId.HoverActive] = new MultiColumnHeaderState.Column
            {
                width = 120f,
                headerContent = new GUIContent("Hover Active"),
            };
            columns[(int)ColumnId.SelectActive] = new MultiColumnHeaderState.Column
            {
                width = 120f,
                headerContent = new GUIContent("Select Active"),
            };
            columns[(int)ColumnId.HoverInteractable] = new MultiColumnHeaderState.Column
            {
                width = 140f,
                headerContent = new GUIContent("Hover Interactable"),
            };
            columns[(int)ColumnId.SelectInteractable] = new MultiColumnHeaderState.Column
            {
                width = 140f,
                headerContent = new GUIContent("Select Interactable"),
            };

            return new MultiColumnHeaderState(columns);
        }

        XRInteractorsTreeView(XRInteractionManager manager, TreeViewState state, MultiColumnHeader header)
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
            if (m_InteractionManager != null && m_InteractionManager.interactors.Count > 0)
            {
                var children = new List<TreeViewItem>();
                foreach (var interactor in m_InteractionManager.interactors)
                {
                    var childItem = new Item
                    {
                        id = interactor.GetInstanceID(),
                        displayName = interactor.name,
                        interactor = interactor,
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

            if (item.interactor != null)
            {
                switch (column)
                {
                    case (int)ColumnId.Type:
                        GUI.Label(cellRect, item.interactor.GetType().Name);
                        break;
                    case (int)ColumnId.HoverActive:
                        GUI.Label(cellRect, item.interactor.isHoverActive.ToString());
                        break;
                    case (int)ColumnId.SelectActive:
                        GUI.Label(cellRect, item.interactor.isSelectActive.ToString());
                        break;
                    case (int)ColumnId.HoverInteractable:
                        item.interactor.GetHoverTargets(m_HoverTargetList);
                        if (m_HoverTargetList.Count > 0)
                        {
                            foreach (var target in m_HoverTargetList)
                                GUI.Label(cellRect, target.name);
                        }
                        break;
                    case (int)ColumnId.SelectInteractable:
                        if (item.interactor.selectTarget != null)
                            GUI.Label(cellRect, item.interactor.selectTarget.name);
                        break;
                }
            }
        }
    }
}
