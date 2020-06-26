using System.Collections;
using System.Collections.Generic;
using UnityEngine.UI;
using UnityEngine;

public class BrowserInit : InitializeBehaviour
{
    public BrowserTextEditor text_editor;
    public Image packages_panel;
    public Image classes_panel;
    public Image methods_panel;
    public Image editor_panel;

    // Start is called before the first frame update
    void Start()
    {
        Color new_color = Random.ColorHSV();
        classes_panel.color = new_color;
        methods_panel.color = new_color;
        editor_panel.color = new_color;
        packages_panel.color = new_color;
    }

    public override void Initialize(Vector3 init_pos, Vector3 final_pos, Vector3 forward, GameObject player)
    {
        base.Initialize(init_pos, final_pos, forward, player);
        text_editor.player = player;
    }
}
