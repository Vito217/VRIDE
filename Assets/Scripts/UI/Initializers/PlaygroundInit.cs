using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class PlaygroundInit : InitializeBehaviour
{
    public Image this_panel;
    public PlaygroundTextEditor text_editor;

    void Start()
    {
        this_panel.color = Random.ColorHSV();
    }

    public override void Initialize(Vector3 init_pos, Vector3 final_pos, Vector3 forward, GameObject player)
    {
        base.Initialize(init_pos, final_pos, forward, player);
        text_editor.player = player;
    }
}
