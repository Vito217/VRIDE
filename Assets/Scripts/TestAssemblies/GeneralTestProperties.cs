using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace TestProperties
{
    public class GeneralTestProperties : MonoBehaviour
    {
        private bool isHovered;

        public void OnPointerEnter()
        {
            isHovered = true;
        }

        public void OnPointerExit()
        {
            isHovered = false;
        }

        public bool IsHovered()
        {
            return isHovered;
        }
    }
}
