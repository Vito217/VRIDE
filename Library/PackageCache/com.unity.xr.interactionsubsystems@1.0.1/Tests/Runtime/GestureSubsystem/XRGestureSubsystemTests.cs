using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine.XR.InteractionSubsystems;

namespace UnityEngine.XR.InteractionSubsystems.Tests
{
    public class XRGestureSubsystemTest : XRGestureSubsystem
    {
        protected override Provider CreateProvider()
        {
            return new XRGestureSubsystemTestProvider();
        }

        class XRGestureSubsystemTestProvider : Provider
        {
            public XRGestureSubsystemTestProvider()
            {
            }

            public override void Start()
            {
            }

            public override void Stop()
            {
            }

            public override void Update()
            {
            }

            public override void Destroy()
            {
                base.Destroy();
            }
        }
    }

    [TestFixture]
    public class XRGestureSubsystemTestFixture
    {
         [Test]
        public void RunningStateTests()
        {
            XRGestureSubsystem subsystem = new XRGestureSubsystemTest();
            
            // Initial state is not running
            Assert.That(subsystem.running == false);

            // After start subsystem is running
            subsystem.Start();
            Assert.That(subsystem.running == true);

            // After start subsystem is running
            subsystem.Stop();
            Assert.That(subsystem.running == false);
        }
    }
}