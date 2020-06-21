using NUnit.Framework;
using System.Threading;
using UnityEditor.Build.Pipeline.Interfaces;
using UnityEditor.Build.Pipeline.Utilities;

namespace UnityEditor.Build.Pipeline.Tests
{
    public class BuildLogTests
    {
        [Test]
        public void WhenBeginAndEndScope_DurationIsCorrect()
        {
            BuildLog log = new BuildLog();
            using (log.ScopedStep(LogLevel.Info, "TestStep"))
                Thread.Sleep(5);
            Assert.AreEqual("TestStep", log.Root.Children[0].Name);
            Assert.Greater(log.Root.Children[0].DurationMS, 4);
        }

        [Test]
        public void WhenAddMessage_EntryIsCreated()
        {
            BuildLog log = new BuildLog();
            using (log.ScopedStep(LogLevel.Info, "TestStep"))
                log.AddEntry(LogLevel.Info, "TestEntry");
            Assert.AreEqual("TestEntry", log.Root.Children[0].Entries[0].Message);
        }

        [Test]
        public void WhenScopeIsThreaded_AndThreadAddsNode_NodeEnteredInThreadedScope()
        {
            BuildLog log = new BuildLog();
            using (log.ScopedStep(LogLevel.Info, "TestStep", true))
            {
                var t = new Thread(() =>
                {
                    log.AddEntry(LogLevel.Info, "ThreadedMsg1");
                    using (log.ScopedStep(LogLevel.Info, "ThreadedStep"))
                    {
                        log.AddEntry(LogLevel.Info, "ThreadedMsg2");
                    }
                });
                t.Start();
                t.Join();
            }
            Assert.AreEqual("ThreadedMsg1", log.Root.Children[0].Entries[0].Message);
            Assert.AreNotEqual(Thread.CurrentThread.ManagedThreadId, log.Root.Children[0].Entries[0].ThreadId);
            Assert.AreEqual("ThreadedStep", log.Root.Children[0].Children[0].Name);
            Assert.AreNotEqual(Thread.CurrentThread.ManagedThreadId, log.Root.Children[0].Children[0].ThreadId);
            Assert.AreEqual("ThreadedMsg2", log.Root.Children[0].Children[0].Entries[0].Message);
            Assert.AreNotEqual(Thread.CurrentThread.ManagedThreadId, log.Root.Children[0].Children[0].Entries[0].ThreadId);
        }

        [Test]
        public void WhenBeginAndEndScopeOnThread_StartAndEndTimeAreWithinMainThreadScope()
        {
            BuildLog log = new BuildLog();
            using (log.ScopedStep(LogLevel.Info, "TestStep", true))
            {
                var t = new Thread(() =>
                {
                    Thread.Sleep(1);
                    log.AddEntry(LogLevel.Info, "ThreadedMsg1");
                    Thread.Sleep(1);
                    using (log.ScopedStep(LogLevel.Info, "ThreadedStep"))
                        Thread.Sleep(2);
                    Thread.Sleep(1);
                });
                t.Start();
                t.Join();
            }

            double testStepStart = log.Root.Children[0].StartTime;
            double threadedMessageStart = log.Root.Children[0].Entries[0].Time;
            double threadedScopeStart = log.Root.Children[0].Children[0].StartTime;
            double threadedScopeEnd = threadedScopeStart + log.Root.Children[0].Children[0].DurationMS;
            double testStepEnd = testStepStart + log.Root.Children[0].DurationMS;

            Assert.Less(testStepStart, threadedMessageStart);
            Assert.Less(threadedMessageStart, threadedScopeStart);
            Assert.Less(threadedScopeStart, threadedScopeEnd);
            Assert.Less(threadedScopeEnd, testStepEnd);
        }
    }
}
