using System;
using System.Collections;
using System.IO;
using System.Threading;
using NUnit.Framework;
using Unity.Android.Logcat;
using UnityEngine;
using UnityEngine.TestTools;

public class AndroidLogcatDispatcherTests
{
    internal struct TaskInputData : IAndroidLogcatTaskInput
    {
        internal int mainThreadId;
    }

    internal struct TaskResultData : IAndroidLogcatTaskResult
    {
        internal int workerThreadId;
        internal int mainThreadId;
    }

    IAndroidLogcatTaskResult PerformAsycnTask(IAndroidLogcatTaskInput input)
    {
        var data = (TaskInputData)input;
        return new TaskResultData() { mainThreadId = data.mainThreadId, workerThreadId = Thread.CurrentThread.ManagedThreadId };
    }

    void IntegrateTask(IAndroidLogcatTaskResult result)
    {
        Assert.Fail();
    }

    [UnityTest]
    public IEnumerator SimpleDispatchingWorks([Values(true, false)] bool synchronousTask)
    {
        var runtime = new AndroidLogcatTestRuntime();
        runtime.Initialize();

        bool taskFinished = false;
        TaskResultData result = new TaskResultData() { mainThreadId = 0, workerThreadId = 0 };
        TaskInputData data = new TaskInputData() { mainThreadId = Thread.CurrentThread.ManagedThreadId };

        runtime.Dispatcher.Schedule(data, PerformAsycnTask, (IAndroidLogcatTaskResult r) =>
        {
            taskFinished = true;
            result = (TaskResultData)r;
        }, synchronousTask);

        var startTime = Time.realtimeSinceStartup;
        const float kMaxWaitTime = 1.0f;
        do
        {
            runtime.OnUpdate();
            yield return null;
        }
        while (!taskFinished && Time.realtimeSinceStartup - startTime < kMaxWaitTime);

        Assert.IsTrue(taskFinished, string.Format("Timeout while waiting for task to be finished, waited {0} seconds", Time.realtimeSinceStartup - startTime));

        if (synchronousTask)
        {
            Assert.IsTrue(result.mainThreadId == result.workerThreadId && result.mainThreadId > 0 && result.workerThreadId > 0,
                string.Format("Expected main ({0}) and worker thread ({1}) to match and be bigger than 0", result.mainThreadId, result.workerThreadId));
        }
        else
        {
            Assert.IsTrue(result.mainThreadId != result.workerThreadId && result.mainThreadId > 0 && result.workerThreadId > 0,
                string.Format("Expected main ({0}) and worker thread ({1}) to not match and be bigger than 0", result.mainThreadId, result.workerThreadId));
        }

        runtime.Shutdown();
    }

    [UnityTest]
    public IEnumerator SchedulingHappensInCorrectOrder([Values(true, false)] bool synchronousTask)
    {
        var runtime = new AndroidLogcatTestRuntime();
        runtime.Initialize();

        const int kMaxCount = 20;
        var itemsReceived = new System.Collections.Generic.List<int>();

        for (int i = 0; i < kMaxCount; i++)
        {
            runtime.Dispatcher.Schedule(
                new TaskInputData() { mainThreadId = i },
                PerformAsycnTask,
                (IAndroidLogcatTaskResult r) =>
                {
                    // Keep it for debugging
                    // Debug.Log("Received " + ((TaskResultData)r).mainThreadId);
                    itemsReceived.Add(((TaskResultData)r).mainThreadId);
                }, synchronousTask);
        }


        var startTime = Time.realtimeSinceStartup;
        const float kMaxWaitTime = 4.0f;
        do
        {
            runtime.OnUpdate();
            yield return null;
        }
        while (itemsReceived.Count < kMaxCount && Time.realtimeSinceStartup - startTime < kMaxWaitTime);

        Assert.AreEqual(kMaxCount, itemsReceived.Count,
            string.Format("Timeout while waiting for task to be finished, waited {0} seconds. Received {1} items, expected {2} items", Time.realtimeSinceStartup - startTime,
                itemsReceived.Count, kMaxCount));

        for (int i = 0; i < kMaxCount; i++)
        {
            Assert.AreEqual(i, itemsReceived[i]);
        }

        runtime.Shutdown();
    }

    IAndroidLogcatTaskResult PerformAsycnTaskThrowException(IAndroidLogcatTaskInput input)
    {
        throw new Exception("Purposely throwing");
    }

    [UnityTest]
    public IEnumerator DispatcherCanSuriveExceptions([Values(true, false)] bool synchronousTask)
    {
        var runtime = new AndroidLogcatTestRuntime();
        runtime.Initialize();

        Assert.AreEqual(0, runtime.Dispatcher.AsyncOperationsExecuted);

        runtime.Dispatcher.Schedule(
            new TaskInputData(),
            PerformAsycnTaskThrowException,
            (IAndroidLogcatTaskResult r) =>
            {
                // Shouldn't be called, since there was exception in async operation
                Assert.Fail();
            }, synchronousTask);

        do
        {
            runtime.OnUpdate();
            yield return null;
        }
        while (runtime.Dispatcher.AsyncOperationsExecuted < 1);

        // Check if we can still schedule stuff, even though previous operation threw exception
        int iWasExecuted = 0;
        runtime.Dispatcher.Schedule(
            new TaskInputData(),
            PerformAsycnTask,
            (IAndroidLogcatTaskResult r) =>
            {
                iWasExecuted = 256;
            }, synchronousTask);

        const float kMaxWaitTime = 4.0f;
        var startTime = Time.realtimeSinceStartup;
        do
        {
            runtime.OnUpdate();
            yield return null;
        }
        while ((runtime.Dispatcher.AsyncOperationsExecuted < 2 || iWasExecuted == 0) && Time.realtimeSinceStartup - startTime < kMaxWaitTime);

        Assert.AreEqual(2, runtime.Dispatcher.AsyncOperationsExecuted);
        Assert.AreEqual(256, iWasExecuted);

        runtime.Shutdown();
    }
}
