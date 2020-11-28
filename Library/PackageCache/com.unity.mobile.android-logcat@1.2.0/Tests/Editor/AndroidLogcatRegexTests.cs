using System.Collections.Generic;
using NUnit.Framework;
using System.Text.RegularExpressions;
using Unity.Android.Logcat;

class AndroidLogcatRegexTests
{
    struct LogcatMessage
    {
        string fullMessage;
        string expectedTag;

        internal string FullMessage { get { return fullMessage; } }
        internal string ExpectedTag { get { return expectedTag; } }

        internal LogcatMessage(string _fullMessage, string _expectedTag)
        {
            fullMessage = _fullMessage;
            expectedTag = _expectedTag;
        }
    }

    // Messages produced via adb logcat -s -v threadtime *:V
    private LogcatMessage[] kLogMessagesWithThreadTimeFormat = new[]
    {
        new LogcatMessage("10-25 14:27:29.803  1277 10543 E ctxmgr  : [AccountAclCallback]Failed Acl fetch: network status=-1", "ctxmgr"),
        new LogcatMessage("10-25 14:27:43.785  2255  2642 I chromium: [2255:2642:INFO: mdns_app_filter.cc(2202)] MdnsAppFilter: responses sent in 32 seconds: 13", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: [2255:2255:INFO:metrics_recorder.cc(89)] Metrics stat: total=8", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Request.AppId.In=3", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Request.In=13", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Request.Namespace.In=11", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.ResponderPing=1", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Response.AppId.Out=3", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Response.Error.NamespaceNotSupported=11", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Response.Out=13", "chromium"),
        new LogcatMessage("10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.SocketPing=2", "chromium"),
        new LogcatMessage("10-25 14:28:10.312  2255  2642 I chromium: [2255:2642:INFO:mdns_cast_service.cc(755)] Recent mDNS app subtypes: [supported:'805741C9',] [unsupported:]", "chromium"),
        new LogcatMessage("10-25 14:28:16.994  2255  2642 I chromium: [2255:2642:INFO:mdns_app_filter.cc(2202)] MdnsAppFilter: responses sent in 33 seconds: 8", "chromium"),
        new LogcatMessage("01-18 14:14:56.254  3777  6386 I BarTender:BatteryStatsDumper: writing to daily db completed", "BarTender:BatteryStatsDumper"),
        new LogcatMessage("01-19 22:21:51.151  1461  5286 D SSRM:k  : SIOP:: AP = 160, PST = 160 (W:14), CP = 18, CUR = 398, LCD = 57", "SSRM:k"),
        new LogcatMessage("01-19 14:58:16.725  3966  3966 D u       : getCurrentNetTypeId, current net type: null", "u"),
        new LogcatMessage("01-19 14:58:16.725  3966  3966 D EPDG -- SIM0 [EpdgSubScription]: getCurrentNetTypeId, current net type: null", "EPDG -- SIM0 [EpdgSubScription]"),
        new LogcatMessage("03-10 14:33:13.505 11287 11287 D Unity   : NewInput[0xFFFFFFFFEA4E9DC0]: Incoming event with sources 'Touchscreen' from android device 3, isGameController: No, unity devices: 4", "Unity"),
        new LogcatMessage("03-10 14:33:13.505 11287 11287 D Unity   :     NewInput[0xFFFFFFFFEA4E9DC0]: Touch 1454.000000 x 343.000000, touchId = 6 (0), phase = kEnded, time = 125.525207 (594002743)", "Unity")
        // Add more as needed
    };


    // Note: -v year is not available on Android 6.0 and below
    // Messages produced via adb logcat -s -v year *:V
    private LogcatMessage[] kLogMessagesWithYearFormat = new[]
    {
        new LogcatMessage("2018-10-25 14:27:29.803  1277 10543 E ctxmgr  : [AccountAclCallback]Failed Acl fetch: network status=-1", "ctxmgr"),
        new LogcatMessage("2018-10-25 14:27:43.785  2255  2642 I chromium: [2255:2642:INFO: mdns_app_filter.cc(2202)] MdnsAppFilter: responses sent in 32 seconds: 13", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: [2255:2255:INFO:metrics_recorder.cc(89)] Metrics stat: total=8", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Request.AppId.In=3", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Request.In=13", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Request.Namespace.In=11", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.ResponderPing=1", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Response.AppId.Out=3", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Response.Error.NamespaceNotSupported=11", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.Response.Out=13", "chromium"),
        new LogcatMessage("2018-10-25 14:27:56.862  2255  2255 I chromium: Cast.Discovery.Mdns.SocketPing=2", "chromium"),
        new LogcatMessage("2018-10-25 14:28:10.312  2255  2642 I chromium: [2255:2642:INFO:mdns_cast_service.cc(755)] Recent mDNS app subtypes: [supported:'805741C9',] [unsupported:]", "chromium"),
        new LogcatMessage("2018-10-25 14:28:16.994  2255  2642 I chromium: [2255:2642:INFO:mdns_app_filter.cc(2202)] MdnsAppFilter: responses sent in 33 seconds: 8", "chromium"),
        new LogcatMessage("2019-01-18 14:14:56.254  3777  6386 I BarTender:BatteryStatsDumper: writing to daily db completed", "BarTender:BatteryStatsDumper"),
        new LogcatMessage("2019-01-18 22:21:51.151  1461  5286 D SSRM:k  : SIOP:: AP = 160, PST = 160 (W:14), CP = 18, CUR = 398, LCD = 57", "SSRM:k"),
        new LogcatMessage("2019-01-18 14:58:16.725  3966  3966 D u       : getCurrentNetTypeId, current net type: null", "u"),
        new LogcatMessage("2020-02-06 12:48:19.406  2579  2813 D EPDG -- SIM0 [EpdgSubScription]: getMnoNameFromDB() hassim :true", "EPDG -- SIM0 [EpdgSubScription]"),
        // Add more as needed
    };
    [Test]
    public void LogMessageRegexMatchesForThreadTimeFormat()
    {
        foreach (var l in kLogMessagesWithThreadTimeFormat)
        {
            var result = AndroidLogcat.m_LogCatEntryThreadTimeRegex.Match(l.FullMessage);
            Assert.IsTrue(result.Success);
            var tagValue = result.Groups["tag"].Value;
            Assert.AreEqual(l.ExpectedTag, tagValue);
        }
    }

    [Test]
    public void LogMessageRegexMatchesForYearFormat()
    {
        foreach (var l in kLogMessagesWithYearFormat)
        {
            var result = AndroidLogcat.m_LogCatEntryYearRegex.Match(l.FullMessage);
            Assert.IsTrue(result.Success, l.FullMessage);
            var tagValue = result.Groups["tag"].Value;
            Assert.AreEqual(l.ExpectedTag, tagValue);
        }
    }

    [Test]
    public void WhitespacesArePreservedInMessage()
    {
        var myTestMessage = "  hello  ";
        var msgWithYearFormat = @"2018-10-25 14:27:29.803  1277 10543 E ctxmgr  : " + myTestMessage;
        var msgWithThreadTimeFormat = "10-25 14:27:29.803  1277 10543 E ctxmgr  : " + myTestMessage;

        var yearMsg = AndroidLogcat.m_LogCatEntryYearRegex.Match(msgWithYearFormat).Groups["msg"].Value;
        Assert.IsTrue(yearMsg.Equals(myTestMessage));

        var threadMsg = AndroidLogcat.m_LogCatEntryThreadTimeRegex.Match(msgWithThreadTimeFormat).Groups["msg"].Value;
        Assert.IsTrue(threadMsg.Equals(myTestMessage));
    }

    [Test]
    public void CorrectlyParsePidsWithWindowsEndlines()
    {
        CorrectlyParsePids("\r\n");
    }

    [Test]
    public void CorrectlyParsePidsWithUnixEndlines()
    {
        CorrectlyParsePids("\n");
    }

    private void CorrectlyParsePids(string separator)
    {
        var expectedPid = 2909;
        // Produced by adb shell ps
        var adbContents = string.Join(separator, new[]
        {
            "USER      PID   PPID  VSIZE  RSS   WCHAN              PC  NAME",
            "root      1     0     29876  1192  SyS_epoll_ 0000000000 S /init",
            "root      2     0     0      0       kthreadd 0000000000 S kthreadd",
            "root      3     2     0      0     smpboot_th 0000000000 S ksoftirqd/0",
            "root      4     2     0      0     worker_thr 0000000000 S kworker/0:0",
            "system    " + expectedPid + "  885   2415772 149576 SyS_epoll_ 0000000000 S com.android.settings",
            "system    " + (expectedPid + 1) + "  885   2415772 149576 SyS_epoll_ 0000000000 S com.android.settings", // This should never happen - two packages but different process id, but let's have it anyways
            "system    3092  885   1824084 76696 SyS_epoll_ 0000000000 S com.sec.epdg",
            "u0_a196   6964  885   1819464 74992 SyS_epoll_ 0000000000 S com.samsung.android.asksmanager",
            "system    6977  885   1820400 77772 SyS_epoll_ 0000000000 S com.samsung.android.bbc.bbcagent",
            "bcmgr     6991  885   1823868 76620 SyS_epoll_ 0000000000 S com.samsung.android.beaconmanager",
            "u0_a190   7004  885   1824168 71740 SyS_epoll_ 0000000000 S com.samsung.android.bluelightfilter",
            "u0_a219   7026  885   1849988 83532 SyS_epoll_ 0000000000 S com.samsung.android.calendar",
            "shell     7045  5404  9864   4124           0 7f7aa45738 R ps"
        });

        var pid = AndroidLogcatUtilities.ParsePidInfo("com.android.settings", adbContents);

        Assert.IsTrue(pid == expectedPid, "Process Id has to be " + expectedPid + ", but was " + pid);

        pid = AndroidLogcatUtilities.ParsePidInfo("com.I.DontExist", adbContents);
        Assert.IsTrue(pid == -1, "Process Id has to be -1 , but was " + pid);


        var invalidAdbContents = "blabla";
        pid = AndroidLogcatUtilities.ParsePidInfo("com.I.DontExist", invalidAdbContents);
        Assert.IsTrue(pid == -1, "Process Id has to be -1 , but was " + pid);
    }

    [Test]
    public void CorrectlyParseTopActivityWithWindowsEndlines()
    {
        CorrectlyParseTopActivit("\r\n");
    }

    [Test]
    public void CorrectlyParseTopActivityWithUnixEndlines()
    {
        CorrectlyParseTopActivit("\n");
    }

    private void CorrectlyParseTopActivit(string separator)
    {
        // Produced by adb shell dumpsys activity
        var adbContents = string.Join(separator, new[]
        {
            "ACTIVITY MANAGER PENDING INTENTS (dumpsys activity intents)",
            "  * PendingIntentRecord{28bcd62 com.google.android.partnersetup startService}",
            "",
            "  Process LRU list (sorted by oom_adj, 84 total, non-act at 2, non-svc at 2):",
            "    PERS #83: sys   F/ /P  trm: 0 1672:system/1000 (fixed)",
            "    PERS #82: pers  F/ /P  trm: 0 2560:com.android.phone/1001 (fixed)",
            "    PERS #81: pers  F/ /P  trm: 0 2589:com.android.systemui/u0a62 (fixed)",
            "    PERS #80: pers  F/ /P  trm: 0 2801:com.sec.imsservice/1000 (fixed)",
            "    PERS #79: pers  F/ /P  trm: 0 3092:com.sec.epdg/1000 (fixed)",
            "    PERS #77: pers  F/ /P  trm: 0 3160:com.sec.sve/1000 (fixed)",
            "    PERS #75: pers  F/ /P  trm: 0 3540:com.sec.android.app.wfdbroker/1000 (fixed)",
            "    PERS #74: pers  F/ /P  trm: 0 3553:com.samsung.android.radiobasedlocation/1000 (fixed)",
            "    PERS #73: pers  F/ /P  trm: 0 3577:com.android.nfc/1027 (fixed)",
            "    PERS #72: pers  F/ /P  trm: 0 3588:com.samsung.android.providers.context/u0a8 (fixed)",
            "    PERS #71: pers  F/ /P  trm: 0 3610:system/u0a82 (fixed)",
            "    PERS #70: pers  F/ /P  trm: 0 3618:com.samsung.vzwapiservice/1000 (fixed)",
            "    PERS #69: pers  F/ /P  trm: 0 3636:com.qualcomm.qti.services.secureui:sui_service/1000 (fixed)",
            "    PERS #68: pers  F/ /P  trm: 0 3658:com.sec.enterprise.knox.shareddevice.keyguard/1000 (fixed)",
            "    Proc #55: psvc  F/ /IF trm: 0 2573:com.android.bluetooth/1002 (service)",
            "        com.android.bluetooth/.pbap.BluetoothPbapService<=Proc{2589:com.android.systemui/u0a62}",
            "    Proc # 0: fore  R/A/T  trm: 0 3766:com.sec.android.app.launcher/u0a65 (top-activity)",
            "    Proc #76: vis   F/ /SB trm: 0 3239:com.google.android.ext.services/u0a194 (service)",
            "        com.google.android.ext.services/android.ext.services.notification.Ranker<=Proc{1672:system/1000}",
            "    Proc #67: vis   F/ /SB trm: 0 3532:com.google.android.googlequicksearchbox:interactor/u0a70 (service)",
            "        {null}<=android.os.BinderProxy@9243680",
            "    Proc #59: prcp  F/ /IF trm: 0 7295:com.samsung.android.oneconnect:QcService/u0a212 (force-fg)",
            "ACTIVITY MANAGER LOCALE CHANGED HISTORY",
            " (nothing) "
        });

        string packageName;
        var pid = AndroidLogcatUtilities.ParseTopActivityPackageInfo(adbContents, out packageName);

        var expectedPid = 3766;
        var expectedPackage = "com.sec.android.app.launcher";
        Assert.AreEqual(expectedPid, pid, "Expected top activity process id to be " + expectedPid + ", but was " + pid);
        Assert.AreEqual(expectedPackage, packageName, "Expected top activity package to be " + expectedPackage + ", but was " + packageName);
    }

    [Test]
    public void CorrectlyParseTopActivitiesWithInvalidData()
    {
        string packageName;
        var invalidAdbContents = "blabla";
        var pid = AndroidLogcatUtilities.ParseTopActivityPackageInfo(invalidAdbContents, out packageName);
        Assert.AreEqual(-1, pid, "Expected top activity process id to be -1 but was " + pid);
        Assert.AreEqual("", packageName, "Expected top activity package to be empty, but was " + packageName);

        invalidAdbContents = "";
        pid = AndroidLogcatUtilities.ParseTopActivityPackageInfo(invalidAdbContents, out packageName);
        Assert.AreEqual(-1, pid, "Expected top activity process id to be -1 but was " + pid);
        Assert.AreEqual("", packageName, "Expected top activity package to be empty, but was " + packageName);
    }

    [Test]
    public void CorrectlyParseSleepingTopActivities()
    {
        var adbContentsGooglePixelXL2 = @"
ACTIVITY MANAGER RUNNING PROCESSES (dumpsys activity processes)
  Isolated process list (sorted by uid):
    Isolated #0: ProcessRecord{8768b3e 22943:com.google.android.webview:sandboxed_process0:org.chromium.content.app.SandboxedProcessService0:0/u0a276i5}
    Isolated #1: ProcessRecord{8dc7ff9 23104:com.google.android.webview:sandboxed_process0:org.chromium.content.app.SandboxedProcessService0:0/u0a53i6}

  UID states:
    UID 1000: UidRecord{1afc23b 1000 PER  procs:3 seq(0,0,0)}
    UID 1001: UidRecord{9a5dd58 1001 PER  change:active procs:5 seq(0,0,0)}
    UID 1027: UidRecord{f203eb1 1027 PER  change:active|uncached procs:1 seq(0,0,0)}
    UID 1068: UidRecord{1bacf96 1068 PER  change:active|uncached procs:1 seq(0,0,0)}
    UID 1073: UidRecord{bce4717 1073 PER  change:active|uncached procs:1 seq(0,0,0)}
    UID u0a1: UidRecord{326bcb1 u0a1 SVC  idle procs:1 seq(0,0,0)}
    UID u0a9: UidRecord{7241e9 u0a9 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a10: UidRecord{6f463d5 u0a10 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a14: UidRecord{83797ea u0a14 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a18: UidRecord{3f13e6e u0a18 CEM  bg:+16m52s470ms idle change:idle procs:1 seq(0,0,0)}
    UID u0a19: UidRecord{5d1490f u0a19 CEM  bg:+13m30s137ms idle change:idle procs:1 seq(0,0,0)}
    UID u0a22: UidRecord{e63db3 u0a22 BFGS procs:1 seq(0,0,0)}
    UID u0a31: UidRecord{446ff70 u0a31 BFGS change:active|uncached procs:2 seq(0,0,0)}
    UID u0a37: UidRecord{3a73ff5 u0a37 SVC  bg:+29d10h59m47s648ms idle procs:1 seq(0,0,0)}
    UID u0a43: UidRecord{ef8dd9c u0a43 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a50: UidRecord{1a44712 u0a50 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a53: UidRecord{fdcf4a5 u0a53 BFGS procs:5 seq(0,0,0)}
    UID u0a58: UidRecord{3ca07a u0a58 BFGS change:uncached procs:1 seq(0,0,0)}
    UID u0a63: UidRecord{d3b7751 u0a63 BFGS procs:1 seq(0,0,0)}
    UID u0a68: UidRecord{62413a5 u0a68 CEM  bg:+13m31s299ms idle change:idle procs:2 seq(0,0,0)}
    UID u0a69: UidRecord{ce44a46 u0a69 FGSL procs:2 seq(0,0,0)}
    UID u0a72: UidRecord{1f6d37a u0a72 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a73: UidRecord{3f92407 u0a73 IMPF procs:1 seq(395,395,395)}
    UID u0a84: UidRecord{8f12adb u0a84 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a94: UidRecord{4bfe687 u0a94 CEM  bg:+1d8h11m14s199ms idle change:idle procs:1 seq(0,0,0)}
    UID u0a109: UidRecord{e5c22b5 u0a109 CEM  bg:+13m30s925ms idle change:idle procs:1 seq(0,0,0)}
    UID u0a110: UidRecord{a202978 u0a110 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a116: UidRecord{40ac4d2 u0a116 IMPB procs:1 seq(0,0,0)}
    UID u0a127: UidRecord{4e3672b u0a127 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a132: UidRecord{4843788 u0a132 CEM  idle change:cached procs:1 seq(0,0,0)}
    UID u0a177: UidRecord{674521 u0a177 CEM  bg:+16m38s912ms idle change:idle procs:1 seq(0,0,0)}
    UID u0a276: UidRecord{ccc1d4a u0a276 LAST bg:+13m30s343ms idle change:cached procs:1 seq(0,0,0)}
    UID u0a340: UidRecord{eaf4b07 u0a340 CAC  bg:+25m17s229ms idle change:idle procs:1 seq(0,0,0)}
    UID u0a341: UidRecord{7e94434 u0a341 TPSL bg:+6m28s818ms idle change:idle procs:1 seq(0,0,0)}
    UID u0i5: UidRecord{51134bb u0i5 LAST bg:+13m30s343ms idle change:cached procs:1 seq(0,0,0)}
    UID u0i6: UidRecord{a069dd8 u0i6 LAST bg:+13m15s940ms idle change:cached procs:1 seq(0,0,0)}

  Process LRU list (sorted by oom_adj, 53 total, non-act at 9, non-svc at 9):
    PERS #52: sys    F/ /PER  trm: 0 1367:system/1000 (fixed)
    PERS #49: pers   F/ /PER  trm: 0 2785:com.android.networkstack.process/1073 (fixed)
    PERS #48: pers   F/ /PER  trm: 0 2852:.dataservices/1000 (fixed)
    PERS #47: pers   F/ /PER  trm: 0 2886:com.qualcomm.qti.telephonyservice/1001 (fixed)
    PERS #46: pers   F/ /PER  trm: 0 2904:com.android.phone/1001 (fixed)
    PERS #45: pers   F/ /PER  trm: 0 3470:com.android.nfc/1027 (fixed)
    PERS #44: pers   F/ /PER  trm: 0 3493:com.android.se/1068 (fixed)
    PERS #43: pers   F/ /PER  trm: 0 3502:com.android.ims.rcsservice/1001 (fixed)
    PERS #42: pers   F/ /PER  trm: 0 3527:com.google.SSRestartDetector/1000 (fixed)
    PERS #51: pers   R/ /BFGS trm: 0 28569:com.android.systemui/u0a63 (pers-top-ui)
    Proc #36: fore   T/ /TOP  trm: 0 0:com.DefaultCompany.InputSystemPlayground/u0a338 (top-activity)
    Proc #35: fore   T/ /TOP  trm: 0 0:com.DefaultCompany.InputSystemPlayground/u0a338 (top-activity)
    Proc #34: fore   T/ /TOP  trm: 0 0:com.DefaultCompany.InputSystemTouchPerformance/u0a339 (top-activity)
    Proc # 0: fore   B/A/TPSL trm: 0 22172:com.Tomas.Test/u0a341 (top-sleeping)
    Proc #39: vis    F/ /FGSL trm: 0 3439:com.google.android.googlequicksearchbox:interactor/u0a69 (service)
";
        string packageName;
        var pid = AndroidLogcatUtilities.ParseTopActivityPackageInfo(adbContentsGooglePixelXL2, out packageName);

        var expectedPid = 22172;
        var expectedPackage = "com.Tomas.Test";
        Assert.AreEqual(expectedPid, pid, "Expected top activity process id to be " + expectedPid + ", but was " + pid);
        Assert.IsTrue(packageName == expectedPackage, "Expected top activity package to be " + expectedPackage + ", but was " + packageName);
    }

    [Test]
    public void CorrectyParseStacktraceCrash()
    {
        var logLines = new[]
        {
            "2020/07/15 15:31:30.887 23271 23292 Error AndroidRuntime    at libunity.0x0041e340(Native Method)",
            "2019-05-17 12:00:58.830 30759-30803/? E/CRASH: \t#00  pc 0041e340  /data/app/com.mygame==/lib/arm/libunity.so",
            "2020/07/15 15:31:30.887 23271 23292 Error AndroidRuntime    at libunity.0x1234567890123456(Native Method)",
            "2019-05-17 12:00:58.830 30759-30803/? E/CRASH: \t#00  pc 1234567890123456  /data/app/com.mygame==/lib/arm/libunity.so",
        };

        var regexs = new List<ReordableListItem>();
        foreach (var r in AndroidLogcatSettings.kAddressResolveRegex)
        {
            regexs.Add(new ReordableListItem() { Enabled = true, Name = r });
        }

        foreach (var line in logLines)
        {
            string address;
            string libName;
            var result = AndroidLogcatUtilities.ParseCrashLine(regexs, line, out address, out libName);
            Assert.IsTrue(result, "Failed to parse " + line);
            Assert.IsTrue(address.Equals("0041e340") ||
                address.Equals("1234567890123456"));
            Assert.IsTrue(libName.Equals("libunity.so"));
        }
    }

    [Test]
    public void ParseBuildInfo()
    {
        var buildType = "Release";
        var cpu = "armeabi-v7a";
        var backend = "mono";
        var buildInfos = new[]
        {
            "Built from '2019.3/staging' branch, Version '2019.3.0f5 (44796c9d3c2c)', Build type '" + buildType + "', Scripting Backend '" + backend + "', CPU '" + cpu + "', Stripping 'Disabled'",
            "Built from '2019.3/staging' branch, Version '2019.3.0f5 (44796c9d3c2c)', Build type '" + buildType + "', Scripting Backend '" + backend + "', CPU '" + cpu + "'"
        };
        foreach (var b in buildInfos)
        {
            var buildInfo = AndroidLogcatUtilities.ParseBuildInfo(b);
            UnityEngine.Debug.Log("Parsing:\n" + b);
            Assert.AreEqual(buildInfo.buildType, buildType.ToLower());
            Assert.AreEqual(buildInfo.cpu, cpu);
            Assert.AreEqual(buildInfo.scriptingImplementation, backend);
        }
    }

    [Test]
    public void ParseIPAddress()
    {
        // Data acquired using command adb -s <deviceId> shell ip route

        const string kLGG4IPOutput = @"
default via 192.168.50.1 dev wlan0  metric 205

192.168.50.0/24 dev wlan0  proto kernel  scope link  src 192.168.50.40  metric 205
";
        var ip = AndroidLogcatIPWindow.ParseIPAddress(kLGG4IPOutput);
        Assert.AreEqual("192.168.50.40", ip);

        const string kGooglePixelXL2IPOutput = @"192.168.50.0/24 dev wlan0 proto kernel scope link src 192.168.50.91";
        ip = AndroidLogcatIPWindow.ParseIPAddress(kGooglePixelXL2IPOutput);
        Assert.AreEqual("192.168.50.91", ip);
    }

    [Test]
    public void ParseDeviceInfo()
    {
        string id;
        IAndroidLogcatDevice.DeviceState state;
        var result = AndroidLogcatDeviceQuery.ParseDeviceInfo("List of devices attached", out id, out state);
        Assert.AreEqual(false, result);

        result = AndroidLogcatDeviceQuery.ParseDeviceInfo("711KPQJ0939020 unauthorized", out id, out state);
        Assert.AreEqual(true, result);
        Assert.AreEqual("711KPQJ0939020", id);
        Assert.AreEqual(IAndroidLogcatDevice.DeviceState.Unauthorized, state);

        result = AndroidLogcatDeviceQuery.ParseDeviceInfo("192.168.50.91:5555\tdevice", out id, out state);
        Assert.AreEqual(true, result);
        Assert.AreEqual("192.168.50.91:5555", id);
        Assert.AreEqual(IAndroidLogcatDevice.DeviceState.Connected, state);

        result = AndroidLogcatDeviceQuery.ParseDeviceInfo("192.168.50.91:5555\toffline", out id, out state);
        Assert.AreEqual(true, result);
        Assert.AreEqual("192.168.50.91:5555", id);
        Assert.AreEqual(IAndroidLogcatDevice.DeviceState.Disconnected, state);

        result = AndroidLogcatDeviceQuery.ParseDeviceInfo("192.168.50.91:5555\tblabla", out id, out state);
        Assert.AreEqual(true, result);
        Assert.AreEqual("192.168.50.91:5555", id);
        Assert.AreEqual(IAndroidLogcatDevice.DeviceState.Unknown, state);
    }
}
