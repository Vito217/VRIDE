using NUnit.Framework;
using System;
using Unity.Android.Logcat;

public class AndroidLogcatMemoryTests
{
    const int kKiloByte = 1000;

    // Produced by adb.exe shell dumpsys meminfo com.unity.TowerDefenceIncremental
    const string kPackageMemoryDump = @"
Applications Memory Usage(in Kilobytes) :
Uptime: 277764755 Realtime: 525450147

** MEMINFO in pid 21501 [com.unity.TowerDefenceIncremental] **
                   Pss  Private Private  SwapPss Heap     Heap Heap
                 Total Dirty    Clean Dirty     Size Alloc     Free
                ------   ------   ------   ------   ------   ------   ------
  Native Heap    25238    25172        0   124690   165624   157805     7818
  Dalvik Heap     1537      548      900      165     1680     1184      496
 Dalvik Other      243      224        0       14
        Stack       36       36        0       20
       Ashmem        4        0        0        0
      Gfx dev    57928    57800      128        0
    Other dev       66        0        8        0
     .so mmap     2070      468      216     1311
    .jar mmap      984        0        0        0
    .apk mmap      255        0        0        0
    .dex mmap       72       72        0       72
    .oat mmap       53        0        8        0
    .art mmap     3609     1928      532     2210
   Other mmap      220       72       16        1
   EGL mtrack    49956    49956        0        0
      Unknown     5402     5356        4    36518
        TOTAL   312674   141632     1812   165001   167304   158989     8314

 App Summary
                       Pss(KB)
                        ------
           Java Heap:     1234
         Native Heap:    43210
                Code:      764
               Stack:       36
            Graphics:   107884
       Private Other:     6580
              System:   169230

               TOTAL:   312674       TOTAL SWAP PSS:   165001

 Objects
               Views:        7         ViewRootImpl:        1
         AppContexts:        6           Activities:        1
              Assets:       16        AssetManagers:        0
       Local Binders:       13        Proxy Binders:       32
       Parcel memory:        6         Parcel count:       25
    Death Recipients:        0      OpenSSL Sockets:        0
            WebViews:        0

 SQL
         MEMORY_USED:        0
  PAGECACHE_OVERFLOW:        0          MALLOC_SIZE:        0
";

    // adb.exe shell dumpsys meminfo sensors.qcom
    const string kSystemProcessDump = @"
Applications Memory Usage (in Kilobytes):
Uptime: 278194816 Realtime: 525880208
                   Pss  Private  Private  SwapPss     Heap     Heap     Heap
                 Total    Dirty    Clean    Dirty     Size    Alloc     Free
                ------   ------   ------   ------   ------   ------   ------
  Native Heap      156      156        0      156        0        2(6)        0
  Dalvik Heap        0        0        0        0        0        3(6)       0
        Stack        4        4        0       32
    Other dev        0        0        0        0
     .so mmap       80       60        0      320
   Other mmap      151       28      120       32
      Unknown      112      112        0      340
        TOTAL     1383      360      120      880        0        0        0

 App Summary
                       Pss(KB)
                        ------
           Java Heap:        1
         Native Heap:      156
                Code:       60
               Stack:        4
            Graphics:        2
       Private Other:      260
              System:      903

               TOTAL:     1383       TOTAL SWAP PSS:      880
";

    const string kProcessWithHugeValues = @"
Applications Memory Usage (in Kilobytes):
Uptime: 278194816 Realtime: 525880208
                   Pss  Private  Private  SwapPss     Heap     Heap     Heap
                 Total    Dirty    Clean    Dirty     Size    Alloc     Free
                ------   ------   ------   ------   ------   ------   ------
  Native Heap      156      156        0      156        0        {0}        0
  Dalvik Heap        0        0        0        0        0        0       0
        Stack        4        4        0       32
    Other dev        0        0        0        0
     .so mmap       80       60        0      320
   Other mmap      151       28      120       32
      Unknown      112      112        0      340
        TOTAL     1383      360      120      880        0        0        0

 App Summary
                       Pss(KB)
                        ------
           Java Heap:        1
         Native Heap:      156
                Code:       60
               Stack:        4
            Graphics:        2
       Private Other:      260
              System:      903

               TOTAL:     1383       TOTAL SWAP PSS:      880
";

    [Test]
    public void CanParseMemoryDumpFromPackage()
    {
        var stats = new AndroidMemoryStatistics();
        stats.Parse(kPackageMemoryDump);

        Assert.AreEqual(312674 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.Total));
        Assert.AreEqual(169230 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.System));
        Assert.AreEqual(43210 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.NativeHeap));
        Assert.AreEqual(107884 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.Graphics));
        Assert.AreEqual(36 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.Stack));
        Assert.AreEqual(1234 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.JavaHeap));

        Assert.AreEqual(157805 * kKiloByte, stats.GetValue(MemoryGroup.HeapAlloc, MemoryType.NativeHeap));
        Assert.AreEqual(1184 * kKiloByte, stats.GetValue(MemoryGroup.HeapAlloc, MemoryType.JavaHeap));
    }

    [Test]
    public void CanParseMemoryDumpFromProcess()
    {
        var stats = new AndroidMemoryStatistics();
        stats.Parse(kSystemProcessDump);


        Assert.AreEqual(1383 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.Total));
        Assert.AreEqual(903 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.System));
        Assert.AreEqual(156 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.NativeHeap));
        Assert.AreEqual(2 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.Graphics));
        Assert.AreEqual(4 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.Stack));
        Assert.AreEqual(1 * kKiloByte, stats.GetValue(MemoryGroup.ProportionalSetSize, MemoryType.JavaHeap));

        Assert.AreEqual(2 * kKiloByte, stats.GetValue(MemoryGroup.HeapAlloc, MemoryType.NativeHeap));
        Assert.AreEqual(3 * kKiloByte, stats.GetValue(MemoryGroup.HeapAlloc, MemoryType.JavaHeap));
        Assert.AreEqual(5 * kKiloByte, stats.GetValue(MemoryGroup.HeapAlloc, MemoryType.Total));
    }

    [Test]
    public void CanParseHugeValues()
    {
        const UInt64 kOneMegaByte = 1000;
        const UInt64 kOneGigabyte = 1000 * kOneMegaByte;
        const UInt64 kOneTerabyte = 1000 * kOneGigabyte;

        var inputs = new[] { kOneGigabyte, kOneTerabyte };
        foreach (var i in inputs)
        {
            // Note: Report contains values in kilobytes
            var contents = string.Format(kProcessWithHugeValues, i);
            var stats = new AndroidMemoryStatistics();
            stats.Parse(contents);
            Assert.AreEqual(i * kKiloByte, stats.GetValue(MemoryGroup.HeapAlloc, MemoryType.NativeHeap));
        }
    }

    [Test]
    public void CanProvidePrettySizeText()
    {
        const UInt64 kOneMegaByte = 1000 * 1000;
        const UInt64 kOneGigabyte = 1000 * kOneMegaByte;
        const UInt64 kOneTerabyte = 1000 * kOneGigabyte;

        Assert.AreEqual("1000 KB", AndroidLogcatMemoryViewer.UInt64ToSizeString(kOneMegaByte));
        Assert.AreEqual("2 MB", AndroidLogcatMemoryViewer.UInt64ToSizeString(2 * kOneMegaByte));
        Assert.AreEqual("1000 MB", AndroidLogcatMemoryViewer.UInt64ToSizeString(kOneGigabyte));
        Assert.AreEqual("2 GB", AndroidLogcatMemoryViewer.UInt64ToSizeString(2 * kOneGigabyte));
        Assert.AreEqual("1000 GB", AndroidLogcatMemoryViewer.UInt64ToSizeString(kOneTerabyte));
        Assert.AreEqual("2 TB", AndroidLogcatMemoryViewer.UInt64ToSizeString(2 * kOneTerabyte));
    }
}
