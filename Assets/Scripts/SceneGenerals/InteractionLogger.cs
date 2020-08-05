using System;
using System.Diagnostics;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

namespace LoggingModule
{
    public static class InteractionLogger
    {
        public static DateTime sessionStart;
        public static DateTime sessionEnd;

        private static bool inEditor = Application.isEditor;
        private static bool usePersistentPath = false;
        public static float totalSessionTime = 0.0f;

        private static Dictionary<string, int> counters = new Dictionary<string, int>() {
            { "totalOpenedBrowsers" , 0 },
            { "totalClosedBrowsers" , 0 },
            { "stillOpenedBrowsers" , 0 },
            { "totalHistorycalBrowsers" , 0 },

            { "totalOpenedPlaygrounds" , 0 },
            { "totalClosedPlaygrounds" , 0 },
            { "stillOpenedPlaygrounds" , 0 },
            { "totalHistorycalPlaygrounds" , 0 },

            { "totalOpenedInspectors" , 0 },
            { "totalClosedInspectors" , 0 },
            { "stillOpenedInspectors" , 0 },
            { "totalHistorycalInspectors" , 0 },

            { "totalOpenedGraphObjects" , 0 },
            { "totalClosedGraphObjects" , 0 },
            { "stillOpenedGraphObjects" , 0 },
            { "totalHistorycalGraphObjects" , 0 },

            { "totalOpenedTranscripts" , 0 },
            { "totalClosedTranscripts" , 0 },
            { "stillOpenedTranscripts" , 0 },
            { "totalHistorycalTranscripts" , 0 },

            { "totalPlaygroundMilliseconds" , 0 },
            { "totalBrowserMilliseconds" , 0 },
            { "totalInspectorMilliseconds" , 0 },
            { "totalTranscriptMilliseconds" , 0 },
            { "totalWindowDraggingMilliseconds", 0 }
        };

        private static Dictionary<string, Stopwatch> timers = new Dictionary<string, Stopwatch>() {
            { "PlaygroundInteraction" , null },
            { "BrowserInteraction" , null },
            { "InspectorInteraction" , null },
            { "WindowDraggingInteraction" , null },
            { "TranscriptInteraction" , null }
        };

        static string persistentPath = Path.Combine(Application.persistentDataPath, "log.txt");
        static string streamingPath = Path.Combine(Application.streamingAssetsPath, "log.txt");

        public static void writeLineToLog(string line)
        {
            if (!inEditor)
            {
                try
                {
                    string path = usePersistentPath ? persistentPath : streamingPath;
                    using (StreamWriter w = File.AppendText(path))
                        w.WriteLine(line);
                }
                catch { }
            }
        }

        public static void SessionStart()
        {
            if (!inEditor)
            {
                //sessionStart = DateTime.Now;
                //writeLineToLog("-------------------------------------------------");
                writeLineToLog("[ " + DateTime.Now + " ] New session started.");
            }
        }

        public static void Count(string window)
        {
            if (!inEditor)
            {
                //counters["totalOpened" + window + "s"] += 1;
                //counters["stillOpened" + window + "s"] += 1;
                //counters["totalHistorycal" + window + "s"] += 1;
                writeLineToLog("[ " + DateTime.Now + " ] Opened new window: " + window);
            }
        }

        public static void Discount(string window)
        {
            if (!inEditor)
            {
                //counters["totalClosed" + window + "s"] += 1;
                //counters["stillOpened" + window + "s"] -= 1;
                writeLineToLog("[ " + DateTime.Now + " ] Closed a window: " + window);
            }
        }

        public static void StartTimerFor(string window)
        {
            if (!inEditor)
            {
                timers[window + "Interaction"] = Stopwatch.StartNew();
                writeLineToLog("[ " + DateTime.Now + " ] Started interaction with a " + window);
            }
        }

        public static void EndTimerFor(string window)
        {
            if (!inEditor)
            {
                timers[window + "Interaction"].Stop();
                int elapsedTime = (int)timers[window + "Interaction"].Elapsed.TotalMilliseconds;
                counters["total" + window + "Milliseconds"] += elapsedTime;
                writeLineToLog(
                    "[ " + DateTime.Now + " ] Finished interaction with a " + window + ". Time Spent: " + elapsedTime.ToString() + " ms"
                );
            }   
        }

        public static void RegisterCodeDefinition(string type, string code, string response)
        {
            if (!inEditor)
            {
                writeLineToLog(
                    "[ " + DateTime.Now + " ] Defined " + type + " with code:\n" +
                    code + "\nand response:\n" + response
                );
            } 
        }

        public static void RegisterCodeExecution(string code, string response)
        {
            if (!inEditor)
            {
                writeLineToLog(
                   "[ " + DateTime.Now + " ] Executed code:\n" +
                   code + "\nwith response:\n" + response
                );
            }
        }

        public static void RegisterCodeInspection(string code, string response)
        {
            if (!inEditor)
            {
                writeLineToLog(
                    "[ " + DateTime.Now + " ] Inspected variable:\n" +
                    code + "\nwith response:\n" + response
                );
            }
                
        }

        public static void SessionEnd()
        {
            if (!inEditor)
            {
                //sessionEnd = DateTime.Now;
                writeLineToLog("[ " + DateTime.Now + " ] Session ended.");
                //writeLineToLog("Summary:");
                //foreach (KeyValuePair<string, int> entry in counters)
                //{
                //    writeLineToLog(entry.Key + ": " + entry.Value.ToString());
                //}
                //writeLineToLog("-------------------------------------------------");
            }
        }
    }
}
