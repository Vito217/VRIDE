using System;
using System.Diagnostics;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;

namespace LoggingModule
{
    public static class InteractionLogger
    {
        public static DateTime sessionStart;
        public static DateTime sessionEnd;

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

            { "totalPlaygroundMilliseconds" , 0 },
            { "totalBrowserMilliseconds" , 0 },
            { "totalInspectorMilliseconds" , 0 }

        };

        private static Dictionary<string, Stopwatch> timers = new Dictionary<string, Stopwatch>() {
            { "PlaygroundInteraction" , null },
            { "BrowserInteraction" , null },
            { "InspectorInteraction" , null },
        };

        public static void writeLineToLog(string line)
        {
            using (StreamWriter w = File.AppendText(Application.persistentDataPath + "/log.txt"))
            {
                w.WriteLine(line);
            }
        }

        public static void SessionStart()
        {
            sessionStart = DateTime.Now;
            writeLineToLog("-------------------------------------------------");
            writeLineToLog("New session started at " + sessionStart.ToString());
        }

        public static void Count(string window)
        {
            counters["totalOpened" + window + "s"] += 1;
            counters["stillOpened" + window + "s"] += 1;
            counters["totalHistorycal" + window + "s"] += 1;
            writeLineToLog("Opened new window: " + window);
        }

        public static void Discount(string window)
        {
            counters["totalClosed" + window + "s"] += 1;
            counters["stillOpened" + window + "s"] -= 1;
            writeLineToLog("Closed a window: " + window);
        }

        public static void StartTimerFor(string window)
        {
            timers[window + "Interaction"] = Stopwatch.StartNew();
            writeLineToLog("Started interaction with a " + window);
        }

        public static void EndTimerFor(string window)
        {
            timers[window + "Interaction"].Stop();
            int elapsedTime = (int) timers[window + "Interaction"].Elapsed.TotalMilliseconds;
            counters["total" + window + "Milliseconds"] += elapsedTime;
            writeLineToLog(
                "Finished interaction with a " + window + "\n" +
                "Time Spent: " + elapsedTime.ToString() + " ms"
            );
        }

        public static void RegisterCodeDefinition(string type, string code, string response)
        {
            writeLineToLog(
                "Defined " + type + " with code:\n" +
                code + "\n" +
                "and response:\n" +
                response
            );
        }

        public static void RegisterCodeExecution(string code, string response)
        {
            writeLineToLog(
               "Executed code:\n" +
               code + "\n" +
               "with response:\n" +
               response
            );
        }

        public static void RegisterCodeInspection(string code, string response)
        {
            writeLineToLog(
                "Inspected variable:\n" + 
                code + "\n" +
                "with response:\n" + 
                response
            );
        }

        public static void SessionEnd()
        {
            sessionEnd = DateTime.Now;
            writeLineToLog("Session ended at " + sessionEnd.ToString());
            writeLineToLog("Summary:");
            foreach (KeyValuePair<string, int> entry in counters)
            {
                writeLineToLog(entry.Key + ": " + entry.Value.ToString());
            }
            writeLineToLog("-------------------------------------------------");
        }
    }
}
