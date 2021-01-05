using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Threading.Tasks;
using UnityEngine;
using System.IO;

/// <summary>
/// Used for storing logs from all the sessions.
/// </summary>
namespace LoggingModule
{
    /// <summary>
    /// Session logger
    /// </summary>
    public static class InteractionLogger
    {
        public static DateTime sessionStart;
        public static DateTime sessionEnd;

        public static bool isUsingVirtualKeyboard = false;
        public static bool isUsingPhysicalKeyboard = false;

        private static bool inEditor = Application.isEditor;
        public static float totalSessionTime = 0.0f;
        static string persistentPath = Path.Combine(
            Application.persistentDataPath, "VRIDE_log.txt");

        private static Dictionary<string, int> counters = new Dictionary<string, int>() {
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

        /// <summary>
        /// Writes a string to the log file.
        /// </summary>
        /// <param name="line">The string</param>
        public static async void writeLineToLog(string line)
        {
            if (!inEditor)
            {
                try
                {
                    await Task.Run(() => {
                        if (!Directory.Exists(Application.persistentDataPath))
                            Directory.CreateDirectory(Application.persistentDataPath);
                        using (StreamWriter w = File.AppendText(persistentPath))
                            w.WriteLine(line);
                    });
                }
                catch { }
            }
        }

        /// <summary>
        /// Register start of a session
        /// </summary>
        public static void SessionStart()
        {
            if (!inEditor) writeLineToLog("[ " + DateTime.Now + " ] New session started.");
        }

        /// <summary>
        /// Register an opened window
        /// </summary>
        /// <param name="window"></param>
        public static void Count(string window, string id)
        {
            if (!inEditor) writeLineToLog("[ " + DateTime.Now + " ] Opened new window: " + window + " (ID: " + id + ")");
        }

        /// <summary>
        /// Register a closed window. 
        /// </summary>
        /// <param name="window"></param>
        public static void Discount(string window, string id)
        {
            if (!inEditor) writeLineToLog("[ " + DateTime.Now + " ] Closed a window: " + window + " (ID: " + id + ")");
        }

        /// <summary>
        /// Starts a timer that counts how many millisecs the user spends using a window.
        /// </summary>
        /// <param name="window">Target window name</param>
        public static void StartTimerFor(string window, string id)
        {
            if (!inEditor)
            {
                timers[window + "Interaction"] = Stopwatch.StartNew();
                writeLineToLog("[ " + DateTime.Now + " ] Started interaction with a " + window +" (ID: " + id + ")");
            }
        }

        /// <summary>
        /// Ends a window timer
        /// </summary>
        /// <param name="window">Target window name</param>
        public static void EndTimerFor(string window, string id)
        {
            if (!inEditor)
            {
                timers[window + "Interaction"].Stop();
                int elapsedTime = (int)timers[window + "Interaction"].Elapsed.TotalMilliseconds;
                counters["total" + window + "Milliseconds"] += elapsedTime;
                writeLineToLog(
                    "[ " + DateTime.Now + " ] Finished interaction with a " + window + " (ID: " + id + "). Time Spent: " + elapsedTime.ToString() + " ms"
                );
            }   
        }

        /// <summary>
        /// Logs the defined code
        /// </summary>
        /// <param name="type">Is it class, method or package?</param>
        /// <param name="code">The code as a string</param>
        /// <param name="response">The response as a string</param>
        public static void RegisterCodeDefinition(string type, string code, string response)
        {
            if (!inEditor) 
                writeLineToLog(
                    "[ " + DateTime.Now + " ] Defined " + type + " with code:\n" +
                    code + "\nand response:\n" + response
                ); 
        }

        /// <summary>
        /// Logs the executed code
        /// </summary>
        /// <param name="code">The code</param>
        /// <param name="response">The response</param>
        public static void RegisterCodeExecution(string code, string response)
        {
            if (!inEditor) 
                writeLineToLog(
                   "[ " + DateTime.Now + " ] Executed code:\n" +
                   code + "\nwith response:\n" + response
                ); 
        }

        /// <summary>
        /// Logs the inspected code
        /// </summary>
        /// <param name="code">The code</param>
        /// <param name="response">The response</param>
        public static void RegisterCodeInspection(string code, string response)
        {
            if (!inEditor)
                writeLineToLog(
                    "[ " + DateTime.Now + " ] Inspected variable:\n" +
                    code + "\nwith response:\n" + response
                );
        }

        /// <summary>
        /// Register the end of a session
        /// </summary>
        public static void SessionEnd()
        {
            if (!inEditor) writeLineToLog("[ " + DateTime.Now + " ] Session ended.");
        }

        /// <summary>
        /// Register if the user started coding with a physical keyboard
        /// </summary>
        public static void RegisterPhysicalKeyboard()
        {
            if (!inEditor)
            {
                if (!isUsingPhysicalKeyboard)
                {
                    isUsingVirtualKeyboard = false;
                    isUsingPhysicalKeyboard = true;
                    writeLineToLog("[ " + DateTime.Now + " ] Changed to physical keyboard");
                }
            }
        }

        /// <summary>
        /// Register if the user started coding with a virtual keyboard
        /// </summary>
        public static void RegisterVirtualKeyboard()
        {
            if (!inEditor)
            {
                if (!isUsingVirtualKeyboard)
                {
                    isUsingVirtualKeyboard = true;
                    isUsingPhysicalKeyboard = false;
                    writeLineToLog("[ " + DateTime.Now + " ] Changed to virtual keyboard");
                }
            }
        }

        /// <summary>
        /// Register if the user opened a Roassal example
        /// </summary>
        public static void RegisterRoassalExample(string example)
        {
            if (!inEditor)
            {
                writeLineToLog("[ " + DateTime.Now + " ] Opened Roassal Example (" + example + ")");
            }
        }

        public static void RegisterPlayerPosition(float x, float y, float z)
        {
            if (!inEditor)
            {
                writeLineToLog("[ " + DateTime.Now + " ] User moved to (" + x.ToString() + ", " + y.ToString() + ", " + z.ToString() + ")");
            }
        }

        public static void RegisterWindowDraggingStart(float x, float y, float z, string window, string id)
        {
            if (!inEditor)
            {
                writeLineToLog("[ " + DateTime.Now + " ] Dragging " + window + " (ID: " + id + ") from position (" +
                    x.ToString() + ", " + y.ToString() + ", " + z.ToString() + ")");
            }
        }

        public static void RegisterWindowDraggingEnd(float x, float y, float z, string window, string id)
        {
            if (!inEditor)
            {
                writeLineToLog("[ " + DateTime.Now + " ] Dragged " + window + " (ID: " + id + ") to position (" +
                    x.ToString() + ", " + y.ToString() + ", " + z.ToString() + ")");
            }
        }

        public static void RegisterWindowChange(string increasedOrDecreased, string window, string id, string property)
        {
            if (!inEditor)
            {
                writeLineToLog("[ " + DateTime.Now + " ] " + increasedOrDecreased + " " + window + " (ID: " + id + ") " + property);
            }
        }
    }
}
