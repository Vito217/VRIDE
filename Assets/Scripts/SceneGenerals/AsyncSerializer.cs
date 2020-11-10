using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;

/// <summary>
/// Namespace for storing sessions
/// </summary>
namespace AsyncSerializer
{
    /// <summary>
    /// The main session serializer. It stores files in persistentDataPath, which
    /// contain information about windows.
    /// </summary>
    public static class AsynchronousSerializer
    {
        /// <summary>
        /// Load a serialized file
        /// </summary>
        /// <param name="sessionPath">Dir. path</param>
        /// <returns>The previous Session</returns>
        public static async Task<Session> Deserialize(string sessionPath)
        {
            Session session = null;
            BinaryFormatter bf = new BinaryFormatter();
            FileStream file = File.Open(sessionPath, FileMode.Open);
            await Task.Run(() => { session = (Session) bf.Deserialize(file); });
            file.Close();
            return session;            
        }

        /// <summary>
        /// Stores a serialized file
        /// </summary>
        /// <param name="sessionPath">Dir. path</param>
        /// <param name="s">Session data</param>
        /// <returns>The current Session</returns>
        public static void Serialize(string sessionPath, Session s)
        {
            BinaryFormatter bf = new BinaryFormatter();
            if (File.Exists(sessionPath)) File.Delete(sessionPath);
            FileStream file = File.Create(sessionPath);
            bf.Serialize(file, s);
            file.Close();
        }
    }
}
