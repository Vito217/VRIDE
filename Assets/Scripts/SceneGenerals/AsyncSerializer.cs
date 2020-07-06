using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.Eventing.Reader;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading.Tasks;
using UnityEngine;

namespace AsyncSerializer
{
    public static class AsynchronousSerializer
    {
        public static async Task<Session> Deserialize(string sessionPath)
        {
            Session session = null;
            await Task.Run(() =>
            {
                BinaryFormatter bf = new BinaryFormatter();
                FileStream file = File.Open(sessionPath, FileMode.Open);
                session = (Session) bf.Deserialize(file);
                file.Close();
            });
            return session;            
        }

        public static async Task Serialize(string sessionPath, Session s)
        {
            await Task.Run(() =>
            {
                BinaryFormatter bf = new BinaryFormatter();
                FileStream file = File.Create(sessionPath);
                bf.Serialize(file, s);
                file.Close();
            });
        }
    }
}
