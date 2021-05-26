using UnityEngine;
using AvailableLanguages;
using System;

namespace UnityEngine.SpeechRecognition {
    public class SpeechRecognizer : IDisposable {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parent">Parent object onto which component BaseSpeechRecognizer will be added</param>
        public SpeechRecognizer( MonoBehaviour parent ) {
            switch ( Application.platform ) {
            case RuntimePlatform.Android: parent.gameObject.AddComponent<AndroidSpeechRecognizer>( ); break;
            case RuntimePlatform.WindowsEditor: parent.gameObject.AddComponent<DesktopSpeechRecognizer>( ); break;
            case RuntimePlatform.WindowsPlayer: parent.gameObject.AddComponent<DesktopSpeechRecognizer>( ); break;
            case RuntimePlatform.LinuxPlayer: parent.gameObject.AddComponent<DesktopSpeechRecognizer>( ); break;
            }
            _speechRecognizer = parent.GetComponent<BaseSpeechRecognizer>( );
            if ( _speechRecognizer == null ) {
                Debug.Log( "empty component speechRecognizer" );
                return;
            }
        }
        /// <summary>
        /// Initiating the object MultiplatformSpeechRecognizer
        /// </summary>
        /// <param name="language">Language - for choosing of directory with language model and dictionary</param>
        /// <param name="grammars">List of grammars with words</param>
        /// <param name="keyword">Keyword initiating the search (ok google)</param>
        /// <param name="threshold">Threshold of triggering of keyword</param>
        /// <param name="vadThreshold">Threshold of voice activity detection</param>
        /// <param name="timeoutInterval">Reading interval of microphone buffer</param>
        public void init( string language = Language.en_US, GrammarFileStruct [ ] grammars = null, string keyword = "", double threshold = 1e+10f, double vadThreshold = 4.0, float timeoutInterval = 50.0f ) {
            if ( _speechRecognizer == null )
                return;
            if ( grammars == null )
                return;
            if ( grammars.Length == 0 )
                return;
            // все слова в нижний регистр
            foreach ( var grammar in grammars ) {
                for ( int i = 0; i < grammar.commands.Length; i++ ) {
                    grammar.commands [ i ] = grammar.commands [ i ].ToLower( );
                }
            }
            _speechRecognizer.keywordThreshold = threshold;
            _speechRecognizer.setVadThreshold( vadThreshold );
            _speechRecognizer.setTimeoutInterval( timeoutInterval );
            initSpeechRecognizer( language, grammars, keyword );
        }

        #region определяем методы-приёмники результатов работы библиотеки распознавания
        /// <summary>
        /// Sets callback function to receive results of recognition
        /// </summary>
        /// <param name="resultReciever">interface link to the object - receiver</param>
        public void setResultRecieverMethod( IGetResult resultReciever ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.recognitionResult += resultReciever.getResult;
        }
        /// <summary>
        /// Sets callback function to receive messages
        /// </summary>
        /// <param name="messagesReciever">interface link to the object - receiver</param>
        public void setMessagesFromLogRecieverMethod( IGetLogMessages messagesReciever ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.logFromRecognizer += messagesReciever.getLogMessages;
        }
        /// <summary>
        ///  Sets callback function to receive error messages
        /// </summary>
        /// <param name="crashMessReciever">interface link to the object - receiver</param>
        public void setCrashMessagesRecieverMethod( IGetCrashMessages crashMessReciever ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.errorMessage += crashMessReciever.getCrashMessages;
        }
        /// <summary>
        /// Sets callback function to receive results of initialization
        /// </summary>
        /// <param name="initResultReciever"></param>
        public void setInitResultRecieverMethod( IGetInitResult initResultReciever ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.initResult += initResultReciever.initComplete;
        }
        #endregion
        /// <summary>
        /// Switches the microphone on - start of recognition
        /// </summary>
        public void startListening( ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.startListening( );
        }
        /// <summary>
        /// Switches the microphone off - end of recognition
        /// </summary>
        public void stopListening( ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.stopListening( );
        }
        /// <summary>
        /// Changes the grammar - List of commands for recognition
        /// </summary>
        /// <param name="grammarName">Name of the grammar</param>
        public void switchGrammar( string grammarName ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.switchGrammar( grammarName );
        }
        /// <summary>
        /// Switches the mode of keyword search on (OK GOOGLE)
        /// </summary>
        public void searchKeyword( ) {
            if ( _speechRecognizer != null )
                _speechRecognizer.searchKeyword( );
        }
        /// <summary>
        /// Adding made-up word
        /// </summary>
        /// <param name="pair">Pair of Grapheme (word) and Phoneme (transcription)</param>
        public void addPairG2P( PairG2P pair ) {
            _speechRecognizer.addPairG2P( pair );
        }

        public void Dispose( ) {
            _speechRecognizer = null;
        }

        private BaseSpeechRecognizer _speechRecognizer = null;

        private void initSpeechRecognizer( string language, GrammarFileStruct [ ] grammars, string keyword ) {
            _speechRecognizer.initialization( language, grammars, keyword );
        }
    }

}
