using UnityEngine;
using System.Runtime.InteropServices;
using System.Collections;
using System.Collections.Generic;
using AvailableLanguages;
using System;

/// <summary>
/// класс распознавания голоса для Windows, Linux
/// </summary>
internal class DesktopSpeechRecognizer : BaseSpeechRecognizer {
    private const string DLL_NAME = "SpeechRecognizer";

    private IntPtr _sp = IntPtr.Zero;

    [DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl )]
    private static extern IntPtr makeSR( );

    [DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl )]
    private static extern void disposeSR( IntPtr ptr );

    #region импортированные из библиотеки статические методы
    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern bool runRecognizerSetup( IntPtr ptr, [ MarshalAs(UnmanagedType.LPStr ) ] string modelPath );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void saveLogIntoFile( IntPtr ptr, [MarshalAs(UnmanagedType.Bool ) ] bool value);

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern bool addGrammarFile( IntPtr ptr, [MarshalAs( UnmanagedType.LPStr ) ] string grammarName, [ MarshalAs( UnmanagedType.LPStr ) ] string grammarFile);

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern bool addGrammarString( IntPtr ptr, [MarshalAs( UnmanagedType.LPStr ) ] string grammarName, [ MarshalAs( UnmanagedType.LPStr ) ] string grammarString );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl)]
    private static extern bool addWordIntoDictionary( IntPtr ptr, [MarshalAs( UnmanagedType.LPStr ) ] string pWord, [ MarshalAs( UnmanagedType.LPStr ) ] string pPhones );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void setBaseGrammar( IntPtr ptr, [MarshalAs( UnmanagedType.LPStr ) ] string grammarName );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void setKeyword( IntPtr ptr, [MarshalAs( UnmanagedType.LPStr ) ] string keyword );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void setThreshold( IntPtr ptr, double pThreshold );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void startListeningMic( IntPtr ptr );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void stopListeningMic( IntPtr ptr );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void readMicBuffer( IntPtr ptr );

    [DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl) ]
    private static extern void changeGrammar( IntPtr ptr, [MarshalAs( UnmanagedType.LPStr ) ] string grammarName );

    [ DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl ) ]
    private static extern void setSearchKeyword( IntPtr ptr );

    [DllImport( DLL_NAME, CharSet = CharSet.Ansi, CallingConvention = CallingConvention.Cdecl )]
    private static extern void setVadThreshold( IntPtr ptr, double pThreshold );
    #endregion
    #region колбэки из библиотеки
    [ UnmanagedFunctionPointer( CallingConvention.Cdecl ) ]
    private delegate void FPtr( string str );
    /// <summary>
    /// метод-приёмник результатов распознавания речи из нативной Win библиотеки SpeechRecognizer.dll
    /// </summary>
    /// <param name="t">указатель на метод</param>
    [ DllImport( DLL_NAME ) ]
    private unsafe static extern void setResultReciever( IntPtr ptr, FPtr t );
    /// <summary>
    /// метод-приёмник сообщений отладки для лога из нативной Win библиотеки SpeechRecognizer.dll
    /// </summary>
    /// <param name="t">указатель на метод</param>
    [ DllImport( DLL_NAME ) ]
    private unsafe static extern void setLogMessReciever( IntPtr ptr, FPtr t );
    /// <summary>
    /// метод-приёмник результатов инициализации SpeechRecognizer из нативной Win библиотеки SpeechRecognizer.dll
    /// </summary>
    /// <param name="t">указатель на метод</param>
    [ DllImport( DLL_NAME ) ]
    private unsafe static extern void setCrashReciever( IntPtr ptr, FPtr t );
    #endregion

    public override void initialization( string language, GrammarFileStruct[ ] grammars, string keyword ) {
        setLogMessReciever( _sp, this.onRecieveLogMess );
        setResultReciever( _sp, this.onRecognitionResult );
        setCrashReciever( _sp, this.onError );
        saveLogIntoFile( _sp, false );

        setVadThreshold( _sp, 4.0 );

        this.onRecieveLogMess( "start initialization" );
        bool result = false;
        #region инициализируем SpeechRecognizer
        var destination = Application.streamingAssetsPath + "/GameVoiceControl/acousticModels/" + language + "/";
        result = runRecognizerSetup( _sp, destination );
        if ( !result ) {
            this.onError( ERROR_ON_INIT + " " + destination );
            //this.initResult.Invoke( false );
            this.onInitResult( FALSE );
            return;
        }
        #endregion

        #region добавляем слова в словарь
        var phonesDict = getWordsPhones( language, ref grammars, ref keyword );
        if ( phonesDict == null ) {
            this.onError( "error on init dictionary" );
            //this.initResult.Invoke( false );
            this.onInitResult( FALSE );
            return;
        }
        foreach ( string word in phonesDict.Keys ) {
            this.onRecieveLogMess( "add word:" + word + " phones:" + phonesDict[ word ] );
            result = addWordIntoDictionary( _sp, word, phonesDict[ word ] );
            if ( !result ) {
                this.onError( ERROR_ON_ADD_WORD + ":" + "[" + word + "] " + "phones:[" + phonesDict[ word ] + "]" );
                //this.initResult.Invoke( false );
                this.onInitResult( FALSE );
                return;
            }
        }
        #endregion

        #region добавляем граматику
        string[ ] grammar = new string[ 2 ];
        foreach ( var gramm in grammars ) {
            grammar[ 0 ] = gramm.name;
            grammar[ 1 ] = gramm.toString( );
            this.onRecieveLogMess( "try add grammar" + grammar[ 1 ] );
            result = addGrammarString( _sp, grammar [ 0 ], grammar[ 1 ] );
            if ( !result ) {
                this.onError( ERROR_ON_ADD_GRAMMAR + " " + gramm.name );
                //this.initResult.Invoke( false );
                this.onInitResult( FALSE );
                return;
            }
        }
        #endregion
        #region добавляем ключевое слово(ok google) для поиска
        if ( keyword != string.Empty ) {
            this.onRecieveLogMess( "try add keyword:" + keyword );
            setKeyword( _sp, keyword );
        }
        #endregion
        //this.initResult.Invoke( true );
        this.onInitResult( TRUE );
    }

    public override void startListening( ) {
        base.startListening( );
        startListeningMic( _sp );
        StartCoroutine( coUpdateWithDelay( _interval ) );
    }

    public override void stopListening( ) {
        base.stopListening( );
        StopCoroutine( coUpdateWithDelay( _interval ) );
        stopListeningMic( _sp );
    }

    public override void switchGrammar( string grammarName ) {
        changeGrammar( _sp, grammarName );
    }

    public override void searchKeyword( ) {
        setSearchKeyword( _sp );
    }
    /// <summary>
    /// таймер-уведомление о том что пора читать буффер микрофона для распознавания
    /// </summary>
    /// <param name="delayTime">интервал в милисекундах</param>
    /// <returns></returns>
    private IEnumerator coUpdateWithDelay( float delayTime ) {
        float interval = _interval / 1000;
        while ( this._isListening ) {
            readMicBuffer( _sp );
            yield return new WaitForSeconds( interval );
        }
    }
        
    private void Awake( ) {
        BaseSpeechRecognizer._instance = this;
        this._sp = makeSR( );
    }

    private void OnDestroy( ) {
        disposeSR( this._sp );
    }

    protected override void setKeywordThreshold( double pValue = 10000000000 ) {
        setThreshold( _sp, pValue );
    }

    public override void setVadThreshold( double value ) {
        setVadThreshold( _sp, value );
    }
}
