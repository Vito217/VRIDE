using System.Collections.Generic;
using UnityEngine;
using JavaWrapperMethodNamesNamespace;
using System;

internal class AndroidSpeechRecognizer : BaseSpeechRecognizer {
    /// <summary>
    /// объект SpeechRecognizer импортированный из нативной android библиотеки
    /// </summary>
    private AndroidJavaObject _recognizerActivity = null;

    private Dictionary< string, string > _phonesDict = null;
    private GrammarFileStruct[ ] _grammars = null;
    private string _keyword = string.Empty;

    private static string GetAndroidExternalFilesDir( ) {
        using ( AndroidJavaClass unityPlayer =
               new AndroidJavaClass( "com.unity3d.player.UnityPlayer" ) ) {
            using ( AndroidJavaObject context =
                   unityPlayer.GetStatic<AndroidJavaObject>( "currentActivity" ) ) {
                // Get all available external file directories (emulated and sdCards)
                AndroidJavaObject[] externalFilesDirectories =
                                   context.Call<AndroidJavaObject[]>
                                   ("getExternalFilesDirs", (object)null);

                AndroidJavaObject emulated = null;
                AndroidJavaObject sdCard = null;

                for ( int i = 0; i < externalFilesDirectories.Length; i++ ) {
                    AndroidJavaObject directory = externalFilesDirectories[i];
                    using ( AndroidJavaClass environment =
                           new AndroidJavaClass( "android.os.Environment" ) ) {
                        // Check which one is the emulated and which the sdCard.
                        bool isRemovable = environment.CallStatic<bool>
                                          ("isExternalStorageRemovable", directory);
                        bool isEmulated = environment.CallStatic<bool>
                                          ("isExternalStorageEmulated", directory);
                        if ( isEmulated )
                            emulated = directory;
                        else if ( isRemovable && isEmulated == false )
                            sdCard = directory;
                    }
                }
                // Return the sdCard if available
                if ( sdCard != null )
                    return sdCard.Call<string>( "getAbsolutePath" );
                else
                    return emulated.Call<string>( "getAbsolutePath" );
            }
        }
    }

    public override void initialization( string language, GrammarFileStruct [ ] grammars, string keyword ) {
        this.onRecieveLogMess( "start initialization" );
        this.getBaseGrammar( grammars );

        var javaUnityPlayer = new AndroidJavaClass( "com.unity3d.player.UnityPlayer" );
        var currentActivity = javaUnityPlayer.GetStatic<AndroidJavaObject>( "currentActivity" );

        _recognizerActivity = new AndroidJavaObject( "com.sss.breter.voicerecognizer.MainActivity", currentActivity );//unity.GetStatic< AndroidJavaObject >( "com.sss.breter.voicerecognizer.MainActivity" );
        if ( _recognizerActivity == null ) {
            this.onError( "empty java object" );
            this.initResult.Invoke( false );
            this.onInitResult( FALSE );
            return;
        }
        #region инициализируем колбэк из jar библиотеки
        try {
            _recognizerActivity.CallStatic( JavaWrapperMethodNames.SET_RECIEVER_OBJECT_NAME.ToString( ), this.gameObject.name );
        } catch ( Exception ex ) {
            this.onError( ex.Message );
        }
        _recognizerActivity.CallStatic( JavaWrapperMethodNames.SET_LOG_RECIEVER_METHOD_NAME, "onRecieveLogMess" );
        _recognizerActivity.CallStatic( JavaWrapperMethodNames.SET_RECOGNITION_RESULT_RECIEVER_METHOD, "onRecognitionResult" );
        _recognizerActivity.CallStatic( JavaWrapperMethodNames.SET_CRASH_MESS_RECIEVER_METHOD, "onError" );
        _recognizerActivity.CallStatic( JavaWrapperMethodNames.SET_INITIALIZATION_COMPLETE_METHOD, "onInitResult" );
        #endregion
        _recognizerActivity.Call( JavaWrapperMethodNames.SET_BASE_GRAMMAR_FILE, _baseGrammar );
        //_recognizerActivity.Call( JavaWrapperMethodNames.SET_TIMEOUT_INTERVAL, this._interval );
        _recognizerActivity.Call( JavaWrapperMethodNames.RUN_RECOGNIZER_SETUP, language );
        _phonesDict = getWordsPhones( language, ref grammars, ref keyword );
        _grammars = grammars;
        _keyword = keyword;
    }

    protected override void onInitResult( string value ) {
        onRecieveLogMess( "onInitResult:" + value );

        foreach ( string word in _phonesDict.Keys ) {
            _recognizerActivity.Call<bool>( JavaWrapperMethodNames.ADD_WORD_INTO_DICTIONARY, word, _phonesDict [ word ] );
        }

        string[ ] grammar = new string[ 2 ];
        foreach ( GrammarFileStruct gramm in _grammars ) {
            grammar [ 0 ] = gramm.name;
            grammar [ 1 ] = gramm.toString( );
            //onRecieveLogMess( "GRAMM" + gramm.toString( ) ); 
            _recognizerActivity.Call<bool>( JavaWrapperMethodNames.ADD_GRAMMAR_STRING, grammar [ 0 ], grammar [ 1 ] );
        }

        #region добавляем ключевое слово(ok google) для поиска
        if ( _keyword != string.Empty ) {
            _recognizerActivity.Call<bool>( JavaWrapperMethodNames.SET_KEYWORD, _keyword );
            this.onRecieveLogMess( "add keyword:" + _keyword );
        }
        #endregion
        _init = Boolean.Parse( value );
        base.onInitResult( value );
    }

    public override void startListening( ) {
        this.onRecieveLogMess( "startListening:" + _init.ToString( ) );
        if ( _init ) {
            _recognizerActivity.Call( JavaWrapperMethodNames.START_LISTENING );
        }
    }

    public override void stopListening( ) {
        if ( _init ) {
            _recognizerActivity.Call( JavaWrapperMethodNames.STOP_LISTENING );
        }
    }

    public override void switchGrammar( string grammarName ) {
        _recognizerActivity.Call( JavaWrapperMethodNames.SWITCH_SEARCH, grammarName );
    }

    public override void searchKeyword( ) {
        _recognizerActivity.Call<bool>( JavaWrapperMethodNames.SET_SEARCH_KEYWORD );
    }

    protected override void setKeywordThreshold( double pValue = 10000000000 ) {
        Debug.LogWarning( "AndroidSpeechRecognizer. setKeywordThreshold not implemented yet!" );
    }

    void Awake( ) {
        BaseSpeechRecognizer._instance = this;
    }

    public override void setVadThreshold( double value ) {
        Debug.LogWarning( "AndroidSpeechRecognizer. setVadThreshold not implemented yet!" );
    }
}
