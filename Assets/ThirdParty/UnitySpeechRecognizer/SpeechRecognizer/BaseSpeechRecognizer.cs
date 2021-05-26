using UnityEngine;
using System.Collections.Generic;
using AvailableLanguages;
using System;
using System.Linq;
using System.Reflection;
using System.Resources;

/// <summary>
/// базовый класс распознования голоса. От него наследуют классы WindowsSpeechRecognizer, AndroidSpeechRecognizer, LinuxSpeechRecognizer
/// </summary>
internal abstract class BaseSpeechRecognizer : MonoBehaviour {
    /// <summary>
    /// порог срабатывания при распознавании ключевого слова
    /// </summary>
    public double keywordThreshold {
        set { setKeywordThreshold( value ); }
    }
    /// <summary>
    /// сигнал с сообщением отладки из библиотеки распознования
    /// </summary>
    public Action<string> logFromRecognizer;
    /// <summary>
    /// сигнал с сообщением об ошибке
    /// </summary>
    public Action<string> errorMessage;
    /// <summary>
    /// сигнал с результатом распознавания
    /// </summary>
    public Action<string> recognitionResult;
    /// <summary>
    /// сигнал с результатом инициализации объекта 
    /// распознавания голоса
    /// </summary>
    public Action<bool> initResult;
    /// <summary>
    /// начало записи голоса с микрофона
    /// </summary>
    public virtual void startListening( ) {
        _isListening = true;
    }
    /// <summary>
    /// окончание записи голоса с микрофона
    /// </summary>
    public virtual void stopListening( ) {
        _isListening = false;
    }
    /// <summary>
    /// смена граматики(перечень слов доступных для распознавания)
    /// </summary>
    /// <param name="grammarName">имя грамматики</param>
    public abstract void switchGrammar( string grammarName );
    /// <summary>
    /// Инициализируем поиск ключевого слова
    /// </summary>
    public abstract void searchKeyword( );
    /// <summary>
    /// инициализируем распознаватель голоса
    /// </summary>
    /// <param name="language">язык - определяет дирректорию с акустической моделью, словарями и файлами граматики</param>
    /// <param name="grammars">массив со структурами грамматики(имя грамматики и массив слов)</param>
    /// <param name="keyword">ключевое слово</param>
    /// <returns>результат инициализации</returns>
    public abstract void initialization( string language, GrammarFileStruct [ ] grammars, string keyword );
    /// <summary>
    /// переопределяет файл грамматики поумолчанию
    /// </summary>
    /// <param name="grammarName">имя файла грамматики</param>
    public void setBaseGrammarName( string grammarName ) {
        _baseGrammar = grammarName;
    }
    /// <summary>
    /// Интервал опроса микрофона
    /// </summary>
    /// <param name="value"></param>
    public void setTimeoutInterval( float value ) {
        this._interval = value;
    }
    /// <summary>
    /// Устанавливает порог срабатывания для Voice Activity Detection
    /// </summary>
    /// <param name="value">Значение порога срабатывания для Voice Activity Detection</param>
    public abstract void setVadThreshold( double value );
    public void addPairG2P( PairG2P pair ) {
        _unknownWords.Add( pair );
    }
    #region baseGrammar
    /// <summary>
    /// имя файла грамматики поумолчанию
    /// </summary>
    protected string _baseGrammar = string.Empty;
    /// <summary>
    /// устанавливает файл грамматики поумолчанию равным первому элементу из массива структур грамматики
    /// </summary>
    /// <param name="grammars">массив со структурами грамматики(имя грамматики и массив слов)</param>
    protected void getBaseGrammar( GrammarFileStruct [ ] grammars ) {
        if ( _baseGrammar == string.Empty ) {
            _baseGrammar = grammars [ 0 ].name;
        }
    }
    #endregion
    protected virtual void onInitResult( string value ) {
        _init = Boolean.Parse( value );
        if ( BaseSpeechRecognizer._instance != null )
            if ( BaseSpeechRecognizer._instance.initResult != null )
                BaseSpeechRecognizer._instance.initResult.Invoke( _init );
            else
                UnityEngine.Debug.LogError( "use setInitResultRecieverMethod method!" );
        else
            UnityEngine.Debug.LogError( ERROR_ON_SPEECH_RECOGNIZER_IS_NULL );
    }
    /// <summary>
    /// метод-приёмник сообщений отладки из библиотек распознавания голоса
    /// </summary>
    /// <param name="message"></param>
    protected void onRecieveLogMess( string message ) {
        if ( BaseSpeechRecognizer._instance != null )
            if ( BaseSpeechRecognizer._instance.logFromRecognizer != null )
                BaseSpeechRecognizer._instance.logFromRecognizer.Invoke( message );
            else
                UnityEngine.Debug.LogWarning( "use setMessagesFromLogRecieverMethod method!" );
        else
            UnityEngine.Debug.LogError( ERROR_ON_SPEECH_RECOGNIZER_IS_NULL );
    }
    /// <summary>
    /// метод-приёмник результатов распознавания из библиотек распознавания голоса
    /// </summary>
    /// <param name="message"></param>
    protected void onRecognitionResult( string message ) {
        if ( BaseSpeechRecognizer._instance != null )
            if ( BaseSpeechRecognizer._instance.recognitionResult != null )
                BaseSpeechRecognizer._instance.recognitionResult.Invoke( message.ToLower( ) );
            else
                UnityEngine.Debug.LogError( "use setResultRecieverMethod method!" );
        else
            UnityEngine.Debug.LogError( ERROR_ON_SPEECH_RECOGNIZER_IS_NULL );
    }
    /// <summary>
    /// метод-приёмник ошибок в работе библиотек распознавания голоса
    /// </summary>
    /// <param name="message"></param>
    protected void onError( string message ) {
        if ( BaseSpeechRecognizer._instance != null )
            if ( BaseSpeechRecognizer._instance.errorMessage != null )
                BaseSpeechRecognizer._instance.errorMessage.Invoke( message );
            else
                UnityEngine.Debug.LogWarning( "use setCrashMessagesRecieverMethod method!" );
        else
            UnityEngine.Debug.LogError( ERROR_ON_SPEECH_RECOGNIZER_IS_NULL );
    }
    /// <summary>
    /// получаем актуальный словарь
    /// </summary>
    /// <param name="language">язык словаря</param>
    /// <param name="grammars">список слов для внесения в словарь</param>
    /// <param name="keyword">ключевое слово</param>
    /// <returns>актуальный словарь (слово, транскрипция)</returns>
    protected Dictionary<string, string> getWordsPhones( string language, ref GrammarFileStruct [ ] grammars, ref string keyword ) {
        if ( language != string.Empty ) {
            string dictName = string.Empty;
            switch ( language ) {
                case Language.en_US: dictName = "EngDictionary"; break;
                case Language.es_ES: dictName = "EspDictionary"; break;
                case Language.fr_FR: dictName = "FrDictionary"; break;
                case Language.de_DE: dictName = "GerDictionary"; break;
                case Language.it_IT: dictName = "ItDictionary"; break;
                case Language.ru_RU: dictName = "RuDictionary"; break;
                // new
                case Language.hi_IN: dictName = "HiDictionary"; break;
                case Language.nl_NL: dictName = "DutDictionary"; break;
                case Language.pt_PT: dictName = "PtDictionary"; break;
                // not implemented
                case Language.zh_CN: dictName = "ZnDictionary"; break;
            }
            if ( dictName != string.Empty ) {
                var baseDict = readDictionaryFromResources( dictName );
                if ( baseDict == null )
                    this.onError( "getWordsPhones empty dict" );

                checkRegistr( baseDict );
                var actualDict = getActualDictionary( ref baseDict, ref grammars, ref keyword );

                foreach ( var pair in _unknownWords ) {
                    var k = ( _graphemeIsLower ) ? pair.Grapheme.ToLower( ) : pair.Grapheme.ToUpper( );
                    // не актуально для ряда словарей - встречаются буквы в разном регистре
                    //var v = ( _phonemeIsLower ) ? pair.Phoneme.ToLower( ) : pair.Phoneme.ToUpper( );
                    var v = pair.Phoneme;
                    actualDict [ k ] = v;
                }
                return actualDict;
            } else
                return null;
        } else
            return null;
    }
    /// <summary>
    /// результат инициализации speechRecognizer
    /// </summary>
    protected bool _init = false;
    /// <summary>
    /// интервал в милисекундах
    /// </summary>
    protected float _interval = 50;
    /// <summary>
    /// статическая ссылка на самого себя чтобы сборщик мусора не уничтожал его
    /// </summary>
    protected static BaseSpeechRecognizer _instance = null;

    protected const string TRUE = "true";
    protected const string FALSE = "false";

    protected const string ERROR_ON_INIT = "crash on init ";
    protected const string ERROR_ON_ADD_GRAMMAR = "crash on add grammar ";
    protected const string ERROR_ON_ADD_WORD = "crash on add word into dictionary ";
    protected const string ERROR_ON_SWITCH_GRAMMAR = "crash on switch grammar ";
    protected const string ERROR_ON_START_LISTENING = "crash on start listening ";

    protected const char DELIMITER = ' ';

    protected bool _isListening = false;
    protected abstract void setKeywordThreshold( double pValue = 1e+10f );
    /// <summary>
    /// считывание полного(базового) словаря из ресурсов
    /// </summary>
    /// <param name="dictName">имя словаря</param>
    /// <returns>словарь (слово, транскрипция)</returns>
    private Dictionary<string, string> readDictionaryFromResources( string dictName ) {
        var rm = new ResourceManager( "MultiplatformSpeechRecognizer.Dictionaries", Assembly.GetExecutingAssembly( ) );
        if ( rm != null ) {
            var dataText = rm.GetString( dictName );
            var transriptionContainer = dataText.TrimEnd( '\n' ).Split( '\n' ).ToDictionary( item => item.Split( DELIMITER )[ 0 ], item => item.Remove( 0, item.IndexOf( " " ) + 1 ) );
            return transriptionContainer;
        } else
            return null;
    }
    /// <summary>
    /// формируем актуальный словарь
    /// </summary>
    /// <param name="dict">полный(базовый) словарь </param>
    /// <param name="grammars">слова для внесения в актуальный словарь</param>
    /// <param name="keyword">ключевое слово</param>
    /// <returns>актуальный словарь со словами из GrammarFileStruct</returns>
    private Dictionary<string, string> getActualDictionary( ref Dictionary<string, string> dict, ref GrammarFileStruct [ ] grammars, ref string keyword ) {
        var actualDict = new Dictionary<string, string>( );

        foreach ( var grammar in grammars ) {
            foreach ( var w in grammar.commands ) {
                var wLst = w.Split( DELIMITER );
                foreach ( var word in wLst ) {
                    var dictKey = string.Empty;
                    if ( _graphemeIsLower )
                        dictKey = word.ToLower( );
                    else
                        dictKey = word.ToUpper( );
                    if ( dict.ContainsKey( dictKey ) ) {
                        grammar.replace( word, dictKey );
                        if ( !actualDict.ContainsKey( dictKey ) ) {
                            actualDict.Add( dictKey, dict [ dictKey ] );
                        } else
                            this.onRecieveLogMess( "dictionary already contains word [" + dictKey + "]" );
                    } else
                        this.onError( "dict not contains:" + dictKey );
                }
            }
        }
        if ( _graphemeIsLower )
            keyword = keyword.ToLower( );
        else
            keyword = keyword.ToUpper( );
        if ( keyword != string.Empty ) {
            if ( dict.ContainsKey( keyword ) ) {
                if ( !actualDict.ContainsKey( keyword ) )
                    actualDict.Add( keyword, dict [ keyword ] );
            }
        }
        return actualDict;
    }

    private const string ERROR_ON_SPEECH_RECOGNIZER_IS_NULL = "BaseSpeechRecognizer singleton null";
    private void OnDestroy( ) {
        this.stopListening( );
    }

    private List<PairG2P> _unknownWords = new List<PairG2P>( );

    private void checkRegistr( Dictionary<string, string> wordsDict ) {
        var middleIndex = wordsDict.Count / 2;
        var key = wordsDict.ElementAt( middleIndex ).Key;
        var val = wordsDict.ElementAt( middleIndex ).Value;
        _graphemeIsLower = char.IsLower( key, 0 );
        _phonemeIsLower = char.IsLower( val, 0 );
    }
    private bool _graphemeIsLower  = true;
    private bool _phonemeIsLower = true;


}
