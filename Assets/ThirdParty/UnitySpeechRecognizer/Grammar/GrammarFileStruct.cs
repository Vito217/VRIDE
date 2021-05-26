using System.Text;
/// <summary>
/// структура описывающая файл грамматики
/// </summary>
[System.Serializable]
public class GrammarFileStruct {
    /// <summary>
    /// имя файла грамматики
    /// </summary>
    public string name;
    /// <summary>
    /// массив слов
    /// </summary>
    public string[ ] commands;
    /// <summary>
    /// преобразует структуру в формализованную строку грамматики
    /// </summary>
    /// <returns>формализованная строка</returns>
    public string toString( ) {
        var sb = new StringBuilder( );
        sb.Append( "#JSGF V1.0;" );
        sb.Append( "grammar commands;" );
        sb.Append( "public <command> = " );

        for ( int i = 0; i < commands.Length; i++ ) {
            sb.Append( "(" );
            sb.Append( commands [ i ] );
            sb.Append( ")" );
            if ( i != commands.Length - 1 )
                sb.Append( " | " );
        }
        sb.Append( ";" );
        return sb.ToString( );
    }
    /// <summary>
    /// Костыль. Необходим для формирования грамматики для 
    /// словарей с различными регистром 
    /// (приведение всех словарей к нижнему регистру не работает)
    /// Меняем слово грамматики на слово из словаря с учетом его регистра
    /// </summary>
    /// <param name="from">слово грамматики</param>
    /// <param name="to">слово словаря</param>
    /// <returns></returns>
    public bool replace( string from, string to ) {
        for ( int i = 0; i < commands.Length; i++ ) {
            commands [ i ] = commands [ i ].Replace( from, to );
        }
        return true;
    }
}


