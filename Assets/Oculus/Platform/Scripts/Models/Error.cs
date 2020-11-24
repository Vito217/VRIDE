namespace Oculus.Platform.Models
{
    public class Error
  {
    public Error(int code, string message, int httpCode)
    {
      Message = message;
      Code = code;
      HttpCode = httpCode;
    }

    public readonly int Code;
    public readonly int HttpCode;
    public readonly string Message;
  }
}
