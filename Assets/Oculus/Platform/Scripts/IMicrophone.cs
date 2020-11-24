namespace Oculus.Platform
{
    public interface IMicrophone
  {
    void Start();

    void Stop();

    float[] Update();
  }
}
