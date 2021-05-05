# Virtual Reality Interactive Development Environment (VRIDE)
A VR IDE made in Unity that supports Pharo and Python programming language.

<i><b>Note:</b> This is still an early WIP, so there could be lot of features missing</i>

<h1><a href="https://github.com/Vito217/VRIDE/releases/tag/v0.29.0">Download VRIDE v0.29.0</a></h1>
<h1><a href="https://github.com/Vito217/VRIDEStreamer/releases/tag/v1.1.0">Download VRIDE Streamer App v1.1.0</a></h1>

![alt text](https://github.com/Vito217/VRIDE/blob/master/Screenshots/2020-07-15.png)
![alt text](https://github.com/Vito217/VRIDE/blob/master/Screenshots/2020-10-27.png)
![alt text](https://github.com/Vito217/VRIDE/blob/master/Screenshots/2020-10-28.png)

### Example Video:

[![Alt text for your video](https://img.youtube.com/vi/rulEHAiN-3M/0.jpg)](http://www.youtube.com/watch?v=rulEHAiN-3M)

## What is VRIDE?

VRIDE is an attempt to make an entire IDE inside a VR game. The main goal of this project is having a big and personalized space, where the programmer can not only develop software in languages like Pharo and Python, but also see and interact with 3D representations of the IDE and Objects in real time.

Our main goals for this project are:
<ul>
  <li>Having one or multiple instances of interactable widgets, like a text editor, system browser, inspector, and debugger.</li>
  <li>Being able to save your environment and continue from where you left.</li>
  <li>Being able to write and see the execution of your project on the go, both in the same shared space.</li>
  <li>Having 3D visualization of your code, such as an object's size, performance, etc.</li>
  <li>Connecting with other programmers on the same space</li>
</ul>

Pharo in VRIDE works much like the Pharo IDE: create and search for classes using the System Browser, instantiate objects using the Playground, and inspect their variables using the inspector. VRIDE also lets you create .py file and execute them.

## How does it work?

### Executing Pharo code
The software uses a C# script that makes HTTP request to a Pharo server. The server is initialized using the Zinc HTTP package. Take this line as an example:

```
ZnReadEvalPrintDelegate startInServerOn: 1701.
```

![alt text](https://github.com/Vito217/VRIDE/blob/master/Screenshots/2020-04-22.png)

A Zinc instruction looks something like this:

```
ZnClient new
    url: 'http://localhost:1701/repl';
    contents: '42 factorial';
    post.
```

Since it is a POST request that has to be done from a C# code, we use HTTP client. Then, the instruction looks like this:

```
using System.Net.Http;
...

public class PharoRequests
{
    private static readonly HttpClient client = new HttpClient();

    async void PharoCall()
    {   
        var content = new StringContent("42 factorial", Encoding.UTF8);

        var response = await client.PostAsync("http://localhost:1701/repl", content);

        var responseString = await response.Content.ReadAsStringAsync();
    }
}
```

Pharo will warn you that there may be linebreaks inside your Pharo code, but it will work anyways.

![alt text](https://github.com/Vito217/VRIDE/blob/master/Screenshots/2020-04-22-(7).png)

Once a new class or method is created, a new Unity GameObject is created and placed in its corresponding scrollable window.

### Executing Python code

VRIDE currently uses IronPython 3.4 Alpha. The code used to run Python is as follows:

```
Microsoft.Scripting.Hosting.ScriptEngine pythonEngine = IronPython.Hosting.Python.CreateEngine();

var paths = new[] {
    Path.Combine(Application.persistentDataPath),
    Path.Combine(Application.streamingAssetsPath),
    Path.Combine(Application.streamingAssetsPath, "Python", "Lib"),
    Path.Combine(Application.streamingAssetsPath, "Python", "Lib", "site-packages"),
    Path.Combine(Application.persistentDataPath, SaveAndLoadModule.username)
};

pythonEngine.SetSearchPaths(paths);

Microsoft.Scripting.Hosting.ScriptScope pythonScope = pythonEngine.CreateScope();

Microsoft.Scripting.Hosting.ScriptSource pythonScript = pythonEngine.CreateScriptSourceFromFile(path);

pythonScript.Execute(pythonScope);
```

### Working with prefabs

Each element is instantiated as a prefab, and most of the prefabs are Canvas. The most relevant prefabs are the following:

<ul>
  <li>Scrollable Window: it is adapted to fit its width and height to its content. It can be used as a class container and a method container</li>
  <li>Text Editor: requires Text Mesh Pro, which uses Rich Text in order to produce Text Highlighting</li>
</ul>

![alt text](https://github.com/Vito217/VRIDE/blob/master/Screenshots/2020-04-22-(6).png)
![alt text](https://github.com/Vito217/VRIDE/blob/master/Screenshots/2020-04-22-(5).png)

### Text Highlighting

The text highlightning is based on regular expressions. You can specify a list of keywords to be used, and define regular expressions that matches those keywords.

```
Dictionary<string, Color> colorDict = new Dictionary<string, Color>()
    {
        { "def", Color.yellow },
        { "if", Color.yellow },
        { "else", Color.yellow },
        ...
    };
    
TMP_TextInfo textInfo = field.textComponent.textInfo;

for (int i = 0; i < textInfo.wordInfo.Length; i++)
{
    try
    {
        TMP_WordInfo wordInfo = textInfo.wordInfo[i];
        string word = wordInfo.GetWord();
        Color color = colorDict[word];
        for (int i = 0; i < wordInfo.characterCount; ++i)
        {
            int charIndex = wordInfo.firstCharacterIndex + i;
            int meshIndex = textInfo.characterInfo[charIndex].materialReferenceIndex;
            int vertexIndex = textInfo.characterInfo[charIndex].vertexIndex;

            Color32[] vertexColors = field.textComponent.textInfo.meshInfo[meshIndex].colors32;
            vertexColors[vertexIndex + 0] = color;
            vertexColors[vertexIndex + 1] = color;
            vertexColors[vertexIndex + 2] = color;
            vertexColors[vertexIndex + 3] = color;
        }
    }
    catch
    {
        continue;
    }
}
field.textComponent.UpdateVertexData(TMP_VertexDataUpdateFlags.All);
```

## What is currently working:

<ul>
  <li>Browser Text Editor and Scrollable Windows: define classes or methods, and select them to see their sourcecode.</li>
  <li>Playground: writes code and prints its result.</li>
  <li>Inspector: inspect a variable and its value.</li>
  <li>Some Roassal2 RTGraphs as SVG and PNG.</li>
  <li>Some Roassal3 RSCanvas as PNG and AFrames</li>
  <li>Basic Python coding, like methods, classes and .py files</li>
</ul>

## What is NOT entirely working (a.k.a. TODO)

<ul>
  <li>Not every SVG extracted from Roassal2 and Roassal3 is compatible with Unity. In that case, an [Error] code wil be thrown.</li>
  <li>Not every Python library is available yet.</li>
</ul>

## Controls

### Keyboard

<ul>
  <li>Left Click: Select and Drag</li>
  <li>Arrows/WASD: Move Player</li>
  <li>Mouse movement: Move Camera</li>
  <li>Shift: Visible cursor mode</li>
  <li>Ctrl/Cmd + P or F4: Print (Playground only)</li>
  <li>Ctrl/Cmd + I or F5: Inspect the selected variable (Playground only)</li>
  <li>Ctrl/Cmd + O + B or F1: Invoke a new Browser</li>
  <li>Ctrl/Cmd + O + W or F2: Invoke a new Playground</li>
  <li>Ctrl/Cmd + O + T or F7: Invoke a new Transcript</li>
  <li>Ctrl/Cmd + S or F6: Accept (Browser only)</li>
  <li>F9: Open Menu</li>
</ul>

### VR Controls

* Y or B: Open Menu.
* Left/Right trigger: Pointer Click/Drag.
* Left/Right grip: Teleport, Grab 3D Objects.

## Installation and Setup

Before opening the application, make sure you have your Pharo image ready. Open Pharo launcher and execute the following code:

```
Author uniqueInstance fullName: 'VRIDE User'.

Metacello new
    baseline: 'Roassal3';
    repository: 'github://ObjectProfile/Roassal3';
    load.    
    
Metacello new
    baseline: 'Roassal3Exporters';
    repository: 'github://ObjectProfile/Roassal3Exporters';
    load.

Metacello new
    baseline: 'Roassal3Exporters';
    repository: 'github://ObjectProfile/Roassal3Exporters';
    load: 'AFrame'.
```

This code does a full installation of Roassal3 and sets up the author. Next, you must run a Pharo ZincHTTP server. Run the following code:

```
ZnReadEvalPrintDelegate startInServerOn: 1701.
```

If you are using Oculus Quest, or if you just want to connect remotely to your local machine, run:

```
(ZnServer on: 1701)
    bindingAddress: NetNameResolver localHostAddress;
    delegate: ZnReadEvalPrintDelegate new;
    start;
    yourself
```

You can also manually enter the IP address:

```
(ZnServer on: 1701)
    bindingAddress: (NetNameResolver addressFromString: 'your IP address');
    delegate: ZnReadEvalPrintDelegate  new;
    start;
    yourself.
```

Now open PharoVRIDE. Right into the first scene, you will see a field asking for the IP address of your Pharo server. It must be as follows:

```
http://<Host name or IP Address>:<Port>/repl
```

If you are running on localhost just as shown in the previous script, press the Enter button to proceed. Otherwise, you must specify an address.

Finally, put on your VR headset, and try some stuff!


## External Resources
* <a href="https://github.com/rjth/Punchkeyboard">Jonathan Ravasz' PunchKeyboard</a>
* <a href="https://assetstore.unity.com/packages/2d/textures-materials/sky/allsky-free-10-sky-skybox-set-146014">Richard Whitelock's AllSkyFree</a>
* <a href="https://github.com/C-Through/VR-XRHands">Andrew L. Connell's XRHands</a>

## Channel Log

<a href="https://github.com/Vito217/PharoVRIDE/blob/master/CHANGELOG.md">Click here to see the Channel Log</a>

## Download

<a href="https://github.com/Vito217/VRIDE/releases/tag/v0.29.0">Download VRIDE v0.29.0</a>
<br>
<a href="https://github.com/Vito217/VRIDEStreamer/releases/tag/v1.1.0">Download VRIDE Streamer App v1.1.0</a>
