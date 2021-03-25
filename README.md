# Pharo Virtual Reality IDE (PharoVRIDE)
A Virtual Reality Interactive Development Environment made in Unity that supports Pharo programming language.

<i><b>Note:</b> This is still an early WIP, so there could be lot of features missing</i>

<a href="https://github.com/Vito217/PharoVRIDE/releases/tag/v0.24.0">Download PharoVRIDE v0.24.0</a>

![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-07-15.png)
![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-10-27.png)
![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-10-28.png)

### Example Video:

[![Alt text for your video](https://img.youtube.com/vi/rulEHAiN-3M/0.jpg)](http://www.youtube.com/watch?v=rulEHAiN-3M)

## What is PharoVRIDE?

PharoVRIDE is an attempt to make an entire IDE inside a VR game. The main goal of this project is having a big and personalized space, where the programmer can not only develop software in Pharo, but also see and interact with 3D representations of the IDE and Objects in real time.

Our main goals for this project are:
<ul>
  <li>Having one or multiple instances of interactable widgets, like a text editor, system browser, inspector, and debugger.</li>
  <li>Being able to save your environment and continue from where you left.</li>
  <li>Being able to write and see the execution of your project on the go, both in the same shared space.</li>
  <li>Having 3D visualization of your code, such as an object's size, performance, etc.</li>
  <li>Connecting with other programmers on the same space</li>
</ul>

It works much like the Pharo IDE: create and search for classes using the System Browser, instantiate objects using the Playground, and inspect their variables using the inspector.

### The System Browser

Just like Pharo's System Browser, you can use the text editor to create your own classes. In order to do so, write your piece of code and execute the corresponding shorcut. Classes are shown in the left window, while their methods are shown in the right window. Once you select one of the objects, you can see its source code.

![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-04-22-(2).png)
![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-04-22-(3).png)
![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-05-02.png)

### The Playground
Once you've defined your own objects, or if you just want to play with some basic operations, write some code using the playground's text editor. Depending on the shorcut you use, you can either execute your code or print the result after selecting a statement.

![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-04-22-(4).png)
![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-05-02_(1).png)

## How does it work?

### Executing Pharo code
The software uses a C# script that makes HTTP request to a Pharo server. The server is initialized using the Zinc HTTP package. Take this line as an example:

```
ZnReadEvalPrintDelegate startInServerOn: 1701.
```

![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-04-22.png)

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

![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-04-22-(7).png)

Once a new class or method is created, a new Unity GameObject is created and placed in its corresponding scrollable window.

### Working with prefabs

Each element is instantiated as a prefab, and most of the prefabs are Canvas. The most relevant prefabs are the following:

<ul>
  <li>Scrollable Window: it is adapted to fit its width and height to its content. It can be used as a class container and a method container</li>
  <li>Text Editor: requires Text Mesh Pro, which uses Rich Text in order to produce Text Highlighting</li>
</ul>

![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-04-22-(6).png)
![alt text](https://github.com/Vito217/PharoVRIDE/blob/master/Screenshots/2020-04-22-(5).png)

### Text Highlighting

The text highlightning is based on regular expressions. You can specify a list of keywords to be used, and define regular expressions that matches those keywords.

## What is currently working:

<ul>
  <li>Browser Text Editor and Scrollable Windows: define classes or methods, and select them to see their sourcecode.</li>
  <li>Playground: writes code and prints its result.</li>
  <li>Inspector: inspect a variable and its value.</li>
  <li>Some Roassal2 RTGraphs as SVG and PNG.</li>
  <li>Some Roassal3 RSCanvas as PNG.</li>
  <li>Desktop Mode controls (mouse + keyboard)</li>
  <li>VR Mode controls for HTC VIVE</li>
</ul>

## What is NOT entirely working (a.k.a. TODO)

<ul>
  <li>A list of bugs (shown below).</li>
</ul>

## List of Bugs

<ul>
  <li>Some details in the text editor, such as incorrect highlightning, coloring, blank spaces, etc.</li>
  <li>Not every SVG extracted from Roassal2 and Roassal3 is compatible with Unity. In that case, an [Error] code wil be thrown.</li>
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

### HTC VIVE Cosmos

* Y or B: Open Menu.
* Left/Right trigger: Pointer Click/Drag.
* Left/Right grip: Teleport, Grab 3D Objects.

All previous commands can be executed using the VR and physical keyboards.

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

If it doesn't work on Mac, you can manually enter the IP address:

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

<a href="https://github.com/Vito217/PharoVRIDE/releases/tag/v0.25.0">Download PharoVRIDE v0.25.0</a>
