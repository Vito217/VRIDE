## Vector Graphics Package

The Vector Graphics package provides an SVG file importer as well as generic vector graphics APIs.

The SVG importer follows a subset of the [SVG 1.1 specification](https://www.w3.org/TR/SVG11/). Once an SVG file is imported, the vector data is tesselated into triangles and a sprite is generated.  This sprite can then be used by the 2D system.  The source for the SVG importer can be found in the `Editor` folder.

The vector graphics APIs can be used to create and manipulate vector constructs directly in code.  Read the [documentation](Documentation~/vectorgraphics.md) to get more information.  The source for the vector graphics APIs can be found in the `VectorGraphics` namespace under the `Runtime` folder.

![Vector Graphics Screenshot](Documentation~/images/screenshot.png)

Unity version 2018.1 or later is required to use the Vector Graphics package.
