# Change log

## [2.0.0-preview.12] - 2020-02-13

### Changes

* Added a "Textured Sprite" option to use the SVG's pixels per unit instead of the texture

### Fixes

* Fixed gradients not showing properly on Metal
* Prevent hang while evaluating segment length with large coordinates

## [2.0.0-preview.11] - 2019-12-04

### Fixes

* Fixed CSS class used on root svg element
* Properly handling referenced stop data defined later in the file
* Fixed alpha blending for UI shaders
* Fixed path corners in polygon definitions

## [2.0.0-preview.10] - 2019-11-19

### Fixes

* Added support for RectMask2D
* Fixed sRGB support for UI shaders
* Fixed unmatched BeginSample/EndSample in GenerateAtlas()

## [2.0.0-preview.9] - 2019-11-15

### Fixes

* Fixed invalid VectorExpandEdges.shader path

## [2.0.0-preview.8] - 2019-11-06

### Changes

* Added "uGUI SVGImage" asset type, which sets the proper stencil state for uGUI's masking system

### Fixes

* Fixed VectorImage winding order
* Fixed LibTess preprocessor-dependent namespaces

## [2.0.0-preview.7] - 2019-10-24

### Changes

* Gradient support for strokes

## [2.0.0-preview.6] - 2019-09-23

### Changes

* Changed default gradient resolution from 128 -> 64 pixels to help batching with UIElements renderer

### Fixes

* Using serializable VectorImageVertex (only works on 2019.3b4 and later)

## [2.0.0-preview.5] - 2019-08-01

### Fixes

* Fixed pivot wrongly computed on textured sprites when SVG origin alignement is selected (case 1172332)
* Added preserve aspect ratio in SVGImage

## [2.0.0-preview.4] - 2019-07-08

### Fixes

* Fixed ArgumentOutOfRangeException when closing paths with not enough segments
* Fixed textured-sprite import issue after a full project reimport

## [2.0.0-preview.3] - 2019-06-26

### Changes

* Added a new asset import type (UIElements Vector Image) for upcoming UIElements support in Unity 2019.3.

### Fixes

* Fixed 'T' path instruction not computing the proper reflected control point

## [2.0.0-preview.2] - 2019-06-18

### Changes

There are some breaking changes when going from `1.0.0-preview` to `2.0.0-preview`:

* The deprecated API have been removed.  Most notably, the removal of the `IDrawable` interface.
* The texture atlas layout for the gradient settings was changed.  It now stores gradient settings in the first 3 columns of the atlas instead of the first row.  This allows for a more efficient atlas packing.  If you made a variant of the `Unlit/VectorGradient` shader, you may have to update it.
* The `SVGImporter` will now ignore the root `viewBox` attribute by default.  Use the "Only Apply Root ViewBox" viewport option to bring back the old behavior.

### Fixes

* Fixed flipYAxis in FillMesh API

## [1.0.0-preview.26] - 2019-05-01

### Fixes

* Removed unused variable to fix warning

## [1.0.0-preview.25] - 2019-05-01

### Changes

* Importing SVG from editor code allows local Texture2D references
* URL scheme whitelisting on image tags
* Using next power-of-two texture size, fixes gradient issues on some Android devices

### Fixes

* Fixed pivot not being applied on textured sprite assets
* Fixed SVGParser rotation center in transforms

## [1.0.0-preview.24] - 2019-04-04

### Fixes

* Applying material's color in VectorGradient.shader
* Fixed duplicated points causing issues with polygons
* Added repository information to package.json

## [1.0.0-preview.23] - 2019-01-04

### Changes

* Deprecating Matrix2D.Rotate in favor of RotateRH and RotateLH
* Added support for gradient fills defined later in the file

### Fixes

* Fixed invalid SVG pen position when 'z' command occurs after 'm'

## [1.0.0-preview.22] - 2018-11-16

### Changes

* Proper support for styling in symbols

### Fixes

* Fixed precision issues with dashed strokes
* Fixed modifying node hierarchy while iterating through it
* Fixed CSS data parsing
* Clearing temp render texture when expanding edges
* Fixed instancing for gradient shader

## [1.0.0-preview.21] - 2018-10-23

### Changes

* Added support for borders (slices) for textured sprites
* SVGImageEditor is now fallback custom editor

### Fixes

* Fixed viewport clipping working when viewBox is applied
* Fixed dark outlines when rendering to texture
* Fixed alpha-blending in VectorGradient.shader
* Silenced obsolete warnings because of WWW usage

## [1.0.0-preview.20] - 2018-09-26

### Changes

* Removed "Per-Axis" texture wrap mode

### Fixes

* Fixed sprite value not being set in SVGImage's sprite property

## [1.0.0-preview.19] - 2018-09-24

### Changes

* Using viewBox for relative coordinates, when available

### Fixes

* Fixed issue with gradient user-units when no viewBox is specified

## [1.0.0-preview.18] - 2018-09-21

### Changes

* Improved texture import editor. Better basic tessellation defaults.
* Enabled GPU instancing, _RendererColor works out-of-the-box
* Made the auto-computed tessellation options less aggressive
* Allowing different width/height when importing to a texture
* Moved external libraries to their own namespaces
* Moved the sprite stats over the preview
* Filling atlas with opaque black to help with SVG sprite picking
* Support for sample count (for import-to-texture)
* Setting DtdProcessing to ignore

### Fixes

* Fixed flipped winding order when flipYAxis is false
* Fixed SVGImageEditor for 2018.1

## [1.0.0-preview.17] - 2018-09-13

### Changes

* Support for sprite mesh type on textured sprites

## [1.0.0-preview.16] - 2018-09-13

### Changes

* Node-by-id support
* Added SVGImage for Canvas UI
* Preserve viewport option
* Support for auto-generate physics outline
* Setting Closed=true closes the path connecting the last segment to the first instead of a straight line
* First iteration of "import to texture" feature
* Deprecated Rectangle, Path, Filled and IDrawable. Only Shape remains.
* Improved SVGOrigin and pivot support
* Added support for flipYAxis in FillMesh method
* Multiple object editing improvements

### Fixes

* Fixed relative positioning with viewBox
* Fixed elliptical-arc-to error with large sweep angles
* Fixed polygon winding after transform
* Fixed `<use>` always overriding fill/stroke even when not set
* Fixed "ProhibitDtd" obsolete warning on .NET 4.x backend
* Fixed issues with symbols and patterns usage
* Fixed support for empty 'd' elements
* Fixed issue when symbols are defined after `<use>`
* Fixed invalid SVG Origin when Y-axis is fipped
* Fixed sprite editor align/pivot to not interfere with SVG origin value
* Fixed missing Apply() after atlas generation

## [1.0.0-preview.15] - 2018-07-18

### Changes

* Updated CHANGELOG.md

## [1.0.0-preview.14] - 2018-07-17

### Changes

* Added QuadraticToCubic helper method

### Fixes

* Taking pixels-per-unit into account to compute tessellation settings
* Fixed rgb() color attributes not parsed properly
* Early exit when trying to tessellate paths without enough segments
* Fixed viewbox computation that were lost during tessellation
* Fixed namespace issues with 2018.3+
* Skip stroke tessellation if the width is 0

## [1.0.0-preview.13] - 2018-06-11

### Changes

* Showing imported sprites stats

### Fixes

* Elements with display:none are not displayed anymore
* Fixed parse issue when loading an unsupported texture from the image tag

## [1.0.0-preview.12] - 2018-06-07

### Fixes

* Using culture invariant float parsing
* Fixed import error when using percentage sizes in svg tag

## [1.0.0-preview.11] - 2018-06-05

### Fixes

* Fixed some precision issues
* More conservative processing of 'none' for 'stroke-dasharray'
* Revert "Fixed handling of 'none' styles"

## [1.0.0-preview.10] - 2018-05-23

### Fixes

* Adjusting the triangle's winding order after scene tessellation

## [1.0.0-preview.9] - 2018-05-15

### Changes

* Renamed Third-Party Notices

### Fixes

* Fixed handling of 'none' styles

## [1.0.0-preview.8] - 2018-05-05

### Changes

* Support for multiple SVG editing
* Updated documentation after docs team revision

## [1.0.0-preview.7] - 2018-04-26

### Changes

* Optimized path for convex shapes

### Fixes

* Fixed SVG StreamReader not being closed
* Fixed polyline corners

## [1.0.0-preview.6] - 2018-04-24

### Changes

* Physics outline fixes and using preview texture for Sprite Editor, when available
* Improved sampling step distance tooltip text
* Removed skin-based animation tools

## [1.0.0-preview.5] - 2018-04-18

### Changes

* Added Third-Party Notices
* Added MakeArc_MakesArcInClockwiseDirection test

## [1.0.0-preview.4] - 2018-04-13

### Changes

* MakeArc now returns a BezierPathSegment[] instead of BezierSegment[].  Added BezierSegmentsToPath API.
* Using the new code naming conventions (CamelCase for properties)

## [1.0.0-preview.3] - 2018-04-09

### Changes

* Exposed BuildRectangleContour API

## [1.0.0-preview.2] - 2018-04-05

### Changes

* Moved SVGParser to Unity.VectorGraphics namespace

## [1.0.0-preview.1] - 2018-04-04

* Initial release