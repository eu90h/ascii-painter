# ascii-painter
Ascii-painter is a toolkit (released under the MIT license) that allows you to make roguelike maps out of ascii tiles. 

Ascii-painter grew out of a roguelike game I'm writing. I needed a way to easily hand-place tiles in certain maps.
In ascii-painter, you can use random map generators to fill out most of the map, then using the brush tools, specially
place tiles in the exact area you need them.

This tool would be good for creating static city/town levels for instance.
Screenshots
-----------
["Blood River" (located in the examples folder)](https://imgur.com/U4cHvWG)

Usage
-----
run 'racket main.rkt'.
Ascii-painter requires jpverkamp's ascii-canvas library.

The camera is controlled with the arrow keys. The default map is 70x30. To change the dimensions, click File->New Map.

To undo, press z.

Features
--------
* Brushes (Paintbrush, single painter, shape brush)
* Generators (Fill map, Random Placement, Random Rectangle)
* Full color support
* All glyphs in code-page 437 supported
* Saving and loading of maps
* Undo

Upcoming Features
-----------------
* Saving and loading maps using the rexpaint .xp format
* More random map generators

Credits
-------
* Thanks to jpverkamp for ascii-canvas
