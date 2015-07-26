# ascii-painter
Ascii-painter is a toolkit that allows you to make roguelike maps out of ascii tiles. 

Ascii-painter grew out of a roguelike game I'm writing. I needed a way to easily hand-place tiles in certain maps.
In ascii-painter, you can use random map generators to fill out most of the map, then using the brush tools, specially
place tiles in the exact area you need them.

This tool would be good for creating static city/town levels for instance.

Usage
-----
run 'racket main.rkt'.
Ascii-painter requires jpverkamp's ascii-canvas library.

Features
--------
* Brushes (Paintbrush, single painter, line brush)
* Generators (Fill map, Random Placement, Random Rectangle)
* Full color support
* All glyphs in code-page 437 supported
* Saving and loading of maps

Upcoming Features
-----------------
* More random map generators
* Circle brush

Known Bugs
----------
* Error with room connector (don't use it for now, just paint a connection between them)
* Changing a tiles description doesn't update the tile drop-down list.

Credits
-------
* Thanks to jpverkamp for ascii-canvas
