# ascii-painter
Ascii-painter is a toolkit that allows you to make maps out of ascii tiles. 
The saved maps can then be imported into, say, a roguelike game. 

Ascii-painter grew out of a roguelike game I'm writing. I needed a way to easily hand-place tiles in certain scenes.
In ascii-painter, you can use a number of random map generators to fill out most of the map, then using the brush tools, specially
place tiles in the exact area you need them.

This would be good for creating city/town levels for instance, where the layout of the town is static.
Features
--------
* Brushes (Paintbrush, single painter, line brush)
* Generators (Fill map, Random Placement, Random Rectangle)
* Full color support
* All glyphs in code-page 437 supported

Upcoming Features
-----------------
* More random map generators
* Circle brush

Known Bugs
----------
* Error in saving and loading (this will be fixed VERY soon)
* Error with room connector (don't use it for now, just paint a connection between them)
