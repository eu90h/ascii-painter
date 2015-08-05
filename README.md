# ascii-painter
Ascii-painter is a toolkit (released under the MIT license) that allows you to make roguelike maps out of ascii tiles. 

Ascii-painter grew out of a roguelike game I'm writing. I needed a way to easily hand-place tiles in certain maps.
In ascii-painter, you can use random map generators to fill out most of the map, then using the brush tools, specially
place tiles in the exact area you need them.

This tool would be good for creating static city/town levels for instance.

Binaries
--------
OS X - [ascii-painter.dmg](https://www.dropbox.com/s/px6q44qe999gfa0/ascii-painter.dmg?dl=0)

Windows - [ascii-painter.zip](https://www.dropbox.com/s/632azukuzxyfc4t/ascii-painter.zip?dl=0)

Screenshots
-----------
A little example

![Example Map](https://i.imgur.com/U4cHvWG.png)

A castle map

![Beginnings of a Castle Map](https://i.imgur.com/6c4psVj.png)

All the various shapes:

![Shape Gallery](https://i.imgur.com/7CeFGMN.png)

Picking a color:

![Color Picker](https://i.imgur.com/yjOMfiE.png)

Finally, no Lisp program is complete without Lambdas!

![Lambda](https://i.imgur.com/DG5qYXV.png)

Usage
-----
The default map is 100x60. To change the dimensions, click File->New Map.
The camera is controlled with the arrow keys.

Features
--------
* Brushes (Paintbrush, single painter, shape brush)
* Generators (Fill map, Random Placement)
* Full color support
* All glyphs in code-page 437 supported
* Saving and loading of maps
* Undo (press ctrl-z)

Upcoming Features
-----------------
* More random map generators

Credits
-------
* Thanks to @jpverkamp for ascii-canvas
