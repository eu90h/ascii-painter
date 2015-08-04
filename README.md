# ascii-painter
Ascii-painter is a toolkit (released under the MIT license) that allows you to make roguelike maps out of ascii tiles. 

Ascii-painter grew out of a roguelike game I'm writing. I needed a way to easily hand-place tiles in certain maps.
In ascii-painter, you can use random map generators to fill out most of the map, then using the brush tools, specially
place tiles in the exact area you need them.

This tool would be good for creating static city/town levels for instance.

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

Binaries
--------
[Windows](https://www.dropbox.com/s/d7yp4i9v6sm85kk/ascii-painter-win.zip?dl=0)
[OSX](https://www.dropbox.com/s/b504p9e9sy7uq4j/ascii-painter-mac.zip?dl=0)

Usage
-----
Ascii-painter requires @jpverkamp's ascii-canvas library.

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
