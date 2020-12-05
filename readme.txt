TR42PRJ 0.80 readme - sapper Nov 2020

A program for the Tomb Raider Level Editor (TRLE) community.

Converts a Tomb Raider 4 .TR4 file to a TRLE project .prj file

Bugfix: Texture sounds now set to default of stone. Earlier versions were setting to mud.

Usage:

Open .TR4 file. The room textures will be displayed.

Click Save As to save the .prj file and .tga file.

Only geometry and doors will be extracted to the .prj.

Opacity (the kind you cannot pass through) setting will also be extracted. But in
some cases of Opacity between stacked rooms some sectors may be incorrectly set
and display incorrectly. Clicking "Draw Doors" in NGLE will fix those sectors.

If you load a TR2PRJ project created from the same .TR4 then you have the option
to import the textures and/or light info from that project.

To save the .tga file only, right click on the image and select Save TGA.

Notes:
The geometry will not be perfect. Tall split sectors cannot be decompiled exactly
for instance.

Also, sometimes NGLE will switch wall sectors next to doors to floor sectors.
Need to convert back to wall sectors in NGLE in that case.

The .prj file will generate some error messages when loaded in the level editor.
  A "Broken map at room 0" error will always appear. This will be auto repaired.
  The cause of this error is known. It is required so the editor links all the rooms
  for moving in the 2D map display.

  Texture importing will generate additional errors that NGLE also reports as fixed.
  These errors are logged in "error.log" in the NGLE folder.

  TR42PRJ will create a text file with same name as the saved prj file which will
  describe the cause of some of these errors.

If aktrekker reading this, please consider releasing TR2PRJ source code.

Sourcecode and releases available on GitHub from sapper-trle.

