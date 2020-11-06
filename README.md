# TR42PRJ
A program for the Tomb Raider Level Editing (TRLE) community.

Converts a Tomb Raider 4 .TR4 file to a TRLE project .prj file

![Screenshot](./Screenshot.jpg)

aktrekker's TR2PRJ program doesn't get the geometry correct especially for some split sectors.
This is an attempt to do a better job.

Advantages over TR2PRJ:
- More accurate geometry maybe
- TGA file alpha channel handled correctly
- Open Source

Disadvantages:
- Only for TR4 files
- No objects extracted
- No lights extracted
- No texturing info extracted (how aktrekker do this?)
- No triggers extracted

This is a Delphi 10.3.3 project.

Delphi 10 Community Edition is currently available for free.
https://www.embarcadero.com/products/delphi/starter

The Vampyre Imaging Library is required for .TGA support.
Download and change the Search Path in the Project Options setting to location of Imaging Library.
Use the version from the Mercurial repository since the latest release version does not compile with Delphi XE7.
http://sourceforge.net/p/imaginglib/code/ci/default/tree/

# Usage:
Open .TR4 file. The room textures will be displayed.

Click Save As to save the .prj file and .tga file.

If you load a TR2PRJ project created from the same TR4 then you have the option to import the textures or light info from that project.
Note from version 0.80 doors will be extracted, no longer need to import them.
