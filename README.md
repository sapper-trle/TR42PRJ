# TR42PRJ
A program for the Tomb Raider Level Editing (TRLE) community.

Converts a Tomb Raider 4 .TR4 file to a TRLE project .prj file

aktrekker's TR2PRJ program doesn't get the geometry correct especially for some split sectors.
This is an attempt to do a better job.

Advantages over TR2PRJ:
- More accurate geometry maybe
- TGA file alpha channel handled correctly
- Open Source

Disadvantages:
- Only for TR4 files
- No door info extracted - must manually reconnect all rooms
- No objects extracted
- No lights extracted
- No box info extracted (how aktrekker do this?)
- No texturing info extracted (how aktrekker do this?)
- No triggers extracted

This is a Delphi XE7 project.

Delphi 10.2 Starter Edition is currently available for free.
https://www.embarcadero.com/products/delphi/starter/promotional-download

The Vampyre Imaging Library is required for .TGA support.
Download and change the Search Path in the Project Options setting to location of Imaging Library.
Use the version form the Mercurial repository since the latest release version does not compile with Delphi XE7.
http://sourceforge.net/p/imaginglib/code/ci/default/tree/

# Usage:
Open .TR4 file. The room textures will be displayed.
Click Save As to save the .prj file and .tga file.
