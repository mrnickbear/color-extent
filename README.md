# color-extent
Approximates the QGIS symbology "min/max value settings", using "Updated canvas" for the Statistics extent

What I'm trying to do is make a web map that approximates the effect of a QGIS document, where the lidar color theme adjusts to the current extents automatically.  That way the full range of colors are always used, providing better contrast. This is saved in QGIS symbology "min/max value settings", using "Updated canvas" for the Statistics extent.  This adds a second or two delay to the display of the Tif panning/zooming, which is an acceptable tradeoff for the sake of adjusting the color ramp to fit the values shown on the screen.

