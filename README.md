# Animated Map (moveVis)

MoveApps

Github repository: *https://www.github.com/movestore/animation-moveVis*

## Description

Generate an animation of your tracks using the [moveVis](https://movevis.org/) 
R package. The animation is saved as an artefact that can be downloaded to your
device. (Note: depending on the size of your data set this App may take a while
to run. Consider activating the 'Notification Setting' (bell icon) to receive an 
email when the Workflow instance has finished.)

## Documentation

To generate an aligned animation of all tracks of the input data set,
first all individual tracks are aligned by time and subsampled to a
defined resolution (see settings below). Then all tracks are plotted as
moving dots on a common map. Tracks can optionally be coloured by
their IDs or by their values for another attribute present in the input
data. The frames of all individual tracks are finally combined and
written as an animated file to download as MoveApps output artefact in one of the 
available formats (.mp4, .gif, .mov, .flv, .avi, .mpeg, .3gp, .ogg). The
original data set is also passed on as output to a possible next App.

### Application scope

#### Generality of App usability

This app was designed to support any point-location movement data.

#### Required data properties

This app is applicable for data that contain multiple point locations that
can be linked by individual, deployment, or tag. Each individual, deployment,
or tag being animated should have at least 2 (preferably more) fixes.
If your data set has high temporal resolution, spans over a large time period 
and/or contains a large amount of individuals, pay close attention to the first
two settings (**Temporal resolution for alignment** and **Unit of your alignment 
resolution**).

### Input type

`move2::move2_loc`

### Output type

`move2::move2_loc`

### Artefacts

`animation_moveVis.***`: Animated file showing an animation of all
tracked positions aligned by time.

### Settings

**Generate a single test image (`dry_run`):** Use this parameter to produce 
a single static image instead of a full animation file. Because rendering
a full animation can take time, you can enable this setting
to test that the animation basemap, colors, and other graphical
parameters are set to your liking before rendering a full animation. You can
also check the app logs after rendering a test image to see how long the full
animation would be with the given settings.

**Temporal resolution for alignment (`res`):** This parameter allows the
user to define the time interval by which the tracks will be thinned for
alignment. Provide a numeric value which will be combined with the selected unit
(see **Unit of your alignment resolution** below) to define the alignment resolution.
This determines the temporal intervals at which point locations will be
selected for each track. Track trajectories will then be animated between 
these points. Higher resolution values improve processing time but can make the
animation appear less smooth.

Note that temporal gaps in your data can lead to errors when using units that 
are unsuitable for the temporal resolution of your data. Also note that large 
data sets can take a long time to process at fine-grained resolutions. Defaults
to 1.

**Unit of your alignment resolution (`unit`):** Parameter to select a
time unit for alignment resolution. Accepts "Seconds", "Minutes", "Hours", or
"Days".

**Speed of animation (`fps`):** Number of frames to be shown per second
in the output animation. Defaults to 25.

**Map type (`map_type`):** Basemap to use for the animation
background. Several map providers are available. Some providers 
([Stamen](https://maps.stamen.com/#terrain/12/37.7706/-122.3782),
[Stadia](https://stadiamaps.com/), 
[Thunderforest](https://www.thunderforest.com/), 
[Mapbox](https://www.mapbox.com/), and 
[MapTiler](https://www.maptiler.com/)) require an associated API key,
which you must have obtained from the map provider website prior to
running the App (see **API key for the given map provider** below). 
Examples of available basemaps can be found on the following pages. After each 
provider is the list of currently available maps in this App:

- [OSM](https://opentopomap.org): "OSM Topographic", "OSM Streets", "OSM Streets DE"
- [Stamen and Stadia](https://stadiamaps.com/explore-the-map/#map=7/52.3/0) (API key needed): 
"Stamen Toner", "Stamen Toner BG" ,"Stamen Terrain","Stamen Terrain BG", "Stamen Watercolor", 
"Stadia Alidade Smooth", "Stadia Alidade Smooth Dark", "Stadia Outdoors", "Stadia OSM Bright"
- [Thunderforest](https://www.thunderforest.com/maps/) (API key needed): "Thunderforest Cycle",
"Thunderforest Transport", "Thunderforest Landscape", "Thunderforest Outdoors", 
"Thunderforest Transport Dark", "Thunderforest Spinal", "Thunderforest Pioneer", 
"Thunderforest Mobile Atlas", "Thunderforest Neighborhood", "Thunderforest Atlas"
- [Carto](https://carto.com/basemaps): "Carto Light", "Carto Light No Labels", 
"Carto Dark", "Carto Dark No Labels", "Carto Voyager", "Carto Voyager No Labels", 
"Carto Voyager Labels Under"
- [Mapbox](https://www.mapbox.com/maps#map-styling) (API key needed): "Mapbox Streets",
"Mapbox Outdoors", "Mapbox Light", "Mapbox Dark", "Mapbox Satellite", 
"Mapbox Hybrid", "Mapbox Terrain"
- [ESRI](https://www.arcgis.com/apps/mapviewer/index.html?webmap): "ESRI NatGeo World Map",
"ESRI USA Topo Maps", "ESRI World Imagery", "ESRI World Physical Map", 
"ESRI World Shaded Relief", "ESRI World Street Map", "ESRI World Terrain Base", 
"ESRI World Topo Map", "ESRI World Dark Gray Base", "ESRI World Light Gray Base", 
"ESRI World Hillshade Dark", "ESRI World Hillshade", "ESRI World Ocean Base", 
"ESRI Antarctic Imagery", "ESRI Arctic Ocean Base", "ESRI World Navigation Charts"
- [MapTiler](https://www.maptiler.com/maps/#style=streets-v2&lang=auto&mode=2d&position=5.86/51.328/10.454) 
(API key needed): "MapTiler Aquarelle", "MapTiler Backdrop", "MapTiler Basic", 
"MapTiler Bright", "MapTiler Dataviz", "MapTiler Landscape", "MapTiler Ocean", 
"MapTiler Outdoor", "MapTiler Satellite", "MapTiler Streets", "MapTiler Toner",
"MapTiler Topo", "MapTiler Winter"

Examples can also be found on the 
[leaflet-extras demo page](https://leaflet-extras.github.io/leaflet-providers/preview/).
Note that not all of the styles provided by a given map service are
supported in this App.

**API key for the given map provider (`map_token`)** API key used to
access maps from providers that require API authorization. The providers 
[Stamen](https://maps.stamen.com/#terrain/12/37.7706/-122.3782),
[Stadia](https://stadiamaps.com/), 
[Thunderforest](https://www.thunderforest.com/), 
[Mapbox](https://www.mapbox.com/), and 
[MapTiler](https://www.maptiler.com/)) require an associated API key,
which you must have obtained from the map provider website prior to
running the App.

**Use high-resolution basemap (`high_res`):** Check this box to use
high-resolution basemap tiles in the animation. This can improve the visual
quality of the basemap but will also increase animation processing times. By
default, high-resolution tiles are used. Note that some map providers (e.g. OSM)
do not provide high resolution tiles, in which case this setting will be
ignored.

**Background map extent (`lon_ext`, `lat_ext`):** Geographic extent (separately
for longitude and latitude) of the background map used in the animation. The 
digits should be separated by a space or comma (e.g. "45, 50"). Enter degrees 
west of the Prime Meridian and
south of the equator as negative numbers (e.g. 5 degrees West would be provided 
as "-5"). Avoid setting an extent that is 
very large relative to the extent of the track data, as this may produce invalid 
geometry calculations and cause the app to fail.

If either (or both)
are left blank, the geographic extent of the track data will be used for 
that dimension.

**Track colour option (`col_opt`):** Method to use when selecting colours for
the tracks in the animation. Default is "Single colour (red) for all tracks" 
(i.e. all tracks have the same colour). The option "Coloured by track ID", 
colours each track ID differently. And "Coloured by event or track attribute (define below)", 
colours based on another attribute in the data (see 
**Other attribute for track colouring** below).

**Other attribute for track colouring (`colour_paths_by`):** Name of the
event or track attribute to use when colouring tracks in the output map.
This must exactly match the name of the attribute in the input data. Check the 
previous app's data overview for available attributes and their spelling. If you 
opt to include a legend, it will show the values of the attribute that correspond 
to each colour.

Only used if **Track colour option** is set to "Coloured by event or track attribute". 
If an attribute of the same name exists in both the event and track data, 
the values in the track data are used.

**Palette (`path_pal`)** Name of the colour palette used to colour the
tracks in the output map. Ignored if **Track colour option** is set to 
"Single colour (red) for all tracks". Palettes are labeled with the type of data 
they are designed to represent (qualitative, sequential, or diverging). Note that 
in this app, integer attributes are treated as qualitative as they often
represent ID values.

Available palettes:

- *Qualitative*: "Set 2", "Pastel 1", "Dark 2", "Dark 3", "Set 3", "Warm", "Cold", "Harmonic", "Dynamic"

- *Sequential*: "Grays", "Light Grays", "Blues 2", "Blues 3", "Purples 2", "Purples 3", "Reds 2", 
"Reds 3", "Greens 2", "Greens 3", "Oslo", "Purple-Blue", "Red-Purple", "Red-Blue",
"Purple-Orange", "Purple-Yellow", "Blue-Yellow", "Green-Yellow", "Red-Yellow", "Heat",
"Heat 2", "Terrain", "Terrain 2", "Viridis", "Plasma", "Inferno", "Rocket", "Mako",
"Dark Mint", "Mint", "BluGrn", "Teal", "TealGrn", "Emrld", "BluYl", "ag_GrnYl",
"Peach", "PinkYl", "Burg", "BurgYl", "RedOr", "OrYel", "Purp", "PurpOr",
"Sunset", "Magenta", "SunsetDark", "ag_Sunset", "BrwnYl", "YlOrRd", "YlOrBr", 
"OrRd", "Oranges", "YlGn", "YlGnBu", "Reds", "RdPu", "PuRd", "Purples",
"PuBuGn", "PuBu", "Greens", "BuGn", "GnBu", "BuPu", "Blues", "Lajolla",
"Turku", "Hawaii", "Batlow"

- *Diverging*: "Blue-Red", "Blue-Red 2", "Blue-Red 3", "Red-Green", "Purple-Green", "Purple-Brown",
"Green-Brown", "Blue-Yellow 2", "Blue-Yellow 3", "Green-Orange", "Cyan-Magenta", "Tropic",
"Broc", "Cork", "Vik", "Berlin", "Lisbon", "Tofino", "ArmyRose", "Earth",
"Fall", "Geyser", "TealRose", "Temps", "PuOr", "RdBu", "RdGy", "PiYG",
"PRGn", "BrBG", "RdYlBu", "RdYlGn", "Spectral", "Zissou 1", "Cividis", "Roma"

Examples of many available palettes can be found 
[here](https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html).

**Include legend (`path_legend`):** Should a legend showing the track
colours be shown? By default, a legend is included. Note that the legend may 
be very large if colouring by an attribute that has many levels. In these 
cases, you may want to suppress the legend.

**Caption (`caption`):** Text for a caption that will be added at the
bottom of the animation window. Typically this is the data reference. 
By default, no caption is included.

**Output file format (`file_format`):** File format for the output animation
file. The following video formats are available: "mp4", "gif", "mov", "flv", 
"avi", "mpeg", "3gp", "ogg". Compatibility depends on computer system. Note 
that the MoveApps system may not render mpeg animations smoothly. We suggest 
using other formats where possible. Default is "mp4".

**Animation output dimensions (`out_width`, `out_height`):** Width and height of the
output animation, in centimeters. By default, creates a 15 centimeter by 15 
centimeter animation. Depending on the dimensions of the map
generated for the animation, the default settings may force plot elements
to overlap or be clipped. In this case, you can modify the dimensions here.
Modifying the dimensions will also change the size of the track paths relative
to the basemap.

Maps are rendered at 300 ppi resolution.

**Hide attribution (`hide_attribution`):** Toggle to remove the basemap tile
attribution label included by default in the output map. Note that if you elect
to remove this label it is your responsibility to ensure the map tiles are 
cited appropriately (e.g. in the map caption). You can find relevant citation 
information in the app logs or consult the online user agreement for the 
selected map provider (see **Map type** above). In general, you should only 
need to use this option if the citation label has been cut off by your map 
boundary and you need to display it in the map caption instead.

### Changes in output data

The input data remain unchanged.

### Most common errors

The **Temporal resolution for alignment** and **Unit of your alignment resolution**
settings must be compatible with the temporal characteristics of the input data. 
For instance, attempting to align using a resolution that approaches (or exceeds) 
the overall length of an individual track will reduce certain tracks to a single 
point, producing an error. On the other hand, fine-grained alignment resolutions 
may lead to memory limit errors given the large number of frames required 
to render the animation. Note that the default resolution may not be appropriate
for all data sets; you may need to experiment with different values to determine
the ideal temporal resolution for your data. Try a coarse resolution first, and than 
scale down to get the desired result, especially if you have a data set with high 
temporal resolution, that spans over a large time period and/or contains a large 
amount of individuals.

If using a basemap from Stamen, Stadia, Thunderforest, Mapbox, or MapTiler,
you must also provide an API key to the **API key for the given map provider**
setting. If you do not provide a key or provide an invalid key, the App will 
fail.

If colouring by an attribute in the input data (by setting 
**Track colour option** to "Coloured by event or track attribute"), the name 
of the attribute to use for colouring (**Other attribute for track colouring**)
must match the column name for that attribute in the input data exactly. Any 
misspellings will prevent the attribute from being located in the data and the 
App will fail.

### Null or error handling:

**Unit of your alignment resolution (`unit`):** This unit parameter
defaults to "Days", which might not be appropriate given the temporal
resolution of your input data. In this case, you may need to select a
different unit.

**Map service API key (`map_token`):** If you select a map 
type from a provider that requires an API key, you must provide an API key
to **API key for the given map provider**. If left NULL, the default basemap 
(Carto Voyager) will be used instead. Map providers that require an API 
key are: Stamen, Stadia, Thunderforest, Mapbox, and MapTiler. You can learn 
more about getting an API key on the map provider's website.

**Use high-resolution basemap (`high_res`):** Some map providers (e.g. OSM) 
do not provide high resolution basemap tiles. In these cases, the app should
automatically provide the standard resolution tiles without failure. If
you encounter errors retrieving any basemaps, please notify us (e.g. via a
GitHub issue).

**Background map extent (`lon_ext`, `lat_ext`):** If either 
**Longitude extent of background map**,
**Latitude extent of background map**, or both are left NULL, the geographic 
extent of the track data will be used as the extent for the empty dimension. 
If the provided extent does not overlap with the extent of the input data, 
an error will be thrown.

**Attribute for track colouring (`colour_paths_by`):** If the track colour option
is set to "Coloured by event or track attribute (define below)"", the 
attribute must be provided here. If NULL or if the entered attribute does not
exist in the input data, an error will be thrown indicating that the column 
was not found.
