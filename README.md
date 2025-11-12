# Animated Map (moveVis)

MoveApps

Github repository: *https://www.github.com/movestore/animation-moveVis*

## Description

Generate an animation of your tracks using the [moveVis](https://movevis.org/) 
R package. The animation is saved as an artefact that can be downloaded to your
device. (Note: depending on the size of your data set this App may take a while
to run [30+ min, see logs].)

## Documentation

To generate an aligned animation of all tracks of the input data set,
first all individual tracks are aligned by time and subsampled to a
defined resolution (see settings below). Then all tracks are plotted as
moving dots on a common map. Tracks can optionally be coloured by
their IDs or by their values for another attribute present in the input
data. The frames of all individual tracks are finally combined and
written as an animated file to download as MoveApps output artefact. The
original data set is also passed on as output to a possible next App.

### Application scope

#### Generality of App usability

This app was designed to support any point-location movement data.

#### Required data properties

This app is applicable for data that contain multiple point locations that
can be linked by individual, deployment, or tag. Each individual, deployment,
or tag being animated should have at least 2 (preferably more) fixes.

### Input type

`move2::move2_loc`

### Output type

`move2::move2_loc`

### Artefacts

`animation_moveVis.***`: Animated file showing an animation of all
tracked positions aligned by time.

### Settings

**Temporal resolution for alignment (`res`):** This parameter allows the
user to define the time interval by which the tracks will be thinned for
alignment. This can either be one of "mean", "minimum", "maximum", or
"median" (to calculate the alignment resolution from the temporal
information in the data) or it can be a numeric value. If a numeric
value, it is used as the alignment resolution directly along with an
associated unit provided below (see `unit`).

**Unit of your alignment resolution (`unit`):** Parameter to select a
time unit for alignment resolution. One of "seconds", "minutes",
"hours", or "days".

**Speed of animation (`fps`):** Number of frames to be shown per second
in the output animation. Default is 25.

**Map type (`map_type`):** Selection of map type for the animation
background. Several map providers are available. Some providers 
([Stamen](https://maps.stamen.com/#terrain/12/37.7706/-122.3782),
[Stadia](https://stadiamaps.com/), 
[Thunderforest](https://www.thunderforest.com/), 
[Mapbox](https://www.mapbox.com/), and 
[MapTiler](https://www.maptiler.com/)) require an associated API key,
which you must have obtained from the map provider website prior to
running the App (see `map_token` below). Examples of available basemaps can
be found on the following pages:

- [Stamen and Stadia](https://stadiamaps.com/explore-the-map/#map=7/52.3/0)
- [Thunderforest](https://www.thunderforest.com/maps/)
- [Mapbox](https://www.mapbox.com/maps#map-styling)
- [MapTiler](https://www.maptiler.com/maps/#style=streets-v2&lang=auto&mode=2d&position=5.86/51.328/10.454)

Examples can also be found on the 
[leaflet-extras demo page](https://leaflet-extras.github.io/leaflet-providers/preview/).
Note that not all of the styles provided by a given map service are
supported in this App.

**API Key for the given map provider (`map_token`)** API key used to
access maps from providers that require API authorization.

**Background map extent (`x_ext`, `y_ext`):** Geographic extent of the
background map used in the animation. `x_ext` controls the 
extent of the bounding box in the x dimension, while `y_ext` controls the 
extent in the y dimension. (In a geographic coordinate system, these correspond
to longitude and latitude, respectively.) If either (or both)
are left NULL, the geographic extent of the track data will be used for 
that dimension. These must be in the same CRS as the input data, which
will also be the output CRS for the map.

**Track colour option (`col_opt`):** Method to use when selecting colours for
the tracks in the animation. Default is `one` (i.e. all tracks
have the same colour). If `"trackid"`, colours each track ID differently.
If `"other"`, colours based on another attribute in the data (see 
`colour_paths_by` below).

**Other attribute for track colouring (`colour_paths_by`):** Name of the
event or track attribute to use when colouring tracks in the output map.
This must exactly match the name of the attribute in the input data.
Only used if `col_opt` is set to `"other"`. If an attribute of the same
name exists in both the event and track data, the values in the track 
data are used.

**Palette (`path_pal`)** Name of the colour palette used to colour the
tracks in the output map. Ignored if `col_opt` is set to `"one"`. Examples
of many available palettes can be found 
[here](https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html).

**Include legend (`path_legend`):** Should a legend showing the track
colours be shown? By default, a legend is included.

**Caption (`caption`):** Text for a caption that will be added at the
bottom of the animation window. By default, no caption is included.

**Output file format (`file_format`):** File format for the output animation
file. The following video formats are available: "mp4", "gif", "mov", "flv", 
"avi", "mpeg", "3gp", "ogg". Compatibility depends on computer system. Note 
that the MoveApps system may not render mpeg animations smoothly. We suggest 
using other formats where possible. Default is "mp4".

**Animation output dimensions (`width`, `height`):** Width and height of the
output animation, in centimeters. By default, creates a 15 centimeter by 15 
centimeter animation. Depending on the dimensions of the map
generated for the animation, the default settings may force plot elements
to overlap or be clipped. In this case, you can modify the dimensions here.
Maps are rendered at 300 ppi resolution.

**Hide attribution (`hide_attribution`):** Toggle to remove the basemap tile
attribution label included by default in the output map. Note that if you elect
to remove this label it is your responsibility to ensure the map tiles are 
cited appropriately (e.g. in the map caption). The App logs will include a 
line with the basemap tile citation information, if needed.

### Changes in output data

The input data remain unchanged.

### Most common errors

The alignment resolution `res` must be compatible with the temporal
characteristics of the input data. For instance, attempting to align 
using a resolution that approaches (or exceeds) the overall length of an 
individual track will reduce certain tracks to a single point, producing an
error. On the other hand, fine-grained alignment resolutions may lead to 
memory limit errors given the large number of frames required 
to render the animation. Note that the default resolution may not be appropriate
for all data sets; you may need to experiment with different values to determine
the ideal temporal resolution for your data.

If using a basemap from Stamen, Stadia, Thunderforest, Mapbox, or MapTiler,
you must also provide an API key to `map_token`. If you do not provide a key
or provide an invalid key, the App will fail.

If colouring by an attribute in the input data (`col_opt = "other"`), the name
of the attribute to use for colouring (`colour_paths_by`) must match the 
column name for that attribute in the input data exactly. Any misspellings 
will prevent the attribute from being located in the data and the App will fail.

If providing a custom geographic extent for the output map (`x_ext`, `y_ext`),
the units of that extent must be relative to the CRS of the input data.
Providing values relative to a different coordinate system can produce
geometries that are invalid or fail to overlap with the extent of the input 
data. If you want your animation to have a different output CRS, use the 
'Project CRS' app in your workflow to reproject the input data first.

### Null or error handling:

**Data:** The full input data set is returned for further use in a next
App and cannot be empty.

**Unit of your alignment resolution (`unit`):** This unit parameter
defaults to `"hours"`, which might not be appropriate given the temporal
resolution of your input data. In this case, you may need to select a
different unit or use a data-driven input for `res`.

**Map service API key (`map_token`):** If you select a map 
type from a provider that requires an API key, you must provide an API key
to `map_token`. If left NULL, the default basemap (OSM Topographic)
will be used instead. Map providers that require an API key are: Stamen, Stadia,
Thunderforest, Mapbox, and MapTiler. You can learn more about getting an API 
key on the map provider's website.

**Background map extent (`x_ext`, `y_ext`):** If either `x_ext`,
`y_ext`, or both are left NULL, the geographic extent of the track data 
will be used as the extent for the empty dimension. Further, providing these
values in a different CRS than the CRS of the input data can easily produce
invalid geometric calculations and cause errors. In some cases, invalid
extent geometries will be caught and the default map extent will be used.
In other cases (e.g. if the provided extent does not overlap with the extent
of the input data), the app will fail.

**Attribute for track colouring (`colour_paths_by`):** If the track colour option
is set to colour by event or track attribute, the attribute must be provided
here. If NULL or if the entered attribute does not exist in the input data,
an error will be thrown indicating that the column was not found.
