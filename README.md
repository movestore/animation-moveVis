# Animated Map (moveVis)

MoveApps

[App GitHub repository](https://www.github.com/movestore/animation-moveVis)

## Description

Generate an animation of your tracks using the [moveVis](https://movevis.org/) 
R package. The animation is saved as an artefact that can be downloaded to your
device. (Note: depending on the size of your data set this app may take a while
to run [30+ min, see logs].)

## Documentation

To generate an aligned animation of all tracks of the input data set,
first all individual tracks are aligned by time and subsampled to a
defined resolution (see settings below). Then all tracks are plotted as
moving dots on a common map. Tracks can optionally be coloured by
their IDs or by their values for another attribute present in the input
data. The frames of all individual tracks are finally combined and
written as an animated file to download as MoveApps output artefact. The
original data set is also passed on as output to a possible next app.

### Input data

move2 location object

### Output data

move2 location object

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
background. Several map providers are available. Some providers (Stamen,
Stadia, Thunderforest, Mapbox, MapTiler) require an associated API key,
which you must have obtained from the map provider website prior to
running the app (see `map_token` below). Since the app can take a long
time to run, we have included some example map types below to guide your
decision. Demos for many of the available map types can also be found
at the map provider's website or [here](https://leaflet-extras.github.io/leaflet-providers/preview/).

**API Key for the given map provider (`map_token`)** API key used to
access maps from providers that require API authorization.

**Resolution of background map (`map_res`):** Resolution of the
background map. Can take values between 0 and 1. Default is 1. Note
that depending on the extent of your data set, excessively high or low
values may produce errors.

**Background map extent (`lat_ext`, `lon_ext`):** Geographic extent of the
background map used in the animation. `lat_ext` controls the latitudinal
extent while `lon_ext` controls the longitudinal extent. If either (or both)
are left NULL, the geographic extent of the track data will be used for 
that dimension. Note that if the
provided extent is very large relative to the extent of the track data, 
spherical geometry calculations may indicate that the extents
do not overlap. If this occurs, you will need to provide a smaller extent.
This is more likely for data in polar latitudes.

**Track colour option (`col_opt`):** Method to use when selecting colours for
the tracks in the animation. Default is `one` (i.e. all tracks
have the same colour). If `"trackid"`, colours each track ID differently.
If `"other"`, colours based on another attribute in the data (see 
`colour_paths_by` below).

**Other attribute for track colouring (`colour_paths_by`):** Name of the
event or track attribute to use when colouring tracks in the output map.
This must exactly match the name of the attribute in the input data.
Only used if `col_opt` is set to `"other"`.

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

**Hide attribution (`hide_attribution`):** Toggle to remove the basemap tile
attribution label included by default in the output map. Note that if you elect
to remove this label it is your responsibility to ensure the map tiles are 
cited appropriately (e.g. in the map caption). The app logs will include a 
line with the basemap tile citation information, if needed.

### Example maps

This is not an exhaustive list of basemaps that can be used in the app. For
examples of other map options, see the websites for the individual map
provider of interest.

#### Stamen Watercolor

![](watercolor_AniMove_map.png)

#### OSM Topographic

![](topographic_AniMove_map.png)

#### Stamen Toner

![](toner_AniMove_map.png)

#### Stamen Terrain

![](terrain_AniMove_map.png)

#### OSM Streets

![](streets_AniMove_map_CoarseScale.png)

### Most common errors

Selecting the "Make plot margins equidistant" option can lead to unexpected
behavior or errors when used alongside certain adaptation factor values.

### Null or error handling:

**Data:** The full input data set is returned for further use in a next
app and cannot be empty.

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

**Background map extent (`lat_ext`, `lon_ext`):** If either `lat_ext`,
`lon_ext`, or both are left NULL, the geographic extent of the track data 
will be used as the extent for the empty dimension. Note that if the
provided extent is very large relative to the extent of the track data, 
spherical geometry calculations may indicate that the extents
do not overlap. If this occurs, you will need to provide a smaller extent.
This is more likely for data in polar latitudes.

**Attribute for track colouring (`colour_paths_by`):** If the track colour option
is set to colour by event or track attribute, the attribute must be provided
here. If NULL or if the entered attribute does not exist in the input data,
an error will be thrown indicating that the column was not found.
