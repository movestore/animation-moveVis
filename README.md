# Animated Map (moveVis)

MoveApps

[App GitHub repository](https://www.github.com/movestore/animation-moveVis)

## Description

Generate an animation of your tracks using the moveVis R package. The
animation is saved as an artefact that can be downloaded to your device.
(Note: depending on the size of your data set this App may take a while
to run [30+ min, see logs].)

## Documentation

To generate an aligned animation of all tracks of the input data set,
first all individual tracks are aligned by time and subsampled to a
defined resolution (see settings below). Then all tracks are plotted as
moving dots on a common map (beware that too frequent calls might lead
to empty background for OSM maps). Tracks can optionally be coloured by
their IDs or by their values for another attribute present in the input
data. The frames of all individual tracks are finally combined and
written as an animated file to download as MoveApps output artefact. The
original data set is also passed on as output to a possible next App.

### Input data

move2 location object

### Output data

move2 location object

### Artefacts

`animation.moveVis.***`: Animated file showing an animation of all
tracked positions aligned by time.

### Settings

**Temporal resolution for alignment (`res`):** This parameter allows the
user to define the time interval by which the tracks will be thinned for
alignment. This can either be one of "mean", "minimum", "maximum", or
"median" to calculate the alignment resolution from the temporal
information in the data or it can be a numeric value. If a numeric
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
running the app (see `map_token` below). Since the App can take a long
time to run, we have included some example maptypes below to guide your
decision (note that these examples were created using OSM maps).

**API Key for the given map provider (`map_token`)** API key used to
access maps from providers that require API authorization.

**Resolution of background map (`map_res`):** Resolution of the
background map. Can take values between 0 and 1. Default is 1. Note
that, depending on the extent of your data set, too high or too low
values here can lead to different errors. Please try out different
values here.

**Adaption factor for map extent (`margin_factor`):** Adaption factor
(multiplicative) for map extent. By default, uses an extent slightly larger
than the extent of the data.

**Make plot margins equidistant (`equidistant`):** If `TRUE`, makes
the output map extent (approximately) equidistant in the x- and y-dimensions.
Defaults to `FALSE`.

**Track colour option (`col_opt`):** Options by which attributes to
colour the tracks in the animation. Default is `one`, i.e. all tracks
have the same colour (red).

**Other attribute for track colouring (`colour_paths_by`):** Name of the
event or track attribute to use when colouring tracks in the output map.
This must exactly match the name of the attribute in the input data.
Only used if `col_opt` is set to `"other"`.

**Palette (`path_pal`)** Name of the colour palette used to colour the
tracks in the output map. Ignored if `col_opt` is set to `"one"`.

**Include legend (`path_legend`):** Should a legend showing the track
colours be shown? Default yes.

**Caption (`caption`):** Text for a caption that will be added at the
bottom of the animation window. Default empty.

**Output file format (`file_format`):** Any of the following video
formats: 'mp4', 'gif', 'mov', 'flv', 'avi', 'mpeg', '3gp', 'ogg'.
Compatibility depends on computer system. Default is 'mp4'.

### Example maps

Please note that the example maps have been made with osm maps. You can find
further examples for other map providers on their associated websites.

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
behavior or errors for different adaptation factor values.

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

**Adaptation factor for map extent (`margin_factor`):** There is a known issue
in the moveVis package that causes incorrect margin behavior if `margin_factor`
is less than 0. This only occurs when the "Make plot margins equidistant" option
is checked. If you select this option, we suggest using an adaptation factor
of 1 or higher.

**Attribute for track coloring (`colour_paths_by`):** If the track color option
is set to color by event or track attribute, the attribute must be provided
here. If NULL or if the entered attribute does not exist in the input data,
an error will be thrown indicating that the column was not found.

**Make plot margins equidistant (`equidistant`):** See note for `margin_factor`
above.
