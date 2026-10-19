FISHERIESXPLORER — INTERACTIVE SPATIAL LANDINGS
Implementation and integration guide
19 October 2026

START HERE

Open Pelagic_piecharts_map.html to try the redesigned map. Use the Landings,
Composition and Species controls, then click a reporting rectangle or choose
one in the details panel. The map, controls and CSV download work offline.

For your R project, copy fisheries_landings_map.R and
example_fisheries_landings.R into the project's R directory. The first is the
complete implementation, including its JavaScript and CSS. The second contains
a replacement for your export loop, a cached-coastline helper, and a Shiny
integration example. Sourcing either file does not load or change your data.

The preview uses the 120 rectangle aggregates already embedded in the supplied
HTML. It preserves those values. Its coordinates cover the Biscay/Iberian
extent, whereas your pasted script selects Celtic Seas. The original HTML does
not retain the selected years, weight units or the species originally grouped
into OTH. Consequently the preview is labelled DATA PREVIEW, source units, and
period not stored. It is a working design demonstration, not a recalculated
2022–2024 Celtic Seas result. Run the revised R workflow with the original
records to produce that result.


1. WHAT THE NEW MAP SHOWS

Landings is the default view. Each ICES reporting rectangle is coloured by its
total mean annual reported landings. A logarithmic log(1 + value) colour scale
keeps low and high values distinguishable; the legend is labelled in the actual
display units. The scale describes values per reporting rectangle, not tonnes
per square kilometre. It is recalculated for the active view and data payload,
so colour darkness alone should not be used to compare separate exported maps.

Composition is a secondary view with equal 34-pixel donuts. Each donut describes
species shares; its size has no quantitative meaning. Keeping the diameter in
screen pixels makes the shares readable when zooming. A deterministic screen
collision check draws a subset at small scales and reveals more as space opens
up. The selected rectangle has priority. Culling a donut never removes its
rectangle, data, regional contribution or export row. All rectangles remain
available from the selector, including those without marine display geometry.

Species colours the reporting rectangles by a chosen displayed species or
group. Clicking a species in the details panel also activates this view.

The linked details panel starts with the regional overview. Selecting a cell
shows its total, temporal completeness, species values and shares. Every share
bar has a common 0–100% scale; the longest bar is not silently stretched to
100%. Labels are rounded for reading; hover titles and CSV retain precision.
Positive tiny values are not shown as an ordinary rounded zero.

Reported zero and unavailable values have separate styles. The map includes a
reset control, optional grid, keyboard-operable rectangle selector and a scroll
zoom switch. Scroll zoom starts off so a map embedded in a longer page does not
capture the reader's normal page scrolling. On small screens or narrow app
containers the details panel stacks below the map.

Download CSV exports all rectangle/category values for the current view. In
Species view it exports the chosen category; the other views export all display
categories. Selecting a cell or panning does not silently limit the export.
The CSV carries method, period, unit, category and rectangle information.


2. HOW COASTAL PLACEMENT WORKS

The geographic unit is the ICES statistical rectangle, 1 degree of longitude
by 0.5 degree of latitude. Optional official rectangle polygons can be supplied;
otherwise validated ICES codes generate the grid. Supplied centre coordinates
are checked against their rectangle IDs.

For each cell, the geometry builder subtracts the supplied land polygons from
the rectangle for display. If the original centre is within the marine part,
it is retained. Otherwise a point inside the largest marine polygon component
becomes the display anchor. The original centre remains in the metadata; when
the rectangle is selected the interface shows the relationship to its original
reporting geometry. Optional manual anchors must be strictly inside the marine
part of their own rectangle or the function stops with an explanatory error.

A wholly land-covered cell is not moved into a neighbouring sea rectangle. It
has no marine polygon or donut anchor, remains in the regional totals, selector,
raw outline and CSV, and is flagged for inspection. In the supplied preview,
23 centres were moved and one rectangle, 15E2, has this no-marine-geometry status
under the Natural Earth mask. Its 1,363 source units remain in the totals.

Clipping is a drawing operation. No landings are apportioned by marine area,
shifted between cells, allocated to the ecoregion polygon, or assigned to a more
precise fishing location. Membership in an ecoregion's rectangle list selects
whole rectangles. A symbol denotes a reporting cell, not a vessel or port.

The guarantee concerns the anchor point. A 34-pixel donut can still overlap the
coast visually at some zoom levels where water is narrow. Increasing coastline
detail or using a carefully chosen valid manual anchor can improve particular
cases; the filled-rectangle view remains the clearest default in coastal areas.

The example's cached Natural Earth 1:50m land layer is appropriate for an
overview. Use a more detailed regional land layer where small islands, inlets or
narrow channels matter. No land argument means no coast checking, and the map
states that limitation. Geometric operations use projected EPSG:3035 by default;
choose a suitable local projected CRS for an application outside its region.


3. PREPARING AND PLOTTING YOUR DATA

Requirements: R 4.0 or newer, dplyr, tidyr, tibble, sf, jsonlite, leaflet,
htmlwidgets and htmltools. Saving a self-contained widget also requires Pandoc,
normally available through RStudio. If Pandoc is unavailable, save with
selfcontained = FALSE and keep the HTML and its dependency directory together.

Install packages once, then use your existing project objects:

    source("./R/fisheries_landings_map.R")
    source("./R/example_fisheries_landings.R")

    lookup <- make_fisheries_guild_lookup(sid)
    land_sf <- load_cached_fisheries_land()

    m <- plot_fisheries_landings(
      data = cl.all2,
      years = 2022:2024,
      weight_unit = "kg",  # Only if the original weight column is kilograms.
      rectangle_ids = ICES_Rect,
      guild_lookup = lookup,
      guild = "Pelagic",
      boundary = shape_ecoregion,
      land = land_sf,
      region = "Celtic Seas",
      top_n = 5,
      annual_policy = "strict",
      species_names = fisheries_species_names,
      species_colors = fisheries_species_colors
    )

    save_fisheries_map(m, "./output/Pelagic_landings_map.html")
    export_fisheries_calculations(m, "./output/Pelagic_calculation")

ICES_Rect above must be a vector of selected rectangle codes, and
shape_ecoregion must be the matching sf boundary. The example script shows how
to reuse your get_csquare() and icesFO::load_ecoregion() inputs outside the loop.

Specify weight_unit explicitly: "kg" converts to tonnes once, "t" keeps tonnes,
and "raw" preserves values with the label source units. No unit is inferred
from a column name. The function sums the supplied weight field. Confirm the
meaning and upstream expansion of CLscientificWeight_perVessel before labelling
its sum as full-fleet landings.

The previous script computed landings value but never used it. This refactor
implements the displayed weight measure. It does not silently present a currency
measure or add a vessel count, neither of which can be defined from the displayed
aggregates alone. A future value view needs its own confirmed currency/unit and
aggregation rules.


4. YEARS, MISSING VALUES AND COMPARABLE MEANS

years is now an actual filter before aggregation. All species in a rectangle
use the same usable years and denominator. This fixes the original pattern of
averaging each species over only the years in which it happened to have rows.

Choose annual_policy deliberately:

  strict (default): A cell is published only if every selected rectangle-year
  is usable. Otherwise the cell is unavailable, not zero. Published cells are
  comparable over the same selected years. The regional total sums only those
  published cells and the panel reports missing cells.

  available: Use the same available year subset for every species within a
  rectangle. The cell reports its actual years and denominator. Different cells
  may then represent different subsets of the requested period.

  zero: Explicitly treat absent rectangle-years as zero recorded landings across
  the selected period. Use only when this interpretation follows from your data
  process. An explicit missing weight or declared incomplete year is never
  converted to zero.

A rectangle-year with any source rows establishes presence in the supplied
regional dataset, before filtering to the selected guild. Within a usable year,
species absent from those records contribute zero to the recorded total. This
is a rule about the supplied dataset, not proof that all countries or fleets
reported. If any selected-guild weight is explicitly NA, that year is unusable
for all selected species, preserving coherent totals and shares. Text suppression
codes must be resolved upstream; they are rejected rather than coerced to zero.

For a stronger declaration, supply coverage as a data frame with rectangle,
year and logical complete columns, unique by rectangle/year. TRUE confirms a
usable complete year and can establish a zero even if no records occur. FALSE
excludes the year even if some records are present. Unspecified keys fall back
to source-row presence and are counted in diagnostics. Duplicate coverage keys,
invalid codes, negative weights and conflicting guild memberships cause clear
errors. Identical-looking source records are not deduplicated automatically;
only your upstream record key can determine whether they are duplicates.


5. SPECIES GROUPING AND COLOURS

top_n counts species or the configured combined species groups, not biological
stocks. ANK/MON/MNZ become ANF, and LDB/MEG become LEZ by default. These recodes
apply to both records and the lookup before joining. Ambiguous guild membership
is rejected rather than multiplying weights through a many-to-many join.

The example retains your REB exception and explicitly applies the operational
CRE/SCE/WHE assignment to Crustacean. Each override replaces that code's other
memberships. These are your workflow rules; the label does not reclassify the
taxonomy of the included species.

coverage_pct = 90 keeps enough positive species to reach at least 90% of the
displayed regional total, including the species that crosses the threshold.
When supplied it takes precedence over top_n. coverage_pct = 100 retains every
positive species. top_n = NULL also keeps all positive species separately.

All remaining canonical species are aggregated into Other species. Its members
and share are recorded in metadata and the calculation archive. A reserved
internal group key prevents a genuine source OTH code from colliding with this
new group. Browser export identifies the generated group as OTHER_GROUP with
an is_other_group field and member list. If the input already contains a source
OTH category, its lost member detail cannot be recovered by the map.

Supply one named species_colors vector across fisheriesXplorer for consistent
colours across years and regions. The example provides a starting palette and
common names; extend it to your species catalogue. Fallback colours distinguish
the displayed categories within a map but do not promise stable cross-map colours
for codes absent from the shared palette. Other species is consistently grey.


6. INTEGRATION AND MAINTENANCE

The implementation separates three reusable steps:

  prepare_fisheries_landings()       Records to documented annual means/groups.
  build_fisheries_map_payload()      Reporting and marine display geometry.
  render_fisheries_landings()        Prepared payload to a Leaflet htmlwidget.

plot_fisheries_landings() combines those steps. save_fisheries_map() writes
HTML; export_fisheries_calculations() writes display and canonical aggregates,
annual tables, temporal coverage, species metadata and diagnostic JSON.

For Shiny, return the widget from leaflet::renderLeaflet() into
leaflet::leafletOutput(). The included example shows the full pattern. Load
records, region lists, guild lookup, boundary and coastline once at startup or
as part of your data refresh. Cache prepared payloads for stable combinations
of region, guild and period; the three map views and local selections require
no new server calculation. Changing years/guilds in the host app should generate
or retrieve the corresponding payload, not relabel an existing one.

For a static fisheriesXplorer page, publish the saved HTML or embed it using your
existing application convention. The delivered preview has all script/style/data
assets inline. Basemap tiles are not necessary. The implementation supports an
optional payload.meta.tileUrl when your application deliberately chooses online
tiles; default exports make no such requests. Test any host application's content
security policy against its existing htmlwidgets requirements.

The land layer embedded in an R payload retains the supplied geographic context.
For smaller files, pass a cached regional land layer with enough padding for the
intended pan range, rather than a very detailed global coastline. Cache geometry
and payload preparation rather than repeating it for each visitor interaction.

Calculation archives stay with the analysis. The public widget receives display
aggregates and methods, not individual source or vessel rows. Apply the same
publication and disclosure rules you already use before generating a public
payload. No automatic publication to fisheriesXplorer is performed by these files.

The old automated PDF screenshot is omitted from the export loop: the HTML is
the interactive deliverable. If you also need a print figure, generate a separate
static map with a fixed extent, legend and caption from the prepared tables.


7. VERIFICATION AND REFERENCES

See VALIDATION_fisheries_landings.txt for the checks actually run and their limits.
Both the standalone preview and a widget exported from R passed browser checks
for fixed donut size, view switching, selection, CSV export, coastal flags,
rerender cleanup and responsive layout. The original annual RDBES records were
not supplied; new 2022–2024 results must be checked against those records.

Primary references used for the grid, geometry and widget behaviour:

ICES statistical reporting rectangle dimensions:
https://data.ices.dk/vms/webservices

Leaflet.minichart width is maximal pie diameter:
https://rte-antares-rpackage.github.io/leaflet.minichart/-_L.Minichart_.html

Leaflet R marker behaviour:
https://rstudio.github.io/leaflet/articles/markers.html

sf geometric unary and binary operations:
https://r-spatial.github.io/sf/reference/geos_unary.html
https://r-spatial.github.io/sf/reference/geos_binary_ops.html

Natural Earth 1:50m land polygons and attribution terms:
https://www.naturalearthdata.com/downloads/50m-physical-vectors/50m-land/
https://www.naturalearthdata.com/about/terms-of-use/
