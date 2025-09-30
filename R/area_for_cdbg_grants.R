library(measurements)
library(sf)
library(fs)
library(mapview)
library(tidyverse)
library(arcgisbinding)

ti <- get_sf_neighborhoods() |>
  filter(nhood == "Treasure Island") |>
  st_transform(crs = 7131)

arc.check_product()

arc_gdb_path <- path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb")

hoods_to_buffer <- arc.open(path(arc_gdb_path, "lowmod_neighborhoods_fy25")) |>
  arc.select() |>
  arc.data2sf() |>
  filter(nhood %in% c("Western Addition", "Tenderloin", "South of Market")) |>
  st_transform(crs = 7131)

blkgroups <- arc.open(path(arc_gdb_path, "lowmod_blockgroups_fy25")) |>
  arc.select() |>
  arc.data2sf() |>
  st_transform(crs = 7131) |>
  filter(tractce != "015701") # remove sliver outside boundary

st_crs(hoods)$units_gdal # 7131 is in meters
meters_to_buffer <- conv_unit(0.25, "mi", "m") # quarter mile is 402.336 meters

hoods_buffed <- st_buffer(hoods_to_buffer, dist = meters_to_buffer)

hoods_and_blks_intersection <- st_intersection(hoods_buffed, blkgroups)

hoods_and_blks_dissolved <- st_union(hoods_to_buffer, hoods_and_blks_intersection) |>
  st_union(ti) |>
  summarize() |>
  st_buffer(dist = 1) |> # fill in cracks between blkgrps and neighborhoods
  mutate(label = "CDBG Area Grant")

# review
# m <- mapview(hoods_and_blks_dissolved, col.regions = "steelblue", layer.name = "Grant Area") +
#   mapview(blkgroups, color = "black", col.regions = "white", alpha.regions = 0.01, lwd = 2, layer.name = "Blockgroups") +
#   mapview(hoods, color = "orange", col.regions = "orange", alpha.regions = 0.01, lwd = 2.5, layer.name = "Neighborhoods") +
#   mapview(hoods_buffed |> summarize(), col.regions = "pink", color = "pink", alpha.regions = 0.2, layer.name = "Buffed Neighborhoods (400 meters)")
# m

arc.write(
  path = path(arc_gdb_path, "cdbg_grant_area"),
  data = hoods_and_blks_dissolved,
  overwrite = TRUE,
  validate = TRUE
)

original_hoods <- st_union(hoods_to_buffer, ti)

arc.write(
  path = path(arc_gdb_path, "original_hoods"),
  data = original_hoods,
  overwrite = TRUE,
  validate = TRUE
)

arc.write(
  path = path(arc_gdb_path, "hoods_buffed_400m"),
  data = hoods_buffed,
  overwrite = TRUE,
  validate = TRUE
)
