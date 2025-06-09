library(sf)
library(mapview)
library(tigris)
library(tidycensus)
library(tidyverse)
library(fs)
library(arcgisbinding)
arc.check_product()

# multipolygon_to_largest_polygon <- function(x) {
#   polygons <- st_cast(x, "POLYGON")
#   out <- polygons[which.max(st_area(polygons)),]
#   return(out)
# }

# Downloaded from HUD: https://www.hudexchange.info/programs/acs-low-mod-summary-data
# lmisd_raw <- readxl::read_excel("data/ACS-2015-Low-Mod-Summarized-All-2023.xlsx")
lmisd_raw <- readxl::read_excel("data/ACS-2020-Low-Mod-Block-Group-All.xlsx")
lmisd_sf <- lmisd_raw %>% filter(COUNTYNAME == "San Francisco County")

# Get tracts w/Analysis Neighborhoods from DataSF, Re-align Parkside
# Cf. https://data.sfgov.org/Geographic-Locations-and-Boundaries/Analysis-Neighborhoods/p5b7-5n3h
tracts_w_neighborhoods <- st_read("https://data.sfgov.org/resource/sevw-6tgi.geojson") %>%
  select(tractce, nhood = neighborhoods_analysis_boundaries) %>%
  mutate(nhood = ifelse(
    tractce %in% c(
      "035400",
      "035300",
      "032901",
      "032801",
      "033000",
      "033001",
      "033002",
      "033100"
    ),
    "Parkside",
    nhood
  )) %>%
  mutate(nhood = recode(nhood, `Sunset/Parkside` = "Sunset"))
# st_write(tracts_w_neighborhoods, "data/sf_neighborhoods.shp", delete_layer = TRUE)

# SF outline
sf_outline <- tracts_w_neighborhoods %>%
  summarize() %>%
  st_transform("EPSG:7131")

# Get block groups from DataSF
# Cf. https://data.sfgov.org/Geographic-Locations-and-Boundaries/Census-2000-Block-Groups-for-San-Francisco-no-wate/wymm-gfht
# sf_blkgrps <- st_read("https://data.sfgov.org/api/geospatial/2uzy-uv2r?method=export&format=GeoJSON") %>%
#   mutate(
#     geoid = paste0("15000US0", geoid),
#     tractce = paste0("0", tractce)
#   )

# Get 2010 block groups from Census
# Erase water, cookie-cut to SF
sf_blkgrps <- block_groups(
  state = "CA",
  county = "San Francisco",
  year = 2020
) %>%
  left_join(
    lmisd_sf,
    join_by(
      TRACTCE== TRACT,
      BLKGRPCE == BLKGRP
    )
  ) %>%
  st_transform("EPSG:7131") %>%
  st_intersection(st_union(sf_outline))

lmod_blkgrps <- sf_blkgrps %>%
  mutate(LOWMOD_PCT = as.numeric(gsub("%", "", LOWMOD_PCT))) %>%
  filter(LOWMOD_PCT > 51)
# st_write(lmod_blkgrps, "data/LMOD Eligible Block Groups (2010).shp", delete_layer = TRUE)

sf_nhoods <- tracts_w_neighborhoods %>%
  left_join(
    lmisd_sf,
    join_by(tractce == TRACT),
    multiple = "all" # multiple blkgrps per tract
  ) %>%
  mutate(across(c(LOWMOD, LOWMODUNIV), \(x) parse_number(x))) %>%
  # Dissolve tracts to neighborhoods
  group_by(nhood) %>%
  summarize(
    LOWMOD = sum(LOWMOD),
    LOWMODUNIV = sum(LOWMODUNIV),
    LOWMODPCT = LOWMOD / LOWMODUNIV
  ) %>%
  ungroup()

lmod_nhoods <- sf_nhoods %>%
  filter(
    LOWMODPCT > 0.51,
    nhood != "Golden Gate Park"
    ) %>%
  select(nhood, LOWMODPCT)
# st_write(lmod_nhoods, "data/LMOD Eligible Neighborhoods.shp", delete_layer = TRUE)
# mapview::mapview(lmod_nhoods, zcol = "LOWMODPCT")

sf_tracts <- tracts_w_neighborhoods %>%
  left_join(
    lmisd_sf,
    join_by(tractce == TRACT),
    multiple = "all" # multiple blkgrps per tract
  ) %>%
  mutate(across(c(LOWMOD, LOWMODUNIV), \(x) parse_number(x))) %>%
  group_by(tractce) %>%
  summarize(
    LOWMOD = sum(LOWMOD),
    LOWMODUNIV = sum(LOWMODUNIV),
    LOWMODPCT = LOWMOD / LOWMODUNIV
  ) %>%
  ungroup()

lmod_tracts <- sf_tracts %>%
  filter(LOWMODPCT > 0.51) %>%
  select(tractce, LOWMODPCT)

# Write to ArcGIS
# Low-to Moderate Income Neighborhoods
# North Shore (12th element) to one polygon
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/lowmod_neighborhoods_fy25"),
  data = lmod_nhoods,
  overwrite = TRUE,
  validate = TRUE
)

# All neighborhoods
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/sf_neighborhoods"),
  data = sf_nhoods %>% select(nhood, LOWMODPCT),
  overwrite = TRUE,
  validate = TRUE
)

# Low-to Moderate Income Census Tracts
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/lowmod_tracts_fy25"),
  data = lmod_tracts,
  overwrite = TRUE,
  validate = TRUE
)

# All tracts
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/sf_tracts"),
  data = sf_tracts %>% select(tractce, LOWMODPCT),
  overwrite = TRUE,
  validate = TRUE
)

# Low-to Moderate Income Census Blocks
# Keep only Treasure Island (3rd element) from MULTIPOLYGON
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/lowmod_blockgroups_fy25"),
  data = lmod_blkgrps %>% select(tractce = TRACTCE, blkgrp = BLKGRPCE, LOWMOD_PCT),
  overwrite = TRUE,
  validate = TRUE
)

# All block groups
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/sf_blockgroups"),
  data = sf_blkgrps %>% select(tractce = TRACTCE, blkgrp = BLKGRPCE, LOWMOD_PCT),
  overwrite = TRUE,
  validate = TRUE
)

# Make maps
m <- mapview(
  lmod_blkgrps,
  layer.name = "LowMod Block Groups",
  alpha.regions = 0.6,
  col.regions = "#7d61b3"
  )
mapviewOptions(fgb = FALSE)
mapshot(
  m,
  remove_controls = c("homeButton", "layersControl", "zoomControl"),
  file = "img/lowmod_blkgrps_sf_fy25.png"
)

m <- ggplot(lmod_nhoods) +
  geom_sf(aes(fill = LOWMODPCT > 0.51), alpha = 0.6) +
  ggrepel::geom_label_repel(
    data = lmod_nhoods %>% filter(LOWMODPCT > 0.51),
    aes(label = nhood, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0
  ) +
  labs(fill = NULL) +
  scale_fill_manual(values = rev(c("grey", "#7d61b3")), labels = c("Not Low-Mod", "Low-Mod")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("img/low-mod-neighborhoods_fy25.png", plot = m, height = 6, width = 6)
