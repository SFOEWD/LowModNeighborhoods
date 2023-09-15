library(sf)
library(mapview)
library(tigris)
library(tidycensus)
library(tidyverse)
library(fs)
library(arcgisbinding)
arc.check_product()

multipolygon_to_largest_polygon <- function(x) {
  polygons <- st_cast(x, "POLYGON")
  out <- polygons[which.max(st_area(polygons)),]
  return(out)
}

# Downloaded from HUD: https://www.hudexchange.info/programs/acs-low-mod-summary-data
lmisd_raw <- readxl::read_excel("data/ACS-2015-Low-Mod-Summarized-All-2023.xlsx")
lmisd_sf <- lmisd_raw %>% filter(COUNTYNAME == "San Francisco County")

# Get block groups from DataSF
# Cf. https://data.sfgov.org/Geographic-Locations-and-Boundaries/Census-2000-Block-Groups-for-San-Francisco-no-wate/wymm-gfht
# sf_blkgrps <- st_read("https://data.sfgov.org/api/geospatial/2uzy-uv2r?method=export&format=GeoJSON") %>%
#   mutate(
#     geoid = paste0("15000US0", geoid),
#     tractce = paste0("0", tractce)
#   )

# Get 2010 block groups from Census
sf_blkgrps <- block_groups(
  state = "CA",
  county = "San Francisco",
  year = 2010
)

lmod_blkgrps <- sf_blkgrps %>%
  left_join(
    lmisd_sf,
    join_by(
      TRACTCE10 == TRACT,
      BLKGRPCE10 == BLKGRP
    )
  ) %>%
  filter(LOWMODPCT > 0.51) %>%
  st_transform("EPSG:7131") %>%
  erase_water()

# st_write(lowmod_blkgrps_sf, "data/LMOD Eligible Block Groups (2010).shp", delete_layer = TRUE)

# Get tracts w/Analysis Neighborhoods from DataSF, Re-align Parkside
# Cf. https://data.sfgov.org/Geographic-Locations-and-Boundaries/Analysis-Neighborhoods/p5b7-5n3h
tracts_w_neighborhoods <- st_read("https://data.sfgov.org/api/geospatial/bwbp-wk3r?method=export&format=GeoJSON") %>%
  select(tractce10, nhood) %>%
  mutate(nhood = ifelse(
    tractce10 %in% c(
      "035400",
      "035300",
      "032901",
      "032801",
      "033000",
      "033100"
    ),
    "Parkside",
    nhood
  )) %>%
  mutate(nhood = recode(nhood, `Sunset/Parkside` = "Sunset"))
# st_write(tracts_w_neighborhoods, "data/sf_neighborhoods.shp", delete_layer = TRUE)

lmod_nhoods <- tracts_w_neighborhoods %>%
  left_join(
    lmisd_sf,
    join_by(tractce10 == TRACT),
    multiple = "all" # multiple blkgrps per tract
  ) %>%
  # Dissolve tracts to neighborhoods
  group_by(nhood) %>%
  summarize(
    LOWMOD = sum(LOWMOD),
    LOWMODUNIV = sum(LOWMODUNIV),
    LOWMODPCT = LOWMOD / LOWMODUNIV
  ) %>%
  ungroup() %>%
  filter(LOWMODPCT > 0.51) %>%
  select(nhood, LOWMODPCT)

# st_write(lmod_nhoods, "data/LMOD Eligible Neighborhoods.shp", delete_layer = TRUE)
# mapview::mapview(lmod_nhoods, zcol = "LOWMODPCT")

lmod_tracts <- tracts_w_neighborhoods %>%
  left_join(
    lmisd_sf,
    join_by(tractce10 == TRACT),
    multiple = "all" # multiple blkgrps per tract
  ) %>%
  group_by(tractce10) %>%
  summarize(
    LOWMOD = sum(LOWMOD),
    LOWMODUNIV = sum(LOWMODUNIV),
    LOWMODPCT = LOWMOD / LOWMODUNIV
  ) %>%
  ungroup() %>%
  filter(LOWMODPCT > 0.51) %>%
  select(tractce10, LOWMODPCT)

# Write to ArcGIS
# Low-to Moderate Income Neighborhoods
# North Shore (12th element) to one polygon
lmod_nhoods[12,] <- multipolygon_to_largest_polygon(lmod_nhoods[12,])
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/lowmod_neighborhoods"),
  data = lmod_nhoods,
  overwrite = TRUE,
  validate = TRUE
)

# Low-to Moderate Income Census Tracts
# 1st and 104th elements are MULTIPOLYGON
lmod_tracts[1,] <- multipolygon_to_largest_polygon(lmod_tracts[1,])
lmod_tracts[104,] <- multipolygon_to_largest_polygon(lmod_tracts[104,])
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/lowmod_tracts"),
  data = lmod_tracts,
  overwrite = TRUE,
  validate = TRUE
)

# Low-to Moderate Income Census Blocks
# Keep only Treasure Island (3rd element) from MULTIPOLYGON
lmod_blkgrps[3,] <- multipolygon_to_largest_polygon(lmod_blkgrps[3,])
arc.write(
  path = path(Sys.getenv("ARCGIS_PROJECTS_PATH"), "CDBG Eligibility/CDBG Eligibility.gdb/lowmod_blockgroups"),
  data = lmod_blkgrps %>% select(tractce10 = TRACTCE10, blkgrp = BLKGRPCE10, LOWMODPCT),
  overwrite = TRUE
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
  file = "img/lowmod_blkgrps_sf.png"
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
  scale_fill_manual(values = c("grey", "#7d61b3"), labels = c("Not Low-Mod", "Low-Mod")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("img/low-mod-neighborhoods.png", plot = m, height = 6, width = 6)
