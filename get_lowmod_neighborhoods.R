library(sf)
library(tidyverse)

# Downloaded from HUD: https://www.hudexchange.info/programs/acs-low-mod-summary-data
lmisd_raw <- readxl::read_excel("data/ACS-2015-Low-Mod-Summarized-All-2023.xlsx")

# Get tracts w/Analysis Neighborhoods from DataSF, Re-align Parkside
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
  )
  ) %>%
  mutate(nhood = recode(nhood, `Sunset/Parkside` = "Sunset"))
# st_write(tracts_w_neighborhoods, "data/sf_neighborhoods.shp", delete_layer = TRUE)

lmod_nhoods <- tracts_w_neighborhoods %>%
  left_join(
    lmisd_raw %>%
      filter(COUNTYNAME == "San Francisco County") %>%
      select(TRACT, LOWMOD, LOWMODUNIV),
    join_by(tractce10 == TRACT)
  ) %>%
  group_by(nhood) %>%
  summarize(
    LOWMOD = sum(LOWMOD),
    LOWMODUNIV = sum(LOWMODUNIV),
    LOWMODPCT = LOWMOD/LOWMODUNIV
  ) %>%
  ungroup() %>%
  select(nhood, LOWMODPCT)

# st_write(lmod_nhoods, "data/LMOD Eligible Neighborhoods.shp", delete_layer = TRUE)
# mapview::mapview(lmod_nhoods, zcol = "LOWMODPCT")

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

ggsave("low-mod-neighborhoods.png", plot = m, height = 6, width = 6)
