library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(sf)
library(stars)
library(rayshader)
library(mapview)
library(ggthemes)

## Read Data ---------------

well_file = "OVGA_MW_data_JW.xlsx"

# See these examples:
# https://readxl.tidyverse.org/articles/readxl-workflows.html

readxl::excel_sheets(well_file) %>% 
  set_names() %>% 
  map_df( ~ readxl::read_excel(path = well_file,
                            sheet = .x, 
                            skip = 2,
                            col_types = c("text",
                                          "numeric",
                                          "numeric",
                                          "numeric",
                                          "numeric",
                                          "text"),
                            na = c("", "Dry", "See Note")),
          .id = "sheet") %>% 
  filter(str_detect(Date, "^[0-9]+")) %>% 
  mutate(Date = lubridate::ymd(Date)) ->
  well_data

# Fix well names from sheet names:
well_data %>% 
  mutate(well = str_extract(sheet, "[^ ]+[^ -]")) %>% 
  select(well, everything()) ->
  well_data


# Spatial Data:
read_excel("OVGA_MW_data_GPS.xlsx") %>% 
  st_as_sf(coords = c("long", "Lat"),
           crs = st_crs(4326)) %>% 
  rename(well = `Well name`) ->
  wells

wells %>%
  right_join(well_data, by = "well") %>% 
  select(everything(), geometry) ->
  well_data

read_excel("NE spring discharge.xlsx") %>% 
  filter(str_detect(Date, "^[0-9]+")) %>% 
  mutate(Date  = lubridate::ymd(Date)) ->
  ne_spring_discharge


dem = stars::read_stars("USGS_13_n38w119_20211004.tif", proxy = FALSE)
dem_crs = dem %>% st_crs()

# making a box manually:
# st_multipoint(rbind(c(-118.53, 37.389),
#                     c(-118.53, 37.89),
#                     c(-118.2351, 37.89),
#                     c(-118.2351, 37.389))) %>%
#   st_cast("POLYGON") %>%
#   st_sfc() %>%
#   st_set_crs(4326) ->
#   b

st_read("tmp.json") %>% 
  st_geometry() %>% 
  st_bbox() %>% 
  st_as_sfc() %>%
  st_sf() ->
  box

box %>% st_transform(dem_crs) ->
  box

dem %>% st_crop(box) -> dem_small
dem_small %>% stars::write_stars("dem_small.tif")
raster::raster("dem_small.tif") %>% 
  raster_to_matrix() ->
elmat 

rm(dem)

## Plot Data ----------------

ne_spring_discharge %>%
  mutate(Year = lubridate::year(Date)) %>% 
  # group_by(Recent = Year > 1980) %>% 
  filter(Year > 1980) %>% 
  ggplot(aes(x = Date, y = `Flow Rate for Month (ac-ft/month)`)) +
  geom_point() +
  # stat_smooth(geom = "line", 
  #             span = 0.1, 
  #             se = FALSE, 
  #             color = "red", 
  #             size = 2, 
  #             alpha = 0.4) + 
  ggtitle("NE Spring Discharge") +
  theme_grey(base_size = 22) + 
  geom_hline(yintercept = 0, color = "red", size = 2) +
  geom_smooth(method='lm')

# elmat %>%
#   sphere_shade(texture = "desert") %>%
#   add_water(detect_water(elmat), color = "desert") %>%
#   add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
#   add_shadow(ambient_shade(elmat), 0) %>%
#   plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
# Sys.sleep(0.2)
# render_snapshot()

well_data %>% 
  filter(sheet != "FS-4 (top FS)_dry") %>% 
  ggplot(aes(x = Date, y = `GW Elev`)) +
  geom_point(color = "blue", alpha = 0.5) +
  facet_wrap(~ sheet,
             scales = "fixed")


well_data %>% 
  filter(sheet != "FS-4 (top FS)_dry") %>% 
  ggplot(aes(x = Date, y = `GW Elev`)) +
  geom_point(aes(color = sheet), alpha = 0.5) +
  geom_smooth(aes(color = sheet),
              method = "loess",
              span = 0.1,
              se = FALSE) +
  scale_color_viridis_d()


# single facet wrapped plot:
well_data %>%
  filter(well != "FS-4") %>%
  filter(well != "FS-2") %>%
  filter(well != "V284") %>%
  filter(well != "T574") %>% 
  group_by(Area) %>% 
  ggplot(aes(x = Date, y = `GW Elev`)) +
  geom_point(aes(color = well), alpha = 0.5) +
  geom_smooth(aes(color = well),
              method = "loess",
              span = 0.1,
              se = FALSE) +
  # ggrepel::geom_label_repel(aes(label = well),
  #                  nudge_x = 1,
  #                  na.rm = TRUE, 
  #                  max.overlaps = 100) +
  #scale_color_viridis_d(option = "D") +
  scale_colour_manual(values =c(colorblind_pal()(8) ,colorblind_pal()(8))) +
  ggtitle(paste("Groundwater Elevation")) +
  theme_grey(base_size = 22) +
  facet_grid( Area ~., space = "free_y", scales = "free_y") +
  scale_y_continuous(breaks = seq(4105, 4300, by = 10)) +
  ylab("Elevation (feet)")

# A plot for each area:
well_data %>%
  filter(sheet != "FS-4 (top FS)_dry") %>%
  filter(well != "FS-2") %>%
  filter(well != "V284") %>% 
  group_by(Area) %>%
  group_map(
    ~ ggplot(data = ., aes(x = Date, y = `GW Elev`)) +
      geom_point(aes(color = well), alpha = 0.5) +
      geom_smooth(
        aes(color = well),
        method = "loess",
        span = 0.1,
        se = FALSE
      ) +
      scale_color_colorblind() +
      ggtitle(paste(.y$Area, "Groundwater Elevation")) +
      theme_grey(base_size = 22)
  ) -> 
  ge_plots



# View the wells on an interactive 2d map:
# add labels  
wells %>% mapview::mapview()


