
library(tidyverse)
library(sf)
library(rmapshaper)
library(mapview)
library(units)
library(readxl)
library(gghighlight)

counties = USAboundaries::us_counties()

conus_counties = counties %>%
  filter(!state_name %in% c("Alaska", "Puerto Rico", "Hawaii")) %>%
  st_transform(5070)

cent_counties = st_centroid(conus_counties)

cent_counties_u = st_union(cent_counties)

counties_uni = st_union(conus_counties)

mapview::npts(counties_uni)

plot(counties_uni)

counties_simp = ms_simplify(counties_uni, keep = .05)

mapview::npts(counties_simp)

plot(counties_simp)

# tesselations

v_grid = st_voronoi(cent_counties_u) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

v_grid_int = st_intersection(v_grid, counties_simp)

mapview::npts(v_grid_int)

t_grid = st_triangulate(cent_counties_u) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

t_grid_int = st_intersection(t_grid, counties_simp)

mapview::npts(t_grid_int)


# coverages

sq_grid = st_make_grid(conus_counties, n = c(70, 50)) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

hex_grid = st_make_grid(conus_counties, n = c(70, 50), square = FALSE) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

# plot function

plot_tess = function(data, title){
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = .2) +
    theme_void() +
    labs(title = title, caption = paste("This tesselation has:", nrow(data), "tiles" )) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}

# plots

plot_tess(hex_grid, "Hexagonal Coverage")

plot_tess(sq_grid, "Square Coverage")

plot_tess(t_grid_int, "Voroni Coverage")

plot_tess(v_grid_int, "Voroni Coverage")

plot_tess(conus_counties, "Counties")

# function for question 2

sum_tess = function(sf_object, char){

  areas = drop_units(set_units(st_area(sf_object), "km2"))

  tess= data.frame(tesselation = char,
    mean_sq = set_units(mean(areas), "km^2"),
    total_feat = length(areas),
    stan_dev = sd(areas),
    total_area = set_units(sum(areas), "km^2"))

  return(tess)
}

# step 2.2

sum_tess(sf_object = v_grid_int, 'Voroni')

sum_tess(sf_object = t_grid_int, 'triangulate')

sum_tess(sf_object = sq_grid, "square")

sum_tess(sf_object = hex_grid, "hexagon")

sum_tess(sf_object = conus_counties, "counties")

# step 2.3

tess_summary = bind_rows(
  sum_tess(sf_object = v_grid_int, 'Voroni'),
  sum_tess(sf_object = t_grid_int, 'Triangulation'),
  sum_tess(sf_object = sq_grid, "Square"),
  sum_tess(sf_object = hex_grid, "Hexagon"),
  sum_tess(sf_object = conus_counties, "Counties"))

# step 2.4

knitr::kable(tess_summary, caption = "US Tesselations by County", col.names =
               c("Tesselation", "Mean", "Total Number of Features", "Standard Deviation"
                 , "Total Area"))

# step 2.5


# step 3.1

dam = read_excel('data/NID2019_U.xlsx')

dam_filt = dam %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE'), crs = 4326) %>%
  st_transform(5070)

# step 3.2

peep = function(point, polygon, group) {
    st_join(polygon, point) %>%
    st_drop_geometry() %>%
    count(get(group)) %>%
    setNames(c(group, "n")) %>%
    left_join(polygon, by = group) %>%
    st_as_sf()

}

# step 3.3

v_peep = peep(dam_filt, v_grid_int, "id")

t_peep = peep(dam_filt, t_grid_int, "id")

sq_peep = peep(dam_filt, sq_grid, "id")

hex_peep = peep(dam_filt, hex_grid, "id")

conus_peep = peep(dam_filt, conus_counties, "geoid")



# step 3.4

plot_peep = function(object_sf, title) {
  ggplot() +
    geom_sf(data = object_sf, aes(fill = n, col = 'NA')) +
    scale_fill_viridis_c() +
    theme_void() +
    labs(title = title,
         caption = paste0(sum(object_sf$n), " dams represented"))
}

# step 3.5

plot_peep(hex_peep, "Hexagonal Grid: U.S. Dams")

plot_peep(sq_peep, "Square Grid: U.S. Dams")

plot_peep(v_peep, "Voroni Grid: U.S. Dams")

plot_peep(t_peep, "Triangulation Grid: U.S. Dams")

plot_peep(conus_peep, "U.S. Counties: Dams")

# step 3.6

# step 4.1

dam_purp = dam_filt %>%
  filter(grepl(c("P", "R", "S", "F"), PURPOSES))

dam_protect = dam_purp %>%
  filter(grepl("P", PURPOSES))

dam_pro_hex = peep(dam_protect, hex_grid, "id")

dam_rec = dam_purp %>%
  filter(grepl("R", PURPOSES))

dam_rec_hex = peep(dam_rec, hex_grid, "id")

dam_ws = dam_purp %>%
  filter(grepl("S", PURPOSES))

dam_ws_hex = peep(dam_ws, hex_grid, "id")

dam_fad = dam_purp %>%
  filter(grepl("F", PURPOSES))

dam_fad_hex = peep(dam_fad, hex_grid, "id")

# step 4.2

plot_peep(dam_fad_hex, "Dam Purpose: Fish and Wildlife") +
  gghighlight(n > (mean(n) + sd(n)))

plot_peep(dam_ws_hex, "Dam Purpose: Water Supply")


plot_peep(dam_pro_hex, "Dam Purpose: Protect") +
  gghighlight(n > (mean(n) + sd(n)))

plot_peep(dam_rec_hex, "Dam Purpose: Recreation")



