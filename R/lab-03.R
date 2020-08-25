library(tidyverse)
library(sf)
library(units)
library(USAboundaries)
library(rnaturalearth)
library(gghighlight)
library(ggrepel)
library(knitr)

eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

### Question 1

states =  USAboundaries::us_states(resolution = "low")

usa = USAboundaries::us_states(resolution = "low") %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(eqdc)

bound = rnaturalearth::countries110 %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(admin %in% c("United States of America", "Mexico", "Canada")) %>%
  st_transform(eqdc)

cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(!state_name %in% c("Hawaii", "Puerto Rico", "Alaska")) %>%
  st_transform(eqdc)

# question 2

# 2.1 Distance to USA border in km

usa_r = states %>%
  st_union() %>%
  st_cast("MULTILINESTRING") %>%
  st_transform(eqdc)

dist_border = st_distance(cities, usa_r) %>%
  set_units("km")

wo_units_border = dist_border %>%
  drop_units()

city_border = cities %>%
  mutate(wo_units_border) %>%
  select(city, state_name, wo_units_border)

five_city_border = cities %>%
  mutate(dist_border) %>%
  slice_max(dist_border, n = 5) %>%
  select(city, state_name, dist_border)

knitr::kable(five_city_border, caption = "The 5 Furthest USA Cities to the USA Border",
             col.names = c("City", "State", "Distance to Border", ""))

# 2.2 Distance to States

usa_p = states %>%
  st_combine() %>%
  st_cast("MULTILINESTRING") %>%
  st_transform(eqdc)

dist_state = st_distance(cities, usa_p) %>%
  set_units("km")

wo_units_state = dist_state %>%
  drop_units()

city_state = cities %>%
  mutate(wo_units_state) %>%
  select(city, state_name, wo_units_state)

five_city_state = cities %>%
  mutate(dist_state) %>%
  slice_max(dist_state, n = 5) %>%
  select(city, state_name, dist_state)

knitr::kable(five_city_state, caption = "The 5 Furthest USA Cities to a  State Boundary",
             col.names = c("City", "State", "Distance", ""))

# 2.3 Distance to Mexico

mex_border = bound %>%
  filter(admin == "Mexico")

dist_mex = st_distance(cities, mex_border) %>%
  set_units("km")

city_mex = cities %>%
  mutate(dist_mex) %>%
  select(city, state_name, dist_mex) %>%
  slice_max(dist_mex, n = 5)


knitr::kable(city_mex, caption = "The 5 Furthest USA Cities to the Mexico Border",
             col.names = c("City", "State", "Distace to the Mexico Border", ""))

# 2.4 Distance to Canada

can_border = bound %>%
  filter(admin == "Canada")

dist_can = st_distance(cities, can_border) %>%
  set_units("km")

city_can = cities %>%
  mutate(dist_can) %>%
  select(city, state_name, dist_can) %>%
  slice_max(dist_can, n = 5)


knitr::kable(city_can, caption = "The 5 Furthest USA Cities to the Canadian Border",
             col.names = c("City", "State", "Distance to Canadian Border", ""))

### Question 3

# 3.1 Data

ten_cities = cities %>%
  slice_max(population, n = 10)

ggplot() +
  geom_sf(data = bound, lty = 5) +
  geom_sf(data = states, lty = 4) +
  geom_sf(data = usa, lty = 7) +
  geom_sf(data = ten_cities) +
  geom_label_repel(data = ten_cities, aes(label = city, geometry = geometry),
                   stat = "sf_coordinates",
                   size = 3) +
  theme_linedraw() +
  labs(title = "The 10 Biggest Cities in the USA by Population",
       caption = "GEOG 176A Summer Session B 2020",
       x = "",
       y = "")


# 3.2 City distance from the border

ggplot() +
  geom_sf(data = usa, lty = 1) +
  geom_sf(data = city_border, aes(col = wo_units_border)) +
  scale_color_gradient(low = "gray", high = "purple") +
  theme_linedraw() +
  labs(title = "USA Cities Distance from the National Border",
       caption = "GEOG 176A Summer Session B 2020",
       x = "",
       y = "")


ggplot() +
  geom_sf(data = usa, lty = 1) +
  geom_sf(data = five_city_border, size = .5) +
  geom_label_repel(data = five_city_border, aes(label = city, geometry = geometry),
                   stat = "sf_coordinates",
                   size = 3) +
  theme_linedraw() +
  labs(title = "The Top 5 USA Cities Furthest from the National Border",
       caption = "GEOG 176A Summer Session B 2020",
       x = "",
       y = "")

# 3.3 city distance from nearest state

ggplot() +
  geom_sf(data = usa, lty = 1) +
  geom_sf(data = city_state, aes(col = wo_units_state)) +
  scale_color_gradient(low = "yellow", high = "purple") +
  theme_linedraw() +
  labs(title = "USA Cities Distance from the Nearest State Border",
       caption = "GEOG 176A Summer Session B 2020",
       x = "",
       y = "")

ggplot() +
  geom_sf(data = usa, lty = 1) +
  geom_sf(data = five_city_state, size = .5) +
  geom_label_repel(data = five_city_state, aes(label = city, geometry = geometry),
                   stat = "sf_coordinates",
                   size = 3) +
  theme_linedraw() +
  labs(title = "The Top 5 USA Cities Furthest from a State Border",
       caption = "GEOG 176A Summer Session B",
       x = "",
       y = "")

# 3.4

dist_mex_can = abs(dist_can - dist_mex)


filter_can = cities %>%
  mutate(dist_mex_can, dist_can) %>%
  select(city, state_name, dist_mex_can, population, dist_can) %>%
  filter(dist_mex_can == dist_can)

filter_mex = cities %>%
  mutate(dist_mex_can, dist_mex) %>%
  select(city, state_name, dist_mex_can, dist_mex, population) %>%
  filter(dist_mex_can == dist_mex)



pop_mex= filter_mex %>%
  slice_max(population, n =5)

pop_can = filter_can %>%
  slice_max(population, n =5)


ggplot() +
  geom_sf(data = bound, lty = 4) +
  geom_sf(data = states, lty = 1) +
  geom_sf(data = filter_mex) +
  geom_sf(data = filter_can) +
  geom_label_repel(data = pop_can, aes(label = city, geometry = geometry),
                    stat = "sf_coordinates",
                    size = 3) +
  geom_label_repel(data = pop_mex, aes(label = city, geometry = geometry),
                    stat = "sf_coordinates",
                    size = 3) +
  labs(title = "USA Cities that are Equal Distance from the Canadian and Mexican Border",
       caption = "GEOG 176A Summer Session B 2020")

### question 4

# 4.1

city_range = 160 %>%
  set_units("km")

within_range = cities %>%
  mutate(dist_state) %>%
  select(city, state_name, dist_state, population) %>%
  mutate(close_by = dist_state < city_range) %>%
  filter(close_by == TRUE)

pop_within_range = cities %>%
  mutate(dist_state) %>%
  select(city, state_name, dist_state, population) %>%
  mutate(close_by = dist_state < city_range) %>%
  filter(close_by == TRUE) %>%
  #summarise(totpop = sum(population))
  count(city) %>%


cities_within_range = cities %>%
  mutate(dist_state) %>%
  select(city, state_name, dist_state, population) %>%
  mutate(close_by = dist_state < city_range) %>%
  filter(close_by == TRUE) %>%
  summarise(totcities = sum(close_by), na.rm = FALSE)

per_pop = cities %>%
  mutate(cities_within_range)




















