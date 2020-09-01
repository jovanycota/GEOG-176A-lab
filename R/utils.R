# plot function

plot_tess = function(data, title){
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = .2) +
    theme_void() +
    labs(title = title, caption = paste("This tesselation has:", nrow(data), "tiles" )) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}


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


# step 3.2
peep = function(point, polygon, group) {
  st_join(polygon, point) %>%
    st_drop_geometry() %>%
    count(get(group)) %>%
    setNames(c(group, "n")) %>%
    left_join(polygon, by = group) %>%
    st_as_sf()
}
