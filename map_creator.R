pacman::p_load(BBmisc, tidyverse, ggbump, sf, rnaturalearthdata, lubridate, countrycode, glue, wesanderson)

df <- read_csv('data/contact.csv') %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  count(iso3, zone)

region_measures <- list(
  'NA' = list(
    name = 'North America',
    xmin = -170, 
    xmax = 0, 
    ymin = 0,
    ymax = 80
  ),
  'SA' = list(
    name = 'South America',
    xmin = -170,
    xmax = 0, 
    ymin = -80,
    ymax = 20
  ),
  'AF' = list(
    name = 'Africa',
    xmin = -170,
    xmax = 0, 
    ymin = -80,
    ymax = 20
  )
)

region_of_interest <- region_measures[['SA']]

sdf <- rnaturalearthdata::countries50 %>%
  st_as_sf() %>%
  st_crop(xmin = region_of_interest[['xmin']], 
          xmax = region_of_interest[['xmax']], 
          ymin = region_of_interest[['ymin']], 
          ymax = region_of_interest[['ymax']]) %>%
  filter(continent == region_of_interest[['name']]) %>% 
  left_join(df, by = c("adm0_a3" = 'iso3')) %>%
  replace_na(list(n = 0))

sdf_clean <- sdf %>%
  filter(n != 0)

ranking <- st_geometry(sdf_clean) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>%
  drop_na() %>%
  bind_cols(tibble(n_cap = normalize(rank(sdf_clean$n), range = c(min(sdf_clean$n), max(sdf_clean$n)), method = "range"),
                   country = sdf_clean$name,
                   xend = 60,
                   x_axis_start = xend + 10,
                   n_cap_x = normalize(sdf_clean$n, range = c(first(x_axis_start), 100), method = "range"),
                   val_txt = paste0(format(sdf_clean$n, digits = 0, nsmall = 0))
                   ))

pal <- wes_palette("Darjeeling2", nrow(ranking), type = "continuous")

p <- ggplot() + 
  geom_sf(data = sdf, size = .25, fill = "transparent", color = "gray17") +
  # Sigmoid from country to start of barchart
  geom_sigmoid(data = ranking, 
               aes(x = X, y = Y, xend = x_axis_start - .2, yend = n_cap, group = country, color = n_cap), 
               alpha = .6, smooth = 10, size = 1) + 
  # Line from xstart to value
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = n_cap, xend = n_cap_x, yend = n_cap, color = n_cap), 
               alpha = .6, size = 1, lineend = "round") +
  # Y axis - black line
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = min(n_cap) - 1, xend = x_axis_start, yend = max(n_cap) + 1), 
               alpha = .6, size = 1.3, color = "black") +
  # dot on centroid of country in map
  geom_point(data = ranking, 
             aes(x = X, y = Y, color = n_cap), size = 2) +
  # Country text
  geom_text(data = ranking, aes(x = x_axis_start-.5, y = n_cap, label = country, color = n_cap), 
            hjust = 1, size = 3.5, nudge_y = 3, check_overlap = T) +
  # Value text
  geom_text(data = ranking, aes(x = n_cap_x, y = n_cap, label = val_txt, color = n_cap), 
            hjust = 0, size = 3.75, nudge_x = .7, nudge_y = 0.75) +
  coord_sf(clip = "off") +
  scale_colour_gradientn(colours = pal) +
  theme_void() +
  labs(title = glue("RStudio Instructors in {region_of_interest[['name']]}"),
       subtitle = glue("At the moment there are {sum(sdf$n)} instructors certified in the Tidyverse and Shiny in this region"),
       caption = "Source: RStudio") +
  theme(plot.margin = margin(.5, 1, .5, .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "#75aadb"),
        plot.caption = element_text(color = "#4d4d4d"),
        plot.title = element_text(color = "#4d4d4d", size = 16, family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(color = "#4d4d4d", size = 8))

p





