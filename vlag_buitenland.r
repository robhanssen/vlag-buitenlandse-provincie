# Vlag voor de Nederlanders in het buitenland - de "spookprovincie"
# https://www.omroepwest.nl/nieuws/4702676/allereerste-spookprovincie-heeft-vandaag-een-taak-stemmen-voor-eerste-kamer
#

library(tidyverse)
library(maps)
theme_set(theme_light() +
    theme(
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()
    ))

world_map <- as_tibble(map_data("world"))

shift <- 180 / 3
bound <- 200

long_bound <- bound * c(-1, -1, 1, 1)

box_red <- tibble(
    long = long_bound,
    lat = c(90, 90 - shift, 90 - shift, 90)
)

box_white <- tibble(
    long = long_bound,
    lat = c(90 - shift, 90 - 2 * shift, 90 - 2 * shift, 90 - shift)
)

box_blue <- tibble(
    long = long_bound,
    lat = c(90 - 2 * shift, 90 - 3 * shift, 90 - 3 * shift, 90 - 2 * shift)
)

world_map %>%
    filter(lat > -60) %>% # verwijder Zuidpool
    mutate(
        lat = lat - 15, # centreren op kaart
        long = long - 10 # centreren op kaart
    ) %>%
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(
        data = box_red, fill = "#A91F32",
        aes(group = NULL)
    ) +
    geom_polygon(
        data = box_white, fill = "#FFFFFF",
        aes(group = NULL)
    ) +
    geom_polygon(
        data = box_blue, fill = "#1E4785",
        aes(group = NULL)
    ) +
    geom_polygon(fill = "#FF7F00", color = NA, alpha = 1) +
    coord_fixed(1.4) +
    expand_limits(x = c(-bound, bound))

ggsave("vlag_nederland_buitenlandse_provincie.png")
