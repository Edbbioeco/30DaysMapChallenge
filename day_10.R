# Desafio -----

# Ar

# Pacotes ----

library(geobr)

library(tidyverse)

library(geodata)

library(terra)

library(tidyterra)

library(magrittr)

library(ggview)

# Dados ----

## Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando ----

ggplot() +
  geom_sf(data = br)

## Velocidade do ar ----

### Importando ----

ar <- geodata::worldclim_country(country = "BR",
                                 var = "wind",
                                 path = getwd())

### Visualizando ----

ar

ggplot() +
  tidyterra::geom_spatraster(data = ar) +
  geom_sf(data = br, color = "black", fill = NA) +
  scale_fill_viridis_c(option = "turbo") +
  facet_wrap(~lyr)

### Tratando ----

names(ar) <- c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December")



ggplot() +
  tidyterra::geom_spatraster(data = ar) +
  geom_sf(data = br, color = "black", fill = NA) +
  scale_fill_viridis_c(option = "turbo") +
  facet_wrap(~lyr)

# Mapa ----

## Recortando para o Brasil ----

bra <- geobr::read_country(year = 2019)

ar %<>%
  terra::crop(bra) %<>%
  terra::mask(bra)

ar |> plot()

ggplot() +
  tidyterra::geom_spatraster(data = ar) +
  geom_sf(data = br, color = "black", fill = NA) +
  scale_fill_viridis_c(option = "turbo") +
  facet_wrap(~lyr)

## Mapa ----

ggplot() +
  tidyterra::geom_spatraster(data = ar) +
  geom_sf(data = br, color = "black", fill = NA) +
  scale_fill_viridis_c(na.value = "transparent") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               frame.colour = "black",
                               ticks.colour = "black")) +
  facet_wrap(~lyr) +
  coord_sf(label_graticule = "SWE") +
  labs(title = "Brazil mounth mean wind speed (m/s)",
       subtitle = "Data Source: WorldClim",
       fill = "Wind speed (m/s)") +
  scale_x_continuous(breaks = seq(-70, -30, 20)) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = ggtext::element_markdown(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(size = 15,
                                                 color = "black", hjust = 0.5),
        plot.caption = ggtext::element_markdown(size = 15,
                                                color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day10.png", height = 10, width = 12)
