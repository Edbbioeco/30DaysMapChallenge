# Desfio ----

# Polígonos

# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(magrittr)

# Dados ----

## Estados do Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019) |>
  dplyr::mutate(name_state = name_state |>
                  stringr::str_replace_all("\\b(Do|De)\\b", tolower))

### Visualizando ----

br

ggplot() +
  geom_sf(data = br)

## Unidades de conservação ----

### Importando ----

uc <- geobr::read_conservation_units() |>
  sf::st_make_valid()

### Visualizando ----

uc

ggplot() +
  geom_sf(data = br) +
  geom_sf(data = uc)

# Mapa ----

ggplot() +
  geom_sf(data = br, aes(fill = "Brazil"), color = "black") +
  geom_sf(data = uc, aes(fill = "Conservation Units"), color = "darkgreen") +
  geom_sf(data = br, fill = NA, color = "black", linewidth = 1) +
  scale_fill_manual(values = c("gray90",
                               "limegreen")) +
  labs(fill = NULL,
       title = "Brazilian Nature Conservation Units distribution",
       subtitle = "Source: Instituto Brasileiro de Geografia Estatística (IBGE)") +
  coord_sf(label_graticule = "SWE") +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = element_text(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 12.5, color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day3.png", height = 10, width = 12)

