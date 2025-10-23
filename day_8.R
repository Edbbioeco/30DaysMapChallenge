# Desafio ----

# Urbanismo

# Pacotes ----

library(tidyverse)

library(sf)

library(geobr)

library(magrittr)

library(maptiles)

library(tidyterra)

library(ggrepel)

library(ggspatial)

library(ggview)

# Dados ----

## APA Aldeia Beberibe -----

### Importando -----

apa <- sf::st_read("apa_aldeiabeberibe.shp") |>
  sf::st_transform(crs = 4674)

### Visualizando ----

apa

ggplot() +
  geom_sf(data = apa)

## Municípios ----

### Importando -----

muni <- geobr::read_municipality(year = 2019)

### Visualizando ----

muni

ggplot() +
  geom_sf(data = muni)

### Tratando ----

muni %<>%
  dplyr::mutate(name_muni = name_muni |>
                  stringr::str_replace_all(c("\\bDa\\b" = "da",
                                             "\\bDo\\b" = "do",
                                             "\\bDe\\b" = "de",
                                             "\\bE\\b"  = "e")))

cidades <- muni |>
  sf::st_intersection(apa) |>
  dplyr::pull(name_muni)

cidades

muni %<>%
  dplyr::filter(abbrev_state == "PE" & name_muni %in% cidades)

muni

ggplot() +
  geom_sf(data = muni)

## Imagem de sate´lite ----

### Importando ----

apa_sat <- maptiles::get_tiles(apa,
                               provider = "Esri.WorldImagery",
                               zoom = 14)

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = apa_sat) +
  geom_sf(data = apa, color = "red", fill = NA) +
  coord_sf(expand = FALSE)

# Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = apa_sat) +
  geom_sf(data = muni |>
            dplyr::filter(name_state == "Pernambuco"),
          aes(color = "Pernambuco Municipality"),
          fill = NA,
          linewidth = 1) +
  ggsflabel::geom_sf_label_repel(data = muni |>
                                   dplyr::filter(name_state == "Pernambuco"),
                                 aes(label = name_muni)) +
  geom_sf(data = apa,
          aes(color = "APA Aldeia Beberibe"),
          fill = NA,
          linewidth = 1) +
  coord_sf(xlim = c(-35.15625, -34.89262),
           ylim = c(-8.037449, -7.776295),
           expand = FALSE,
           label_graticule = "SWE") +
  scale_color_manual(values = c("gold",
                                "royalblue"),
                     name = NULL) +
  labs(title = "Map of Environmental Protected Area (APA) Aldeia Beberibe, the largest<br>Continuous Atlantic Forest fragment in Recife Metropolitan Region, Pernambuco",
       subtitle = "Data Source: Pernambuco State Environmental Agency CPRH") +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = ggtext::element_markdown(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = ggtext::element_markdown(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(size = 15,
                                                 color = "black", hjust = 0.5),
        plot.caption = ggtext::element_markdown(size = 15,
                                                color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day8.png", height = 10, width = 12)
