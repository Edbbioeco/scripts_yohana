# Pacotes ----

library(tuneR)

library(seewave)

library(tidyverse)

library(patchwork)

# Dados ----

## Importando ----

filo <- tuneR::readWave("phyllomedusa_tetraploidea_comp.wav")

## Visualizando ----

filo

# Analisando ----

## Espectrotrograma ----

### Espectrotro ----

spectro <- filo |> 
  seewave::spectro(wl = 512,
                   ovl = 99,
                   flim = c(0, 13))

### ggplot ----

tibl_spectro <- tibble::tibble(`Time (s)` = rep(spectro$time, 
                                                each = length(spectro$freq)),
                               `Frequency (KHz)` = rep(spectro$freq, 
                                                       length(spectro$time)),
                               Amplitude = spectro$amp |> 
                                 as.vector()) |> 
  dplyr::filter(Amplitude > -75) |> 
  dplyr::mutate(Amplitude = dplyr::case_when(`Time (s)` |> 
                                               dplyr::between(0, 0.237) ~ 0,
                                             `Time (s)` |> 
                                               dplyr::between(0.890, 3.829) ~ 0,
                                             `Time (s)` |> 
                                               dplyr::between(6.166, 8.977) ~ 0,
                                             .default = Amplitude))

tibl_spectro 

gg_spectro <- tibl_spectro |> 
  ggplot(aes(`Time (s)`, `Frequency (KHz)`, z = Amplitude)) +
  stat_contour(geom = "polygon", aes(fill = after_stat(level)),
               bins = 150) +
  scale_fill_viridis_c(option = "inferno") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, seewave::duration(filo))) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 13)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(color = "black", size = 12, 
                                  face = "bold.italic"),
        legend.position = "none",
        plot.margin = margin(0.5, -0.5, 0, 0.5, "cm"),
        panel.grid = element_line(linetype = "dashed"),
        panel.background = element_rect(fill = "black"))

gg_spectro

## Densidade de frequência ----

### Anúncio ----

#### Analisando ----

spec_anuncio <- filo |> 
  seewave::meanspec(from = 0,
                    to = 4.754,
                    dB = "C")

spec_anuncio

#### data frame ----

spec_anuncio_df <- spec_anuncio |> 
  tibble::as_tibble() |> 
  dplyr::rename("Frequency (KHz)" = x,
                "Amplitude density (dB/KHz)" = y) |> 
  dplyr::filter(`Frequency (KHz)` <= 13 &
                  `Amplitude density (dB/KHz)` >= -75) |> 
  dplyr::mutate(type = "Advertisement call")

spec_anuncio_df

### Agonístico ----

#### Analisando ----

spec_ago <- filo |> 
  seewave::meanspec(from = 4.754,
                    to = seewave::duration(filo),
                    dB = "C")

spec_ago

#### data frame ----

spec_ago_df <- spec_ago |> 
  tibble::as_tibble() |> 
  dplyr::rename("Frequency (KHz)" = x,
                "Amplitude density (dB/KHz)" = y) |> 
  dplyr::filter(`Frequency (KHz)` <= 13 &
                  `Amplitude density (dB/KHz)` >= -75) |> 
  dplyr::mutate(type = "Agonistic call")

spec_ago_df

### ggplot ----

spec_df <- dplyr::bind_rows(spec_anuncio_df,
                            spec_ago_df)

spec_df

gg_m_spec <- spec_df |> 
  ggplot(aes(`Frequency (KHz)`, `Amplitude density (dB/KHz)`, color = type)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 13)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-45, 1),
                     position = "right") +
  coord_flip() +
  scale_color_manual(values = c("Advertisement call" = "cyan4",
                                "Agonistic call" = "orangered")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.y = element_blank(),
        axis.title = element_text(color = "black", size = 12),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(color = "black", size = 12, 
                                  face = "bold.italic"),
        legend.position = "none",
        panel.grid = element_line(linetype = "dashed"),
        plot.margin = margin(0.5, 0.5, 0, -0.5, "cm"))

gg_m_spec

## Oscilograma ----

### Calculando ----

filo |> 
  seewave::oscillo()

tbl_osc <- tibble::tibble(`Time (s)` = seq(0, seewave::duration(filo), 
                                           length.out = length(filo@left)),
                          `Amplitude (KU)` = filo@left) |> 
  dplyr::filter(`Time (s)` |> dplyr::between(0, seewave::duration(filo))) |> 
  dplyr::mutate(`Amplitude (KU)` = dplyr::case_when(`Time (s)` |> 
                                                      dplyr::between(0, 0.237) ~ 0,
                                                    `Time (s)` |> 
                                                      dplyr::between(0.890, 3.829) ~ 0,
                                                    `Time (s)` |> 
                                                      dplyr::between(6.166, 8.977) ~ 0,
                                                    .default = `Amplitude (KU)`),
                type = dplyr::case_when(`Time (s)` |> 
                                          dplyr::between(0, 4.754) ~ "Advertisement call",
                                        .default = "Agonistic call"))

tbl_osc

### ggplot ----

gg_osc <- tbl_osc %>%
  ggplot(aes(`Time (s)`, `Amplitude (KU)`, color = type)) +
  geom_line()  +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, seewave::duration(filo))) +
  scale_color_manual(values = c("Advertisement call" = "cyan4",
                                "Agonistic call" = "orangered")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12),
        plot.margin = margin(0, -0.5, 0.5, 0.5, "cm"),
        legend.position = "none",
        panel.grid = element_line(linetype = "dashed"))

gg_osc

# Gráfico final ----

gg_spectro + gg_m_spec + gg_osc + 
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(tag_levels = 'A')

ggsave(filename = "vocalizacoes_phyllomedusa.png", height = 10, width = 12)
