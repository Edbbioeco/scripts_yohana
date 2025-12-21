  # Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Canto de anúncio ----
  
### Nptas ----

#### Importando ----

notas_anuncio <- readxl::read_xlsx("bioacustica_phyllomedusa_tetraploidea.xlsx")

#### Visualizando ----

notas_anuncio

notas_anuncio |> 
  dplyr::glimpse()
  
### Intervalos ----

intervalos_anuncio <- readxl::read_xlsx("bioacustica_phyllomedusa_tetraploidea.xlsx",
                                       sheet = 2)
  
#### Importando ----

intervalos_anuncio

intervalos_anuncio |> 
  dplyr::glimpse()
  
#### Visualizando ----
  
## Canto agonístico ----

### Notas ----

#### Importando ----

notas_agonistico <- readxl::read_xlsx("bioacustica_filomedusa.xlsx") |>  
  tidyr::fill(dplyr::contains("Amp"), 
              .direction = "updown") |>  
  tidyr::fill(dplyr::contains(c("Freq", "Power")), 
              .direction = "downup") |> 
  dplyr::distinct(Selection, 
                  .keep_all = TRUE)

#### Visualizando ----

notas_agonistico

notas_agonistico |> dplyr::glimpse()

### Intervalos ----

#### Importando ----

intervalos_agonistico <- readxl::read_xlsx("bioacustica_filomedusa.xlsx",
                                sheet = 2)

#### Visualizando ----

intervalos_agonistico

intervalos_agonistico |> dplyr::glimpse()

# Análises ----

## Canto de anúncio ----

### Notas ----

notas_anuncio |> 
  tidyr::pivot_longer(cols = `Low Freq`:`Peak Power Density`,
                      names_to = "Variable",
                      values_to = "Values") |>
  dplyr::select(1, 8:9) |> 
  tidyr::drop_na() |> 
  dplyr::summarise(Média = Values |> mean(),
                   `Desvio Padrão` = Values |> sd(),
                   .by = Variable) |> 
  as.data.frame()

### Intervalos ----

intervalos_anuncio |> 
  dplyr::summarise(Média = `Inter Note Delta Time` |> mean(),
                   `Desvio Padrão` = `Inter Note Delta Time` |> sd())

## Canto agonístico -----

### Notas ----

notas_agonistico |> 
  tidyr::pivot_longer(cols = `Low Freq`:`Peak Power Density`,
                      names_to = "Variable",
                      values_to = "Values") |>
  dplyr::select(1, 8:9) |> 
  tidyr::drop_na() |> 
  dplyr::summarise(Média = Values |> mean(),
                   `Desvio Padrão` = Values |> sd(),
                   .by = Variable) |> 
  as.data.frame()

### Intervalos ----

intervalos_agonistico |> 
  dplyr::summarise(Média = `Inter Note Delta Time` |> mean(),
                   `Desvio Padrão` = `Inter Note Delta Time` |> sd())

## Gráfico ----

df_filo <- notas_anuncio |> 
  tidyr::pivot_longer(cols = `Low Freq`:`Peak Power Density`,
                      names_to = "Variable",
                      values_to = "Values") |> 
  dplyr::select(1, 8:9) |> 
  tidyr::drop_na() |> 
  dplyr::bind_rows(intervalos_anuncio |> 
                     tidyr::pivot_longer(cols = `Inter Note Delta Time`,
                                         names_to = "Variable",
                                         values_to = "Values")) |> 
  dplyr::mutate(Call = "Advertisement call") |> 
  dplyr::bind_rows(notas_agonistico |> 
                     tidyr::pivot_longer(cols = `Low Freq`:`Peak Power Density`,
                                         names_to = "Variable",
                                         values_to = "Values") |> 
                     dplyr::select(1, 8:9) |> 
                     tidyr::drop_na() |> 
                     dplyr::bind_rows(intervalos_agonistico |> 
                                        tidyr::pivot_longer(cols = `Inter Note Delta Time`,
                                                            names_to = "Variable",
                                                            values_to = "Values")) |> 
                     dplyr::mutate(Call = "Agonistic call")) |> 
  dplyr::mutate(Values = dplyr::case_when(Variable |> 
                                            stringr::str_detect("Freq") ~ Values / 1000,
                                          .default = Values),
                Variable = dplyr::case_when(Variable |> 
                                               stringr::str_detect("Freq") ~ stringr::str_c(Variable, " (KHz)"),
                                             Variable |> 
                                               stringr::str_detect("Time") ~ stringr::str_c(Variable, " (s)"),
                                             Variable |> 
                                               stringr::str_detect("Amp") ~ stringr::str_c(Variable, " (U)"),
                                             Variable |> 
                                               stringr::str_detect("Density") ~ stringr::str_c(Variable, " (dB/KHz)")))

df_filo |> as.data.frame()

medias <- df_filo |> 
  dplyr::summarise(Values = Values |> mean(),
                .by = c(Variable, Call)) 

medias

df_filo |> 
  ggplot(aes(Call, Values, fill = Call)) +
  ggbeeswarm::geom_quasirandom(size = 2,
                               shape = 21,
                               show.legend = FALSE) +
  geom_point(data = medias, aes(Call, Values, fill = Call),
             shape = 24,
             size = 3,
             stroke = 1) +
  geom_line(data = medias, aes(x = Call, y = Values, , group = 1),
            linewidth = 1,
            linetype = "dashed") +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_fill_manual(values = c("Advertisement call" = "cyan4",
                                "Agonistic call" = "orangered")) +
  labs(y = NULL,
       x = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        strip.text = element_text(color = "black", size = 15),
        legend.position = "none")

ggsave(filename = "filomedusa_medias.tif", 
       height = 10, 
       width = 12,
       compression = "lzw")
