# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(janitor)

library(vegan)

library(flextable)

library(ggdendro)

# Dados ----

## Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar ----

dados <- "Consultoria ciclo ambiental/Turvo, PR/Dados" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("Planilha")) |>
  dplyr::pull(id) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path) |>
  readxl::read_xlsx()

## Visualizar ----

dados

dados |> dplyr::glimpse()

## Tratar ----

dados <- dados |>
  dplyr::select(-1) |>
  dplyr::slice(-1) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~dplyr::if_else(dplyr::row_number() == 2 & is.na(.x),
                             dplyr::lag(.x),
                             .x)
      )
    ) |>
  dplyr::slice(-1) |>
  janitor::row_to_names(row_number = 1) |>
  dplyr::rename("Área" = `ÁREA  DE REGISTRO`)

dados

dados |> dplyr::glimpse()

# Rarefação ----

## Calcular rarefação ----

rare <- dados |>
  dplyr::summarise(abundancia = dplyr::n(),
                   .by = c(Área,
                           `Nome Científico`)) |>
  tidyr::pivot_wider(names_from = `Nome Científico`,
                     values_from = abundancia,
                     values_fill = 0) |>
  vegan::estaccumR(permutations = 1000) |>
  summary(display = c("S", "chao"))

rare

## Transformar em Data frame ----

rare_df <- purrr::imap_dfr(
  rare,
  \(valores, indice){

    valores |>
      as.data.frame() |>
      dplyr::rename("Quantidade de unidades amostrais" = 1,
                    Riqueza = 2) |>
      dplyr::mutate(tipo = indice)

    },
  .progress = TRUE) |>
  dplyr::mutate(tipo = dplyr::case_when(

    tipo == "chao" ~ "Estimado (Chao1)",
    .default = "Observado"

    ))

rare_df

## Visualizar os dados ----

rare_df |>
  ggplot(aes(`Quantidade de unidades amostrais`, Riqueza,
             color = tipo,
             fill = tipo)) +
  geom_ribbon(aes(x = `Quantidade de unidades amostrais`,
                  y = Riqueza,
                  ymin = Riqueza - Std.Dev,
                  ymax = Riqueza + Std.Dev),
              alpha = 0.3) +
  geom_line(linewidth = 2) +
  geom_point(shape = 21, color = "black", stroke = 1, size = 7.5) +
  scale_fill_manual(breaks = c("Observado", "Estimado (Chao1)"),
                    values = c("orange", "royalblue")) +
  scale_color_manual(breaks = c("Observado", "Estimado (Chao1)"),
                     values = c("orange", "royalblue")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 30),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 20),
        axis.line = element_line(color = "black", linewidth = 1))

ggsave(filename = "rarefacao_12.11.2026.png",
       height = 12,
       width = 16)

"rarefacao_12.11.2026.png" |>
  googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                            name = "rarefacao_12.11.2026.png",
                            overwrite = TRUE)

# Diversidade alfa ----

## Tratar os dados ----

dados_div <- dados |>
  dplyr::summarise(abundancia = dplyr::n(),
                   .by = c(`Fase de licenciamento`,
                           Área,
                           `Nome Científico`)) |>
  tidyr::pivot_wider(names_from = `Nome Científico`,
                     values_from = abundancia,
                     values_fill = 0)

dados_div

## Calcular a diversidade ----

div_alfa <- purrr::map_dfr(
  dados_div$`Fase de licenciamento`,
  \(fase){

    riq <- dados_div |>
      dplyr::filter(`Fase de licenciamento` == fase) |>
      dplyr::select(-`Fase de licenciamento`) |>
      tibble::column_to_rownames(var = "Área") |>
      vegan::specnumber()

    shannon <-  dados_div |>
      dplyr::filter(`Fase de licenciamento` == fase) |>
      dplyr::select(-`Fase de licenciamento`) |>
      tibble::column_to_rownames(var = "Área") |>
      vegan::diversity()

    tibble::tibble(`Fase de licenciamento` = fase,
                   Área = shannon |> names(),
                   Riqueza = riq,
                   `H'` = shannon)

  },
  .progress = TRUE) |>
  dplyr::mutate(`Fase de licenciamento` = dplyr::case_when(

    `Fase de licenciamento` == "pré-obra" ~ "Pré-obra",
    .default = `Fase de licenciamento`

    ) |>
      forcats::fct_relevel(c("Pré-obra", "Durante obra")))

div_alfa

## Gráfico ----

div_alfa |>
  tidyr::pivot_longer(cols = 3:4,
                      values_to = "Diversidade",
                      names_to = "indice") |>
  ggplot(aes(`Fase de licenciamento`, Diversidade, label = Área,
             group = Área)) +
  geom_line(linewidth = 1) +
  geom_label(size = 7.5, fill = "gray", linewidth = 1) +
  facet_wrap(~indice, scale = "free_y") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 30))
