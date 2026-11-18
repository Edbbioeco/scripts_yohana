# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(janitor)

library(vegan)

library(flextable)

library(officer)

library(writexl)

library(ggdendro)

library(patchwork)

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
  readxl::read_xlsx(sheet = 2)

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
  dplyr::rename("Área" = `Área de Registro`)

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

ggsave(filename = "rarefacao_meli_12.11.2026.png",
       height = 12,
       width = 16)

"rarefacao_meli_12.11.2026.png" |>
  googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                            name = "rarefacao_meli_12.11.2026.png",
                            overwrite = TRUE)

## Rarefação baseado no número de visitas ----

### Calcular ----

rare_total <- dados |>
  dplyr::summarise(abundancia = dplyr::n(),
                   .by = c(Área,
                           `Fase de licenciamento`,
                           `Nome Científico`)) |>
  tidyr::pivot_wider(names_from = `Nome Científico`,
                     values_from = abundancia,
                     values_fill = 0) |>
  vegan::estaccumR(permutations = 1000) |>
  summary(display = c("S", "chao"))

rare_total

### Transformar em data frame ----

rare_sd_total <- purrr::imap_dfr(
  rare_total,
  \(valores, indice){

    valores |>
      as.data.frame() |>
      dplyr::rename("Quantidade de amostragens nas áreas" = 1,
                    Riqueza = 2) |>
      dplyr::mutate(tipo = indice)

  },
  .progress = TRUE) |>
  dplyr::mutate(tipo = dplyr::case_when(

    tipo == "chao" ~ "Estimado (Chao1)",
    .default = "Observado"

  ))

rare_sd_total

### Visualizar os dados ----

rare_sd_total |>
  ggplot(aes(`Quantidade de amostragens nas áreas`, Riqueza,
             color = tipo,
             fill = tipo)) +
  geom_ribbon(aes(x = `Quantidade de amostragens nas áreas`,
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
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 30),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 20),
        axis.line = element_line(color = "black", linewidth = 1))

ggsave(filename = "rarefacao_meli_total_12.11.2026.png",
       height = 12,
       width = 16)

"rarefacao_total_12.11.2026.png" |>
  googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                            name = "rarefacao_meli_total_12.11.2026.png",
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

    pielou <- shannon / log(riq)

    tibble::tibble(`Fase de licenciamento` = fase,
                   Área = shannon |> names(),
                   S = riq,
                   `H'` = shannon,
                   J = dplyr::if_else(
                     pielou |> is.nan(),
                     0,
                     pielou
                   ))

  },
  .progress = TRUE) |>
  dplyr::mutate(`Fase de licenciamento` = dplyr::case_when(

    `Fase de licenciamento` == "pré-obra" ~ "Pré-obra",
    .default = `Fase de licenciamento`

  ) |>
    forcats::fct_relevel(c("Pré-obra", "Durante obra")))

div_alfa
