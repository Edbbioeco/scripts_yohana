# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(janitor)

library(vegan)

library(flextable)

library(officer)

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

    pielou <- shannon / log(riq)

    tibble::tibble(`Fase de licenciamento` = fase,
                   Área = shannon |> names(),
                   Riqueza = riq,
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

## Gráfico ----

div_alfa |>
  tidyr::pivot_longer(cols = 3:5,
                      values_to = "Diversidade",
                      names_to = "indice") |>
  dplyr::mutate(indice = indice |>
                  forcats::fct_relevel(c("Riqueza", "H'", "J"))) |>
  ggplot(aes(`Fase de licenciamento`, Diversidade, label = Área,
             group = Área)) +
  geom_line(linewidth = 1) +
  geom_label(size = 7.5, fill = "gray", linewidth = 1) +
  facet_wrap(~indice, scale = "free_y") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 30),
        strip.background = element_rect(linewidth = 2, fill = "gray"),
        axis.line = element_line(color = "black", linewidth = 1))

ggsave(filename = "diversidade_alfa_12.11.2026.png",
       height = 12,
       width = 16)

"diversidade_alfa_12.11.2026.png" |>
  googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                            name = "diversidade_alfa_12.11.2026.png",
                            overwrite = TRUE)

# Tabela das espécies ----

## Tratar os dados ----

dados_tabela <- dados |>
  dplyr::mutate(Campanha = dplyr::case_when(

    `Fase de licenciamento` |>
      stringr::str_detect("pré-") ~ "1",
    .default = "2"

    )) |>
  dplyr::group_by(`Nome Científico`, `Nome vulgar`) |>
  dplyr::summarise(Área = paste0(Área |> unique(),
                                 collapse = ", "),
                   Amostragem = paste0(`Metodologias empregadas` |> unique(),
                                       collapse = ", "),
                   Campanha = paste0(Campanha |> unique(),
                                     collapse = ", ")) |>
  dplyr::mutate(ordem = dplyr::case_when(

    `Nome Científico` %in% c("Tomodon dorsatus",
                             "Salvator merianae",
                             "Ophiodes fragilis") ~ "REPTILIA",
    .default = "AMPHIBIA"

    ),
  familia = dplyr::case_when(

    `Nome Científico` |>
      stringr::str_detect("Rhinella") ~ "Bufonidae",
    `Nome Científico` |>
      stringr::str_detect("Scinax|Boana|Aplastodiscus|Dendropsophus minutus") ~ "Hylidae",
    `Nome Científico` |>
      stringr::str_detect("Leptodactylus|Physalaemus") ~ "Leptodactylidae",
    `Nome Científico` |>
      stringr::str_detect("Proceratophrys|Odontophrynus") ~ "Odontophrynidae",
    `Nome Científico` |>
      stringr::str_detect("Iscnhnocnema") ~ "Brachycephalidae",
    `Nome Científico` |>
      stringr::str_detect("Phyllomedusa") ~ "Phyllomedusidae",
    `Nome Científico` |>
      stringr::str_detect("Salvator") ~ "Teiidae",
    `Nome Científico` |>
      stringr::str_detect("Tomodon") ~ "Colubridae",
    `Nome Científico` |>
      stringr::str_detect("Ophiodes") ~ "Diploglossidae"

  )) |>
  dplyr::select(ordem,
                familia,
                `Nome Científico`,
                `Nome vulgar`,
                Amostragem,
                Área,
                Campanha) |>
  as.data.frame() |>
  dplyr::arrange(ordem, familia, `Nome Científico`)

dados_tabela

## Criar estrutura da tabela ----

tabela <- dados_tabela |>
  dplyr::group_split(ordem, familia) |>
  purrr::map_dfr(
    \(df){

      ordem <- tibble::tibble(`Ordenamento taxonômico` = df$ordem |>
                                unique(),
                              `Nome comum` = NA_character_,
                              Amostragem = NA_character_,
                              Área = NA_character_,
                              Campanha = NA_character_)

      familia <- tibble::tibble(`Ordenamento taxonômico` = df$familia |>
                                  unique(),
                                `Nome comum` = NA_character_,
                                Amostragem = NA_character_,
                                Área = NA_character_,
                                Campanha = NA_character_)

      sps <- tibble::tibble(`Ordenamento taxonômico` = df$`Nome Científico`,
                            `Nome comum` = df$`Nome vulgar`,
                            Amostragem = df$Amostragem,
                            Área = df$Área,
                            Campanha = df$Campanha)

      dplyr::bind_rows(ordem,
                       familia,
                       sps)

      },
    .progress = TRUE) |>
  dplyr::group_by(`Ordenamento taxonômico`) |>
  dplyr::mutate(linha = dplyr::row_number()) |>
  dplyr::filter(linha == 1) |>
  dplyr::select(-linha) |>
  as.data.frame()

tabela

## Adicionar as informações dos status de ameaça ----

tabela <- tabela |>
  dplyr::left_join(dados |>
                     dplyr::distinct(`Nome Científico`, .keep_all = TRUE) |>
                     dplyr::select("Ordenamento taxonômico" = `Nome Científico`,
                                   IUCN,
                                   "Brasil" = Nacional,
                                   "PR" = Estadual),
                   by = "Ordenamento taxonômico")

tabela

## Tabela flextable ----

tabela_flex <- tabela |>
  flextable::flextable() |>
  flextable::add_header_row(top = TRUE,
                            values = c("Ordenamento taxonômico",
                                       "Nome comum",
                                       "Amostragem",
                                       "Área",
                                       "Campanha",
                                       "Status"),
                            colwidths = c(1, 1, 1, 1, 1, 3)) |>
  flextable::merge_v(part = "header") |>
  flextable::hline(i = 1,
                   border = officer::fp_border(width = 0),
                   part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::bold(j = 1,
                  i = ~`Ordenamento taxonômico` |>
                    stringr::str_detect("IA|idae")) |>
  flextable::bold(part = "header") |>
  flextable::italic(j = 1,
                    i = ~!`Ordenamento taxonômico` |>
                      stringr::str_detect("IA|idae")) |>
  flextable::bg(bg = "gray",
                part = "header") |>
  flextable::border_inner_v(border = officer::fp_border(color = "black",
                                                        width = 2),
                            part = "all") |>
  flextable::border_outer(border = officer::fp_border(width = 0),
                          part = "all") |>
  flextable::hline(i = 2,
                   j = 1:8,
                   border = officer::fp_border(color = "black",
                                               width = 2),
                   part = "header") |>
  flextable::width(j = 1,
                   width = 2) |>
  flextable::width(j = 4,
                   width = 2)

tabela_flex

tabela_flex |>
  flextable::save_as_docx(path = "tabela_especies_12.11.2026.docx")

"tabela_especies_12.11.2026.docx" |>
  googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                            name = "tabela_especies_12.11.2026.docx",
                            overwrite = TRUE)

## Estatística das espécies ----

### Número de espécies por ordem e campanha ----

dados_tabela |>
  tidyr::separate_rows(Campanha, sep = ", ") |>
  dplyr::summarise(riqueza = dplyr::n(),
                   .by = c(ordem, Campanha))

### Quantidade de famílias ----

dados_tabela |>
  dplyr::pull(familia) |>
  unique()

## Riqueza em campanhas isoladas ----

dados_tabela |>
  dplyr::summarise(riqueza = dplyr::n(),
                   .by = c(ordem, Campanha))

# Diversidade beta ----

## Calcular matrizes de similaridade ----

### Total ----

matriz_sim_total <- 1- dados |>
  dplyr::summarise(abundancia = dplyr::n(),
                   .by = c(Área,
                           `Nome Científico`)) |>
  tidyr::pivot_wider(names_from = `Nome Científico`,
                     values_from = abundancia,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  vegan::vegdist()

matriz_sim_total

### Por fase do licenciamento ----

matriz_sim_fases <- purrr::map(
  dados$`Fase de licenciamento` |> unique(),
  \(fase){

    1 - dados |>
      dplyr::filter(`Fase de licenciamento` == fase) |>
      dplyr::summarise(abundancia = dplyr::n(),
                       .by = c(Área,
                               `Nome Científico`)) |>
      tidyr::pivot_wider(names_from = `Nome Científico`,
                         values_from = abundancia,
                         values_fill = 0) |>
      tibble::column_to_rownames(var = "Área") |>
      vegan::vegdist()

    },
  .progress = TRUE) |>
  purrr::set_names(dados$`Fase de licenciamento` |> unique())

matriz_sim_fases

## UPGMA ----

### Total ----

upgma_total <- matriz_sim_total |>
  hclust(method = "average")

upgma_total

### Por fase do licenciamento ----

upgma_fases <- purrr::map(
  matriz_sim_fases,
  \(matriz){

    matriz |>
      hclust(method = "average")

    },
  .progress = TRUE)

upgma_fases

## Dendogramas ----

### Total ----

#### Extrait valores ----

upgma_data_total <- upgma_total |>
  as.dendrogram() |>
  ggdendro::dendro_data()

upgma_data_total

ramos_total <- upgma_data_total$segments

ramos_total

area_total <- upgma_data_total$label

area_total

#### Gráfico ----

ggplot() +
  geom_segment(data = ramos_total, aes(x = x, y = y,
                                 xend = xend, yend = yend),
               linewidth = 2) +
  labs(x = NULL,
       y = "Similaridade de Bray-Curtis") +
  scale_x_continuous(breaks = 1:4,
                     labels = area_total$label) +
  scale_y_continuous(limits = c(0, 0.38), expand = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0,
                                   color = "black", size = 35),
        axis.text = element_text(color = "black", size = 35),
        axis.title = element_text(color = "black", size = 35),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(filename = "upgma_total_12.11.2026.png",
       height = 12,
       width = 16)

"upgma_total_12.11.2026.png" |>
  googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                            name = "upgma_total_12.11.2026.png",
                            overwrite = TRUE)

### Por fases ----

### Extrair valores ----

upgma_data_fases <- purrr::map(
  upgma_fases,
  \(upgma){

    upgma_data <- upgma |>
      as.dendrogram() |>
      ggdendro::dendro_data()

    ramos <- upgma_data$segments

    area <- upgma_data$label

    list(upgma_data,
         ramos,
         area) |>
      purrr::set_names(c("Dados brutos",
                         "Ramo",
                         "Áreas"))

    },
  .progress = TRUE)

upgma_data_fases

#### Gráfico ----

upgma_grafico_pre <- ggplot() +
  geom_segment(data = upgma_data_fases |>
                 purrr::pluck(1) |>
                 purrr::pluck("Ramo"),
               aes(x = y, y = x,
                   xend = yend, yend = xend),
               linewidth = 2) +
  labs(y = NULL,
       x = NULL,
       title = "Pré-obra") +
  scale_y_continuous(breaks = 1:4,
                     labels = NULL,
                     sec.axis =dup_axis(
                       breaks = 1:4,
                       labels = upgma_data_fases |>
                         purrr::pluck(1) |>
                         purrr::pluck("Áreas") |>
                         purrr::pluck("label"),
                       name = NULL)) +
  scale_x_continuous(limits = c(0, 0.04),
                     transform = "reverse") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0,
                                   color = "black", size = 35),
        axis.text = element_text(color = "black", size = 35),
        axis.title = element_text(color = "black", size = 35),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 35))

upgma_grafico_pre

upgma_grafico_durante <- ggplot() +
  geom_segment(data = upgma_data_fases |>
                 purrr::pluck(2) |>
                 purrr::pluck("Ramo"),
               aes(x = y, y = x,
                   xend = yend, yend = xend),
               linewidth = 2) +
  labs(y = NULL,
       x = NULL,
       title = "Durante-obra") +
  scale_y_continuous(breaks = 1:4,
                     labels = upgma_data_fases |>
                       purrr::pluck(1) |>
                       purrr::pluck("Áreas") |>
                       purrr::pluck("label"),
                     name = NULL,
                     sec.axis = dup_axis(
                       breaks = 1:4,
                       labels = NULL)) +
  scale_x_continuous(limits = c(0, 0.4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0,
                                   color = "black", size = 35),
        axis.text = element_text(color = "black", size = 35),
        axis.title = element_text(color = "black", size = 35),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  color = "black",
                                  size = 35))

upgma_grafico_durante

(upgma_grafico_pre + upgma_grafico_durante) +
  patchwork::plot_annotation(
    caption = "Similaridade de Bray-Curtis",
    theme = theme(plot.caption = element_text(hjust = 0.5,
                                              color = "black",
                                              size = 35)))

ggsave(filename = "upgma_fases_12.11.2026.png",
       height = 12,
       width = 16)

"upgma_fases_12.11.2026.png" |>
  googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                            name = "upgma_fases_12.11.2026.png",
                            overwrite = TRUE)

## Permanova ----

### Callcular matriz de similaridade ----

matriz_permanova <- 1 - dados |>
  dplyr::summarise(abundancia = dplyr::n(),
                   .by = c(Área,
                           `Nome Científico`,
                           `Fase de licenciamento`)) |>
  tidyr::pivot_wider(names_from = `Nome Científico`,
                     values_from = abundancia,
                     values_fill = 0) |>
  dplyr::arrange(Área) |>
  dplyr::arrange(`Fase de licenciamento` |> dplyr::desc()) |>
  dplyr::mutate(area = purrr::pmap_chr(
    list(Área, `Fase de licenciamento`),
    \(Área, `Fase de licenciamento`){

      paste0(Área,
             "_",
             `Fase de licenciamento`)

      },
    .progress = TRUE)) |>
  tibble::column_to_rownames(var = "area") |>
  dplyr::select_if(is.numeric) |>
  vegan::vegdist()

matriz_permanova

### Calcular permanova ----

permanova <- vegan::adonis2(matriz_permanova ~ `Fase de licenciamento`,
                            data = dados |>
                              dplyr::summarise(abundancia = dplyr::n(),
                                               .by = c(Área,
                                                       `Nome Científico`,
                                                       `Fase de licenciamento`)) |>
                              tidyr::pivot_wider(names_from = `Nome Científico`,
                                                 values_from = abundancia,
                                                 values_fill = 0) |>
                              dplyr::arrange(Área) |>
                              dplyr::arrange(`Fase de licenciamento` |>
                                               dplyr::desc()),
                            permutations = 999)

permanova
