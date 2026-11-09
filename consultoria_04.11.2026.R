# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(vegan)

library(patchwork)

# Dados ----

## Autenticar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Immportar ----

dados <- purrr::map(
  "Recitech/CGH Cacatu" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name %in% c("indices cgh Cacatu - mastofauna- 1 a 2- L.O",
                              "Dados Mastofauna - CGH Cacatu")) |>
    dplyr::pull(id),
  \(arquivo){

    arquivo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx()

    },
  .progress = TRUE) |>
  purrr::set_names(
    "Recitech/CGH Cacatu" |>
      googledrive::drive_get() |>
      googledrive::drive_ls() |>
      dplyr::filter(name %in% c("indices cgh Cacatu - mastofauna- 1 a 2- L.O",
                                "Dados Mastofauna - CGH Cacatu")) |>
      dplyr::pull(name) |>
      stringr::str_remove(".xlsx"))

## Visualizar ----

dados

dados |> dplyr::glimpse()

## Tratar ----

dados <- dados |>
  purrr::pluck(1) |>
  dplyr::select(Campanha, Área, Espécie:Quantidade) |>
  dplyr::mutate(Campanha = Campanha |>
                  stringr::str_remove("2- ") |>
                  stringr::str_to_title()) |>
  dplyr::bind_rows(
    dados |>
      purrr::pluck(2) |>
      dplyr::select(Campanha, Área, Espécie,
                    `Nome comum` = `Nome Vulgar`,
                    "Quantidade" = `Nº Ind.2`) |>
      dplyr::mutate(Campanha = Campanha |>
                      stringr::str_replace(stringr::fixed("/"),
                                           " "),
                    Área = Área |>
                      stringr::str_remove("TR."))
    )

dados

dados |> dplyr::glimpse()

# Curva de rarefação ----

## Calcular a riqueza ----

riq <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specaccum(method = "rarefaction") |>
  purrr::pluck("richness")

riq

## Calcular desvio padrão ----

sd <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specaccum(method = "rarefaction") |>
  purrr::pluck("sd")

sd

## Calcular jackknife 1 ----

jackk <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specpool() |>
  purrr::pluck("jack1")

jackk

## Criar data frame com as informações ----

df_rare <- tibble::tibble(Campanhas = 0:3,
                          Espécies = c(0, riq),
                          sd = c(0, sd[1] |> rep(3)))

df_rare

## Gráfico ----

ggplot() +
  geom_line(data = df_rare,
            aes(x = Campanhas, y = Espécies + sd,
                linetype = "Desvio Padrão"),
            color = "black",
            linewidth = 3) +
  geom_line(data = df_rare,
            aes(x = Campanhas, y = Espécies,
                linetype = "Curva de Acumulação de Espécies"),
            color = "black",
            linewidth = 3) +
  geom_line(data = df_rare,
            aes(x = Campanhas, y = Espécies - sd,
                linetype = "Desvio Padrão"),
            color = "black",
            linewidth = 3) +
  labs(y = "Número de Espécies",
       title = "Curva do coletor",
       linetype = NULL) +
  scale_x_continuous(breaks = seq(0, 3, 1),
                     limits = c(0, 3),
                     expand = FALSE) +
  scale_y_continuous(breaks = seq(0, 12, 2),
                     limits = c(0, 12),
                     expand = FALSE) +
  guides(linetype = guide_legend(keywidth = unit(90, "pt"))) +
  theme_classic() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey60", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black", size = 35),
        axis.text.x = element_text(margin = margin(t = 20)),
        axis.text.y = element_text(margin = margin(r = 20)),
        axis.title = element_text(color = "black", size = 35),
        axis.title.y = element_text(margin = margin(r = 6)),
        axis.title.x = element_text(margin = margin(t = 6)),
        plot.title = element_text(hjust = 0.5, size = 45,
                                  face = "plain", margin = margin(b = 8)),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.width = unit(28, "pt"),
        legend.text = element_text(size = 25),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = unit(12, "pt"),
        plot.margin = margin(10, 14, 8, 8))

ggsave(filename = "gráfico_rarefacao_04.11.2026.png",
       height = 10,
       width = 20)

"gráfico_rarefacao_04.11.2026.png" |>
  googledrive::drive_upload(path = "Recitech/CGH Cacatu",
                            name = "gráfico_rarefacao_04.11.2026.png",
                            overwrite = TRUE)

# Diversidade de espécies ----

## Calcular ----

div_df <- purrr::pmap_dfr(
  tidyr::expand_grid(campanha = dados$Campanha |> unique(),
                     indice = c("simpson", "shannon", "pielou")),
  \(campanha, indice){

    if(indice == "pielou"){

      shannon <- dados |>
        dplyr::select(- `Nome comum`) |>
        dplyr::filter(Campanha == campanha) |>
        tidyr::pivot_wider(names_from = Espécie,
                           values_from = Quantidade,
                           values_fill = 0,
                           values_fn = max) |>
        tibble::column_to_rownames(var = "Área") |>
        dplyr::select_if(is.numeric) |>
        vegan::diversity(index = "shannon")

      riq <- dados |>
        dplyr::select(- `Nome comum`) |>
        dplyr::filter(Campanha == campanha) |>
        tidyr::pivot_wider(names_from = Espécie,
                           values_from = Quantidade,
                           values_fill = 0,
                           values_fn = max) |>
        tibble::column_to_rownames(var = "Área") |>
        dplyr::select_if(is.numeric) |>
        vegan::specnumber()

      tibble::tibble(indice = indice,
                     diversidade = (shannon / log(riq)),
                     Campanha = campanha,
                     Área = shannon |> names())

    } else {

      div <- dados |>
        dplyr::select(- `Nome comum`) |>
        dplyr::filter(Campanha == campanha) |>
        tidyr::pivot_wider(names_from = Espécie,
                           values_from = Quantidade,
                           values_fill = 0,
                           values_fn = max) |>
        tibble::column_to_rownames(var = "Área") |>
        dplyr::select_if(is.numeric) |>
        vegan::diversity(index = indice)

      tibble::tibble(indice = indice,
                     diversidade = div,
                     Campanha = campanha,
                     Área = div |> names())

      }

    },
  .progress = TRUE) |>
  dplyr::mutate(diversidade = dplyr::case_when(

    diversidade |> is.nan() ~ 0,
    .default = diversidade
  ))

div_df

## Grpáficos ----

### Criar ----

graficos <- purrr::map2(
  div_df$indice |> unique(),
  paste0(c("Dominância de Simpson",
             "Diversidade de Shannon-wiener",
             "Equitabilidade de Pielou"),
           " - Mastofauna"),
  \(ind, titulo){

    cores <- c(A = "#595959", B = "#B2B2B2", C = "#8C8C8C", D = "#262626")

    xlim_comum <- c(0.3, 3.5)

    eixo_y <- tibble::tibble(y = seq(0, 1, 0.1) |> rep(2),
                             indice = rep(c(ind, "pielou"),
                                          each = length(y) / 2)) |>
      dplyr::bind_rows(tibble::tibble(y = seq(0, 1.5, 0.1),
                                      indice = "shannon")) |>
      dplyr::mutate(rotulo = purrr::map_chr(y, ~ format(.x, decimal.mark = ","))) |>
      dplyr::filter(indice == ind)

    grafico <- div_df |>
      dplyr::filter(indice == ind) |>
      tidyr::complete(Campanha, Área,
                      fill = list(indice = ind, diversidade = 0)) |>
      dplyr::mutate(Campanha = Campanha |>
                      forcats::fct_relevel(c("Verão 2026",
                                             "Outono 2026",
                                             "Inverno 2026")),
                    x = as.integer(Campanha)) |>
      ggplot2::ggplot() +
      ## Linhas de grade (começam onde começa a área das barras)
      ggplot2::geom_segment(data = eixo_y,
                            ggplot2::aes(x = 0.5, xend = 3.5, y = y, yend = y),
                            color = "grey85", linewidth = 0.4) +
      ## Rótulos do eixo y, dentro da coluna da esquerda
      ggplot2::geom_text(data = eixo_y,
                         ggplot2::aes(x = 0.4, y = y, label = rotulo),
                         color = "grey20", size = 3.5) +
      ggplot2::geom_col(ggplot2::aes(x = x, y = diversidade, fill = Área),
                        position = ggplot2::position_dodge(width = 0.73),
                        width = 0.73) +
      ggplot2::scale_fill_manual(values = cores) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::coord_cartesian(xlim = xlim_comum,
                               ylim = c(0, eixo_y$y |> max()),
                               clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none",
                     plot.margin = ggplot2::margin(t = 8, r = 0, b = 0, l = 0))

    celulas <- div_df |>
      dplyr::filter(indice == ind) |>
      dplyr::mutate(Campanha = Campanha |>
                      forcats::fct_relevel(c("Verão 2026",
                                             "Outono 2026",
                                             "Inverno 2026")),
                    Área = Área |>
                      forcats::fct_relevel(LETTERS[1:4])) |>
      tidyr::complete(Campanha, Área,
                      fill = list(indice = ind, diversidade = 0)) |>
      dplyr::mutate(x = as.integer(Campanha),
                    y = 5 - as.integer(Área),
                    rotulo = diversidade |>
                      round(2) |>
                      format(decimal.mark = ","))

    cabecalho <- tibble::tibble(x = 1:length(div_df$Campanha |> unique()),
                                y = length(div_df$Área |> unique()) + 1,
                                rotulo = c("Verão 2026",
                                           "Outono 2026",
                                           "Inverno 2026"))

    legenda <- tibble::tibble(grupo = LETTERS[1:4],
                              y = length(div_df$Área |> unique()):1)

    tabela <- ggplot2::ggplot() +
      ## Células de dados e cabeçalho
      ggplot2::geom_tile(data = dplyr::bind_rows(celulas, cabecalho),
                         ggplot2::aes(x = x, y = y),
                         fill = NA, color = "grey85", width = 1, height = 1) +
      ggplot2::geom_text(data = dplyr::bind_rows(celulas, cabecalho),
                         ggplot2::aes(x = x, y = y, label = rotulo),
                         color = "grey20", size = 3.5) +
      ## Coluna da legenda (célula + quadrado + letra)
      ggplot2::geom_tile(data = legenda,
                         ggplot2::aes(x = 0.4, y = y),
                         fill = NA, color = "grey85", width = 0.2, height = 1) +
      ggplot2::geom_point(data = legenda,
                          ggplot2::aes(x = 0.35, y = y, color = grupo),
                          shape = 15, size = 5) +
      ggplot2::geom_text(data = legenda,
                         ggplot2::aes(x = 0.42, y = y, label = grupo),
                         color = "grey20", size = 5, hjust = 0) +
      ggplot2::scale_color_manual(values = cores, guide = "none") +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::coord_cartesian(xlim = xlim_comum, ylim = c(0.5, 5.5)) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

    (grafico / tabela) +
      patchwork::plot_layout(heights = c(4, 1.6)) +
      patchwork::plot_annotation(
        title = titulo,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5,
                                             size = 30,
                                             color = "grey10"),
          plot.margin = ggplot2::margin(10, 10, 10, 10),
          plot.background = ggplot2::element_rect(fill = "white",
                                                  color = "grey60",
                                                  linewidth = 0.8)
        )
      )

    },
  .progress = TRUE) |>
  purrr::set_names(div_df$indice |> unique())

graficos

### Salvar ----

purrr::imap(
  graficos,
  \(grafico, ind){

    ggsave(plot = grafico,
           filename = paste0("grafico_", ind, "04.11.2026.png"),
           height = 10, width = 16)

    },
  .progress = TRUE)

### Subir para o Google Drive ----

purrr::map(
  list.files(pattern = "^grafico_.+04\\.11\\.2026\\.png$"),
  \(arquivo){

    arquivo |>
      googledrive::drive_upload(path = "Recitech/CGH Cacatu",
                                name = arquivo,
                                overwrite = TRUE)

  },
  .progress = TRUE)
