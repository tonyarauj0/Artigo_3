# Analise Exploratoria Caracteristicas Pessoais
library(tidyverse)

# 1. Carregar os dados ----------------------------------------------------

dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))

# 1.1 Selecionar apenas quem votou na EMA 22

base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate_if(is.character, str_to_upper) |>
    mutate_if(is.factor, str_to_upper) |>
    mutate_if(is.character, as.factor)

# 2. ocupacao ------------------------------------------------------------
(barras_ocupacao <-
    base |>
     count(ema22, ocupacao, sort = T) |>
     group_by(ema22) |>
     slice_max(n, n = 10) |>
     ungroup()  |>
     mutate(ocupacao = tidytext::reorder_within(ocupacao, n, ema22))  |>
     ggplot(aes(x = (n/sum(n))*100, y = ocupacao, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), size = 5,color = "white", show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free") +
     labs(x = "", y = "", caption = "Nota: Valores absolutos dentro das barras.") +
     scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
     theme_minimal() +
     theme(plot.caption.position = "plot",
           plot.caption = element_text(hjust = 0)) +
     scale_x_continuous(labels = function(x) paste(format(x,
                                                          big.mark = ".",
                                                          decimal.mark = ",",
                                                          scientific = FALSE),
                                                   "%"
     )
     )
)

ggplot2::ggsave(
    filename = here::here("figures", "dados", "pessoais", "barras_ocupacao.png"),
    plot = barras_ocupacao ,
    dpi = 600,
    width = 15,
    height = 6
)

# 3. instrucao ------------------------------------------------------------
(barras_instrucao <-
     base |>
     count(ema22, instrucao, sort = T) |>
     group_by(ema22) |>
     slice_max(n, n = 3) |>
     ungroup()  |>
     mutate(instrucao = tidytext::reorder_within(instrucao, n, ema22))  |>
     ggplot(aes(x = (n/sum(n))*100, y = instrucao, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n),color = "white", size = 5, show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free") +
     labs(x = "", y = "", caption = "Nota: Valores absolutos dentro das barras.") +
     scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
     theme_minimal() +
     theme(plot.caption.position = "plot",
           plot.caption = element_text(hjust = 0)) +
     scale_x_continuous(labels = function(x) paste(format(x,
                                                          big.mark = ".",
                                                          decimal.mark = ",",
                                                          scientific = FALSE),
                                                   "%"
     )
     )
)


ggplot2::ggsave(
    filename = here::here("figures", "dados", "pessoais", "barras_instrucao.png"),
    plot = barras_instrucao ,
    dpi = 600,
    width = 15,
    height = 6
)



# 3. estado civil ------------------------------------------------------------
(barras_estado_civil <-
     base |>
     count(ema22, estado_civil, sort = T) |>
     group_by(ema22) |>
     slice_max(n, n = 4) |>
     ungroup()  |>
     mutate(estado_civil = tidytext::reorder_within(estado_civil, n, ema22)) |>
     ggplot(aes(x = (n/sum(n))*100, y = estado_civil, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), color = "white", size = 5, show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free") +
     labs(x = "", y = "", caption = "Nota: Valores absolutos dentro das barras.") +
     scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
     theme_minimal() +
     theme(plot.caption.position = "plot",
           plot.caption = element_text(hjust = 0)) +
     scale_x_continuous(labels = function(x) paste(format(x,
                                                          big.mark = ".",
                                                          decimal.mark = ",",
                                                          scientific = FALSE),
                                                   "%"
     )
     )
)

barras_estado_civil


ggplot2::ggsave(
    filename = here::here("figures", "dados", "pessoais", "barras_estado_civil.png"),
    plot = barras_estado_civil ,
    dpi = 600,
    width = 15,
    height = 6
)


# 5. raca ------------------------------------------------------------
(barras_raca <-
     base |>
     count(ema22, raca, sort = T) |>
     group_by(ema22) |>
     slice_max(n, n = 4) |>
     ungroup()  |>
     mutate(raca = tidytext::reorder_within(raca, n, ema22)) |>
     ggplot(aes(x = (n/sum(n))*100, y = raca, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), size = 5,color = "white", show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free") +
     labs(x = "", y = "", caption = "Nota: Valores absolutos dentro das barras.") +
     scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
     theme_minimal() +
     theme(plot.caption.position = "plot",
           plot.caption = element_text(hjust = 0)) +
     scale_x_continuous(labels = function(x) paste0(format(x,
                                                           big.mark = ".",
                                                           decimal.mark = ",",
                                                           scientific = FALSE),
                                                    "%"
     )
     )
)

ggplot2::ggsave(
    filename = here::here("figures", "dados", "pessoais", "barras_raca.png"),
    plot = barras_raca ,
    dpi = 600,
    width = 15,
    height = 6
)




# 6. regiao ---------------------------------------------------------------
(barras_regiao <-
        base |>
        count(ema22, regiao, sort = T) |>
        group_by(ema22) |>
        slice_max(n, n = 5) |>
        ungroup()  |>
        mutate(regiao = tidytext::reorder_within(regiao, n, ema22)) |>
        ggplot(aes(x = (n/sum(n))*100,
                   y = regiao,
                   fill = ema22)) +
        geom_col(show.legend = FALSE, alpha = 0.8) +
        geom_label(aes(label = n), size = 5, color = "white", show.legend = F) +
        tidytext::scale_y_reordered() +
        facet_wrap(~ema22, scales = "free") +
        labs(x = "%", y = "", caption = "Nota: Valores absolutos dentro das barras") +
        scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
        theme_minimal() +
        theme(plot.caption.position = "plot",
           plot.caption = element_text(hjust = 0)) +
     scale_x_continuous(labels = function(x) paste0(format(x,
                                                          big.mark = ".",
                                                          decimal.mark = ",",
                                                          scientific = FALSE),
                                                   "%"
     )
     )
)

ggplot2::ggsave(
    filename = here::here("figures", "dados", "pessoais", "barras_regiao.png"),
    plot = barras_regiao ,
    dpi = 600,
    width = 15,
    height = 6
)



# 6. uf ---------------------------------------------------------------
(barras_deputado_sigla_uf <-
     base |>
     count(ema22, deputado_sigla_uf, sort = T) |>
     group_by(ema22) |>
     slice_max(n, n = 10) |>
     ungroup()  |>
     mutate(deputado_sigla_uf = tidytext::reorder_within(deputado_sigla_uf, n, ema22)) |>
     ggplot(aes(x = (n/sum(n))*100,
                y = deputado_sigla_uf,
                fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), size = 5, color = "white", show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free") +
     labs(x = "", y = "", caption = "Nota: Valores absolutos dentro das barras.") +
     scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
     theme_minimal() +
     theme(plot.caption.position = "plot",
           plot.caption = element_text(hjust = 0)) +
     scale_x_continuous(labels = function(x) paste(format(x,
                                                    big.mark = ".",
                                                    decimal.mark = ",",
                                                    scientific = FALSE),
                                                   "%"
     )
     )
)

ggplot2::ggsave(
    filename = here::here("figures", "dados", "pessoais", "barras_deputado_sigla_uf.png"),
    plot = barras_deputado_sigla_uf ,
    dpi = 600,
    width = 15,
    height = 6
)


# # Juntar os gráficos ------------------------------------------------------
# library(patchwork)
# barras_ocupacao +
#     barras_estado_civil +
#     barras_instrucao +
#     # barras_raca +
#     barras_regiao +
#     plot_layout(ncol = 1)
