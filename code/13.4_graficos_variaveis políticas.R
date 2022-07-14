# Analise Exploratoria variáveis políticas
library(tidyverse)

# 1. Carregar os dados ----------------------------------------------------

dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))

# 2 Selecionar apenas quem votou na EMA 22 ---

base <- dados |> filter(ema22 != "Artigo 17") |>
    mutate(orientacao_ema22 = ifelse(is.na(orientacao_ema22), "AUSENTE", orientacao_ema22)) |>
    mutate_if(is.character, str_to_upper) |>
    mutate_if(is.character, as.factor)

base.1 <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate_if(is.character, str_to_upper) |>
    mutate_if(is.factor, str_to_upper) |>
    mutate_if(is.character, as.factor)


# 2. Orientacao partido --------------------------------------
(barras_orientacao <-
     base |>
     count(ema22, orientacao_ema22, sort = T) |>
     mutate(orientacao = tidytext::reorder_within(orientacao_ema22, n, ema22))  |>
     ggplot(aes(x = (n/sum(n))*100, y = orientacao, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), size = 5,color = "white", show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free") +
     labs(x = "", y = "ORIENTAÇÃO PARTIDÁRIA", caption = "Nota: Valores absolutos dentro das barras.") +
     scale_fill_manual(values = c("#A9A9A9", "firebrick", "dodgerblue4")) +
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

#obs: 100% = 458 (votaram)

ggplot2::ggsave(
    filename = here::here("figures", "dados", "politicos", "orientacao.png"),
    plot = barras_orientacao ,
    dpi = 600,
    width = 8,
    height = 6
)


# 3. Governo x Oposição --------------------------------------
(barras_governo <-
     base.1 |>
     count(ema22, governo, sort = T) |>
     mutate(orientacao = tidytext::reorder_within(governo, n, ema22))  |>
     ggplot(aes(x = (n/sum(n))*100, y = orientacao, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), size = 5,color = "white", show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free_y") +
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
    filename = here::here("figures", "dados", "politicos", "governo.png"),
    plot = barras_governo ,
    dpi = 600,
    width = 8,
    height = 6
)

# 4. Boxplot votos --------------------------------------
rain_height <- .1
(gr_votos <- base.1 |> select(deputado_id, votos, ema22) |>
        ggplot(aes(x = "", y = log10(votos), fill = ema22)) +
        introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                       position = position_nudge(x = rain_height+.05)) +
        geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE,
                     outlier.shape = NA,
                     position = position_nudge(x = -rain_height*2)) +
        stat_summary(fun.data = mean_cl_normal, mapping = aes(color = ema22), show.legend = FALSE,
                     position = position_nudge(x = rain_height * 3)) +
        scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
        coord_flip() +
        scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
        scale_colour_manual(values = c("firebrick", "dodgerblue4")) +
        theme_minimal() +
        labs(fill = "", y = "") +
        theme(panel.grid.major.y = element_blank(),
              legend.background = element_rect(fill = "white", color = "white"))
)

ggplot2::ggsave(
    filename = here::here("figures", "dados", "politicos", "votos.png"),
    plot = gr_votos ,
    dpi = 600,
    width = 8,
    height = 6
)

# 3. situacao eleicao --------------------------------------
(barras_situacao <-
     base.1 |>
     count(ema22, resultado, sort = T) |>
     mutate(orientacao = tidytext::reorder_within(resultado, n, ema22))  |>
     ggplot(aes(x = (n/sum(n))*100, y = orientacao, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), size = 5,color = "white", show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free_y") +
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
    filename = here::here("figures", "dados", "politicos", "situacao.png"),
    plot = barras_situacao ,
    dpi = 600,
    width = 8,
    height = 6
)

