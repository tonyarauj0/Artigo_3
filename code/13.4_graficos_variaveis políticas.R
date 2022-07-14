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

ggplot2::ggsave(
    filename = here::here("figures", "dados", "politicos", "orientacao.png"),
    plot = barras_orientacao ,
    dpi = 600,
    width = 8,
    height = 6
)


# 2. Governo x Oposição --------------------------------------
(barras_orientacao <-
     base.1 |>
     count(ema22, governo, sort = T) |>
     mutate(orientacao = tidytext::reorder_within(governo, n, ema22))  |>
     ggplot(aes(x = (n/sum(n))*100, y = orientacao, fill = ema22)) +
     geom_col(show.legend = FALSE, alpha = 0.8) +
     geom_label(aes(label = n), size = 5,color = "white", show.legend = F) +
     tidytext::scale_y_reordered() +
     facet_wrap(~ema22, scales = "free") +
     labs(x = "", y = "ORIENTAÇÃO PARTIDÁRIA", caption = "Nota: valores absolutos dentro das barras.") +
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

ggplot2::ggsave(
    filename = here::here("figures", "dados", "politicos", "orientacao.png"),
    plot = barras_orientacao ,
    dpi = 600,
    width = 8,
    height = 6
)
