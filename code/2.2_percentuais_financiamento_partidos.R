# Preparar receitas dos partidos por tipo
library(tidyverse)

# 1. Importar os dados brutos ----------------------------------------------------
receitas_partidos <- readxl::read_excel("data/raw/receitas_partidos_2014_brasil.xlsx")

# 2. Agregar receitas por tipo e partido ----------------------------------

receitas_partidos.1 <- receitas_partidos |>
    janitor::clean_names() |>
    group_by(sigla_partido, tipo_receita) |>
    summarise(valor = sum(valor_receita)) |>
    ungroup()

# 3. Mudar formato para wide ----------------------------------------------
receitas_partidos.wide <- receitas_partidos.1 |>
    tidyr::pivot_wider(names_from = tipo_receita,
                       values_from = valor,
                       values_fill = 0) |>
    janitor::clean_names() |>
    dplyr::rename(p_cand = recursos_de_outros_candidatos_comites,
                  p_pol = recursos_de_partido_politico,
                  p_pf = recursos_de_pessoas_fisicas,
                  p_pj = recursos_de_pessoas_juridicas,
                  p_apf = rendimentos_de_aplicacoes_financeiras,
                  p_ni = recursos_de_origens_nao_identificadas,
                  p_net = doacoes_pela_internet
    )

# 4. Criar total e percentuais ----------------------------------------------------------
receitas_partidos.wide$rec_partido_total <- rowSums(receitas_partidos.wide[2:8])

receitas_partidos.wide.1 <- receitas_partidos.wide |>
    dplyr::mutate(dplyr::across(2:8,
                                ~ (.x/rec_partido_total)*100,
                                .names = "per_{.col}"
                                    )
                  )


# 5. Atulizar nomes dos partidos ------------------------------------------

receitas_partidos.wide.1 <- receitas_partidos.wide.1 |>
    dplyr::mutate(sigla_partido = dplyr::case_when(
        sigla_partido == "PT do B" ~ "AVANTE",
        sigla_partido == "PATRIOTA" ~ "PEN",
        sigla_partido == "SOLIDARIEDADE" ~ "SD",
        sigla_partido == "PODE" ~ "PTN",
        TRUE ~ sigla_partido

    ))



# salvar ------------------------------------------------------------------
saveRDS(receitas_partidos.wide.1, file = here::here("data","clean","receitas_partidos.rds"))
