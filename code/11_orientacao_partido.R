# Criar variaveis de orientacao dos partidos para cada votacao de financiamento

library(tidyverse)
# 1 Carregar votacoes de financiamento ------------------------------------

votacoes_financiamento <- readRDS(file = here::here("data", "clean", "votacoes_financiamento.rds"))

# 1.1 Selecionar ids unicos
ids_votacoes_financiamento <- votacoes_financiamento |>
    dplyr::select(id_votacao, objeto) |>
    dplyr::distinct()


# 2. Carregar todas as votacoes de 2015 -----------------------------------
votacoes_2015 <- readRDS(file = here::here("data", "raw", "votacoes_2015.rds"))

# 3 Selecionar PEC 182 ----------------------------------------------------

votacoes_pec_182 <- votacoes_2015 |>
    dplyr::filter(type_bill == "PEC",
                  number_bill == 182)

# 3. Selecionar votacoes ligadas ao financiamento de campanha ----------------------
# 3.1 Emendas Aglutinativas que discorrem sobre o financiamento
#obs: ver planilha, demais emas foram prejudicadas.
emas <- paste0("EMENDA AGLUTINATIVA Nº ", c(10, 22, 32))

# 3.2 Filtrar emas selecionadas e votacao em segundo turno
votacoes_financiamento_pec182 <- votacoes_pec_182 |>
    dplyr::filter(rollcall_subject %in% emas |
                      rollcall_subject == "§ 5º DO ART. 17 DA C.F., CONSTANTE DO ART. 1º DA PEC") |>
    #Selecionar variavel de orientacao
    dplyr::select(rollcall_id, decision_summary, decision_date,
                  decision_time, rollcall_subject, session_id,
                  dplyr::contains("orientation")) |>
    dplyr::distinct() |>
    tidyr::pivot_longer(cols = dplyr::contains("orientation"),
                        names_to = "partido",
                        values_to = "orientacao",
                        values_drop_na = T) |>
    dplyr::mutate(partido = stringr::str_remove(
        partido, pattern = "_orientation")) |>
    # Padronizar nomes dos partidos
    dplyr::mutate(partido = dplyr::case_when(
        partido == "PrbPtnPmnPrpPsdcPrtbPtcPslPtdoB" ~
        "PRB/PTN/PMN/PRP/PSDC/PRTB/PTC/PSL/AVANTE",
        partido == "PmdbPpPtbPscPhsPen" ~
            "PMDB/PP/PTB/PSC/PHS/PEN",
        partido == "ReprPSOL" ~ "PSOL",
        partido == "Solidaried" ~ "SD" ,
        partido == "PCdoB" ~ "PC do B",
        TRUE ~ partido))



# 3.3 Criar dummies para cada partido
votacoes_financiamento_pec182.1 <- votacoes_financiamento_pec182 |>
    fastDummies::dummy_cols("partido",
                            split = "/") |>
    tidyr::pivot_longer(partido_DEM:partido_SD,
                        names_to = "sigla",
                        values_to = "values") |>
    dplyr::filter(values == 1) |>
    dplyr::mutate(
        sigla = stringr::str_remove(sigla, pattern = "partido_")
        ) |>
    dplyr::select(-values)


# 3.4 Criar orientacao por votacao -----------------
orientacao_partido <- votacoes_financiamento_pec182.1 |>
    dplyr::mutate(
        votacao = dplyr::case_when(
            rollcall_subject == "EMENDA AGLUTINATIVA Nº 22" ~
                "orientacao_ema22",
            rollcall_subject == "EMENDA AGLUTINATIVA Nº 32" ~
                "orientacao_ema32",
            rollcall_subject == "EMENDA AGLUTINATIVA Nº 10" ~
                "orientacao_ema10",
            rollcall_subject == "§ 5º DO ART. 17 DA C.F., CONSTANTE DO ART. 1º DA PEC" ~
                "orientacao_x2turno"
        )
    ) |>
    # Modificar nomes das orientacoes para ser compativel c deputados
    dplyr::mutate(orientacao = dplyr::case_when(
        orientacao == "Nao" ~ "Não",
        orientacao == "Abstencao" ~ "Abstenção",
        TRUE ~ orientacao
    )) |>
    dplyr::select(votacao, sigla, orientacao) |>
    tidyr::pivot_wider(names_from = votacao,
                       values_from = orientacao)



# salvar ------------------------------------------------------------------

saveRDS(orientacao_partido,
        file = here::here("data", "clean", "orientacao_partido.rds"))



