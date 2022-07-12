# Agregar dados de financiamento de camapanha por tipo de doador


# 1.Carregar os dados de financiamento ------------------------------------

financiamento <- readRDS(file = here::here("data","raw","financiamento.rds"))


# 2. Agregar dados por tipo de doador e recebedor -------------------------

financiamento.1 <- financiamento |>
    dplyr::group_by(cpf_candidato, id_candidato_bd, sequencial_candidato,
                    sigla_partido, origem_receita) |>
    dplyr::summarise(total_doado = sum(valor_receita)) |>
    dplyr::ungroup()

# 3. Mudar formato para wide ----------------------------------------------
financiamento.wide <- financiamento.1 |>
    tidyr::pivot_wider(names_from = origem_receita,
                       values_from = total_doado,
                       values_fill = 0) |>
    janitor::clean_names() |>
    dplyr::rename(cand = recursos_de_outros_candidatos_comites,
                  pol = recursos_de_partido_politico,
                  pf = recursos_de_pessoas_fisicas,
                  pj = recursos_de_pessoas_juridicas,
                  prop = recursos_proprios,
                  apf = rendimentos_de_aplicacoes_financeiras,
                  ni = recursos_de_origens_nao_identificadas,
                  net = doacoes_pela_internet,
                  comerc = comercializacao_de_bens_ou_realizacao_de_eventos
                  )

# 4. Criar total e percentuais ----------------------------------------------------------
financiamento.wide$receita_total <- rowSums(financiamento.wide[5:13])

financiamento.wide.1 <- financiamento.wide |>
    dplyr::mutate(dplyr::across(5:13,
                                ~ (.x/receita_total)*100,
                                .names = "per_{.col}"
                                )
                  )

# salvar ------------------------------------------------------------------
saveRDS(financiamento.wide.1, file = here::here("data","clean","percentuais_financiamento.rds"))

