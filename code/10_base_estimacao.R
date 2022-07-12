# Montar a base dd estimacao

# 1 Carregar dados de votacoes de interesse ----
votos <- readRDS(file = here::here("data",
                                   "clean",
                                   "merge_votos_cpfs_deputados.rds"))
votos.1 <- votos |>
    dplyr::select(deputado_id,
                  cpf,
                  deputado_nome,
                  deputado_sigla_partido,
                  deputado_sigla_uf,
                  objeto,
                  voto) |>
    tidyr::pivot_wider(names_from = objeto,
                       values_from = voto)


# 2. Carregar dados de patrimonio -----------------------------------------
patrimonio <- readRDS(file = here::here("data",
                                        "clean",
                                        "patrimonio_agregado_deputados.rds"))

# 3. Carregar dados pessoais ----------------------------------------------
dados_pessoais <- readRDS(file = here::here("data",
                                            "raw",
                                            "dados_pessoais.rds"))

# 4. Carregar votacao obtida pelo deputado -------------------------------
votacao_obtida <- readRDS(file = here::here("data",
                                            "raw",
                                            "votacao_deputado.rds"))

# 5. Carregar percentuais de financiamento --------------------------------
percentuais_financiamento <- readRDS(file = here::here("data",
                                                       "clean",
                                                       "percentuais_financiamento.rds"))


# 6. Merge das bases ---------------------------------------------------------
base_estimacao <- votos.1 |>  #votacao nominal
    dplyr::left_join(
        dados_pessoais |>
            dplyr::select(
                cpf,
                sequencial,
                sigla_partido,
                ocupacao,
                idade,
                genero,
                instrucao,
                estado_civil,
                raca
            ) |>
            dplyr::rename(partido_eleicao = sigla_partido,
                          sequencial_candidato = sequencial) |> #dados pessoais
            dplyr::left_join(patrimonio |>
                                 dplyr::select(-id_candidato_bd)) |> #patrimonio
            dplyr::left_join(percentuais_financiamento |>
                                 dplyr::select(-c(
                                     id_candidato_bd, sigla_partido
                                 ))) |> # percentuais de financiamento
            dplyr::left_join(
                votacao_obtida |>
                    dplyr::select(sequencial_candidato, votos, resultado)
            ) #votos obtidos no momento da eleicao
    )


# 7. Melhorar nomes das variaveis -----------------------------------------

base_estimacao <- base_estimacao |> janitor::clean_names()

# salvar ------------------------------------------------------------------

saveRDS(base_estimacao, file = here::here("data",
                                          "clean", "base_estimacao.rds"),
        compress = F)


#pacotes
usethis::use_package("janitor")
