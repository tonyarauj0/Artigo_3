# Criar algumas categorias de interesse na base de estimacao
library(tidyverse)


# 1. Carregar base de dados -----------------------------------------------

base <- readRDS(file = here::here("data",
                          "clean", "base_estimacao.rds"))



# 2 Mudar interger64 para integer -----------------------------------------
base.1 <- base |> mutate_if(bit64::is.integer64, as.numeric)


# 3. Criar categorias de interesse ----------------------------------------
# 3.1 Mudou de partido apos eleicao ----
base.2 <- base.1 |>
    mutate(
        mudou_partido = ifelse(deputado_sigla_partido == partido_eleicao,
                               0, 1)
    )


# 3.2 Regiao ----
ne <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
s <- c("PR", "SC", "RS")
co <- c("GO", "DF", "MT", "MS")
se <- c("ES", "MG", "RJ", "SP")
no <- c ("AC","AP","AM", "PA", "RO", "RR", "TO")

base.2 <- base.2 |>
    dplyr::mutate(
        regiao = dplyr::case_when(
            deputado_sigla_uf %in% ne ~ "Nordeste",
            deputado_sigla_uf %in% s ~ "Sul",
            deputado_sigla_uf %in% se ~ "Sudeste",
            deputado_sigla_uf %in% co ~ "Centro-Oeste",
            deputado_sigla_uf %in% no ~ "Norte",
            TRUE ~ "Ausente"
        ),
        regiao = as.factor(regiao)
    )

# 3.3 Ocupacao politico ----
base.2 <- base.2 |>
    mutate(politico = case_when(
        ocupacao %in% c("deputado", "vereador", "senador") ~ 1,
        TRUE ~ 0)
    )

# 3.4 Sexo, ensino superior completo, casado, branco ----
base.2 <-  base.2 |>
    mutate(masculino = ifelse(genero == "masculino", 1, 0),
           superior = ifelse(instrucao == "ensino superior completo",
                                     1,
                                     0),
           casado = ifelse(estado_civil == "casado(a)",
                           1,
                           0),
           branco = ifelse(raca == "branca",
                         1,
                         0),
           )

# 3.5 Suplente ----
base.2 <- base.2 |>
    mutate(suplente = ifelse(resultado == "suplente",
                            1,
                             0)
    )


# 4. Atualizar nomes dos partidos -----------------------------------------
base.2 <- base.2 |>
    dplyr::mutate(nm_atual_partido =
                      dplyr::case_when(
                          deputado_sigla_partido == "PMDB" ~ "MDB",
                          deputado_sigla_partido == "PTN" ~ "PODE",
                          deputado_sigla_partido == "PTdoB" ~ "AVANTE",
                          deputado_sigla_partido == "PEN" ~ "PATRI",
                          deputado_sigla_partido == "PSDC" ~ "DC",
                          deputado_sigla_partido == "PR" ~ "PL",
                          deputado_sigla_partido == "PRB" ~ "REPUB",
                          deputado_sigla_partido == "PPS" ~ "CIDAD",
                          deputado_sigla_partido == "PSDC" ~ "DC",
                          TRUE ~ deputado_sigla_partido
                      ))

# 5. criar novo identificador para deputados --------------------------------
base.2 <- base.2 |>
    mutate(new_deputado_id = glue::glue("{deputado_id}_{nm_atual_partido}"))

# 6. Acrescentar pontos ideais(ideologia) --------------------------------------------
load(here::here("data", "raw", "pontos_ideais_data_frame.Rda"))

base.3 <- base.2 |>
    left_join(pontos_ideais_df |>
                  select(new_deputado_id, governo, dim1, dim2, metodo) |>
                  filter(metodo == "wnominate"))


# 7. Criar novas variaveis de financiamento -------------------------------
base.3 <- base.3 |>
    mutate(diretos = pj + pf,
           indiretos = pol + cand,
           per_diretos = per_pj + per_pf,
           per_indiretos = per_pol + per_cand)

# salvar ------------------------------------------------------------------
saveRDS(base.3,
        file = here::here("data", "clean", "base_estimacao_completa.rds"))

