# Tabelas com descritivas e teste T

pacman::p_load(tidyverse, flextable, gtsummary)


# 1. Carregar os dados ----------------------------------------------------

dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))


# 2. Limpeza e modificacao ------------------------------------------------
# 2.1 Manter somente quem votou sim ou nao
base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate(#ema22 = ifelse(ema22 == "Sim", 1, 0),
           oposicao = ifelse(governo == "Governo", 0, 1)) |>
    mutate(across(
        c(pj, pf, prop, pol, cand, patrimonio, receita_total),
        ~ .x/1000 ))

# 2.2 variaveis por grupo

pessoais <- c("patrimonio","politico", "idade", "masculino",
              "superior", "casado", "branco", "ema22")

politicas <- c("dim1", "mudou_partido", "suplente",
               "oposicao", "votos","or_contra_ema22", "ema22")

financiamento <-c("pj", "pf","prop","pol", "cand", "per_pj",
                  "per_pf", "per_prop", "per_pol",
                  "per_cand", "receita_total", "ema22")


# 3. Configuracoes da tabela ----------------------------------------------

#configuracao das tabelas flextable
set_flextable_defaults(
    font.family = "Times New Roman",
    font.size = 12,
    font.color = "black",
    big.mark = ".",
    decimal.mark = ",",
    border.color = "black",
    digits = 2,
    table.layout = "autofit"
)

#configuracao das tabelas gt
theme_gtsummary_language(
    language = "pt",
    decimal.mark = ",",
    big.mark = ".",
    iqr.sep = "-",
    ci.sep = NULL,
    set_theme = TRUE
)




# 3.1 Criar labels para as variaveis ----
base <- base |>
    dplyr::mutate(
        obs = TRUE
    ) |>
    labelled::set_variable_labels(
        obs = "Total",
        patrimonio = "Patrimônio",
        politico = "Político",
        idade = "Idade",
        masculino = "Sexo Masculino",
        superior = "Ensino Superior",
        casado = "Casado",
        branco = "Branco",
        ema22 = "Ema22",
        dim1 = "Ideologia",
        mudou_partido = "Mudou de Partido",
        suplente = "Suplente",
        oposicao ="Oposicao",
        votos ="Votos",
        or_contra_ema22 = "Orientação Contra",
        pj = "Rec.PJ (Absoluto)",
        pf = "Rec.PF (Absoluto)",
        prop = "Rec.Próprios (Absoluto)",
        pol = "Rec.Partido (Absoluto)",
        cand = "Rec.Candidatos (Absoluto)",
        per_pj = "Rec.PJ (Percentual)",
        per_pf = "Rec.PF (Percentual)",
        per_prop = "Rec.Próprios (Percentual)",
        per_pol = "Rec.Partido (Percentual)",
        per_cand = "Rec.Candidatos (Percentual)",
        receita_total = "Receita Total (Absoluto)",
        )

# 4. Pessoais -------------------------------------------------------------
(tabela_pessoais <- base  |> select(all_of(pessoais)) |>
    gtsummary::tbl_summary(
        by = ema22,
        digits = everything() ~ 2,
        type = everything() ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}",
                                     # "{p25}", "{p50}", "{p75}",
                                     "{min}", "{max}"),
        missing = "no",
        missing_text = "Dados Ausentes") |>
    gtsummary::add_p(test = everything() ~ "t.test") |>
    # gtsummary::show_header_names()
    gtsummary::modify_header(label = "Estatísticas") |>
    # gtsummary::add_overall(last = T, col_label = "Total") |>
    gtsummary::modify_footnote(everything() ~ NA) |>
    gtsummary::as_flex_table()
)

saveRDS(tabela_pessoais,
        file = here::here("tables", "media_pessoais.rds"))


# 5. Financiamento -----------------------------------------------------------
(tabela_financiamento <- base  |> select(all_of(financiamento)) |>
     gtsummary::tbl_summary(
         by = ema22,
         digits = everything() ~ 2,
         type = everything() ~ "continuous2",
         statistic = everything() ~ c("{mean}", "{sd}",
                                      # "{p25}", "{p50}", "{p75}",
                                      "{min}", "{max}"),
         missing = "no",
         missing_text = "Dados Ausentes") |>
     gtsummary::add_p(test = everything() ~ "t.test") |>
     # gtsummary::show_header_names()
     gtsummary::modify_header(label = "Estatísticas") |>
     # gtsummary::add_overall(last = T, col_label = "Total") |>
     gtsummary::modify_footnote(everything() ~ NA) |>
     gtsummary::as_flex_table()
)

saveRDS(tabela_financiamento,
        file = here::here("tables", "media_financiamento.rds"))


# 6 Políticas -------------------------------------------------------------

(tabela_politicas <- base  |> select(all_of(politicas)) |>
     gtsummary::tbl_summary(
         by = ema22,
         digits = everything() ~ 2,
         type = everything() ~ "continuous2",
         statistic = everything() ~ c("{mean}", "{sd}",
                                      # "{p25}", "{p50}", "{p75}",
                                      "{min}", "{max}"),
         missing = "no",
         missing_text = "Dados Ausentes") |>
     gtsummary::add_p(test = everything() ~ "t.test") |>
     # gtsummary::show_header_names()
     gtsummary::modify_header(label = "Estatísticas") |>
     # gtsummary::add_overall(last = T, col_label = "Total") |>
     gtsummary::modify_footnote(everything() ~ NA) |>
     gtsummary::as_flex_table()
)

saveRDS(tabela_politicas,
        file = here::here("tables", "media_politicas.rds"))
