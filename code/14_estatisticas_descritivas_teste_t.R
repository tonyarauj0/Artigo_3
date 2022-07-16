# Tabelas com descritivas e teste T

# 1. Carregar os dados ----------------------------------------------------

dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))


# 2. Limpeza e modificacao ------------------------------------------------
# 2.1 Manter somente quem votou sim ou nao
base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate(ema22 = ifelse(ema22 == "Sim", 1, 0),
           governo = ifelse(governo == "Governo", 1, 0))

# 2.2 variaveis destaque
destaque <-
    c("dim1",
      "pj",
      "pf",
      "prop",
      "pol",
      "cand",
      "patrimonio",
      "per_pj",
      "per_pf",
      "per_prop",
      "per_pol",
      "per_cand",
      # "regiao",
      "mudou_partido",
      "politico",
      "suplente",
      "governo",
      "votos",
      "idade",
      "masculino",
      "superior",
      "casado",
      "branco",
      "ema22",
      "or_contra_ema22",
      "receita_total")



base  |> select(all_of(destaque)) |>
    gtsummary::tbl_summary(
        by = ema22,
        digits = everything() ~ 2,
        type = everything() ~ "continuous2",
        statistic = everything() ~ c("{mean}", "{sd}",
                                     "{p25}", "{p50}", "{p75}",
                                     "{min}", "{max}"),
        # missing = "no",
        # label = rice_medio ~ "Índice de Rice"
    ) |>
    gtsummary::add_p(test = everything() ~ "t.test")

) |>
    # gtsummary::show_header_names()
    gtsummary::modify_header(label = "Estatísticas") |>
    # gtsummary::add_overall(last = T, col_label = "Total") |>
    gtsummary::modify_footnote(everything() ~ NA) |>
    gtsummary::as_flex_table()
