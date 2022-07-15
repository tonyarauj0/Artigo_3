# Modelo Logit Clássico

# 1. Carregar os dados ----------------------------------------------------

dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))


# 2. Limpeza e modificacao ------------------------------------------------
# 2.1 Manter somente quem votou sim ou nao
base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate(ema22 = ifelse(ema22 == "Sim", 1, 0))

# 2.2 Modificar valores monetarios igual a zero para 1
base <- base |>
    mutate(across(
        c(diretos, indiretos, pj, pf, prop, pol, cand, patrimonio),
        ~ case_when(.x == 0 ~ 1,
                    TRUE ~ .x))) |>
    mutate(across(
    c(diretos, indiretos, pj, pf, prop, pol, cand, patrimonio, receita_total),
    ~ log10(.x)))


# 3. Modelos (melhorar) ----
# 3.2 Logit ----

logit <- glm(ema22 ~ dim1
             + or_contra_ema22
             # + receita_total
             + per_pj
             + per_pf
             + per_prop
             + per_pol
             # + cand
             + regiao
             + governo
             + politico
             + superior
             + casado
             ,
             family = binomial(link="logit"),
             data = base)

# 3.2 OLS
ols <- lm(ema22 ~ dim1
          + or_contra_ema22
          # + receita_total
          + per_pj
          + per_pf
          + per_prop
          + per_pol
          # + cand
          + regiao
          + governo
          + politico
          + superior
          + casado
             ,
             data = base)


# 4. Erros robustos Stata ----
# 4.1 Logit
logit_robust <- lmtest::coeftest(logit, sandwich::vcovHC, type = "HC1")

# 4.2 Ols
ols_robust <- lmtest::coeftest(ols, sandwich::vcovHC, type = "HC1")



# 5. Configuracao Tabelas ----------------------------------------------------
#configuracao das tabelas
flextable::set_flextable_defaults(
    font.family = "Times New Roman",
    font.size = 12,
    font.color = "black",
    big.mark = ".",
    decimal.mark = ",",
    border.color = "black",
    digits = 2,
    table.layout = "autofit"
)

#configuracao das tabelas
gtsummary::theme_gtsummary_language(
    language = "pt",
    decimal.mark = ",",
    big.mark = ".",
    iqr.sep = "-",
    ci.sep = NULL,
    set_theme = TRUE
)

# Renomeando parametros

renomear = c("dim1" = "Ideologia",
             "or_contra_ema22" = "Orientação Contra",
             "masculino" = "Sexo M",
             "regiaoNordeste" = "NE",
             "regiaoNorte" = "N",
             "regiaoSudeste" = "SE",
             "regiaoSul" = "S",
             "branco" = "Branco",
             "superior" = "Superior",
             "receita_total" = "Rec.Totais",
             "mudou_partido" = "Mudou Partido",
             "politico" = "Político",
             "per_pf" = "Rec.PF",
             "per_pj" = "Rec.PJ",
             "per_prop" = "Rec.Próprios",
             "casado" = "Casado",
             "per_pol" = "Rec.Partido",
             "governoOposição" = "Oposição",
             "per_cand" = "Rec.Candidatos")


# 6. Tabela de Estimação --------------------------------------------------
(tabela_comparacao_log_ols <- modelsummary::msummary(list("Logit" = logit,
                                      "Logit\nRobusto" = logit_robust,
                                      "OLS" = ols,
                                      "OLS\nRobusto" = ols_robust
                                      ),
                                 statistic = "std.error",
                                 coef_omit = "(Intercept)",
                                 stars = c("*"= .05, '**' = .01, '***' = .001),
                                 coef_rename = renomear,
                                 output = "flextable") |>
        flextable::fontsize(part = "footer", size = 10) |>
        flextable::align(align = "center", part = "body") |>
        flextable::align(align = "center", part = "header") |>
        flextable::hline_top(border = officer::fp_border(width = 1),
                             part = "all"))

saveRDS(tabela_comparacao_log_ols,
        file = here::here("tables", "tabela_comparacao_log_ols.rda"))

# 6.1. Tabela de Estimação adc parametros --------------------------------------------------

# Modelos ----
logit_1 <- glm( ema22 ~ dim1
    + or_contra_ema22
    # + receita_total
    + per_pj
    + per_pf
    + per_prop
    + per_pol
    # + cand
    + regiao
    # + governo
    # + politico
    # + superior
    # + casado
,
family = binomial(link="logit"), data = base)

logit_2 <- glm(ema22 ~ dim1
    + or_contra_ema22
    # + receita_total
    + per_pj
    + per_pf
    + per_prop
    + per_pol
    # + cand
    + regiao
    + governo
    # + politico
    # + superior
    # + casado
    ,
    family = binomial(link="logit"), data = base)


logit_3 <- glm(ema22 ~ dim1
    + or_contra_ema22
    # + receita_total
    + per_pj
    + per_pf
    + per_prop
    + per_pol
    # + cand
    + regiao
    + governo
    + politico
    # + superior
    # + casado
    ,
    family = binomial(link="logit"), data = base)

logit_4 <- glm(ema22 ~ dim1
    + or_contra_ema22
    # + receita_total
    + per_pj
    + per_pf
    + per_prop
    + per_pol
    # + cand
    + regiao
    + governo
    + politico
    + superior
    # + casado
    ,
    family = binomial(link="logit"), data = base)

logit_5 <- glm(ema22 ~ dim1
    + or_contra_ema22
    # + receita_total
    + per_pj
    + per_pf
    + per_prop
    + per_pol
    # + cand
    + regiao
    + governo
    + politico
    + superior
    + casado
    ,
    family = binomial(link="logit"), data = base)

# Tabela
(tabela_comparacao_logits <- modelsummary::msummary(list(
    logit_1, logit_2, logit_3, logit_4, logit_5
),
statistic = "std.error",
coef_omit = "(Intercept)",
stars = c("*"= .05, '**' = .01, '***' = .001),
coef_rename = renomear,
output = "flextable") |>
    flextable::fontsize(part = "footer", size = 10) |>
    flextable::align(align = "center", part = "body") |>
    flextable::align(align = "center", part = "header") |>
    flextable::hline_top(border = officer::fp_border(width = 1),
                         part = "all"))

saveRDS(tabela_comparacao_logits,
        file = here::here("tables", "tabela_comparacao_logits.rda"))



# 7. Data frame com estimacao ---------------------------------------------
df_estimacao <- broom::tidy(logit_robust, conf.int = TRUE)

#Renomaer Variaveis
df_estimacao1 <- df_estimacao |>
    mutate(term = case_when(
        term == "(Intercept)" ~ "Intercepto",
        term == "dim1" ~ "Ideologia",
        term == "or_contra_ema22" ~ "Orientação Contra",
        # term == "masculino" ~ "Sexo M",
        term == "regiaoNordeste" ~ "NE",
        term == "regiaoNorte" ~ "N",
        term == "regiaoSudeste" ~ "SE",
        term == "regiaoSul" ~ "S",
        # term == "branco" ~ "Branco",
        term == "superior" ~ "Ens.Superior",
        term == "receita_total" ~ "Rec.Totais",
        # term == "mudou_partido" ~ "Mudou Partido",
        term == "per_politico" ~ "Político",
        term == "per_pf" ~ "Rec.PF",
        term == "per_pj" ~ "Rec.PJ",
        term == "per_prop" ~ "Rec.Próprios",
        term == "casado" ~ "Casado",
        term == "per_pol" ~ "Rec.Partido",
        term == "governoOposição" ~ "Oposição",
        term == "politico" ~ "Político",
        # term == "patrimonio" ~ "Patrimônio",
        # term == "cand" ~ "Rec.Candidatos"
    )) |>
    mutate(p = case_when(
               p.value < .05 ~ "p < 0,05",
               TRUE ~ "p > 0,05"
           ),
           sig = ifelse(estimate > 0 , "POS", "NEG"))

# Grafico
(gr_estimacao <- ggplot(
        df_estimacao1 |> filter(term != "Intercepto"),
        aes(
            x = estimate,
            y = reorder(term,-estimate),
            xmin = conf.low,
            xmax = conf.high,
            color = sig
        )
    ) +
        geom_point(aes(shape = p,), size = 2, show.legend = F) +
        geom_vline(
            xintercept = 0,
            lty = 4,
            color = "orange"
        ) +
        scale_shape_manual(values = c(16, 1)) +
        geom_linerange() +
        theme_minimal() +
        scale_color_manual(values = c("firebrick", "dodgerblue4")) +
        labs(
            x = "",
            y = "",
            color = "",
            shape = "",
            caption  = "Nota: Parâmetros significantes indicados por círculo preenchido."
        ) +
        theme(
            legend.position = "none",
            plot.caption = element_text(hjust = 0),
            panel.border = element_blank(),
            # panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )
)

# Salvar

ggplot2::ggsave(
    filename = here::here("figures", "resultados", "ml", "gr_estimacao.png"),
    plot = gr_estimacao ,
    dpi = 600,
    width = 8,
    height = 6
)

