# Lasso para estimar chance de votar Sim na EMA 22

# Pacotes
library(tidymodels)

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
        c(diretos, indiretos, pj, pf, prop, pol, cand, patrimonio,
          p_pj, p_pf, p_pol, p_cand),
        ~ case_when(.x == 0 ~ 1,
                    TRUE ~ .x)))

# 2.3 Manter variaveis julgadas importantes
base <- base |>
    select(
        c(dim1,
          pj,
          pf,
          prop,
          pol,
          cand,
          patrimonio,
          per_pj,
          per_pf,
          per_prop,
          per_pol,
          per_cand,
          regiao,
          mudou_partido,
          # nm_atual_partido,
          # deputado_sigla_uf,
          politico,
          suplente,
          governo,
          votos,
          idade,
          masculino,
          superior,
          casado,
          branco,
          ema22,
          or_contra_ema22,
          receita_total
        )) |>
    mutate_if(is.character, as.factor)


# 3. Separando amostra -----------------------------------------------------

base_split <- initial_split(base, strata = ema22)
base_train <- training(base_split)
base_test <- testing(base_split)




# 4. Pre processamento ----------------------------------------------------

base_rec <- recipe(ema22 ~ ., data = base_train) |>
    step_dummy(all_nominal_predictors()) |>
    # step_log(pf, pj, prop, cand, pol, receita_total) |>
    step_impute_knn(all_predictors()) |>
    step_nzv(all_predictors())


# 5. Modelo ---------------------------------------------------------------

reg_mod <- linear_reg(penalty = tune(), mixture = tune() )|>
    set_engine("glmnet", family = binomial(link = "logit"))


# 6. Workflow -------------------------------------------------------------
wf  <- workflow() |>
    add_model(reg_mod) |>
    add_recipe(base_rec)


# 7. Training ------------------------------------------------------------------
# 7.1 Vfolds
val_set <- vfold_cv(base_train, v = 4, strata = ema22)

# 7.2 Treino
set.seed(1234)
reg_trainned <- wf |>
    tune_grid(
        val_set,
        grid = 10,
        control = control_grid(save_pred = T),
        metrics = metric_set(rmse)
    )


# 8. Avaliando modelo ---------------------------------------------------
reg_trainned |> show_best(n = 10)


# 8.1 Montar tabela -----
# Configuracao Tabelas
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

# Tabela
tabela_tunning <-
reg_trainned |> show_best(n = 10) |>
    select(penalty, mixture, mean, std_err) |>
    mutate(across(where(is.numeric), ~ format(.x,
                                       digits = 2,
                                       decimal.mark = ",",
                                       scientific = T))) |>
    flextable::flextable() |>
    flextable::fontsize(part = "footer", size = 10) |>
    flextable::align(align = "left", part = "body") |>
    flextable::align(align = "center", part = "header") |>
    flextable::hline_top(border = officer::fp_border(width = 1),
                         part = "all") |>
    flextable::hline_bottom(border = officer::fp_border(width = 1),
                            part = "all") |>
    flextable::set_header_labels(penalty = "Lambda",
                                 mixture = "Alpha",
                                 mean = "Média",
                                 std_err = "Desvio\nPadrão"
    )

saveRDS(tabela_tunning, file = here::here("tables", "tabela_tunning.rds"))



# 8.1 Penalizacao ----
(gr_penalizacao <-
reg_trainned  |>
    collect_metrics()  |>
    ggplot(aes(penalty, mean)) +
    geom_errorbar(aes(
        ymin = mean - std_err,
        ymax = mean + std_err
    ),
    alpha = 0.5
    ) +
    geom_line(size = 1.5, show.legend = F) +
    scale_y_continuous(labels = function(x) format(x,
                                                   big.mark = ".",
                                                   decimal.mark = ",",
                                                   scientific = FALSE)) +
    scale_x_log10() +
    theme_minimal() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          ) +
    labs(x = expression(lambda), y = "Média")
)

ggplot2::ggsave(
    filename = here::here("figures", "resultados", "ml", "penalizacao.png"),
    plot = gr_penalizacao ,
    dpi = 600,
    width = 8,
    height = 6
)


# 8.2 Mixture ----
reg_trainned  |>
    collect_metrics()  |>
    ggplot(aes(mixture, mean)) +
    geom_errorbar(aes(
        ymin = mean - std_err,
        ymax = mean + std_err
    ),
    alpha = 0.5
    ) +
    geom_line(size = 1.5, show.legend = F) +
    # facet_wrap(~.metric, scales = "free", nrow = 2) +
    scale_x_log10() +
    theme(legend.position = "none") +
    theme_minimal() +
    labs(x = "Mixture", y = "Média")



# 9 Importancia das variaveis ------------------------------------------------
# 9.1 Escolhendo melhor modelo
lowest_rmse <- reg_trainned  |>
    select_best("rmse")

# 9.2 Estimando o Melhor modelo (modo 1)
wf1  <- workflow() |>
    add_recipe(base_rec)

final_reg_model_mod1<- finalize_workflow(wf1  |>  add_model(reg_mod),
                                 lowest_rmse)

# 9.3 Estimando o Melhor modelo (modo 1)
final_reg_model_mod2 <- reg_mod |> finalize_model(lowest_rmse)


# 9.4 Grafico variaveis importantes ----
library(vip)

(var_importantes <-
final_reg_model_mod1 |>
    fit(base_train)  |>
    extract_fit_parsnip()  |>
    vi(lambda = lowest_rmse$penalty)  |>
    head(15) |>
    mutate(Variable = case_when(
        Variable == "dim1" ~ "Ideologia",
        Variable == "or_contra_ema22" ~ "Orientação Contra",
        Variable == "regiao_Norte" ~ "Norte",
        Variable == "regiao_Nordeste" ~ "Nordeste",
        Variable == "branco" ~ "Branco",
        Variable == "masculino" ~ "Masculino",
        Variable == "suplente" ~ "Suplente",
        Variable == "superior" ~ "Ens.Superior",
        Variable == "regiao_Sudeste" ~ "Sudeste",
        Variable == "receita_total" ~ "Rec.Totais",
        Variable == "regiao_Sul" ~ "Sul",
        Variable == "governo_Oposição" ~ "Oposição",
        Variable == "politico" ~ "Político",
        Variable == "pf" ~ "Rec.PF(Absoluta)",
        Variable == "pj" ~ "Rec.PJ(Absoluta)",
        Variable == "patrimonio" ~ "Patrimônio",
        Variable == "idade" ~ "Idade",
        Variable == "per_pf" ~ "% Rec.PF",
        Variable == "per_pj" ~ "% Rec.PJ",
        Variable == "per_cand" ~ "% Rec.Candidatos",
        Variable == "cand" ~ "Rec.Candidatos(Absoluta)",
        Variable == "per_prop" ~ "% Rec.Próprios",
        Variable == "prop" ~ "Rec.Próprios(Absoluta)",
        Variable == "casado" ~ "Casado",
        Variable == "per_pol" ~ "% Rec.Partido",
        Variable == "pol" ~ "Rec.Partido(Absoluta)",
        Variable == "votos" ~ "Votos"
    )) |>
    mutate(
        Importance = abs(Importance),
        Variable = forcats::fct_reorder(Variable, Importance)
    )  |>

    ggplot(aes(x = Importance,
               y = Variable,
               fill = Sign)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0),
                       labels = function(x) format(x,
                                                   big.mark = ".",
                                                   decimal.mark = ",",
                                                   scientific = FALSE)) +
    labs(y = NULL, fill = "Sinal", x = "Importância") +
    scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
    theme_minimal() +
    theme(panel.border = element_blank(),
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
)
# Salvar
ggplot2::ggsave(
    filename = here::here("figures", "resultados", "ml", "var_importantes.png"),
    plot = var_importantes ,
    dpi = 600,
    width = 8,
    height = 6
)



# 10. Estimando melhor modelo na base de treino ---------------------------

# 10.1 Modo 1
final_fitted <- last_fit( final_reg_model_mod1, base_split)

# Metricas
final_fitted |> collect_metrics()

# Previsão
collect_predictions(final_fitted)

# Estimacao
ema22_fit <- extract_fit_parsnip(final_fitted$.workflow[[1]])

tidy(ema22_fit)  |>
    arrange(estimate)

# Outra Visualizacao
tidy(ema22_fit)  |>
    filter(term != "(Intercept)")  |>
    group_by(sign = estimate > 0) |>
    slice_max(abs(estimate), n = 100)  |>
    ungroup()  |>
    mutate(
        sign = if_else(sign, "Votar a Favor", "Votar Contra")
    )  |>
    ggplot()+
    aes(abs(estimate),
        forcats::fct_reorder(term, abs(estimate)), fill = sign) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~sign, scales = "free")


# 10.2 Gráfico de Previsão (Usando Modo 2)
workflow()|>
    add_recipe(base_rec) |>
    add_model(final_reg_model_mod2) |>
    last_fit(base_split) |>
    collect_predictions() |>
    select(.row, .pred, ema22) |>
    ggplot() +
    aes(x = ema22, y = .pred) +
    geom_point()


a <- workflow()|>
    add_recipe(base_rec) |>
    add_model(final_reg_model_mod2) |>
    last_fit(base_split) |>
    extract_fit_parsnip(final_fitted$.workflow[[1]]) |>
    tidy() |>
    filter(penalty)


# 11. Estimando o modelo com os parametros indicados  ---------------------
x <- base |>
    select(or_contra_ema22, receita_total, prop,
           pj, pf , cand, pol,regiao, branco) |>
    mutate(across(c(receita_total, prop,
                  pj, pf , cand, pol), .fun = ~log10(.x)),
           sul = ifelse(regiao == "Sul", 1, 0)) |>
    select(- regiao) |>
    as.matrix()

y = base |> select(ema22) |> as.matrix()

fit <- glmnet(x = x, y = y, alpha = .402, lambda = 0.0682)



