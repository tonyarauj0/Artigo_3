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
      regiao,
      mudou_partido,
      nm_atual_partido,
      deputado_sigla_uf,
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

# folds <- vfold_cv(dados_train, v = 5, strata = ema22)


# 4. Pre processamento ----------------------------------------------------

base_rec <- recipe(ema22 ~ ., data = base_train) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_log(pf, pj, prop, cand, pol, receita_total) |>
    step_other(nm_atual_partido, deputado_sigla_uf)



# 5. Modelo ---------------------------------------------------------------

lasso_spec <- linear_reg(penalty = .1, mixture = 1 )|>
    set_engine("glmnet", family = binomial(link = "logit"))

# grid_control <- control_grid(
#         save_pred = T,
#         parallel_over = "everything",
#         save_workflow = T
#     )

# 6. Workflow -------------------------------------------------------------
wf  <- workflow() |>
    add_recipe(base_rec)


# 7. Fit ------------------------------------------------------------------
lasso_fit <- wf |>
    add_model(lasso_spec) |>
    fit(data = base_train)


# 8. Tunando parametros ---------------------------------------------------
set.seed(1234)
base_boot <- bootstraps(base_train, strata = ema22)

tune_spec <- linear_reg(penalty = tune(), mixture = 1)  |>
    set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 50)

doParallel::registerDoParallel()

set.seed(2020)
lasso_grid <- tune_grid(
    wf |> add_model(tune_spec),
    resamples = base_boot,
    grid = lambda_grid
)

lasso_grid  |>
    collect_metrics()


lasso_grid  |>
    collect_metrics()  |>
    ggplot(aes(penalty, mean, color = .metric)) +
    geom_errorbar(aes(
        ymin = mean - std_err,
        ymax = mean + std_err
    ),
    alpha = 0.5
    ) +
    geom_line(size = 1.5) +
    facet_wrap(~.metric, scales = "free", nrow = 2) +
    scale_x_log10() +
    theme(legend.position = "none")


# Escolhendo melhor modelo ------------------------------------------------

lowest_rmse <- lasso_grid  |>
    select_best("rmse", maximize = FALSE)

final_lasso <- finalize_workflow(
    wf  |>  add_model(tune_spec),
    lowest_rmse
)

library(vip)

final_lasso |>
    fit(base_train)  |>
    extract_fit_parsnip()  |>
    vi(lambda = lowest_rmse$penalty)  |>
    mutate(
        Importância = abs(Importance),
        Variável = fct_reorder(Variable, Importance)
    )  |>
    ggplot(aes(x = Importância, y = Variável, fill = Sign)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = NULL, fill = "Sinal")

last_fit(
    final_lasso,
    base_split
)  |>
    collect_metrics()



