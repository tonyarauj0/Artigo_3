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

# 2.3 Excluir algumas variaveis
identificacao <- c("deputado_id", "cpf", "deputado_nome", "sequencial_candidato", "cpf_candidato", "new_deputado_id")

politicas <- c("deputado_sigla_partido",
               "x2turno", "partido_eleicao",
               "ema10", "ema32", "x2turno",
               "orientacao_ema32", "orientacao_ema10", "orientacao_x2turno",
               "or_contra_ema32", "or_contra_ema10", "or_contra_x2turno",
               "orientacao_ema22")

financeiras <- c ("apf", "ni", "net", "comerc",
                  "p_apf", "p_ni", "p_net", "patrimonio")

outras <- c("dim2", "metodo")

base <- base |>
    select(- c(all_of(identificacao),
               all_of(politicas),
               all_of(financeiras),
               all_of(outras))) |>
    select(- contains("p_")
    ) |>
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
    step_log(pf, pj, prop, cand, pol, receita_total,
             # p_cand, p_pol, p_pf, p_pj,patrimonio
             rec_partido_total)



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
    pull_workflow_fit()  |>
    vi(lambda = lowest_rmse$penalty)  |>
    mutate(
        Importância = abs(Importance),
        Variável = fct_reorder(Variable, Importance)
    )  |>
    ggplot(aes(x = Importância, y = Variável, fill = Sign)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = NULL)

last_fit(
    final_lasso,
    base_split
)  |>
    collect_metrics()


