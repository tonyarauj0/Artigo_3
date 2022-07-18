# Métricas adicionais ao modelo


# 1. Carregar o modelo e a base ----------------------------------------------------
# 1.1 Modelo
logit <- readRDS(file = here::here("data","clean","logit.rds"))

# 1.2 Base
dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))

# 2.2 Manter somente quem votou sim ou nao
base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate(EMA22 = ema22,
           ema22 = ifelse(ema22 == "Sim", 1, 0)
           )

# 2.Probabilidade predita ---------------------------------------------------
# 2.1 Probabilidade
glm_probs = data.frame(probs = predict(logit, type="response"))

# 2.2 Logito
logito <- logit$linear.predictors
base$logito <- logito

# 2.3 Relacao linear da variavel com logito
base |>
    ggplot() +
    aes(x = logito, y = superior) +
    geom_point(size = .5, alpha = .5) +
    geom_smooth(method = "loess") +
    theme_minimal()

# 2.4 Matriz de confusão
glm_pred = glm_probs |>
    mutate(pred = ifelse(probs>.5, "Favor", "Contra"))

glm_pred = cbind(base |>mutate(EMA22 = ifelse(ema22==1, "Sim","Não")) ,
                 glm_pred)

mat_confusao <- glm_pred |>
    count(pred, EMA22) |>
    spread(EMA22, n, fill = 0)

# 2.4.1 Gráfico
TClass <- factor(c("Não", "Não", "Sim", "Sim"))
PClass <- factor(c("Não", "Sim", "Não", "Sim"))
Y      <- c(150, 53, 20, 231)
Prop <- c("(73,9%)", "(26,1%)", "(0,08%)", "(92%)")
df <- data.frame(TClass, PClass, Y, Prop)

gr_matix_conf <- ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = paste(Y, Prop)), vjust = 1) +
    scale_fill_gradient(low="white", high="#009194") +
    theme_minimal() +
    scale_x_discrete(expand = c(.01, .01)) +
    scale_y_discrete(expand = c(.01, .01)) +
    theme(axis.ticks.x=element_blank(), #remove x axis ticks
          axis.ticks.y=element_blank(),  #remove y axis ticks
          legend.position = "none",

    )+
    labs(y = "Previsão", x = "Voto")

# Salvar
ggplot2::ggsave(
    filename = here::here("figures", "resultados", "matriz_confusao.png"),
    plot = gr_matix_conf ,
    dpi = 600,
    width = 8,
    height = 6
)


# glm_pred |> ggplot() +
#     aes(x = dim1, y = pred) +
#     geom_point(alpha = .5) +
#     stat_smooth(method="glm",
#                 se=FALSE,
#                 method.args = list(family=binomial))

# 2.5 Diversas estatísticas
QuantPsyc::ClassLog(logit, base$ema22)


# 3. Teste LM ----------------------------------------------------------------

anova(logit, test = "LRT")


# 4. Razao de Chances --------------------------------------------------------
exp(cbind(OR = coef(logit), confint.default(logit)))

library(questionr)
questionr::odds.ratio(logit)

library(sjPlot)
sjPlot::plot_model(logit, sort.est = TRUE,
                   show.values = TRUE, value.offset = .3, vcov.type = "HC1", digits = 2,
                   big.mark = ".", decimal.mark = ",") +
    labs(title = "") +
    geom_hline(
        yintercept = 1,
        lty = 4,
        color = "orange"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("firebrick", "dodgerblue4"))




# Pseudo R2 ---------------------------------------------------------------
DescTools::PseudoR2(logit, "McFadden")


# Efeitos Marginais -------------------------------------------------------
exp(logit$coefficients)

# Multicolinearidade ----
car::vif(logit) # valores abaixo de 5 - OK

# Gráfico dos efeitos ----

plot(effects::allEffects(logit))
effectsc


# Desempenho do modelo ----

# Curva ROC e AUC
library(pROC)
auc <- pROC::roc(base$ema22, glm_probs$probs)
pROC::plot.roc(auc, print.thres = T) # descobrimos o ponto de corte que fornece melhor soma de S e E

# Usando o novo ponto de corte
result2 <- as.factor(ifelse(glm_probs$probs > .648,1,0))
confusionMatrix(result2, test$Survived, positive = "1")


