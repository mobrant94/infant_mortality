# Converter investimento_agua_prestador e investimento_esgoto_prestador para numérico (caso estejam como caracteres)
im_data <- im_data %>%
  mutate(
    investimento_agua_prestador = as.numeric(investimento_agua_prestador),
    investimento_esgoto_prestador = as.numeric(investimento_esgoto_prestador)
  )

im_data <- im_data %>% 
  mutate(contagem = ano - 2000)  # Define contagem conforme o ano

# Ajuste dos splines
im_data <- im_data %>% 
  mutate(
    spline1 = if_else(ano <= 2011, contagem, 11),  # Conta até 11 (2000-2011), mantém 11 depois
    spline2 = if_else(ano <= 2011, 0, if_else(ano <= 2017, contagem - 11, 6)),  # Conta de 0 a 5 (2012-2017), mantém 6 depois
    spline3 = if_else(ano <= 2017, 0, contagem - 17)  # Conta de 0 a 5 (2018-2022)
  )


#######################

library(corrplot)

t = im_data


# Selecionando as variáveis de interesse
dados_corr <- t %>% ungroup() %>% select(rate, cobertura_bf, bcg, rotav, vpc,  
                                         densidade_leitos, profissionais, gasto_aps,                                                                                                       
                                         cobertura_aps, proporcao_favela, investimento_agua_prestador, 
                                         investimento_esgoto_prestador,
                                         cobertura_agua, cobertura_esgoto, valor_repassado_bolsa_familia_def, gasto_aps,                                        
                                         spline1, spline2, spline3)

# Calculando a matriz de correlação
matriz_cor <- cor(dados_corr, use="pairwise.complete.obs")

# Ajustando o tamanho da janela gráfica para melhor visualização
par(mfrow=c(1,1))  # Garantindo uma única figura
par(mai=c(1.5, 1.5, 1.5, 1.5))  # Aumentando as margens (baixo, esquerda, topo, direita)

# Plotando a matriz de correlação com melhorias
corrplot(matriz_cor, method="color", type="lower", 
         tl.col="black", tl.cex=0.8, # Cor e tamanho dos rótulos
         addCoef.col="black", number.cex=0.7, # Cor e tamanho dos coeficientes
         col=colorRampPalette(c("blue", "white", "red"))(200)) 


library(MASS)


modelo_glm1 <- glm.nb(rate ~ spline1+spline2+spline3, data = im_data, weights = populacao)
summary(modelo_glm1)


modelo_glm2 <- glm.nb(rate ~ spline1*presenca_bf + spline2*presenca_bf+ spline3*presenca_bf,
                      data = im_data, weights = populacao)
summary(modelo_glm2)

modelo_glm3 <- glm.nb(rate ~ spline1*presenca_aps+spline2*presenca_aps+spline3*presenca_aps,
                      data = im_data, weights = populacao)
summary(modelo_glm3)

modelo_glm4 <- glm.nb(rate ~ spline1*favela_d+spline2*favela_d+spline3*favela_d,
                      data = im_data, weights = populacao)
summary(modelo_glm4)

agua = im_data %>% filter(cobertura_agua>0 & (contagem>1 & contagem<22))
modelo_glm5 <- glm.nb(rate ~ spline1*agua_d+spline2*agua_d+spline3*agua_d,
                      data = agua, weights = populacao)
summary(modelo_glm5)

esgoto = im_data %>% filter(cobertura_esgoto>0 & (contagem>1 & contagem<22))
modelo_glm6 <- glm.nb(rate ~ spline1*esgoto_d+spline2*esgoto_d+spline3*esgoto_d,
                      data = esgoto, weights = populacao)
summary(modelo_glm6)

modelo_glm7 <- glm.nb(rate ~ spline1*bcg_d+spline2*bcg_d+spline3*bcg_d,
                      data = im_data %>% filter(bcg>0), weights = populacao)
summary(modelo_glm7)

modelo_glm8 <- glm.nb(rate ~ spline1*rotav_d+spline2*rotav_d+spline3*rotav_d,
                      data = im_data %>% filter(rotav>0), weights = populacao)
summary(modelo_glm8)

modelo_glm9 <- glm.nb(rate ~ spline2*vpc_d+spline3*vpc_d,
                      data = im_data %>% filter(vpc>0), weights = populacao)
summary(modelo_glm9)



###interação com favela


modelo_glm13 <- glm.nb(rate ~ spline1*presenca_bf + 
                         spline2*presenca_bf + 
                         spline3*presenca_bf,
                       data = im_data %>% filter(proporcao_favela>0), weights = populacao)
summary(modelo_glm13)

modelo_glm14 <- glm.nb(rate ~ spline1*presenca_aps+ spline2*presenca_aps + spline3*presenca_aps,
                       data = im_data %>% filter(proporcao_favela>0), weights = populacao)
summary(modelo_glm14)

modelo_glm16 <- glm.nb(rate ~ spline1*agua_d + spline2*agua_d + spline3*agua_d,
                       data = im_data %>% filter(proporcao_favela>0 & cobertura_agua>0), 
                       weights = populacao)
summary(modelo_glm16)

modelo_glm17 <- glm.nb(rate ~ spline1*esgoto_d + 
                         spline2*esgoto_d + 
                         spline3*esgoto_d,
                       data = im_data %>% filter(proporcao_favela>0 & cobertura_esgoto>0), weights = populacao)
summary(modelo_glm17)

modelo_glm18 <- glm.nb(rate ~ spline1*bcg_d + spline2*bcg_d + spline3*bcg_d,
                       data = im_data %>% filter(bcg>0), weights = populacao)

summary(modelo_glm18)

modelo_glm19 <- glm.nb(rate ~ spline1*rotav_d + spline2*rotav_d + spline3*rotav_d,
                       data = im_data %>% filter(mening>0), weights = populacao)
summary(modelo_glm19)

modelo_glm20 <- glm.nb(rate ~ spline2*vpc_d + spline3*vpc_d,
                       data = im_data %>% filter(vpc>0), weights = populacao)
summary(modelo_glm20)



library(broom)    # Para extrair resultados dos modelos
library(writexl)  # Para exportar para Excel

# Lista de modelos
modelos <- list(
  modelo_glm1, modelo_glm2, modelo_glm3, modelo_glm4, modelo_glm5, modelo_glm6, 
  modelo_glm7, modelo_glm8, modelo_glm9, 
  modelo_glm13, modelo_glm14, modelo_glm16, modelo_glm17, modelo_glm18, modelo_glm19,
  modelo_glm20)

# Função para extrair resultados dos modelos com riscos relativos
extrair_resultados <- function(modelo, nome_modelo) {
  resumo <- tidy(modelo, conf.int = TRUE, exponentiate = FALSE) %>%
    mutate(
      Modelo = nome_modelo,
      `IC 95% Inferior` = conf.low,
      `IC 95% Superior` = conf.high,
      `Risco Relativo` = exp(estimate),
      `RR IC 95% Inferior` = exp(conf.low),
      `RR IC 95% Superior` = exp(conf.high),
      p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))
    ) %>%
    select(Modelo, term, estimate, `IC 95% Inferior`, `IC 95% Superior`, p.value,
           `Risco Relativo`, `RR IC 95% Inferior`, `RR IC 95% Superior`)
  
  return(resumo)
}

# Criar tabela com os resultados de todos os modelos
resultados_todos <- bind_rows(
  lapply(seq_along(modelos), function(i) extrair_resultados(modelos[[i]], paste0("Modelo ", i)))
)

# Exportar para Excel
write_xlsx(resultados_todos, "resultados_modelos_novo28d.xlsx")

# Agregar os dados para obter a média anual da taxa de mortalidade infantil
df_plot <- im_data %>%
  group_by(ano) %>%
  summarise(rate_media = mean(rate, na.rm = TRUE))

# Agregar os dados para obter a média anual da taxa de mortalidade infantil
df_plot <- im_data %>%
  group_by(ano) %>%
  summarise(mortes = sum(num_obitos, na.rm = TRUE),
            nascimentos = sum(num_nasc, na.rm=TRUE))

df_plot$rate = df_plot$mortes/df_plot$nascimentos*1000

# Criar o gráfico com smooth
g28d = ggplot(df_plot, aes(x = ano, y = rate)) +
  geom_point(color = "blue", alpha = 0.5) +  # Pontos médios por ano
  geom_smooth(method = "loess", color = "firebrick", size = 1) + # Curva suavizada
  labs(x = "Ano",
       y = "Taxa de portalidade por 1.000 nascimentos",
       title = "<28 dias") +
  theme_classic()+
  ylim(0,25)+
  theme(axis.title = element_text(face="bold"))


library(dplyr)
library(broom)
library(writexl)
library(purrr)
library(MASS)

# Modelos sem interação
models <- list(
  "APS" = glm.nb(rate ~ gasto_aps, data = im_data %>% filter(gasto_aps >= 0)),
  "Água" = glm.nb(rate ~ investimento_agua_prestador, data = im_data %>% filter(investimento_agua_prestador >= 0)),
  "Esgoto" = glm.nb(rate ~ investimento_esgoto_prestador, data = im_data %>% filter(investimento_esgoto_prestador >= 0)),
  "Bolsa Família" = glm.nb(rate ~ valor_repassado_bolsa_familia_def, data = im_data %>% filter(valor_repassado_bolsa_familia_def >= 1))
)

# Modelos com interação (proporção de favela)
models_interaction <- list(
  "APS x Favela" = glm.nb(rate ~ gasto_aps:proporcao_favela, data = im_data %>% filter(gasto_aps >= 0)),
  "Água x Favela" = glm.nb(rate ~ investimento_agua_prestador:proporcao_favela, data = im_data %>% filter(investimento_agua_prestador >= 0)),
  "Esgoto x Favela" = glm.nb(rate ~ investimento_esgoto_prestador:proporcao_favela, data = im_data %>% filter(investimento_esgoto_prestador >= 0)),
  "Bolsa Família x Favela" = glm.nb(rate ~ valor_repassado_bolsa_familia_def:proporcao_favela, data = im_data %>% filter(valor_repassado_bolsa_familia_def >= 1))
)

# Função para extrair coeficientes, erro padrão, IC 95% e valor p
extract_results <- function(model, name) {
  results <- tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%  # Remove intercepto
    mutate(Modelo = name) %>%
    select(Modelo, term, estimate, std.error, conf.low, conf.high, p.value) %>%
    rename(IC_Lower = conf.low, IC_Upper = conf.high)
  
  return(results)
}

# Extrai resultados de todos os modelos e converte em uma única tabela
results <- bind_rows(
  map2_df(models, names(models), extract_results),
  map2_df(models_interaction, names(models_interaction), extract_results)
)

# Salvar em um arquivo Excel
write_xlsx(results, "resultados_modelos28d.xlsx")
