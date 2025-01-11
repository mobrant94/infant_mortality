################estatística#####################################################

codigos_estados <- c("110000", "120000", "130000", "140000", "150000", "160000", 
                     "170000", "210000", "220000", "230000", "240000", "250000", 
                     "260000", "270000", "280000", "290000", "310000", "320000", 
                     "330000", "350000", "410000", "420000", "430000", "500000", 
                     "510000", "520000", "530000", "5000000")

codigos_estados2 <- c("11000", "12000", "13000", "14000", "15000", "16000", 
                      "17000", "21000", "22000", "23000", "24000", "25000", 
                      "26000", "27000", "28000", "29000", "31000", "32000", 
                      "33000", "35000", "41000", "42000", "43000", "50000", 
                      "51000", "52000", "53000")

infant_trend4 <- infant_mort %>%
  # Arredondar 'dtobito' para o primeiro dia do mês
  mutate(data = floor_date(dtobito, unit = "month")) %>%
  # Agrupar por 'code_muni' e 'month_obito'
  group_by(data, code_muni) %>%
  # Criar sumário - aqui somando o número de óbitos
  summarize(num_obitos = n()) %>%
  ungroup()

# Filtrar o banco de dados para remover os estados
infant_trend4 <- infant_trend4 %>%
  filter(!code_muni %in% codigos_estados)

# Filtrar o banco de dados para remover os estados
infant_trend4 <- infant_trend4 %>%
  filter(!code_muni %in% codigos_estados2)

infant_trend4 = infant_trend4 %>% filter(!(code_muni=="5000000"))

birth_trend4 <- df %>% filter(!(code_muni=="Brasil")) %>% 
  # Filtrar para remover datas de 2023
  filter(year(data) != 2023) %>%
  group_by(code_muni,data) %>%
  # Criar sumário - aqui somando o número de nascimentos masculinos
  summarize(num_nasc = sum(nascimentos, na.rm = TRUE)) %>%
  ungroup()

birth_trend4$code_muni = as.double(birth_trend4$code_muni)

imb_data4 <- left_join(birth_trend4, infant_trend4, by = c("code_muni","data"))

imb_data4$count = 1:1537596
imb_data4$seas = month(imb_data4$data)
imb_data4$ano = year(imb_data4$data)
imb_data4$rate = imb_data4$num_obitos/imb_data4$num_nasc*1000
imb_data4 <- imb_data4 %>%
  mutate(rate = if_else(num_nasc > 0 & is.na(rate), 0, rate))
social$municipio =  NULL
imb_data4 <- left_join(imb_data4, social, by = c("code_muni"))
snis$sigla_uf =NULL
imb_data4 <- left_join(imb_data4, snis, by = c("code_muni","ano"))

dfbf2 = dfbf %>% select(code_muni,data,media_bf_def,media_bf_def)
dfbf2$code_muni = as.double(dfbf2$code_muni)
imb_data4 <- left_join(imb_data4, dfbf2, by = c("code_muni","data"))

# Criar a variável dummy considerando NA como 0
imb_data4 <- imb_data4 %>%
  mutate(dummy_bf_def = ifelse(is.na(media_bf_def) | media_bf_def <= 0, 0, 1))

# Criar tercis balanceados para IDH
imb_data4 <- imb_data4 %>%
  mutate(tercis_idhm = ntile(idhm, 3),  # Dividir IDH em tercis
         tercis_idhm = factor(tercis_idhm, levels = c(1, 2, 3), 
                              labels = c("Baixo", "Intermediário", "Alto")))

# Criar tercis balanceados para Gini
imb_data4 <- imb_data4 %>%
  mutate(tercis_gini = ntile(gini, 3),  # Dividir Gini em tercis
         tercis_gini = factor(tercis_gini, levels = c(1, 2, 3), 
                              labels = c("Baixo", "Intermediário", "Alto")))

# Ajustar categorias para que "Intermediário" seja a referência
imb_data4 <- imb_data4 %>%
  mutate(tercis_idhm = relevel(tercis_idhm, ref = "Intermediário"),
         tercis_gini = relevel(tercis_gini, ref = "Intermediário"))

# Criar tercis balanceados para cobertura de água
imb_data4 <- imb_data4 %>%
  mutate(tercis_agua = ntile(cobertura_agua, 3),  # Dividir cobertura de água em tercis
         tercis_agua = factor(tercis_agua, levels = c(1, 2, 3), 
                              labels = c("Baixo", "Intermediário", "Alto")))

# Criar tercis balanceados para cobertura de esgoto
imb_data4 <- imb_data4 %>%
  mutate(tercis_esgoto = ntile(cobertura_esgoto, 3),  # Dividir cobertura de esgoto em tercis
         tercis_esgoto = factor(tercis_esgoto, levels = c(1, 2, 3), 
                                labels = c("Baixo", "Intermediário", "Alto")))

# Ajustar categorias para que "Intermediário" seja a referência
imb_data4 <- imb_data4 %>%
  mutate(tercis_agua = relevel(tercis_agua, ref = "Intermediário"),
         tercis_esgoto = relevel(tercis_esgoto, ref = "Intermediário"))

# Pontos de corte para IDH
quantile(imb_data4$idhm, probs = c(1/3, 2/3), na.rm=TRUE)

# Pontos de corte para Gini
quantile(imb_data4$gini, probs = c(1/3, 2/3), na.rm=TRUE)

# Pontos de corte para Cobertura de Água
quantile(imb_data4$cobertura_agua, probs = c(1/3, 2/3), na.rm=TRUE)

# Pontos de corte para Cobertura de Esgoto
quantile(imb_data4$cobertura_esgoto, probs = c(1/3, 2/3), na.rm=TRUE)


####################idh##################################################################
idh = imb_data4 %>% filter(!(is.na(tercis_idhm)))
idh = idh %>% group_by(code_muni, tercis_idhm) %>% summarise(rate = mean(rate, na.rm=TRUE))

library(ggbeeswarm)

# Remover NAs e valores infinitos ou NaN
idh_clean <- idh[!is.na(idh$rate) & !is.na(idh$tercis_idhm) & is.finite(idh$rate), ]

# Ajustar a ordem das categorias
idh_clean$tercis_idhm <- factor(idh_clean$tercis_idhm, levels = c("Baixo", "Intermediário", "Alto"))

# Criar o gráfico
ggplot(idh_clean, aes(x = tercis_idhm, y = rate, color = tercis_idhm)) +
  geom_beeswarm() +
  labs(
    title = "Taxa de Mortalidade por Estrato de IDH",
    x = "Estrato de IDH",
    y = "Taxa de Mortalidade", color = ""
  ) +
  theme_classic() +
  facet_wrap(~ tercis_idhm, scales = "free") +  # Cria um gráfico para cada categoria
  scale_color_brewer(palette = "Set2") +
  ylim(0, 80)  # Definindo a escala fixa do eixo y de 0 a 80

idh = imb_data4 %>% filter(!(is.na(tercis_idhm)))
idh = idh %>% group_by(code_muni, tercis_idhm) %>% summarise(rate = mean(rate, na.rm=TRUE))

#######gini#####################################################################################
gini = imb_data4 %>% filter(!(is.na(tercis_gini)))
gini = gini %>% group_by(code_muni, tercis_gini) %>% summarise(rate = mean(rate, na.rm=TRUE))

library(ggbeeswarm)

gini_clean <- gini[!is.na(gini$rate) & !is.na(gini$tercis_gini) & is.finite(gini$rate), ]

# Ajustar a ordem das categorias
gini_clean$tercis_gini <- factor(gini_clean$tercis_gini, levels = c("Baixo", "Intermediário", "Alto"))

# Criar o gráfico
ggplot(gini_clean, aes(x = tercis_gini, y = rate, color = tercis_gini)) +
  geom_beeswarm() +
  labs(
    title = "Taxa de Mortalidade por Estrato de Gini",
    x = "Estrato de Gini",
    y = "Taxa de Mortalidade", color = ""
  ) +
  theme_classic() +
  facet_wrap(~ tercis_gini, scales = "free") +  # Cria um gráfico para cada categoria
  scale_color_brewer(palette = "Set2") +
  ylim(0, 80)

####################################################
tapply(gini_clean$rate, gini_clean$tercis_gini, mean, na.rm=TRUE)
tapply(idh_clean$rate, idh_clean$tercis_idhm, mean, na.rm=TRUE)

a = glm(rate~tercis_gini, data = gini_clean)
b = glm(rate~tercis_idhm, data = idh_clean)

test = imb_data4 %>% filter(rate>0)
test = test %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
summary(glm(rate~dummy_bf_def, data=test))



##graficos
infant_trend <- infant_mort %>%
  mutate(data = floor_date(dtobito, unit = "month")) %>%
  group_by(data,region_cond) %>%
  summarize(num_obitos = n()) 

# Adicionar a categoria "Brasil" com a soma de 'num_obitos' por 'data'
infant_trend <- infant_trend %>%
  bind_rows(
    infant_trend %>%
      group_by(data) %>%
      summarize(num_obitos = sum(num_obitos, na.rm = TRUE)) %>%
      mutate(region_cond = "Brasil")
  )


birth_trend <- df %>%
  # Filtrar para remover datas de 2023
  filter(year(data) != 2023) %>%
  # Agrupar por 'data'
  group_by(data, region_cond) %>%
  # Criar sumário - aqui somando o número de nascimentos masculinos
  summarize(num_nasc = sum(nascimentos, na.rm = TRUE)) %>%
  ungroup()

imb_data <- left_join(birth_trend, infant_trend, by = c("data","region_cond"))

imb_data$mort_rate = imb_data$num_obitos/imb_data$num_nasc*1000
brasil = imb_data %>% filter(region_cond=="Brasil")
bfm = dfbf %>% group_by(data) %>% summarise(media = mean(media_bf_def, na.rm=TRUE))
brasil <- left_join(brasil, bfm, by = c("data"))
brasil <- brasil %>%
  mutate(dummy_bf_def = ifelse(is.na(media) | media <= 0, 0, 1))
# Substituir NA e NaN por 0 na coluna 'media'
brasil$media[is.na(brasil$media) | is.nan(brasil$media)] <- 0










# Filtrando e agrupando os dados para cobertura de água
agua = imb_data4 %>% filter(!(is.na(tercis_agua)))
agua = agua %>% group_by(code_muni, tercis_agua) %>% summarise(rate = mean(rate, na.rm=TRUE))

# Remover NAs e valores infinitos ou NaN
agua_clean <- agua[!is.na(agua$rate) & !is.na(agua$tercis_agua) & is.finite(agua$rate), ]

# Ajustar a ordem das categorias
agua_clean$tercis_agua <- factor(agua_clean$tercis_agua, levels = c("Baixo", "Intermediário", "Alto"))
agua_clean = agua_clean %>% filter(rate>0)
# Criar o gráfico para cobertura de água
ggplot(agua_clean, aes(x = tercis_agua, y = rate, color = tercis_agua)) +
  geom_beeswarm() +
  labs(
    title = "Taxa de Mortalidade por Estrato de Cobertura de Água",
    x = "Estrato de Cobertura de Água",
    y = "Taxa de Mortalidade", color = ""
  ) +
  theme_classic() +
  facet_wrap(~ tercis_agua, scales = "free") +  # Cria um gráfico para cada categoria
  scale_color_brewer(palette = "Set2") +
  ylim(0, 80)  # Definindo a escala fixa do eixo y de 0 a 80



# Filtrando e agrupando os dados para cobertura de esgoto
esgoto = imb_data4 %>% filter(!(is.na(tercis_esgoto)))
esgoto = esgoto %>% group_by(code_muni, tercis_esgoto) %>% summarise(rate = mean(rate, na.rm=TRUE))

# Remover NAs e valores infinitos ou NaN
esgoto_clean <- esgoto[!is.na(esgoto$rate) & !is.na(esgoto$tercis_esgoto) & is.finite(esgoto$rate), ]

# Ajustar a ordem das categorias
esgoto_clean$tercis_esgoto <- factor(esgoto_clean$tercis_esgoto, levels = c("Baixo", "Intermediário", "Alto"))
esgoto_clean = esgoto_clean %>% filter(rate>0)

# Criar o gráfico para cobertura de esgoto
ggplot(esgoto_clean, aes(x = tercis_esgoto, y = rate, color = tercis_esgoto)) +
  geom_beeswarm() +
  labs(
    title = "Taxa de Mortalidade por Estrato de Cobertura de Esgoto",
    x = "Estrato de Cobertura de Esgoto",
    y = "Taxa de Mortalidade", color = ""
  ) +
  theme_classic() +
  facet_wrap(~ tercis_esgoto, scales = "free") +  # Cria um gráfico para cada categoria
  scale_color_brewer(palette = "Set2") +
  ylim(0, 80)  # Definindo a escala fixa do eixo y de 0 a 80


tapply(agua_clean$rate, agua_clean$tercis_agua, mean, na.rm=TRUE)
tapply(esgoto_clean$rate, esgoto_clean$tercis_esgoto, mean, na.rm=TRUE)

c = glm(rate~tercis_agua, data = agua_clean)
d = glm(rate~tercis_esgoto, data = esgoto_clean)

coef(c)
confint(c)

coef(d)
confint(d)


final_model = imb_data4 %>% 
  filter(
    !(is.na(tercis_idhm)) & 
      !(is.na(tercis_gini)) & 
      !(is.na(tercis_agua)) & 
      !(is.na(tercis_esgoto)) &
      is.finite(tercis_idhm) & 
      is.finite(tercis_gini) & 
      is.finite(tercis_agua) & 
      is.finite(tercis_esgoto)
  )

# Remover linhas com "Inf" na variável 'rate'
final_model = final_model[!is.infinite(final_model$rate), ]
final_model = final_model[!is.na(final_model$rate), ]
final_model = final_model %>% filter(rate>0)
library(MASS)
summary(glm(rate~tercis_esgoto+tercis_agua+tercis_gini+tercis_idhm, data = final_model))

plot(residuals(a), main = "Resíduos do Modelo Gamma")
# Verificando dispersão excessiva
dispersion <- sum(residuals(a)^2) / df.residual(a)
dispersion
# Ajustando um modelo Poisson
modelo_poisson <- glm(num_obitos ~ tercis_esgoto + tercis_agua + tercis_gini + tercis_idhm, family = "poisson", data = final_model)
summary(modelo_poisson)
AIC(modelo_poisson)
a = glm(num_obitos ~ tercis_esgoto + tercis_agua + tercis_gini + tercis_idhm, 
       data = final_model, family = "Gamma")

anova(a, test = "Chisq")

exp(coef(a))
exp(confint(a))




##graficos

imb_data$meses = month(imb_data$data)

imb_data = imb_data %>% filter(region_cond=="Brasil")

imb_data$count = 1:276


# Exemplo de ajuste do modelo usando glm
# Ajustar o modelo GLM com a taxa de mortalidade como variável resposta
modelo_glm <- glm(mort_rate ~ count + mes, data = imb_br, family = gaussian())

# Fazer previsão para os próximos valores (por exemplo, adicionando novos valores de mes)
# Aqui assumimos que você terá novos dados ou estenderá os existentes
novo_dados <- data.frame(
  mes = 1:100 + max(imb_br$mes),
  num_nasc = mean(imb_br$num_nasc), # Ajuste conforme necessário
  num_obitos = mean(imb_br$num_obitos), # Ajuste conforme necessário
  region_cond = "Brasil" # Ajuste conforme necessário
)

# Previsão usando o modelo ajustado
previsao_glm <- predict(modelo_glm, newdata = novo_dados, se.fit = TRUE)

# Criar um dataframe para as previsões
novo_dados$predicao <- previsao_glm$fit
novo_dados$inferior <- previsao_glm$fit - 1.96 * previsao_glm$se.fit
novo_dados$superior <- previsao_glm$fit + 1.96 * previsao_glm$se.fit

# Plotar os resultados
ggplot() +
  geom_line(data = imb_br, aes(x = mes, y = mort_rate), color = "blue") +
  geom_line(data = novo_dados, aes(x = mes, y = predicao), color = "red") +
  geom_ribbon(data = novo_dados, aes(x = mes, ymin = inferior, ymax = superior), alpha = 0.2) +
  labs(title = "Previsão da Taxa de Mortalidade", x = "Mês", y = "Taxa de Mortalidade")







# Criar a variável spline1
imb_data$spline1 <- ifelse(1:nrow(imb_data) <= 149, 0:(149 - 1), 149)

# Criar a variável spline2
imb_data$spline2 <- c(rep(0, 149), 0:(200 - 150), rep(49, nrow(imb_data) - 200))

# Criar a variável spline3
imb_data$spline3 <- c(rep(0, 200), 0:(242 - 201), rep(41, nrow(imb_data) - 242))

# Criar a variável spline4
imb_data$spline4 <- c(rep(0, 242), 0:(276 - 243))

# Exibir as primeiras linhas para verificar
head(imb_data)





# Ajustar o modelo GLM usando count e month como preditores
modelo_glm <- glm(mort_rate ~ meses + spline1 + spline2 + spline3 + spline4, data = imb_data, family = "gaussian")

# Criar novos dados para previsão (adicionando 96 pontos ao count)
novo_count <- seq(max(imb_data$count) + 1, max(imb_data$count) + 96)
novo_month <- rep(1:12, length.out = length(novo_count))

novo_dados <- data.frame(
  count = novo_count,
  meses = novo_month
)

# Criar novos dados para previsão (adicionando 96 pontos ao count)
novo_count <- seq(max(imb_data$count) + 1, max(imb_data$count) + 96)
novo_month <- rep(1:12, length.out = length(novo_count))

# Criar novo dataframe com splines configurados como 0 para as novas observações
novo_dados <- data.frame(
  count = novo_count,
  meses = novo_month,
  spline3 = 0,
  spline4 = 0
)

# Criar a variável spline1
novo_dados$spline1 <- 149

# Criar a variável spline2
novo_dados$spline2 = 49

# Prever os valores futuros usando o modelo ajustado
previsao_glm <- predict(modelo_glm, newdata = novo_dados, se.fit = TRUE)

# Adicionar previsões ao dataframe
novo_dados$predicao <- previsao_glm$fit
novo_dados$inferior <- previsao_glm$fit - 1.96 * previsao_glm$se.fit
novo_dados$superior <- previsao_glm$fit + 1.96 * previsao_glm$se.fit

# Prever os valores passados usando o modelo ajustado
previsao_passado <- predict(modelo_glm, newdata = imb_data, se.fit = TRUE)

# Adicionar previsões ao dataframe original
imb_data$predicao <- previsao_passado$fit
imb_data$inferior <- previsao_passado$fit - 1.96 * previsao_passado$se.fit
imb_data$superior <- previsao_passado$fit + 1.96 * previsao_passado$se.fit

# Plotar os resultados com previsões passadas e futuras
ggplot() +
  geom_line(data = imb_data, aes(x = count, y = predicao), color = "blue") +
  geom_line(data = novo_dados, aes(x = count, y = predicao), color = "red") +
  geom_ribbon(data = imb_data, aes(x = count, ymin = inferior, ymax = superior), alpha = 0.2, fill = "blue") +
  geom_ribbon(data = novo_dados, aes(x = count, ymin = inferior, ymax = superior), alpha = 0.2, fill = "red") +
  labs(title = "Previsão da Taxa de Mortalidade até 2030", x = "Período", y = "Taxa de Mortalidade") +
  theme_classic()

# Adicionar a coluna de ano aos dados históricos
imb_data$ano <- 2000 + (imb_data$count - 1) %/% 12

# Adicionar a coluna de ano aos novos dados para previsão
novo_dados$ano <- 2000 + (novo_dados$count - 1) %/% 12

ggplot() +
  # Linhas de predição com legendas
  geom_line(data = imb_data, aes(x = ano, y = predicao, color = "Passado"), size = 1, linetype = "solid") +
  geom_line(data = novo_dados, aes(x = ano, y = predicao, color = "Futuro"), size = 1, linetype = "solid") +
  # Intervalo de confiança (áreas sombreadas) com legendas
  geom_ribbon(data = imb_data, aes(x = ano, ymin = inferior, ymax = superior, fill = "IC 95% Passado"), alpha = 0.2) +
  geom_ribbon(data = novo_dados, aes(x = ano, ymin = inferior, ymax = superior, fill = "IC 95% Futuro"), alpha = 0.2) +
  # Linha vertical e texto indicando "Começo da Pandemia de COVID-19"
  geom_vline(xintercept = 2020, linetype = "dashed", size = 1.2, color = "orange2") +  # linha vertical no ano de 2020
  geom_text(aes(x = 2020, y = max(imb_data$predicao)-1, label = "Começo da Pandemia de COVID-19"), 
            angle = 360, vjust = 0, hjust = 1.01, color = "orange2") +
  # Labels e tema
  labs(title = "Previsão da Taxa de Mortalidade até 2030", 
       x = "Ano", 
       y = "Taxa de Mortalidade",
       fill = "", 
       color = "") + 
  ylim(0, 25) +
  scale_x_continuous(breaks = seq(2000, 2030, by = 2)) +
  scale_color_manual(values = c("Passado" = "blue", "Futuro" = "red")) +
  scale_fill_manual(values = c("IC 95% Passado" = "blue", "IC 95% Futuro" = "red")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(face = "bold")) +
  # Texto "ODS 3" em verde4 na posição especificada
  geom_text(aes(x = 2029.5, y = 8, label = "ODS 3"), color = "green4", size = 4, hjust = 1) +
  # Ponto ao lado do texto "ODS 3"
  geom_point(aes(x = 2030, y = 8), color = "green4", size = 3, shape = 16)
