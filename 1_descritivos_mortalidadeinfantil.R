####data discrpition apenas de mortalidade infantil
pacman::p_load(gtsummary,ggplot2,geobr,tidyverse,flextable,gganimate,forecast, patchwork, sf)

demographics = tbl_summary(
  infant_mort,
  include = c(extrato, racacor, escmae, idademae, causabas, linhaa, region_cond),
  label = list(extrato~"Extrato de faixa etária",
               racacor~"Raça/cor",
               idademae~"Idade da mãe",
               escmae~"Escolaridade da mãe",
               causabas~"Causa básica do óbito",
               linhaa~"Causa imediata do óbito",
               region_cond~"Região"),
  missing="no",
  sort = list(racacor ~ "frequency",
              causabas ~"frequency",
              linhaa ~ "frequency")) %>% 
  bold_labels() %>% as_flex_table()




####garbage code

# Renomear as categorias para português
infant_mort$garbage_severity <- factor(infant_mort$garbage_severity,
                                            levels = c("Level 1", "Level 2", "Level 3", "Level 4"),
                                            labels = c("Nível 1", "Nível 2", "Nível 3", "Nível 4"))

# Calcular os percentuais
garbage_summary <- infant_mort %>%
  group_by(garbage_severity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)


# Adicionar "Adequado" como um nível no fator
levels(garbage_summary$garbage_severity) <- c(levels(garbage_summary$garbage_severity), "Adequado")

# Substituir os NAs por "Adequado"
garbage_summary$garbage_severity[is.na(garbage_summary$garbage_severity)] <- "Adequado"

garbage_summary = garbage_summary %>% filter(!(garbage_severity=="Adequado"))

# Criar o gráfico
ggplot(garbage_summary, aes(x = garbage_severity, y = percentage)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black", size = 1) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 5) + # Percentual acima das barras
  theme_classic() + 
  labs(x = "Definição", y = "Percentual") +
  theme(axis.title.x = element_text(face = "bold"),  # Títulos dos eixos em negrito
        axis.title.y = element_text(face = "bold"),
        axis.text = element_text(size = 12, color = "black"))  


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


# Criar o gráfico de linhas animado
p <- ggplot(imb_data, aes(x = data, y = mort_rate, color = region_cond, group = region_cond)) +
  # Adicionar os pontos para as outras regiões
  geom_point(data = filter(imb_data, region_cond != "Brasil"), 
             size = 1.3, alpha = 0.5) +
  # Adicionar a linha para 'Brasil' - mais espessa e verde escuro
  geom_line(data = filter(imb_data, region_cond == "Brasil"), 
            size = 1.5, linetype = "solid") +
  # Definir a cor manualmente para as regiões
  scale_color_manual(values = c("Brasil" = "green4", 
                                "Centro-Oeste" = "orange2", 
                                "Nordeste" = "#8B0000", 
                                "Norte" = "yellow3", 
                                "Sudeste" = "#8B008B", 
                                "Sul" = "#00008B")) +
  labs(title = "Tendência geral",
        y = 'Óbitos por 1,000 nascimentos',
       x = "Período") +
  theme_classic() +
  theme(legend.title = element_blank(),
        axis.title = element_text(face = "bold")) 


p_smoothed <- ggplot(imb_data, aes(x = data, y = mort_rate, color = region_cond, group = region_cond)) +
  # Adicionar a linha suavizada para 'Brasil' e as outras regiões
  geom_smooth(size = 1.5, method = "loess", se = FALSE, linetype = "solid") +
  # Definir a cor manualmente para as regiões
  scale_color_manual(values = c("Brasil" = "green4", 
                                "Centro-Oeste" = "orange2", 
                                "Nordeste" = "#8B0000", 
                                "Norte" = "yellow3", 
                                "Sudeste" = "#8B008B", 
                                "Sul" = "#00008B")) +
  labs(title = 'Tendência suavizada', 
       x = 'Período', 
       y = 'Óbitos por 1,000 nascimentos') +
  theme_classic() +   
  theme(legend.title = element_blank(),
                            axis.title = element_text(face = "bold"))

p/p_smoothed + plot_layout(axis_titles = "collect")


############################sazonalidade########################################
###########################brasil###############################################
imb_data$mes = month(imb_data$data)

# Carregar bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggfortify)

# Adicionando a coluna mes em 'imb_data'
imb_data$mes <- month(imb_data$data)

############################### BRASIL ###############################

# Filtrando para Brasil
imb_br <- imb_data %>% filter(region_cond == "Brasil")
imb_br$count <- 1:nrow(imb_br)

# Ajuste do modelo GLM para Brasil
brs <- glm(mort_rate ~ count + factor(mes, levels = c("6", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12")), 
           data = imb_br)
summary(brs)

# Criando a série temporal para Brasil
ts_br <- ts(imb_br$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Brasil
br <- ggsubseriesplot(ts_br, main = "Brasil",
                      ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(8, 36)  # Ajuste da escala do eixo y

# Exibindo gráfico para Brasil
print(br)

############################### NORTE ###############################

# Filtrando para a região Norte
imb_norte <- imb_data %>% filter(region_cond == "Norte")
imb_norte$count <- 1:nrow(imb_norte)

# Ajuste do modelo GLM para Norte
norte_model <- glm(mort_rate ~ count + factor(mes, levels = c("6", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12")), 
                   data = imb_norte)
summary(norte_model)

# Criando a série temporal para Norte
ts_norte <- ts(imb_norte$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Norte
norte <- ggsubseriesplot(ts_norte, main = "Norte",
                         ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(8, 36)  # Ajuste da escala do eixo y

# Exibindo gráfico para Norte
print(norte)

############################### SUL ###############################

# Filtrando para a região Sul
imb_sul <- imb_data %>% filter(region_cond == "Sul")
imb_sul$count <- 1:nrow(imb_sul)

# Ajuste do modelo GLM para Sul
sul_model <- glm(mort_rate ~ count + factor(mes, levels = c("6", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12")), 
                 data = imb_sul)
summary(sul_model)

# Criando a série temporal para Sul
ts_sul <- ts(imb_sul$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sul
sul <- ggsubseriesplot(ts_sul, main = "Sul",
                       ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(8, 36)  # Ajuste da escala do eixo y

# Exibindo gráfico para Sul
print(sul)

############################### SUDESTE ###############################

# Filtrando para a região Sudeste
imb_sudeste <- imb_data %>% filter(region_cond == "Sudeste")
imb_sudeste$count <- 1:nrow(imb_sudeste)

# Ajuste do modelo GLM para Sudeste
sudeste_model <- glm(mort_rate ~ count + factor(mes, levels = c("6", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12")), 
                     data = imb_sudeste)
summary(sudeste_model)

# Criando a série temporal para Sudeste
ts_sudeste <- ts(imb_sudeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sudeste
sudeste <- ggsubseriesplot(ts_sudeste, main = "Sudeste",
                           ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(8, 36)  # Ajuste da escala do eixo y

# Exibindo gráfico para Sudeste
print(sudeste)

############################### NORDESTE ###############################

# Filtrando para a região Nordeste
imb_nordeste <- imb_data %>% filter(region_cond == "Nordeste")
imb_nordeste$count <- 1:nrow(imb_nordeste)

# Ajuste do modelo GLM para Nordeste
nordeste_model <- glm(mort_rate ~ count + factor(mes, levels = c("6", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12")), 
                      data = imb_nordeste)
summary(nordeste_model)

# Criando a série temporal para Nordeste
ts_nordeste <- ts(imb_nordeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Nordeste
nordeste <- ggsubseriesplot(ts_nordeste, main = "Nordeste",
                            ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(8, 36)  # Ajuste da escala do eixo y

# Exibindo gráfico para Nordeste
print(nordeste)

############################### CENTRO-OESTE ###############################

# Filtrando para a região Centro-Oeste
imb_centro_oeste <- imb_data %>% filter(region_cond == "Centro-Oeste")
imb_centro_oeste$count <- 1:nrow(imb_centro_oeste)

# Ajuste do modelo GLM para Centro-Oeste
centro_oeste_model <- glm(mort_rate ~ count + factor(mes, levels = c("6", "1", "2", "3", "4", "5", "7", "8", "9", "10", "11", "12")), 
                          data = imb_centro_oeste)
summary(centro_oeste_model)

# Criando a série temporal para Centro-Oeste
ts_centro_oeste <- ts(imb_centro_oeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Centro-Oeste
centro_oeste <- ggsubseriesplot(ts_centro_oeste, main = "Centro-Oeste",
                                ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(8, 36)  # Ajuste da escala do eixo y

# Exibindo gráfico para Centro-Oeste
print(centro_oeste)

# Plotando todos os gráficos na mesma tela com o layout desejado
br + norte + nordeste +
  sudeste + sul + centro_oeste + plot_layout(axis_titles = "collect")


######Sazonalidade por extrato#########
infant_trend2 <- infant_mort %>%
  # Arredondar 'dtobito' para o primeiro dia do mês
  mutate(data = floor_date(dtobito, unit = "month")) %>%
  # Agrupar por 'code_muni' e 'month_obito'
  group_by(extrato, data, region_cond) %>%
  # Criar sumário - aqui somando o número de óbitos
  summarize(num_obitos = n()) %>%
  ungroup()

# Adicionar a categoria "Brasil" com a soma de 'num_obitos' por 'data'
infant_trend2 <- infant_trend2 %>%
  bind_rows(
    infant_trend2 %>%
      group_by(data, extrato, region_cond) %>%
      summarize(num_obitos = sum(num_obitos, na.rm = TRUE), .groups = 'drop') %>%
      # Criar a categoria "Brasil" por extrato
      group_by(data, extrato) %>%
      summarize(num_obitos = sum(num_obitos, na.rm = TRUE), .groups = 'drop') %>%
      mutate(region_cond = "Brasil")
  )

imb_data2 <- left_join(birth_trend, infant_trend2, by = c("data","region_cond"))
imb_data2$mes <- as.factor(month(imb_data2$data))

imb_data2 <- imb_data2 %>%
  group_by(region_cond, extrato) %>%
  mutate(count = pmin(row_number(), 276)) %>%
  ungroup()
imb_data2$mort_rate = imb_data2$num_obitos/imb_data2$num_nasc*1000


# Neonatal Precoce - Brasil
neo_pbr = imb_data2 %>% filter(extrato=="Neonatal Precoce" & region_cond == "Brasil")
neo_pbr$mes <- relevel(factor(neo_pbr$mes), ref = "6")
# Ajuste do modelo GLM para Brasil
noe_pbr_model <- glm(mort_rate ~ count + factor(mes),data = neo_pbr)
summary(noe_pbr_model)

# Criando a série temporal para Brasil
ts_neobr_p <- ts(neo_pbr$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Brasil
neonatal_precocebr <- ggsubseriesplot(ts_neobr_p, main = "Brasil",
                                      ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(4, 15)

# Exibindo gráfico para Brasil
print(neonatal_precocebr)

# Neonatal Precoce - Norte
neo_pnorte = imb_data2 %>% filter(extrato=="Neonatal Precoce" & region_cond == "Norte")
neo_pnorte$mes <- relevel(factor(neo_pnorte$mes), ref = "6")
# Ajuste do modelo GLM para Norte
noe_pnorte_model <- glm(mort_rate ~ count + factor(mes),data = neo_pnorte)
summary(noe_pnorte_model)

# Criando a série temporal para Norte
ts_neonorte <- ts(neo_pnorte$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Norte
neonatal_preconorte <- ggsubseriesplot(ts_neonorte, main = "Norte",
                                       ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(4, 15)

# Exibindo gráfico para Norte
print(neonatal_preconorte)

# Neonatal Precoce - Nordeste
neo_pnordeste = imb_data2 %>% filter(extrato=="Neonatal Precoce" & region_cond == "Nordeste")
neo_pnordeste$mes <- relevel(factor(neo_pnordeste$mes), ref = "6")
# Ajuste do modelo GLM para Nordeste
noe_pnordeste_model <- glm(mort_rate ~ count + factor(mes),data = neo_pnordeste)
summary(noe_pnordeste_model)

# Criando a série temporal para Nordeste
ts_neonordeste <- ts(neo_pnordeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Nordeste
neonatal_preconordeste <- ggsubseriesplot(ts_neonordeste, main = "Nordeste",
                                          ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(4, 15)

# Exibindo gráfico para Nordeste
print(neonatal_preconordeste)

# Neonatal Precoce - Sudeste
neo_psudeste = imb_data2 %>% filter(extrato=="Neonatal Precoce" & region_cond == "Sudeste")
neo_psudeste$mes <- relevel(factor(neo_psudeste$mes), ref = "6")
# Ajuste do modelo GLM para Sudeste
noe_psudeste_model <- glm(mort_rate ~ count + factor(mes),data = neo_psudeste)
summary(noe_psudeste_model)

# Criando a série temporal para Sudeste
ts_neosudeste <- ts(neo_psudeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sudeste
neonatal_presudeste <- ggsubseriesplot(ts_neosudeste, main = "Sudeste",
                                       ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(4, 15)

# Exibindo gráfico para Sudeste
print(neonatal_presudeste)

# Neonatal Precoce - Sul
neo_psul = imb_data2 %>% filter(extrato=="Neonatal Precoce" & region_cond == "Sul")
neo_psul$mes <- relevel(factor(neo_psul$mes), ref = "6")
# Ajuste do modelo GLM para Sul
noe_psul_model <- glm(mort_rate ~ count + factor(mes),data = neo_psul)
summary(noe_psul_model)

# Criando a série temporal para Sul
ts_neosul <- ts(neo_psul$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sul
neonatal_presul <- ggsubseriesplot(ts_neosul, main = "Sul",
                                   ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(4, 15)

# Exibindo gráfico para Sul
print(neonatal_presul)

# Neonatal Precoce - Centro-Oeste
neo_pco = imb_data2 %>% filter(extrato=="Neonatal Precoce" & region_cond == "Centro-Oeste")
neo_pco$mes <- relevel(factor(neo_pco$mes), ref = "6")
# Ajuste do modelo GLM para Centro-Oeste
noe_pco_model <- glm(mort_rate ~ count + factor(mes),data = neo_pco)
summary(noe_pco_model)

# Criando a série temporal para Centro-Oeste
ts_neoco <- ts(neo_pco$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Centro-Oeste
neonatal_precoceco <- ggsubseriesplot(ts_neoco, main = "Centro-Oeste",
                                      ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(4, 15)

# Exibindo gráfico para Centro-Oeste
print(neonatal_precoceco)

##### Plotando todos os gráficos na mesma tela com o layout desejado##########
neonatal_precocebr + neonatal_preconorte + neonatal_preconordeste +
  neonatal_presudeste + neonatal_presul + neonatal_precoceco + plot_layout(axis_titles = "collect")


# Neonatal Tardia - Brasil
neo_tbr = imb_data2 %>% filter(extrato=="Neonatal Tardia" & region_cond == "Brasil")
neo_tbr$mes <- relevel(factor(neo_tbr$mes), ref = "6")
# Ajuste do modelo GLM para Brasil
noe_tbr_model <- glm(mort_rate ~ count + factor(mes), data = neo_tbr)
summary(noe_tbr_model)

# Criando a série temporal para Brasil
ts_neobr_t <- ts(neo_tbr$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Brasil
neonatal_tardia_br <- ggsubseriesplot(ts_neobr_t, main = "Brasil",
                                      ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 4)

# Exibindo gráfico para Brasil
print(neonatal_tardia_br)

# Neonatal Tardia - Norte
neo_tnorte = imb_data2 %>% filter(extrato=="Neonatal Tardia" & region_cond == "Norte")
neo_tnorte$mes <- relevel(factor(neo_tnorte$mes), ref = "6")
# Ajuste do modelo GLM para Norte
noe_tnorte_model <- glm(mort_rate ~ count + factor(mes), data = neo_tnorte)
summary(noe_tnorte_model)

# Criando a série temporal para Norte
ts_neonorte_t <- ts(neo_tnorte$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Norte
neonatal_tardia_norte <- ggsubseriesplot(ts_neonorte_t, main = "Norte",
                                         ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 4)

# Exibindo gráfico para Norte
print(neonatal_tardia_norte)

# Neonatal Tardia - Nordeste
neo_tnordeste = imb_data2 %>% filter(extrato=="Neonatal Tardia" & region_cond == "Nordeste")
neo_tnordeste$mes <- relevel(factor(neo_tnordeste$mes), ref = "6")
# Ajuste do modelo GLM para Nordeste
noe_tnordeste_model <- glm(mort_rate ~ count + factor(mes), data = neo_tnordeste)
summary(noe_tnordeste_model)

# Criando a série temporal para Nordeste
ts_neonordeste_t <- ts(neo_tnordeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Nordeste
neonatal_tardia_nordeste <- ggsubseriesplot(ts_neonordeste_t, main = "Nordeste",
                                            ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 4)

# Exibindo gráfico para Nordeste
print(neonatal_tardia_nordeste)

# Neonatal Tardia - Sudeste
neo_tsudeste = imb_data2 %>% filter(extrato=="Neonatal Tardia" & region_cond == "Sudeste")
neo_tsudeste$mes <- relevel(factor(neo_tsudeste$mes), ref = "6")
# Ajuste do modelo GLM para Sudeste
noe_tsudeste_model <- glm(mort_rate ~ count + factor(mes), data = neo_tsudeste)
summary(noe_tsudeste_model)

# Criando a série temporal para Sudeste
ts_neosudeste_t <- ts(neo_tsudeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sudeste
neonatal_tardia_sudeste <- ggsubseriesplot(ts_neosudeste_t, main = "Sudeste",
                                           ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 4)

# Exibindo gráfico para Sudeste
print(neonatal_tardia_sudeste)

# Neonatal Tardia - Sul
neo_tsul = imb_data2 %>% filter(extrato=="Neonatal Tardia" & region_cond == "Sul")
neo_tsul$mes <- relevel(factor(neo_tsul$mes), ref = "6")
# Ajuste do modelo GLM para Sul
noe_tsul_model <- glm(mort_rate ~ count + factor(mes), data = neo_tsul)
summary(noe_tsul_model)

# Criando a série temporal para Sul
ts_neosul_t <- ts(neo_tsul$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sul
neonatal_tardia_sul <- ggsubseriesplot(ts_neosul_t, main = "Sul",
                                       ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 4)

# Exibindo gráfico para Sul
print(neonatal_tardia_sul)

# Neonatal Tardia - Centro-Oeste
neo_tco = imb_data2 %>% filter(extrato=="Neonatal Tardia" & region_cond == "Centro-Oeste")
neo_tco$mes <- relevel(factor(neo_tco$mes), ref = "6")
# Ajuste do modelo GLM para Centro-Oeste
noe_tco_model <- glm(mort_rate ~ count + factor(mes), data = neo_tco)
summary(noe_tco_model)

# Criando a série temporal para Centro-Oeste
ts_neoco_t <- ts(neo_tco$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Centro-Oeste
neonatal_tardia_co <- ggsubseriesplot(ts_neoco_t, main = "Centro-Oeste",
                                      ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 4)

# Exibindo gráfico para Centro-Oeste
print(neonatal_tardia_co)

# Plotando todos os gráficos na mesma tela com o layout desejado
neonatal_tardia_br + neonatal_tardia_norte + neonatal_tardia_nordeste +
  neonatal_tardia_sudeste + neonatal_tardia_sul + neonatal_tardia_co + plot_layout(axis_titles = "collect")



#########################
                        ##########################
########################

# Pós-Neonatal - Brasil
post_neo_br = imb_data2 %>% filter(extrato=="Pós-Neonatal" & region_cond == "Brasil")
post_neo_br$mes <- relevel(factor(post_neo_br$mes), ref = "6")
# Ajuste do modelo GLM para Brasil
post_neo_br_model <- glm(mort_rate ~ count + factor(mes), data = post_neo_br)
summary(post_neo_br_model)

# Criando a série temporal para Brasil
ts_post_neo_br <- ts(post_neo_br$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Brasil
post_neonatal_br <- ggsubseriesplot(ts_post_neo_br, main = "Brasil",
                                    ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 12)

# Exibindo gráfico para Brasil
print(post_neonatal_br)

# Pós-Neonatal - Norte
post_neo_norte = imb_data2 %>% filter(extrato=="Pós-Neonatal" & region_cond == "Norte")
post_neo_norte$mes <- relevel(factor(post_neo_norte$mes), ref = "6")
# Ajuste do modelo GLM para Norte
post_neo_norte_model <- glm(mort_rate ~ count + factor(mes), data = post_neo_norte)
summary(post_neo_norte_model)

# Criando a série temporal para Norte
ts_post_neo_norte <- ts(post_neo_norte$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Norte
post_neonatal_norte <- ggsubseriesplot(ts_post_neo_norte, main = "Norte",
                                       ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1,13)


# Exibindo gráfico para Norte
print(post_neonatal_norte)

# Pós-Neonatal - Nordeste
post_neo_nordeste = imb_data2 %>% filter(extrato=="Pós-Neonatal" & region_cond == "Nordeste")
post_neo_nordeste$mes <- relevel(factor(post_neo_nordeste$mes), ref = "6")
# Ajuste do modelo GLM para Nordeste
post_neo_nordeste_model <- glm(mort_rate ~ count + factor(mes), data = post_neo_nordeste)
summary(post_neo_nordeste_model)

# Criando a série temporal para Nordeste
ts_post_neo_nordeste <- ts(post_neo_nordeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Nordeste
post_neonatal_nordeste <- ggsubseriesplot(ts_post_neo_nordeste, main = "Nordeste",
                                          ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1,13)



# Exibindo gráfico para Nordeste
print(post_neonatal_nordeste)

# Pós-Neonatal - Sudeste
post_neo_sudeste = imb_data2 %>% filter(extrato=="Pós-Neonatal" & region_cond == "Sudeste")
post_neo_sudeste$mes <- relevel(factor(post_neo_sudeste$mes), ref = "6")
# Ajuste do modelo GLM para Sudeste
post_neo_sudeste_model <- glm(mort_rate ~ count + factor(mes), data = post_neo_sudeste)
summary(post_neo_sudeste_model)

# Criando a série temporal para Sudeste
ts_post_neo_sudeste <- ts(post_neo_sudeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sudeste
post_neonatal_sudeste <- ggsubseriesplot(ts_post_neo_sudeste, main = "Sudeste",
                                         ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1,13)


# Exibindo gráfico para Sudeste
print(post_neonatal_sudeste)

# Pós-Neonatal - Sul
post_neo_sul = imb_data2 %>% filter(extrato=="Pós-Neonatal" & region_cond == "Sul")
post_neo_sul$mes <- relevel(factor(post_neo_sul$mes), ref = "6")
# Ajuste do modelo GLM para Sul
post_neo_sul_model <- glm(mort_rate ~ count + factor(mes), data = post_neo_sul)
summary(post_neo_sul_model)

# Criando a série temporal para Sul
ts_post_neo_sul <- ts(post_neo_sul$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sul
post_neonatal_sul <- ggsubseriesplot(ts_post_neo_sul, main = "Sul",
                                     ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1,13)


# Exibindo gráfico para Sul
print(post_neonatal_sul)

# Pós-Neonatal - Centro-Oeste
post_neo_co = imb_data2 %>% filter(extrato=="Pós-Neonatal" & region_cond == "Centro-Oeste")
post_neo_co$mes <- relevel(factor(post_neo_co$mes), ref = "6")
# Ajuste do modelo GLM para Centro-Oeste
post_neo_co_model <- glm(mort_rate ~ count + factor(mes), data = post_neo_co)
summary(post_neo_co_model)

# Criando a série temporal para Centro-Oeste
ts_post_neo_co <- ts(post_neo_co$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Centro-Oeste
post_neonatal_co <- ggsubseriesplot(ts_post_neo_co, main = "Centro-Oeste",
                                    ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1,13)


# Exibindo gráfico para Centro-Oeste
print(post_neonatal_co)

# Plotando todos os gráficos na mesma tela com o layout desejado
post_neonatal_br + post_neonatal_norte + post_neonatal_nordeste +
  post_neonatal_sudeste + post_neonatal_sul + post_neonatal_co + plot_layout(axis_titles = "collect")



# Entre 1 e 5 anos - Brasil
one_to_five_br = imb_data2 %>% filter(extrato=="Entre 1 e 5 anos" & region_cond == "Brasil")
one_to_five_br$mes <- relevel(factor(one_to_five_br$mes), ref = "6")
# Ajuste do modelo GLM para Brasil
one_to_five_br_model <- glm(mort_rate ~ count + factor(mes), data = one_to_five_br)
summary(one_to_five_br_model)

# Criando a série temporal para Brasil
ts_one_to_five_br <- ts(one_to_five_br$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Brasil
one_to_five_br_plot <- ggsubseriesplot(ts_one_to_five_br, main = "Brasil",
                                       ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 7)

# Exibindo gráfico para Brasil
print(one_to_five_br_plot)

# Entre 1 e 5 anos - Norte
one_to_five_norte = imb_data2 %>% filter(extrato=="Entre 1 e 5 anos" & region_cond == "Norte")
one_to_five_norte$mes <- relevel(factor(one_to_five_norte$mes), ref = "6")
# Ajuste do modelo GLM para Norte
one_to_five_norte_model <- glm(mort_rate ~ count + factor(mes), data = one_to_five_norte)
summary(one_to_five_norte_model)

# Criando a série temporal para Norte
ts_one_to_five_norte <- ts(one_to_five_norte$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Norte
one_to_five_norte_plot <- ggsubseriesplot(ts_one_to_five_norte, main = "Norte",
                                          ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 7)

# Exibindo gráfico para Norte
print(one_to_five_norte_plot)

# Entre 1 e 5 anos - Nordeste
one_to_five_nordeste = imb_data2 %>% filter(extrato=="Entre 1 e 5 anos" & region_cond == "Nordeste")
one_to_five_nordeste$mes <- relevel(factor(one_to_five_nordeste$mes), ref = "6")
# Ajuste do modelo GLM para Nordeste
one_to_five_nordeste_model <- glm(mort_rate ~ count + factor(mes), data = one_to_five_nordeste)
summary(one_to_five_nordeste_model)

# Criando a série temporal para Nordeste
ts_one_to_five_nordeste <- ts(one_to_five_nordeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Nordeste
one_to_five_nordeste_plot <- ggsubseriesplot(ts_one_to_five_nordeste, main = "Nordeste",
                                             ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 7)

# Exibindo gráfico para Nordeste
print(one_to_five_nordeste_plot)

# Entre 1 e 5 anos - Sudeste
one_to_five_sudeste = imb_data2 %>% filter(extrato=="Entre 1 e 5 anos" & region_cond == "Sudeste")
one_to_five_sudeste$mes <- relevel(factor(one_to_five_sudeste$mes), ref = "6")
# Ajuste do modelo GLM para Sudeste
one_to_five_sudeste_model <- glm(mort_rate ~ count + factor(mes), data = one_to_five_sudeste)
summary(one_to_five_sudeste_model)

# Criando a série temporal para Sudeste
ts_one_to_five_sudeste <- ts(one_to_five_sudeste$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sudeste
one_to_five_sudeste_plot <- ggsubseriesplot(ts_one_to_five_sudeste, main = "Sudeste",
                                            ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 7)

# Exibindo gráfico para Sudeste
print(one_to_five_sudeste_plot)

# Entre 1 e 5 anos - Sul
one_to_five_sul = imb_data2 %>% filter(extrato=="Entre 1 e 5 anos" & region_cond == "Sul")
one_to_five_sul$mes <- relevel(factor(one_to_five_sul$mes), ref = "6")
# Ajuste do modelo GLM para Sul
one_to_five_sul_model <- glm(mort_rate ~ count + factor(mes), data = one_to_five_sul)
summary(one_to_five_sul_model)

# Criando a série temporal para Sul
ts_one_to_five_sul <- ts(one_to_five_sul$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Sul
one_to_five_sul_plot <- ggsubseriesplot(ts_one_to_five_sul, main = "Sul",
                                        ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 7)
# Exibindo gráfico para Sul
print(one_to_five_sul_plot)

# Entre 1 e 5 anos - Centro-Oeste
one_to_five_co = imb_data2 %>% filter(extrato=="Entre 1 e 5 anos" & region_cond == "Centro-Oeste")
one_to_five_co$mes <- relevel(factor(one_to_five_co$mes), ref = "6")
# Ajuste do modelo GLM para Centro-Oeste
one_to_five_co_model <- glm(mort_rate ~ count + factor(mes), data = one_to_five_co)
summary(one_to_five_co_model)

# Criando a série temporal para Centro-Oeste
ts_one_to_five_co <- ts(one_to_five_co$mort_rate, start = c(2000, 1), end = c(2022, 12), frequency = 12)

# Gráfico de sazonalidade para Centro-Oeste
one_to_five_co_plot <- ggsubseriesplot(ts_one_to_five_co, main = "Centro-Oeste",
                                       ylab = "Óbitos por 1,000 Nascimentos") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold")) +
  ylim(1, 7)

# Exibindo gráfico para Centro-Oeste
print(one_to_five_co_plot)

# Plotando todos os gráficos na mesma tela com o layout desejado
one_to_five_br_plot + one_to_five_norte_plot + one_to_five_nordeste_plot +
  one_to_five_sudeste_plot + one_to_five_sul_plot + one_to_five_co_plot + plot_layout(axis_titles = "collect")



#########MAPAS########

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

infant_trend3 <- infant_mort %>% 
  # Arredondar 'dtobito' para o primeiro dia do mês
  mutate(ano = year(dtobito)) %>% filter(ano=="2000" | ano=="2022") %>% 
  # Agrupar por 'code_muni' e 'month_obito'
  group_by(extrato, code_muni, ano) %>%
  # Criar sumário - aqui somando o número de óbitos
  summarize(num_obitos = n()) %>%
  ungroup()

# Filtrar o banco de dados para remover os estados
infant_trend3 <- infant_trend3 %>%
  filter(!code_muni %in% codigos_estados)

# Filtrar o banco de dados para remover os estados
infant_trend3 <- infant_trend3 %>%
  filter(!code_muni %in% codigos_estados2)

infant_trend3 = infant_trend3 %>% filter(!(code_muni=="5000000"))

birth_trend2 <- df %>% filter(!(code_muni=="Brasil")) %>% 
  # Filtrar para remover datas de 2023
  filter(year(data)== 2022|year(data)== 2000) %>%
  mutate(ano = year(data)) %>%
  # Agrupar por 'data'
  group_by(code_muni,ano) %>%
  # Criar sumário - aqui somando o número de nascimentos masculinos
  summarize(num_nasc = sum(nascimentos, na.rm = TRUE)) %>%
  ungroup()

birth_trend2$code_muni = as.double(birth_trend2$code_muni)

imb_data3 <- left_join(birth_trend2, infant_trend3, by = c("code_muni","ano"))

#General
general <- imb_data3 %>% 
  # Agrupar por 'code_muni'
  group_by(code_muni,ano) %>% summarise(nascimentos = mean(num_nasc, na.rm=TRUE),
                                    obitos = sum(num_obitos, na.rm=TRUE))

general$rate = general$obitos/general$nascimentos*1000

# Carregar os dados geoespaciais dos municípios brasileiros
municipios = read_municipality(year = 2020)
municipios2 = read_municipality(year = 2000)
# Verificar a estrutura dos dados de 'municipios'
municipios_2020 <- municipios %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

municipios_2000 <- municipios2 %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))


# Modificar 'code_muni' para remover o sétimo dígito
dados_mapa_2020 <- left_join(municipios_2020, general %>% filter(ano == 2022), by = c("code_muni" = "code_muni"))
dados_mapa_2000 <- left_join(municipios_2000, general %>% filter(ano == 2000), by = c("code_muni" = "code_muni"))


# Juntar os dados de 'general' com os dados de 'municipios' por 'code_muni'
dados_mapa_2020 <- dados_mapa_2020 %>% mutate(ano = 2022)
dados_mapa_2000 <- dados_mapa_2000 %>% mutate(ano = 2000)
dados_mapa_combined <- bind_rows(dados_mapa_2000, dados_mapa_2020)


library(fields)  # Para usar a função tim.colors

# Plotar o mapa de calor com escala personalizada
mort = ggplot(data = dados_mapa_combined) +
  geom_sf(aes(fill = rate), color = NA) +
  scale_fill_gradientn(colors = tim.colors(50), limits = c(0, 100), na.value = "white") +
  labs(title = "Mortalidade: 2000 e 2020", fill = "Taxa de Mortalidade") +
  facet_wrap(~ano) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


dados_mapa_2000 <- general %>% filter(ano == 2000)
dados_mapa_2022 <- general %>% filter(ano == 2022)

# Juntar os dados de 2000 e 2022 por 'code_muni'
dados_diferenca <- left_join(dados_mapa_2022, dados_mapa_2000, by = "code_muni", suffix = c("_2022", "_2000"))

# Calcular a diferença percentual relativa
dados_diferenca <- dados_diferenca %>%
  mutate(diferenca_percentual = case_when(
    rate_2000 == 0 & rate_2022 == 0 ~ 0,                       # Sem mudança
    rate_2000 == 0 & rate_2022 > 0 ~ 100,                      # Aumento de 100% quando 2000 é 0 e 2022 é positivo
    rate_2000 != 0 & rate_2022 == 0 ~ -100,                    # Redução total
    TRUE ~ ((rate_2022 - rate_2000) / rate_2000) * 100         # Cálculo normal de diferença percentual
  ))

municipios <- read_municipality(year = 2022) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

dados_diferenca <- dados_diferenca %>%
  left_join(municipios, by = "code_muni")


str(dados_diferenca)  # Verifique se a estrutura está correta

# Plotar o mapa
dif = ggplot(data = dados_diferenca) +
  geom_sf(aes(geometry = geom, fill = diferenca_percentual), color = NA) +
  scale_fill_gradientn(colors = c("#1F65CC","steelblue", "#1b9e77", "green2", "yellow3", "orange", "firebrick2"), 
                       limits = c(-300, 611), 
                       na.value = "white") +
  labs(title = "Diferença Percentual Relativa na Taxa de Mortalidade (2022 vs 2000)",
       fill = "Variação (%)") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())



# Neonatal Precoce###############################################################
# Filtrando os dados para "Neonatal Precoce"
neo_p = imb_data3 %>% filter(extrato == "Neonatal Precoce")

# Agrupar por 'code_muni' e 'ano', e calcular as variáveis desejadas
neo_p_general <- neo_p %>% 
  group_by(code_muni, ano) %>% 
  summarise(nascimentos = mean(num_nasc, na.rm = TRUE),
            obitos = sum(num_obitos, na.rm = TRUE))

neo_p_general$rate = neo_p_general$obitos / neo_p_general$nascimentos * 1000

# Carregar os dados geoespaciais dos municípios brasileiros
municipios = read_municipality(year = 2020)
municipios2 = read_municipality(year = 2000)

# Modificar 'code_muni' para remover o sétimo dígito
municipios_2020 <- municipios %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

municipios_2000 <- municipios2 %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

# Juntar os dados de 'neo_p_general' com os dados de 'municipios' por 'code_muni'
dados_mapa_2020 <- left_join(municipios_2020, neo_p_general %>% filter(ano == 2022), by = c("code_muni" = "code_muni"))
dados_mapa_2000 <- left_join(municipios_2000, neo_p_general %>% filter(ano == 2000), by = c("code_muni" = "code_muni"))

# Juntar os dados de 2000 e 2022
dados_mapa_2020 <- dados_mapa_2020 %>% mutate(ano = 2022)
dados_mapa_2000 <- dados_mapa_2000 %>% mutate(ano = 2000)
dados_mapa_combined <- bind_rows(dados_mapa_2000, dados_mapa_2020)

# Plotar o mapa de calor com escala personalizada
neo_p_mort = ggplot(data = dados_mapa_combined) +
  geom_sf(aes(fill = rate), color = NA) +
  scale_fill_gradientn(colors = tim.colors(50), limits = c(0, 100), na.value = "white") +
  labs(title = "Mortalidade Neonatal Precoce: 2000 e 2020", fill = "Taxa de Mortalidade") +
  facet_wrap(~ano) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Juntar os dados de 2000 e 2022 por 'code_muni'
neo_p_dados_2000 <- neo_p_general %>% filter(ano == 2000)
neo_p_dados_2022 <- neo_p_general %>% filter(ano == 2022)

# Calcular a diferença percentual relativa
neo_p_dados_diferenca <- left_join(neo_p_dados_2022, neo_p_dados_2000, by = "code_muni", suffix = c("_2022", "_2000"))
neo_p_dados_diferenca <- neo_p_dados_diferenca %>%
  mutate(diferenca_percentual = case_when(
    rate_2000 == 0 & rate_2022 == 0 ~ 0,                       # Sem mudança
    rate_2000 == 0 & rate_2022 > 0 ~ 100,                      # Aumento de 100% quando 2000 é 0 e 2022 é positivo
    rate_2000 != 0 & rate_2022 == 0 ~ -100,                    # Redução total
    TRUE ~ ((rate_2022 - rate_2000) / rate_2000) * 100         # Cálculo normal de diferença percentual
  ))

municipios <- read_municipality(year = 2022) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

neo_p_dados_diferenca <- neo_p_dados_diferenca %>%
  left_join(municipios, by = "code_muni")

# Plotar o mapa da diferença percentual
neo_p_dif = ggplot(data = neo_p_dados_diferenca) +
  geom_sf(aes(geometry = geom, fill = diferenca_percentual), color = NA) +
  scale_fill_gradientn(colors = c("#1F65CC","steelblue", "#1b9e77", "green2", "yellow3", "orange", "firebrick2"), 
                       limits = c(-300, 611), 
                       na.value = "white") +
  labs(title = "Diferença Percentual Relativa na Taxa de Mortalidade Neonatal Precoce (2022 vs 2000)",
       fill = "Variação (%)") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


#############################################################################################################

# Neonatal Tardia
neo_t = imb_data3 %>% filter(extrato == "Neonatal Tardia")

# Agrupar por 'code_muni' e 'ano', e calcular as variáveis desejadas
neo_t_general <- neo_t %>% 
  group_by(code_muni, ano) %>% 
  summarise(nascimentos = mean(num_nasc, na.rm = TRUE),
            obitos = sum(num_obitos, na.rm = TRUE))

neo_t_general$rate = neo_t_general$obitos / neo_t_general$nascimentos * 1000

# Carregar os dados geoespaciais dos municípios brasileiros
municipios = read_municipality(year = 2020)
municipios2 = read_municipality(year = 2000)

# Modificar 'code_muni' para remover o sétimo dígito
municipios_2020 <- municipios %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

municipios_2000 <- municipios2 %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

# Juntar os dados de 'neo_t_general' com os dados de 'municipios' por 'code_muni'
dados_mapa_2020 <- left_join(municipios_2020, neo_t_general %>% filter(ano == 2022), by = c("code_muni" = "code_muni"))
dados_mapa_2000 <- left_join(municipios_2000, neo_t_general %>% filter(ano == 2000), by = c("code_muni" = "code_muni"))

# Juntar os dados de 2000 e 2022
dados_mapa_2020 <- dados_mapa_2020 %>% mutate(ano = 2022)
dados_mapa_2000 <- dados_mapa_2000 %>% mutate(ano = 2000)
dados_mapa_combined <- bind_rows(dados_mapa_2000, dados_mapa_2020)

# Plotar o mapa de calor para Neonatal Tardia
neo_t_mort = ggplot(data = dados_mapa_combined) +
  geom_sf(aes(fill = rate), color = NA) +
  scale_fill_gradientn(colors = tim.colors(50), limits = c(0, 60), na.value = "white") +
  labs(title = "Mortalidade Neonatal Tardia: 2000 e 2020", fill = "Taxa de Mortalidade") +
  facet_wrap(~ano) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Juntar os dados de 2000 e 2022 por 'code_muni'
neo_t_dados_2000 <- neo_t_general %>% filter(ano == 2000)
neo_t_dados_2022 <- neo_t_general %>% filter(ano == 2022)

# Calcular a diferença percentual relativa
neo_t_dados_diferenca <- left_join(neo_t_dados_2022, neo_t_dados_2000, by = "code_muni", suffix = c("_2022", "_2000"))
neo_t_dados_diferenca <- neo_t_dados_diferenca %>%
  mutate(diferenca_percentual = case_when(
    rate_2000 == 0 & rate_2022 == 0 ~ 0,                       # Sem mudança
    rate_2000 == 0 & rate_2022 > 0 ~ 100,                      # Aumento de 100% quando 2000 é 0 e 2022 é positivo
    rate_2000 != 0 & rate_2022 == 0 ~ -100,                    # Redução total
    TRUE ~ ((rate_2022 - rate_2000) / rate_2000) * 100         # Cálculo normal de diferença percentual
  ))

municipios <- read_municipality(year = 2022) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

neo_t_dados_diferenca <- neo_t_dados_diferenca %>%
  left_join(municipios, by = "code_muni")

# Plotar o mapa da diferença percentual para Neonatal Tardia
neo_t_dif = ggplot(data = neo_t_dados_diferenca) +
  geom_sf(aes(geometry = geom, fill = diferenca_percentual), color = NA) +
  scale_fill_gradientn(colors = c("#1F65CC", "steelblue", "#1b9e77", "green2", "yellow3", "orange", "firebrick2"), 
                       limits = c(-300, 611), 
                       na.value = "white") +
  labs(title = "Diferença Percentual Relativa na Taxa de Mortalidade Neonatal Tardia (2022 vs 2000)",
       fill = "Variação (%)") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 



####################################################################################################
# Pós-Neonatal
postneo = imb_data3 %>% filter(extrato == "Pós-Neonatal")

# Agrupar por 'code_muni' e 'ano', e calcular as variáveis desejadas
postneo_general <- postneo %>% 
  group_by(code_muni, ano) %>% 
  summarise(nascimentos = mean(num_nasc, na.rm = TRUE),
            obitos = sum(num_obitos, na.rm = TRUE))

postneo_general$rate = postneo_general$obitos / postneo_general$nascimentos * 1000

# Carregar os dados geoespaciais dos municípios brasileiros
municipios = read_municipality(year = 2020)
municipios2 = read_municipality(year = 2000)

# Modificar 'code_muni' para remover o sétimo dígito
municipios_2020 <- municipios %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

municipios_2000 <- municipios2 %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

# Juntar os dados de 'postneo_general' com os dados de 'municipios' por 'code_muni'
dados_mapa_2020 <- left_join(municipios_2020, postneo_general %>% filter(ano == 2022), by = c("code_muni" = "code_muni"))
dados_mapa_2000 <- left_join(municipios_2000, postneo_general %>% filter(ano == 2000), by = c("code_muni" = "code_muni"))

# Juntar os dados de 2000 e 2022
dados_mapa_2020 <- dados_mapa_2020 %>% mutate(ano = 2022)
dados_mapa_2000 <- dados_mapa_2000 %>% mutate(ano = 2000)
dados_mapa_combined <- bind_rows(dados_mapa_2000, dados_mapa_2020)

# Plotar o mapa de calor para Pós-Neonatal
postneo_mort = ggplot(data = dados_mapa_combined) +
  geom_sf(aes(fill = rate), color = NA) +
  scale_fill_gradientn(colors = tim.colors(50), limits = c(0, 60), na.value = "white") +
  labs(title = "Mortalidade Pós-Neonatal: 2000 e 2020", fill = "Taxa de Mortalidade") +
  facet_wrap(~ano) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Juntar os dados de 2000 e 2022 por 'code_muni'
postneo_dados_2000 <- postneo_general %>% filter(ano == 2000)
postneo_dados_2022 <- postneo_general %>% filter(ano == 2022)

# Calcular a diferença percentual relativa
postneo_dados_diferenca <- left_join(postneo_dados_2022, postneo_dados_2000, by = "code_muni", suffix = c("_2022", "_2000"))
postneo_dados_diferenca <- postneo_dados_diferenca %>%
  mutate(diferenca_percentual = case_when(
    rate_2000 == 0 & rate_2022 == 0 ~ 0,                       # Sem mudança
    rate_2000 == 0 & rate_2022 > 0 ~ 100,                      # Aumento de 100% quando 2000 é 0 e 2022 é positivo
    rate_2000 != 0 & rate_2022 == 0 ~ -100,                    # Redução total
    TRUE ~ ((rate_2022 - rate_2000) / rate_2000) * 100         # Cálculo normal de diferença percentual
  ))

municipios <- read_municipality(year = 2022) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

postneo_dados_diferenca <- postneo_dados_diferenca %>%
  left_join(municipios, by = "code_muni")

# Plotar o mapa da diferença percentual para Pós-Neonatal
postneo_dif = ggplot(data = postneo_dados_diferenca) +
  geom_sf(aes(geometry = geom, fill = diferenca_percentual), color = NA) +
  scale_fill_gradientn(colors = c("#1F65CC", "steelblue", "#1b9e77", "green2", "yellow3", "orange", "firebrick2"), 
                       limits = c(-300, 611), 
                       na.value = "white") +
  labs(title = "Diferença Percentual Relativa na Taxa de Mortalidade Pós-Neonatal (2022 vs 2000)",
       fill = "Variação (%)") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 



####################################################################################################
# Entre 1 e 5 anos
between1_5_br = imb_data3 %>% filter(extrato == "Entre 1 e 5 anos")

# Agrupar por 'code_muni' e 'ano', e calcular as variáveis desejadas
between1_5_br_general <- between1_5_br %>% 
  group_by(code_muni, ano) %>% 
  summarise(nascimentos = mean(num_nasc, na.rm = TRUE),
            obitos = sum(num_obitos, na.rm = TRUE))

between1_5_br_general$rate = between1_5_br_general$obitos / between1_5_br_general$nascimentos * 1000

# Carregar os dados geoespaciais dos municípios brasileiros
municipios = read_municipality(year = 2022)
municipios2 = read_municipality(year = 2000)

# Modificar 'code_muni' para remover o sétimo dígito
municipios_2020 <- municipios %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

municipios_2000 <- municipios2 %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

# Juntar os dados de 'between1_5_br_general' com os dados de 'municipios' por 'code_muni'
dados_mapa_2020 <- left_join(municipios_2020, between1_5_br_general %>% filter(ano == 2022), by = c("code_muni" = "code_muni"))
dados_mapa_2000 <- left_join(municipios_2000, between1_5_br_general %>% filter(ano == 2000), by = c("code_muni" = "code_muni"))

# Juntar os dados de 2000 e 2022
dados_mapa_2020 <- dados_mapa_2020 %>% mutate(ano = 2022)
dados_mapa_2000 <- dados_mapa_2000 %>% mutate(ano = 2000)
dados_mapa_combined <- bind_rows(dados_mapa_2000, dados_mapa_2020)

# Plotar o mapa de calor para Entre 1 e 5 anos
between1_5_br_mort = ggplot(data = dados_mapa_combined) +
  geom_sf(aes(fill = rate), color = NA) +
  scale_fill_gradientn(colors = tim.colors(50), limits = c(0, 60), na.value = "white") +
  labs(title = "Mortalidade Entre 1 e 5 Anos: 2000 e 2020", fill = "Taxa de Mortalidade") +
  facet_wrap(~ano) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# Juntar os dados de 2000 e 2022 por 'code_muni'
between1_5_br_dados_2000 <- between1_5_br_general %>% filter(ano == 2000)
between1_5_br_dados_2022 <- between1_5_br_general %>% filter(ano == 2022)

# Calcular a diferença percentual relativa
between1_5_br_dados_diferenca <- left_join(between1_5_br_dados_2022, between1_5_br_dados_2000, by = "code_muni", suffix = c("_2022", "_2000"))
between1_5_br_dados_diferenca <- between1_5_br_dados_diferenca %>%
  mutate(diferenca_percentual = case_when(
    rate_2000 == 0 & rate_2022 == 0 ~ 0,                       # Sem mudança
    rate_2000 == 0 & rate_2022 > 0 ~ 100,                      # Aumento de 100% quando 2000 é 0 e 2022 é positivo
    rate_2000 != 0 & rate_2022 == 0 ~ -100,                    # Redução total
    TRUE ~ ((rate_2022 - rate_2000) / rate_2000) * 100         # Cálculo normal de diferença percentual
  ))

municipios <- read_municipality(year = 2022) %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

between1_5_br_dados_diferenca <- between1_5_br_dados_diferenca %>%
  left_join(municipios, by = "code_muni")

# Plotar o mapa da diferença percentual para Entre 1 e 5 anos
between1_5_br_dif = ggplot(data = between1_5_br_dados_diferenca) +
  geom_sf(aes(geometry = geom, fill = diferenca_percentual), color = NA) +
  scale_fill_gradientn(colors = c("#1F65CC", "steelblue", "#1b9e77", "green2", "yellow3", "orange", "firebrick2"), 
                       limits = c(-300, 611), 
                       na.value = "white") +
  labs(title = "Diferença Percentual Relativa na Taxa de Mortalidade Entre 1 e 5 Anos (2022 vs 2000)",
       fill = "Variação (%)") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 

