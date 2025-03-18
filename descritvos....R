tapply(im_data$proporcao_favela, im_data$ano, mean, na.rm=TRUE)
tapply(im_data$cobertura_agua, im_data$ano, mean, na.rm=TRUE)
tapply(im_data$cobertura_esgoto, im_data$ano, mean, na.rm=TRUE)
tapply(im_data$cobertura_aps, im_data$ano, mean, na.rm=TRUE)
tapply(im_data$cobertura_bf, im_data$ano, mean, na.rm=TRUE)


# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)

medias_anuais <- im_data %>%
  group_by(ano, region_cond) %>%
  summarise(
    `Prop. pessoas em situação de favela` = mean(proporcao_favela, na.rm = TRUE),
    `Água encanada` = mean(cobertura_agua, na.rm = TRUE),
    `Esgoto tratado` = mean(cobertura_esgoto, na.rm = TRUE),
    `APS` = mean(cobertura_aps, na.rm = TRUE),
    `Bolsa Família` = mean(cobertura_bf, na.rm = TRUE),
    `BCG`= mean(bcg, na.rm = TRUE),
    `Rotavírus`= mean(rotav, na.rm = TRUE),
    `VPC-10`= mean(vpc, na.rm = TRUE),
    `Médicos - mais médicos`= mean(profissionais, na.rm = TRUE),
    .groups = "drop"  # Remove agrupamento após o summarise
  ) %>%
  pivot_longer(cols = -c(ano, region_cond), names_to = "Cobertura", values_to = "Média")


ggplot(medias_anuais, aes(x = ano, y = Média, color = region_cond)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_classic() +
  facet_wrap(~Cobertura, scales = "free_y") +  # Permite escalas diferentes no eixo Y para cada variável
  labs(
    title = "",
    x = "Ano",
    y = "Cobertura",
    color = "Região"
  ) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold") # Negrito nos títulos dos facetes
  )




# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)

# Calcular as médias anuais por região
medias_anuais <- im_data %>%
  group_by(ano, region_cond) %>%
  summarise(
    `Prop. pessoas em situação de favela` = mean(proporcao_favela, na.rm = TRUE),
    `Água encanada` = mean(cobertura_agua, na.rm = TRUE),
    `Esgoto tratado` = mean(cobertura_esgoto, na.rm = TRUE),
    `APS` = mean(cobertura_aps, na.rm = TRUE),
    `Bolsa Família` = mean(cobertura_bf, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -c(ano, region_cond), names_to = "Cobertura", values_to = "Média")

# Criar gráfico
ggplot(medias_anuais, aes(x = ano, y = Média, color = region_cond)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_classic() +
  facet_wrap(~Cobertura, scales = "free_y") + # Divide os gráficos por variável
  labs(
    title = "",
    x = "Ano",
    y = "Cobertura",
    color = "Região"
  ) +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold") # Deixa os títulos dos facetes em negrito
  )
  

# Pacotes necessários
library(dplyr)
library(readr)
library(openxlsx)

# Supondo que infant_mort e df já estão carregados

# Criando as faixas etárias com base em idade_dias
infant_mort <- infant_mort %>%
  mutate(faixa_etaria = case_when(
    idade_dias < 28 ~ "Menor que 28 dias",
    idade_dias >= 28 & idade_dias < 365 ~ "28 dias a 1 ano",
    idade_dias >= 365 & idade_dias < 1825 ~ "1 a 4 anos",
    idade_dias < 1825 ~ "Menor que 5 anos"
  ))

# Sumarizando óbitos por ano e faixa etária
mort_summary <- infant_mort %>%
  group_by(year, faixa_etaria) %>%
  summarise(obitos = n(), .groups = "drop")

# Agregando o número de nascimentos por ano
nascimentos_ano <- df %>%
  group_by(ano) %>%
  summarise(nascimentos = sum(nascimentos), .groups = "drop")

# Juntando os óbitos e os nascimentos
mort_rate <- mort_summary %>%
  left_join(nascimentos_ano, by = c("year" = "ano")) %>%
  mutate(taxa_mortalidade = (obitos / nascimentos) * 1000)

# Exportando para Excel
write.xlsx(mort_rate, "taxa_mortalidade.xlsx")

# Exibindo o resultado
print(mort_rate)