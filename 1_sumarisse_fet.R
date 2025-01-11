library(tidyverse)

load("dfet0122.RData")
df = dados_unidos
rm(dados_unidos)

# Convertendo a variável 'PESO' para numérica (caso esteja como caractere)
df$PESO <- as.numeric(df$PESO)

# Criando a variável categórica 'weight' baseada em 'PESO'
df <- df %>%
  mutate(weight_cat = case_when(
    PESO < 2500 ~ "Low birth weight",
    PESO >= 2500 & PESO <= 4000 ~ "Adequate",
    PESO > 4000 ~ "High birth weight",
    TRUE ~ NA_character_  # caso haja valores faltantes ou desconhecidos
  ))

# Criando a variável categórica 'ig_cat' baseada em 'GESTACAO'
df <- df %>%
  mutate(ig_cat = case_when(
    GESTACAO == 9 ~ "Ignored",
    GESTACAO == 1 ~ "Less than 22 weeks",
    GESTACAO == 2 ~ "22 - 27 weeks",
    GESTACAO == 3 ~ "28 - 31 weeks",
    GESTACAO == 4 ~ "32 - 36 weeks",
    GESTACAO == 5 ~ "37 - 41 weeks",
    GESTACAO == 6 ~ "42 weeks or more",
    TRUE ~ NA_character_  # caso haja valores faltantes ou desconhecidos
  ))

# Verificando a estrutura do novo data frame
str(df)

df$data = lubridate::dmy(df$DTOBITO)

library(lubridate)

# Arredondando as datas para o primeiro dia de cada mês
df <- df %>%
  mutate(data_mes = floor_date(data, unit = "month"))

# Reformular a variável SEXO
df <- df %>%
  mutate(SEXO = recode(SEXO,
                       `0` = "Ignored",
                       `1` = "Masculino",
                       `2` = "Feminino",
                       `9` = "Ignored",
                       `M` = "Masculino",
                       `F` = "Feminino",
                       `I` =  "Ignored"
  ))

# Passo 1: Sumarização por 'CODMUNRES' e 'data_mes'
df_summary <- df %>%
  group_by(CODMUNRES, data_mes) %>%
  summarise(
    count = n(),  # Contagem total
    ig_22_27 = sum(ig_cat == "22 - 27 weeks", na.rm = TRUE),
    ig_28_31 = sum(ig_cat == "28 - 31 weeks", na.rm = TRUE),
    ig_32_36 = sum(ig_cat == "32 - 36 weeks", na.rm = TRUE),
    ig_37_41 = sum(ig_cat == "37 - 41 weeks", na.rm = TRUE),
    ig_42_or_more = sum(ig_cat == "42 weeks or more", na.rm = TRUE),
    ig_ignored = sum(ig_cat == "Ignored", na.rm = TRUE),
    ig_less_22 = sum(ig_cat == "Less than 22 weeks", na.rm = TRUE),
    weight_adequate = sum(weight_cat == "Adequate", na.rm = TRUE),
    weight_high = sum(weight_cat == "High birth weight", na.rm = TRUE),
    weight_low = sum(weight_cat == "Low birth weight", na.rm = TRUE),
    SEXO_ignored = sum(SEXO == "Ignored", na.rm = TRUE),
    SEXO_masculino = sum(SEXO == "Masculino", na.rm = TRUE),
    SEXO_feminino = sum(SEXO == "Feminino", na.rm = TRUE)
  )

# Passo 2: Transformar os dados de formato largo para longo
df_long_fet <- df_summary %>%
  pivot_longer(
    cols = starts_with("ig_") | starts_with("weight_") | starts_with("SEXO_"),
    names_to = "categoria",
    values_to = "valor"
  )

save(df_long_fet,file="df_fet.RData")
