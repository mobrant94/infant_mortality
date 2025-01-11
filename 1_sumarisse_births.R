pacman::p_load(tidyverse,dplyr)

setwd("C:/Users/MARCOS.ANTUNES/Documents")

load("df.RData")

df$data = lubridate::dmy(df$DTNASC)

# Arredondando as datas para o primeiro dia de cada mês
df <- df %>%
  mutate(data = floor_date(data, unit = "month"))

# Criando a variável categórica 'prenatal' baseada em 'CONSULTAS'
df <- df %>%
  mutate(prenatal_cat = case_when(
    CONSULTAS < 4 ~ "Less than 4",
    CONSULTAS >= 4 & CONSULTAS <= 7 ~ "Between 4 - 7",
    CONSULTAS >= 8 ~ "8 or more",
    TRUE ~ NA_character_  # caso haja valores faltantes ou desconhecidos
  ))

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

# Criando a variável categórica 'ig_cat' baseada em 'GESTACAO'
df <- df %>%
  mutate(idade_cat = case_when(
    IDADEMAE < 18 ~ "Low",
    IDADEMAE >= 18 & IDADEMAE <= 40 ~ "Adequate age",
    IDADEMAE > 40 ~ "Risk age",
    TRUE ~ NA_character_  # caso haja valores faltantes ou desconhecidos
  ))

df$IDANOMAL = NULL
df$IDADEMAE = NULL
df$CONSULTAS = NULL
df$PESO = NULL
df$GRAVIDEZ = NULL
df$ESTCIVMAE = NULL
df$CODOCUPMAE = NULL
df$DTNASC = NULL
df$SEXO = NULL
df$GESTACAO = NULL
df$APGAR1 = NULL
df$APGAR5 = NULL


# Sumarização dos dados agrupados por CODMUNRES e data
# Passo 1: Sumarização por 'CODMUNRES' e 'data'
df_summary <- df %>%
  group_by(CODMUNRES, data) %>%
  summarise(
    count = n(),  # Contagem total
    escmae_1 = sum(ESCMAE == 1, na.rm = TRUE),  # Nenhuma
    escmae_2 = sum(ESCMAE == 2, na.rm = TRUE),  # 1 a 3 anos
    escmae_3 = sum(ESCMAE == 3, na.rm = TRUE),  # 4 a 7 anos
    escmae_4 = sum(ESCMAE == 4, na.rm = TRUE),  # 8 a 11 anos
    escmae_5 = sum(ESCMAE == 5, na.rm = TRUE),  # 12 e mais
    escmae_9 = sum(ESCMAE == 9, na.rm = TRUE),  # Ignorado
    parto_1 = sum(PARTO == 1, na.rm = TRUE),    # Vaginal
    parto_2 = sum(PARTO == 2, na.rm = TRUE),    # Cesário
    parto_9 = sum(PARTO == 9, na.rm = TRUE),    # Ignorado
    racacor_1 = sum(RACACOR == 1, na.rm = TRUE), # Branca
    racacor_2 = sum(RACACOR == 2, na.rm = TRUE), # Preta
    racacor_3 = sum(RACACOR == 3, na.rm = TRUE), # Amarela
    racacor_4 = sum(RACACOR == 4, na.rm = TRUE), # Parda
    racacor_5 = sum(RACACOR == 5, na.rm = TRUE), # Indígena
    prenatal_less_4 = sum(prenatal_cat == "Less than 4", na.rm = TRUE),
    prenatal_4_7 = sum(prenatal_cat == "Between 4 - 7", na.rm = TRUE),
    prenatal_8_or_more = sum(prenatal_cat == "8 or more", na.rm = TRUE),
    weight_adequate = sum(weight_cat == "Adequate", na.rm = TRUE),
    weight_high = sum(weight_cat == "High birth weight", na.rm = TRUE),
    weight_low = sum(weight_cat == "Low birth weight", na.rm = TRUE),
    ig_22_27 = sum(ig_cat == "22 - 27 weeks", na.rm = TRUE),
    ig_28_31 = sum(ig_cat == "28 - 31 weeks", na.rm = TRUE),
    ig_32_36 = sum(ig_cat == "32 - 36 weeks", na.rm = TRUE),
    ig_37_41 = sum(ig_cat == "37 - 41 weeks", na.rm = TRUE),
    ig_42_or_more = sum(ig_cat == "42 weeks or more", na.rm = TRUE),
    ig_less_22 = sum(ig_cat == "Less than 22 weeks", na.rm = TRUE),
    ig_ignored = sum(ig_cat == "Ignored", na.rm = TRUE),
    age_adequate = sum(idade_cat == "Adequate age", na.rm = TRUE),
    age_low = sum(idade_cat == "Low", na.rm = TRUE),
    age_risk = sum(idade_cat == "Risk age", na.rm = TRUE)  # Nova categoria
  )

# Transformando os dados de formato largo para longo
df_long <- df_summary %>%
  pivot_longer(
    cols = starts_with("escmae_") | starts_with("parto_") | starts_with("racacor_") |
      starts_with("prenatal_") | starts_with("weight_") | starts_with("ig_") | 
      starts_with("age_"),
    names_to = "categoria",
    values_to = "valor"
  )


df_long = df_long %>% arrange(data,CODMUNRES)

save(df_long,file="df_f.RData")
