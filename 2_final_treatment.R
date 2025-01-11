library(tidyverse)

load("pop.RData")
load("df_fet.RData")
load("df_f.RData")

# Criar um vetor com o código IBGE dos estados e suas respectivas siglas
estado_abrev <- c("11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", 
                  "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE", 
                  "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE", 
                  "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP", 
                  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT", 
                  "52" = "GO", "53" = "DF")

# Criar a variável 'uf' com base nos dois primeiros dígitos de 'CODMUNRES' e substituir pelos códigos de estado
df_long <- df_long %>%
  mutate(uf = estado_abrev[substr(as.character(CODMUNRES), 1, 2)])

df_long <- df_long %>%
  group_by(uf, data, categoria) %>%
  summarise(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')

# Criar a variável 'uf' com base nos dois primeiros dígitos de 'CODMUNRES' e substituir pelos códigos de estado
df_long_fet <- df_long_fet %>%
  mutate(uf = estado_abrev[substr(as.character(CODMUNRES), 1, 2)])

df_long_fet <- df_long_fet %>%
  group_by(uf, data_mes, categoria) %>%
  summarise(total_valor = sum(valor, na.rm = TRUE), .groups = 'drop')


births <- df_long %>% filter(!(data>"2022-12-01")) 
fetal <- df_long_fet %>% filter(!(data_mes>"2022-12-01")) 
rm(df_long,df_long_fet)

# Filtrar as linhas onde 'categoria' começa com "ig_"
ig_cat <- births %>%
  select(categoria, data, total_valor, uf) %>%
  filter(str_detect(categoria, "^ig_"))
ig_cat = ig_cat %>% filter(!(categoria=="ig_ignored"))
  

peso_n <- births %>%
  select(categoria, data, total_valor, uf) %>%
  filter(str_detect(categoria, "^weight_"))


prenatal <- births %>%
  select(categoria, data, total_valor, uf) %>%
  filter(str_detect(categoria, "^prenatal_"))


parto <- births %>%
  select(categoria, data, total_valor, uf) %>%
  filter(str_detect(categoria, "^parto_"))


escmae <- births %>%
  select(categoria, data, total_valor, uf) %>%
  filter(str_detect(categoria, "^escmae_"))


agemae <- births %>%
  select(categoria, data, total_valor, uf) %>%
  filter(str_detect(categoria, "^age_"))


births <- births %>%
  select(categoria, data, total_valor, uf) %>%
  filter(str_detect(categoria, "^weight_"))
births <- births %>%
  group_by(uf, data) %>%
  summarise(total_valor = sum(total_valor, na.rm = TRUE), .groups = 'drop')




# Transformando a coluna categoria em fator com níveis e rótulos apropriados
prenatal$categoria <- factor(prenatal$categoria, 
                             levels = c("prenatal_less_4", "prenatal_4_7", "prenatal_8_or_more"),
                             labels = c("Less than 4", "Between 4 - 7", "8 or more"),
                             ordered = FALSE)


# Transformando a coluna categoria em fator com níveis e rótulos apropriados
peso_n$categoria <- factor(peso_n$categoria, 
                           levels = c("weight_low", "weight_adequate", "weight_high"),
                           labels = c("Low birth weight", "Adequate", "High birth weight"),
                           ordered = FALSE)

# Transformando a coluna categoria em fator com níveis e rótulos
ig_cat$categoria <- factor(ig_cat$categoria, 
                           levels = c("ig_less_22", "ig_22_27", "ig_28_31", "ig_32_36", "ig_37_41", "ig_42_or_more"),
                           labels = c("Less than 22 weeks", "22 - 27 weeks", "28 - 31 weeks", 
                                      "32 - 36 weeks", "37 - 41 weeks", "42 weeks or more"),
                           ordered = FALSE)

# Transformando a coluna ESCMAE em fator ordenado
escmae <- escmae %>%
  mutate(categoria = factor(categoria, 
                         levels = c("escmae_1", "escmae_2", "escmae_3", "escmae_4", "escmae_5", "escmae_9"), 
                         labels = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 e mais", "Ignorado"),
                         ordered = FALSE))

escmae = escmae %>% filter(!(categoria=="Ignorado"))

# Transformando a coluna categoria em fator ordenado
parto <- parto %>%
  mutate(categoria = factor(categoria, 
                            levels = c("parto_1", "parto_2", "parto_9"), 
                            labels = c("Vaginal", "Cesário", "Ignorado"),
                            ordered = FALSE))

parto = parto %>% filter(!(categoria=="Ignorado"))

# Converter a coluna 'categoria' em fator com níveis específicos
agemae <- agemae %>%
  mutate(categoria = factor(categoria, 
                            levels = c("age_adequate", "age_low", "age_risk"),
                            labels = c("Adequate Age", "Low Age", "Risk Age"),
                            ordered = FALSE))

prenatal$categoria <- relevel(prenatal$categoria, ref='8 or more')
peso_n$categoria <- relevel(peso_n$categoria, ref = "Adequate")
ig_cat$categoria <- relevel(ig_cat$categoria, ref = "37 - 41 weeks")
parto$categoria <- relevel(parto$categoria, ref = "Cesário")
agemae$categoria <- relevel(agemae$categoria, ref = "Adequate Age")
escmae$categoria <- relevel(escmae$categoria, ref = "Nenhuma")

preterm <- ig_cat %>%
  filter(data >= as.Date("2012-01-01") &
           (categoria == "22 - 27 weeks" | 
              categoria == "28 - 31 weeks" | 
              categoria == "32 - 36 weeks")) %>%
  group_by(data, uf) %>%
  summarize(total_contagem = sum(total_valor, na.rm = TRUE))

###############fetal##########################################

# Filtrar as linhas onde 'categoria' começa com "ig_" e excluir "ig_ignored"
ig_cat_fetal <- fetal %>%
  select(categoria, data_mes, total_valor, uf) %>%
  filter(str_detect(categoria, "^ig_")) %>%
  filter(categoria != "ig_ignored")

# Filtrar as linhas onde 'categoria' começa com "weight_"
peso_fetal <- fetal %>%
  select(categoria, data_mes, total_valor, uf) %>%
  filter(str_detect(categoria, "^weight_"))

# Filtrar as linhas onde 'categoria' começa com "SEXO_"
sexo_fetal <- fetal %>%
  select(categoria, data_mes, total_valor, uf) %>%
  filter(str_detect(categoria, "^SEXO_"))


# Transformando a coluna categoria em fator com níveis e rótulos apropriados para as categorias 'ig_'
ig_cat_fetal <- ig_cat_fetal %>%
  mutate(categoria = factor(categoria, 
                            levels = c("ig_less_22", "ig_22_27", "ig_28_31", "ig_32_36", "ig_37_41", "ig_42_or_more"),
                            labels = c("Less than 22 weeks", "22 - 27 weeks", "28 - 31 weeks", 
                                       "32 - 36 weeks", "37 - 41 weeks", "42 weeks or more"),
                            ordered = FALSE))

# Transformando a coluna categoria em fator com níveis e rótulos apropriados para as categorias 'weight_'
peso_fetal <- peso_fetal %>%
  mutate(categoria = factor(categoria, 
                            levels = c("weight_low", "weight_adequate", "weight_high"),
                            labels = c("Low birth weight", "Adequate", "High birth weight"),
                            ordered = FALSE))

# Transformando a coluna categoria em fator com níveis e rótulos apropriados para as categorias 'SEXO_'
sexo_fetal <- sexo_fetal %>%
  mutate(categoria = factor(categoria, 
                            levels = c("SEXO_feminino", "SEXO_masculino", "SEXO_ignored"),
                            labels = c("Female", "Male", "Ignored"),
                            ordered = FALSE))



fetal <- fetal %>%
  select(categoria, data_mes, total_valor, uf) %>%
  filter(str_detect(categoria, "^SEXO_"))

fetal <- fetal %>%
  group_by(uf, data_mes) %>%
  summarise(total_valor = sum(total_valor, na.rm = TRUE), .groups = 'drop')


# Releveling as categorias para os dataframes filtrados
ig_cat_fetal$categoria <- relevel(ig_cat_fetal$categoria, ref = "37 - 41 weeks")
peso_fetal$categoria <- relevel(peso_fetal$categoria, ref = "Adequate")
sexo_fetal$categoria <- relevel(sexo_fetal$categoria, ref = "Male")


pop_brasil_ptb = pop_brasil %>% filter(mes_ano >= as.Date("2012-01-01") & mes_ano < as.Date("2023-01-01"))
births_ptb = births %>% filter(data >= as.Date("2012-01-01") & data < as.Date("2023-01-01")) %>%select(data,total_valor)







births = left_join(births,fetal,by="data_mes")
births = left_join(births, pop_brasil,by="data_mes")
preterm = left_join(preterm, pop_brasil_ptb,by="data_mes")
preterm = left_join(preterm, births_ptb,by="data_mes")
rm(births_ptb,df_sum,df_sum_fet,pop_brasil,pop_brasil_ptb,fetal)
                   
