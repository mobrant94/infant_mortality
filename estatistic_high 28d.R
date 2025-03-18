###HIGH HDI
test = left_join(infant_mort,social,by="code_muni")

hdi_high = test %>% filter(quintis_idhm=="Muito Alto" & idade_dias<29) 

infant_trend <- hdi_high %>%
  # Arredondar 'dtobito' para o primeiro dia do mês
  mutate(data = floor_date(dtobito, unit = "month")) %>%
  # Agrupar por 'code_muni' e 'month_obito'
  group_by(ano =year(data), code_muni, quintis_idhm, idhm) %>%
  # Criar sumário - aqui somando o número de óbitos
  summarize(num_obitos = n()) %>%
  ungroup()

birth_trend <- df %>% filter(!(code_muni=="Brasil")) %>% 
  # Filtrar para remover datas de 2023
  filter(year(data) != 2023) %>%
  group_by(code_muni,ano = year(data)) %>%
  # Criar sumário - aqui somando o número de nascimentos masculinos
  summarize(num_nasc = sum(nascimentos, na.rm = TRUE)) %>%
  ungroup()

birth_trend$code_muni = as.double(birth_trend$code_muni)

infant_trend$ano = as.double(infant_trend$ano)

im_data <- left_join(infant_trend, birth_trend, by = c("code_muni","ano"))

im_data$rate = im_data$num_obitos/im_data$num_nasc*1000

snis$sigla_uf = NULL
im_data <- left_join(im_data, snis, by = c("code_muni","ano"))
im_data$populacao_urbana=NULL


dfbf = dfbf %>% select(code_muni,ano,cobertura_bf,valor_repassado_bolsa_familia_def)
dfbf$code_muni = as.double(dfbf$code_muni)

im_data <- left_join(im_data, dfbf, by = c("code_muni","ano"))
im_data <- left_join(im_data, aps, by = c("code_muni","ano"))

favelas$code_muni = as.double(favelas$code_muni)
im_data <- left_join(im_data, favelas, by = c("code_muni","ano"))

pop_expanded$code_muni = as.double(pop_expanded$code_muni)
im_data <- left_join(im_data, pop_expanded, by = c("code_muni","ano"))

im_data$proporcao_favela = im_data$populacao_favela/im_data$populacao*100
im_data$natalidade = im_data$num_nasc/im_data$populacao*1000

im_data <- im_data[!duplicated(im_data[, c("ano", "code_muni", "quintis_idhm", "num_obitos", "num_nasc")]), ]
im_data <- im_data %>%  filter(is.finite(rate))
im_data <- im_data %>%  filter(num_nasc >= num_obitos)

vacinas$code_muni = as.double(vacinas$code_muni)
vacinas$ano = as.double(vacinas$ano)

im_data <- left_join(im_data, vacinas, by = c("code_muni","ano"))



library(dplyr)

# Calculando a média de cada variável apenas para valores dentro do limite
medias <- im_data %>%
  summarise(
    rate = mean(rate[rate <= 150], na.rm = TRUE),
    cobertura_esgoto = mean(cobertura_esgoto[cobertura_esgoto <= 120], na.rm = TRUE),
    cobertura_bf = mean(cobertura_bf[cobertura_bf <= 120], na.rm = TRUE),
    cobertura_agua = mean(cobertura_agua[cobertura_agua <= 120], na.rm = TRUE)
  )

# Substituindo os valores acima do limite pela média calculada
im_data <- im_data %>%
  mutate(
    rate = ifelse(rate > 150, medias$rate, rate),
    cobertura_esgoto = ifelse(cobertura_esgoto > 120, medias$cobertura_esgoto, cobertura_esgoto),
    cobertura_bf = ifelse(cobertura_bf > 120, medias$cobertura_bf, cobertura_bf),
    cobertura_agua = ifelse(cobertura_agua > 120, medias$cobertura_agua, cobertura_agua)
  )


# Criar um vetor com o código IBGE dos estados e suas respectivas siglas
estado_abrev <- c("11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", 
                  "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE", 
                  "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE", 
                  "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP", 
                  "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT", 
                  "52" = "GO", "53" = "DF")

# Mapear os códigos de municípios para as siglas dos estados
im_data <- im_data %>%
  mutate(natural_cond = substr(as.character(code_muni), 1, 2),  # Extrair os dois primeiros dígitos do código do município
         natural_cond = estado_abrev[natural_cond])  # Substituir pelos nomes das UFs

# Agrupar as UFs em regiões
im_data <- im_data %>%
  mutate(region_cond = case_when(
    natural_cond %in% c("RO", "AC", "AM", "RR", "PA", "AP", "TO") ~ "Norte",
    natural_cond %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste",
    natural_cond %in% c("MG", "ES", "RJ", "SP") ~ "Sudeste",
    natural_cond %in% c("PR", "SC", "RS") ~ "Sul",
    natural_cond %in% c("MS", "MT", "GO", "DF") ~ "Centro-Oeste"
  ))



# Criar a variável população pediátrica (26% de pop_all_years)
im_data <- im_data %>%
  mutate(populacao_pediatrica = populacao * 0.21)


# Se quiser garantir que a coluna continua numérica
im_data$cobertura_bf <- as.numeric(im_data$cobertura_bf)


im_data$cobertura_bf[is.na(im_data$cobertura_bf)] <- 0





library(dplyr)
library(zoo)

# Aplicando as transformações no im_data
im_data  <-  im_data %>%
  group_by(code_muni) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(cobertura_aps = ifelse(is.na(cobertura_aps), 
                                (lag(cobertura_aps) + lead(cobertura_aps)) / 2, 
                                cobertura_aps)) %>%
  ungroup()

# Primeiro, substituir os valores 0 pelo primeiro valor posterior disponível dentro de cada município
im_data <- im_data %>%
  group_by(code_muni) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(cobertura_aps = ifelse(cobertura_aps == 0, NA, cobertura_aps)) %>%  # Transformando 0 em NA
  mutate(cobertura_aps = na.locf(cobertura_aps, na.rm = FALSE, fromLast = FALSE)) %>%  # Preenchendo para frente
  ungroup()

# Aplicando as transformações no im_data
im_data  <-  im_data %>%
  group_by(code_muni) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(cobertura_agua = ifelse(is.na(cobertura_agua), 
                                 (lag(cobertura_agua) + lead(cobertura_agua)) / 2, 
                                 cobertura_agua)) %>%
  ungroup()

im_data  <-  im_data %>%
  group_by(code_muni) %>%
  arrange(ano, .by_group = TRUE) %>%
  mutate(cobertura_esgoto = ifelse(is.na(cobertura_esgoto), 
                                   (lag(cobertura_esgoto) + lead(cobertura_esgoto)) / 2, 
                                   cobertura_esgoto)) %>%
  ungroup()

# Substituir NA por 0 apenas para as variáveis a serem usadas na criação das novas variáveis
im_data <- im_data %>%
  mutate(across(
    c(cobertura_bf, cobertura_aps, cobertura_agua, cobertura_esgoto, 
      bcg, vpc, mening, rotav, proporcao_favela), 
    ~ ifelse(is.na(.), 0, .)
  ))

# Definir pontos de corte (mediana dos valores positivos) para cada variável
quantis_bf <- quantile(im_data$cobertura_bf[im_data$cobertura_bf > 0], probs = 0.5, na.rm = TRUE)
quantis_aps <- quantile(im_data$cobertura_aps[im_data$cobertura_aps > 0], probs = 0.5, na.rm = TRUE)

# Definir pontos de corte (mediana dos valores positivos) para cada variável
quantis_bf <- quantile(im_data$cobertura_bf[im_data$cobertura_bf > 0], probs = 0.5, na.rm = TRUE)
quantis_aps <- quantile(im_data$cobertura_aps[im_data$cobertura_aps > 0], probs = 0.5, na.rm = TRUE)

# Criar variáveis categóricas garantindo que 0 seja "Nenhuma" e os demais sigam os novos pontos de corte
im_data <- im_data %>%
  mutate(
    presenca_bf = case_when(
      cobertura_bf == 0 ~ "Nenhuma",
      cobertura_bf > 0 & cobertura_bf <= quantis_bf ~ "Baixa",
      cobertura_bf > quantis_bf ~ "Alta"
    ),
    presenca_aps = case_when(
      cobertura_aps == 0 ~ "Nenhuma",
      cobertura_aps > 0 & cobertura_aps <= quantis_aps ~ "Baixa",
      cobertura_aps > quantis_aps ~ "Alta"
    ),
    agua_d = case_when(
      cobertura_agua == 0 ~ "Nenhuma",
      cobertura_agua < 84 ~ "Baixa",
      cobertura_agua >= 84 ~ "Alta"
    ),
    esgoto_d = case_when(
      cobertura_esgoto == 0 ~ "Nenhuma",
      cobertura_esgoto < 42 ~ "Baixa",
      cobertura_esgoto >= 42 ~ "Alta"
    ),
    bcg_d = case_when(
      bcg == 0 ~ "Nenhuma",
      bcg < 70 ~ "Baixa",
      bcg >= 70 ~ "Alta"
    ),
    vpc_d = case_when(
      vpc == 0 ~ "Nenhuma",
      vpc < 78 ~ "Baixa",
      vpc >= 78 ~ "Alta"
    ),
    mening_d = case_when(
      mening == 0 ~ "Nenhuma",
      mening > 0 & mening <= quantis_mening ~ "Baixa",
      mening > quantis_mening ~ "Alta"
    ),
    rotav_d = case_when(
      rotav == 0 ~ "Nenhuma",
      rotav < 80 ~ "Baixa",
      rotav >= 80 ~ "Alta"
    ),
    favela_d = case_when(
      proporcao_favela == 0 ~ "Nenhuma",
      proporcao_favela < 3 ~ "Baixa",
      proporcao_favela >= 3 ~ "Alta"
    ))

# Converter para fatores ordenados
im_data <- im_data %>%
  mutate(
    presenca_bf = factor(presenca_bf, levels = c("Nenhuma", "Baixa", "Alta")),
    presenca_aps = factor(presenca_aps, levels = c("Nenhuma", "Baixa", "Alta")),
    agua_d = factor(agua_d, levels = c("Nenhuma", "Baixa", "Alta")),
    esgoto_d = factor(esgoto_d, levels = c("Nenhuma", "Baixa", "Alta")),
    bcg_d = factor(bcg_d, levels = c("Nenhuma", "Baixa", "Alta")),
    vpc_d = factor(vpc_d, levels = c("Nenhuma", "Baixa", "Alta")),
    mening_d = factor(mening_d, levels = c("Nenhuma", "Baixa", "Alta")),
    rotav_d = factor(rotav_d, levels = c("Nenhuma", "Baixa", "Alta")),
    favela_d = factor(favela_d, levels = c("Nenhuma", "Baixa", "Alta")))

