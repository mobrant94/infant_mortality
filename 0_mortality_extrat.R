
pacman::p_load(tidyverse,janitor,vroom)

# Função para filtrar indivíduos com menos de 1 ano
filtro <- function(df) {
  # Converter as datas para o formato Date
  df$DTOBITO <- dmy(df$DTOBITO)
  df$DTNASC <- dmy(df$DTNASC)
  # Calcular a diferença em dias entre DTNASC e DTOBITO
  df$idade_dias <- as.numeric(difftime(df$DTOBITO, df$DTNASC, units = "days"))
  # Filtrar indivíduos com idade < 365 dias
  df_filtrado <- df[df$idade_dias < 1825, ]
  return(df_filtrado)
}

# Função para transformar variáveis em numéricas
transformar_variaveis_numericas <- function(df) {
  df %>%
    mutate(across(
      c("CODMUNRES", "IDADEMAE", "ESCMAE", "OCUPMAE", "QTDFILVIVO", 
        "QTDFILMORT", "GESTACAO", "PARTO", "OBITOPARTO", "PESO", 
        "OBITOGRAV", "OBITOPUERP", "ESC", "CAUSABAS", "LINHAA", 
      "idade_dias", "SEXO", "RACACOR","PESO","QTDFILVIVO","QTDFILMORT"), 
      as.numeric
    ))
}

df_2000 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2000.csv")
df_2000 = filtro(df_2000)
df_2001 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2001.csv")
df_2001 = filtro(df_2001)
df_2002 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2002.csv")
df_2002 = filtro(df_2002)
df_2003 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2003.csv")
df_2003 = filtro(df_2003)
df_2004 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2004.csv")
df_2004 = filtro(df_2004)
df_2005 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2005.csv")
df_2005 <- data.frame(lapply(df_2005, function(x) {
  if (is.character(x)) iconv(x, from = "latin1", to = "UTF-8")
  else x
}))

gc()

df_2005 = filtro(df_2005)
df_2006 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2006.csv")
df_2006 = filtro(df_2006)
df_2007 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2007.csv")
df_2007 = filtro(df_2007)
df_2008 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2008.csv")
df_2008 = filtro(df_2008)
df_2009 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2009.csv")
df_2009 = filtro(df_2009)
df_2010 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2010.csv")
df_2010 = filtro(df_2010)
df_2011 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2011.csv")
df_2011 = filtro(df_2011)
df_2012 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2012.csv")
df_2012 = filtro(df_2012)
df_2013 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2013.csv")
df_2013 = filtro(df_2013)
df_2014 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2014.csv")
df_2014 = filtro(df_2014)
df_2015 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2015.csv")
df_2015 = filtro(df_2015)
df_2016 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2016.csv")
df_2016 = filtro(df_2016)
df_2017 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2017.csv")
df_2017 = filtro(df_2017)
df_2018 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2018.csv")
df_2018 = filtro(df_2018)
df_2019 <- vroom("https://diaad.s3.sa-east-1.amazonaws.com/sim/Mortalidade_Geral_2019.csv")
df_2019 = filtro(df_2019)
df_2020 <- read.csv("Mortalidade_Geral_2020.csv",header = TRUE,sep = ";")
df_2020 = filtro(df_2020)
df_2021 <- vroom("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/Mortalidade_Geral_2021.csv")
df_2021 = filtro(df_2021)
df_2022 <- vroom("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO22OPEN.csv")
df_2022 = filtro(df_2022)
df_2023 <- vroom("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")
df_2023 = filtro(df_2023)
gc()

gc()
transformar_variaveis_numericas(df_2007)
transformar_variaveis_numericas(df_2008)
transformar_variaveis_numericas(df_2009)
transformar_variaveis_numericas(df_2010)
transformar_variaveis_numericas(df_2011)
transformar_variaveis_numericas(df_2012)
transformar_variaveis_numericas(df_2013)
transformar_variaveis_numericas(df_2014)
transformar_variaveis_numericas(df_2015)
transformar_variaveis_numericas(df_2016)
transformar_variaveis_numericas(df_2017)
transformar_variaveis_numericas(df_2018)
transformar_variaveis_numericas(df_2019)
transformar_variaveis_numericas(df_2020)
transformar_variaveis_numericas(df_2021)
transformar_variaveis_numericas(df_2022)
transformar_variaveis_numericas(df_2023)

# Lista de dataframes de 2001 a 2023
dfs <- list(df_2007, df_2008, df_2009, df_2010, df_2011, df_2012, 
            df_2013, df_2014, df_2015, df_2016, df_2017, df_2018, 
            df_2019, df_2021, df_2022, df_2023)

# Obter as colunas de df_2005
colunas_df_2005 <- colnames(df_2007)

# Função para adicionar colunas ausentes com NA e selecionar colunas de interesse
adicionar_colunas_e_selecionar <- function(df, colunas_ref, colunas_interesse) {
  # Adicionar colunas ausentes
  colunas_faltantes <- setdiff(colunas_ref, colnames(df))
  for (coluna in colunas_faltantes) {
    df[[coluna]] <- NA
  }
  
  # Selecionar apenas as colunas de interesse
  return(df[, colunas_interesse, drop = FALSE])
}

# Colunas de interesse
colunas_interesse <- c("DTNASC","CODMUNRES","IDADEMAE","ESCMAE","OCUPMAE","QTDFILVIVO","QTDFILMORT",
                       "GESTACAO","PARTO","OBITOPARTO","PESO","OBITOGRAV","OBITOPUERP",
                       "DTOBITO", "ESC", 
                       "CAUSABAS","LINHAA","idade_dias", 
                       "SEXO", "RACACOR")

# Aplicar a função a todos os dataframes
dfs_ajustados <- lapply(dfs, adicionar_colunas_e_selecionar, 
                        colunas_ref = colunas_df_2005, 
                        colunas_interesse = colunas_interesse)

# Combinar todos os dataframes ajustados
df_final <- do.call(rbind, dfs_ajustados)


selected_columns <- intersect(names(df_2020), names(df_final))


df_final$QTDFILVIVO = as.numeric(df_final$QTDFILVIVO)
df_final$QTDFILMORT = as.numeric(df_final$QTDFILMORT)
df_final$PESO = as.numeric(df_final$PESO)

# Juntando os dois dataframes
df_final <- df_final %>%
  select(all_of(selected_columns)) %>%
  bind_rows(df_2020)

# Adicionar um contador a partir de 1 até o número total de linhas
df_final$CONTADOR <- seq(1, nrow(df_final))
df_final$year = year(df_final$DTOBITO)
# Reorganizar as colunas com uma lógica mais clara
# Colocando o contador primeiro, seguido das datas, dados demográficos e outras variáveis
colunas_reordenadas <- c(
  "CONTADOR",        # Contador
  "DTNASC", "DTOBITO",  # Datas (primeiras)
  
  # Dados demográficos
  "IDADEMAE", "ESCMAE", "OCUPMAE", "SEXO", "RACACOR", 
  
  # Variáveis relacionadas à gestação e parto
  "GESTACAO", "PARTO", "OBITOPARTO", "OBITOGRAV", "OBITOPUERP",
  
  # Outras variáveis
  "CODMUNRES", "CAUSABAS", "LINHAA", "idade_dias", 
  "QTDFILVIVO", "QTDFILMORT", "PESO", "ESC"
)


# Reorganizar o banco de dados com as colunas na nova ordem
df <- df_final[, colunas_reordenadas]

write.csv(df,file = "infant_mort.csv")



save(infant_mort,file = "im.RData")









