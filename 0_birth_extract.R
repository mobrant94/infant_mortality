#############Data treatment

pacman::p_load(tidyverse,janitor,vroom)

remover_colunas_extra <- function(df_base, df) {
  # Identificar as colunas do df_base (df_2001)
  colunas_base <- names(df_base)
  
  # Remover colunas que não estão no df_base
  colunas_para_remover <- setdiff(names(df), colunas_base)
  df_corrigido <- df[, !(names(df) %in% colunas_para_remover)]
  
  return(df_corrigido)
}

df_2000 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2000.csv")
df_2000$UFINFORM = NULL
df_2001 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2001.csv")
df_2001 = remover_colunas_extra(df_2000, df_2001)
df_2002 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2002.csv")
df_2002 = remover_colunas_extra(df_2000, df_2002)
df_2003 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2003.csv")
df_2003 = remover_colunas_extra(df_2000, df_2003)
df_2004 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2004.csv")
df_2004 = remover_colunas_extra(df_2000, df_2004)
df_2005 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2005.csv")
df_2005 = remover_colunas_extra(df_2000, df_2005)
df_0105 = rbind(df_2000, df_2001, df_2002, df_2003, df_2004, df_2005)
save(df_0105,file = "df_0105.RData")
rm(df_2001, df_2002,df_2003,df_2004,df_2005,df_0105)
gc()
df_2006 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2006.csv")
df_2006 = remover_colunas_extra(df_2000, df_2006)
df_2007 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2007.csv")
df_2007 = remover_colunas_extra(df_2000, df_2007)
df_2008 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2008.csv")
df_2008 = remover_colunas_extra(df_2000, df_2008)
df_2009 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2009.csv")
df_2009 = remover_colunas_extra(df_2000, df_2009)
df_2010 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2010.csv")
df_2010 = remover_colunas_extra(df_2000, df_2010)
df_0610 = rbind(df_2006, df_2007, df_2008, df_2009, df_2010)
save(df_0610,file = "df_0610.RData")
rm(df_2006,df_2007,df_2008,df_2009,df_2010,df_0610)
gc()
df_2011 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2011.csv")
df_2011 = remover_colunas_extra(df_2000, df_2011)
df_2012 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2012.csv")
df_2012 = remover_colunas_extra(df_2000, df_2012)
df_2013 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2013.csv")
df_2013 = remover_colunas_extra(df_2000, df_2013)
df_2014 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2014.csv")
df_2014 = remover_colunas_extra(df_2000, df_2014)
df_2015 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2015.csv")
df_2015 = remover_colunas_extra(df_2000, df_2015)
df_2016 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2016.csv")
df_2016 = remover_colunas_extra(df_2000, df_2016)
df_1116 = rbind(df_2011, df_2012, df_2013, df_2014, df_2015, df_2016)
save(df_1116,file = "df_1116.RData")
rm(df_2011,df_2012,df_2013,df_2014,df_2015,df_2016,df_1116)
gc()
df_2017 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2017.csv")
df_2017 = remover_colunas_extra(df_2000, df_2017)
df_2018 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2018.csv")
df_2018 = remover_colunas_extra(df_2000, df_2018)
df_2019 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2019.csv")
df_2019 = remover_colunas_extra(df_2000, df_2019)
df_2020 = vroom("https://diaad.s3.sa-east-1.amazonaws.com/sinasc/SINASC_2020.csv")
df_2020 = remover_colunas_extra(df_2000, df_2020)
df_2021 = vroom("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/SINASC_2021.csv")
df_2021 = remover_colunas_extra(df_2000, df_2021)
df_2022 = vroom("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN22.csv")
df_2022 = remover_colunas_extra(df_2000, df_2022)
df_2023 = vroom("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SINASC/DNOPEN23.csv")
df_2023$CONTADOR = c(1:2532053)
df_2023 = remover_colunas_extra(df_2000, df_2023)
df_1723 = rbind(df_2017, df_2018, df_2019, df_2020, df_2021, df_2022, df_2023)
save(df_1723,file = "df_1723.RData")
rm(df_2000,df_2017,df_2018,df_2020,df_2021,df_2022,df_2023,df_1723)
gc()
