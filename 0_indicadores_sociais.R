#IDH e GINI

library(readxl)
library(writexl)
library(tidyverse)

setwd("C:/Users/MARCOS.ANTUNES/Downloads/Mortalidade INFANTIL")

idh = read_excel("idh.xlsx") #https://www.undp.org/pt/brazil/idhm-municipios-2010
gini = read.csv2("ginibr.csv") #http://tabnet.datasus.gov.br/cgi/ibge/censo/cnv/ginibr.def

idh$code_muni <- as.numeric(substr(idh$code_muni, 1, nchar(idh$code_muni) - 1))
social <- left_join(idh, gini, by = "code_muni")

save(social,file = "indicadores.RData")
