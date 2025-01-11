#####load de databases and final treatment of infant_mort
pacman::p_load(readxl,tidyverse,janitor)

setwd("C:/Users/MARCOS.ANTUNES/Downloads/Mortalidade INFANTIL/Tese")

snis = read.csv2("snis.csv")
load("dfbf.RData")
load("im.RData")
infant_mort = df
infant_mort = infant_mort %>% filter(year<=2022)
load("nascimentos_summary.RData")
df <- df %>% filter(year(data) != 2023)
load("pop.RData")
load("indicadores.RData")

snis = snis %>%
  arrange(id_municipio, ano)
snis$id_municipio <- as.numeric(substr(snis$id_municipio, 1, nchar(snis$id_municipio) - 1))
snis = snis %>% rename("code_muni"="id_municipio")

pop_expanded <- pop_expanded %>%
  mutate(code_muni = substr(municipio_codigo, 1, nchar(municipio_codigo) - 1)) %>%
  select(-municipio_codigo)
dfbf = dfbf %>%  rename(code_muni = ibge)
df = df %>% rename(code_muni = code_mune)

infant_mort = clean_names(infant_mort)
infant_mort = infant_mort %>% rename(code_muni = codmunres)

# Adicionando a coluna extrato ao data frame infant_mort
infant_mort$extrato <- cut(infant_mort$idade_dias,
                           breaks = c(-Inf, 7, 28, 365, Inf),
                           labels = c("Neonatal Precoce",
                                      "Neonatal Tardia",
                                      "Pós-Neonatal",
                                      "Entre 1 e 5 anos"),
                           right = TRUE)


infant_mort = infant_mort %>%
  filter(is.na(obitograv) | obitograv != 1)

infant_mort$code_muni <- ifelse(nchar(as.character(infant_mort$code_muni)) == 7,
                                as.numeric(substr(infant_mort$code_muni, 1, 6)),
                                infant_mort$code_muni)
#Creating a vector with state codes
# Mapeamento dos códigos de estados e regiões
uf_codes <- c("11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", 
              "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE", 
              "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE", 
              "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP", 
              "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT", 
              "52" = "GO", "53" = "DF")

# Mapeamento de estados para regiões
region_codes <- c("RO" = "Norte", "AC" = "Norte", "AM" = "Norte", "RR" = "Norte", "PA" = "Norte", 
                  "AP" = "Norte", "TO" = "Norte", "MA" = "Nordeste", "PI" = "Nordeste", "CE" = "Nordeste", 
                  "RN" = "Nordeste", "PB" = "Nordeste", "PE" = "Nordeste", "AL" = "Nordeste", 
                  "SE" = "Nordeste", "BA" = "Nordeste", "MG" = "Sudeste", "ES" = "Sudeste", 
                  "RJ" = "Sudeste", "SP" = "Sudeste", "PR" = "Sul", "SC" = "Sul", "RS" = "Sul", 
                  "MS" = "Centro-Oeste", "MT" = "Centro-Oeste", "GO" = "Centro-Oeste", "DF" = "Centro-Oeste")

# Aplicando a transformação
infant_mort <- infant_mort %>%
  mutate(
    natural_cond = ifelse(substr(code_muni, 1, 2) %in% names(uf_codes),
                          uf_codes[substr(code_muni, 1, 2)],
                          'Foreign'),
    region_cond = ifelse(natural_cond != 'Foreign', region_codes[natural_cond], 'Desconhecido')
  )

# To factor
infant_mort$sexo <- factor(infant_mort$sexo, levels = c(0,1,2,9), 
                            labels = c("Ignorado", 
                                       "Masculino",
                                       "Feminino",
                                       "Ignorado")) #corrigido sexo


infant_mort$racacor <- factor(infant_mort$racacor, levels = c(1,2,3,4,5), 
                               labels = c("Branca", 
                                          "Preta",
                                          "Amarela",
                                          "Parda",
                                          "Indigena"))


infant_mort$racacor <- fct_explicit_na(infant_mort$racacor, na_level = "Ignorado")


infant_mort$escmae <- factor(infant_mort$escmae, levels = c(0,1,2,3,4,5,9), 
                              labels = c("Ignorado", 
                                         "Nenhuma",
                                         "1 a 3 anos",
                                         "4 a 7 anos",
                                         "8 a 11 anos",
                                         "12 anos ou mais",
                                         "Ignorado"))

infant_mort$escmae <- fct_explicit_na(infant_mort$escmae, na_level = "Ignorado")

# Função para categorizar os códigos de lixo e nível de gravidade
categorize_garbage_code <- function(causabas) {
  
  # Definir Garbage Codes para cada nível de gravidade
  garbage_codes <- list(
    level_1 = c( "A40", "A419", "A480", "A483", "A490", "A491", "A59", "A599", "A71", "A719", 
                 "A740", "B07", "B079", "B30", "B309", "B35", "B369", "B85", "B854", "B87", 
                 "B889", "B940", "D50", "D500", "D509", "D62", "D630", "D638", "D64", "D641", 
                 "D659", "D68", "D699", "E15", "E16", "E50", "E509", "E641", "E853", "E876", 
                 "E879", "F062", "F064", "F072", "F09", "F099", "F19", "F239", "F25", 
                 "F49", "F51", "F990", "G06", "G080", "G32", "G328", "G43", "G442", 
                 "G444", "G448", "G47", "G472", "G474", "G479", "G50", "G609", "G62", 
                 "G620", "G622", "G652", "G80", "G839", "G89", "G894", "G91", "G912", 
                 "G914", "G93", "G931", "G932", "G934", "G936", "G940", "G948", "G99", 
                 "G998", "H00", "H05", "H052", "H699", "H71", "H99", "I26", "I269", 
                 "I312", "I314", "I46", "I469", "I50", "I504", "I76", "I95", "I951", "I958", 
                 "I959", "J69", "J699", "J80", "J809", "J81", "J811", "J90", "J900", 
                 "J94", "J941", "J948", "J949", "K00", "K19", "K30", "K65", "K661", "K669", 
                 "K681", "K689", "K71", "K716", "K718", "K729", "K75", "K750", "L20", 
                 "L30", "L309", "L40", "L509", "L52", "L548", "L56", "L562", "L564", 
                 "L565", "L57", "L579", "L59", "L689", "L70", "L768", "L80", "L879", 
                 "L90", "L929", "L94", "L96", "L985", "L998", "M04", "M10", "M120", 
                 "M122", "M29", "M37", "M39", "M432", "M49", "M492", "M64", "M651", 
                 "M71", "M712", "M724", "M728", "M73", "M738", "M799", "M83", "M862", 
                 "M865", "M869", "M872", "M879", "M891", "M894", "M90", "M999", "N17", 
                 "N179", "N19", "N199", "N321", "N322", "N328", "N338", "N35", "N359", 
                 "N37", "N378", "N393", "N398", "N42", "N434", "N441", "N448", "N46", 
                 "N489", "N50", "N539", "N61", "N649", "N82", "N829", "N91", "N915", 
                 "N95", "N951", "N959", "N97", "N979", "R02", "R029", "R031", "R070", 
                 "R08", "R09", "R093", "R110", "R140", "R196", "R198", "R23", "R231", 
                 "R309", "R320", "R501", "R508", "R579", "R580", "R729", "R748", "R786", 
                 "R948", "R969", "R999", "U05", "U07", "U81", "U899", "U99", "X40", 
                 "X449", "X469", "X499", "Y10", "Y149", "Y169", "Y199", "Z00", "Z158", 
                 "Z17"),
    level_2 = c("A149", "A29", "A309", "A45", "A459", "A47", "A48", "A488", "A49", "A493", 
                "A499", "A61", "A62", "A72", "A73", "A76", "A97", "B08", "B09", "B11", 
                "B14", "B28", "B29", "B31", "B324", "B34", "B349", "B61", "B62", "B68", 
                "B689", "B73", "B742", "B76", "B769", "B78", "B818", "B84", "B92", "B94", 
                "B948", "B949", "B956", "B973", "B977", "B999", "D59", "D594", "D598", 
                "D599", "F17", "F179", "G443", "G913", "G930", "G933", "I10", "I109", 
                "I15", "I159", "I27", "I278", "I279", "I50", "I508", "I509", "I674", 
                "I70", "I701", "I709", "I74", "I758", "J81", "J811", "J90", "J900", 
                "J94", "J941", "J948", "J949", "K920", "K922", "N70", "N719", "N73", 
                "N740", "N742", "N748", "R03", "R030", "R04", "R069", "R090", "R092", 
                "R098", "R109", "R13", "R139", "R230", "R58", "S00", "T983", "W47", 
                "W48", "W63", "W71", "W72", "W769", "W82", "W95", "W97", "W98", "X07", 
                "X55", "X56", "X59", "X599", "Y20", "Y349", "Y86", "Y87", "Y872", 
                "Y89", "Y899", "Y999"),
    level_3 = c("A01", "A31", "A319", "A42", "A449", "A492", "A64", "A640", "A99", "A990",
                "B17", "B171", "B178", "B179", "B19", "B190", "B192", "B199", "B37", "B46", 
                "B469", "B49", "B499", "B55", "B551", "B559", "B58", "B599", "B89", "B942",
                "C14", "C149", "C229", "C26", "C29", "C35", "C36", "C39", "C399", "C42", 
                "C46", "C469", "C55", "C559", "C579", "C59", "C639", "C68", "C689", "C74", 
                "C749", "C759", "C809", "C87", "C97", "D000", "D01", "D014", "D02", "D024", 
                "D029", "D07", "D073", "D076", "D09", "D091", "D097", "D099", "D10", "D109", 
                "D13", "D139", "D14", "D144", "D17", "D219", "D28", "D289", "D29", "D299", 
                "D30", "D309", "D36", "D369", "D370", "D376", "D38", "D386", "D39", "D397", 
                "D399", "D40", "D409", "D41", "D419", "D44", "D449", "D48", "D487", "D491", 
                "D495", "D497", "D499", "D54", "D759", "D79", "D85", "D87", "D88", "D899", 
                "D99", "E078", "E089", "E17", "E19", "E34", "E349", "E358", "E37", "E39", 
                "E47", "E49", "E62", "E69", "E877", "E90", "E998", "F04", "F061", "F065", 
                "F070", "F078", "F08", "F50", "F508", "F509", "G09", "G099", "G15", "G19", 
                "G21", "G212", "G214", "G220", "G27", "G29", "G33", "G34", "G38", "G39", 
                "G42", "G48", "G49", "G66", "G69", "G74", "G79", "G84", "G88", "G938", 
                "G94", "G969", "G989", "G998", "I000", "I03", "I04", "I14", "I14", "I16", 
                "I19", "I29", "I299", "I44", "I459", "I49", "I499", "I51", "I516", "I59", 
                "I90", "I94", "I969", "I984", "I988", "I99", "ID59", "J029", "J039", 
                "J043", "J06", "J069", "J40", "J409", "J47", "J59", "J71", "J79", "J819", 
                "J83", "J859", "J87", "J89", "J909", "J936", "J970", "J980", "J998", 
                "K21", "K219", "K227", "K319", "K34", "K39", "K47", "K49", "K53", "K54", 
                "K63", "K634", "K638", "K639", "K69", "K704", "K709", "K78", "K79", 
                "K84", "K87", "K89", "K92", "K929", "K93", "K96", "K99", "L06", "L07", 
                "L09", "L15", "L19", "L31", "L39", "L69", "L77", "L79", "N09", "N135", 
                "N137", "N139", "N24", "N288", "N289", "N38", "N399", "N409", "N54", 
                "N59", "N66", "N69", "N78", "N79", "N84", "N842", "N86", "N909", "N929", 
                "N949", "N950", "O08", "O089", "O17", "O19", "O27", "O37", "O39", "O49", 
                "O59", "O78", "O79", "O93", "O959", "P06", "P16", "P18", "P30", "P342", 
                "P40", "P49", "P62", "P69", "P73", "P79", "P82", "P85", "P89", "P969", 
                "P999", "Q08", "Q103", "Q19", "Q29", "Q36", "Q369", "Q46", "Q49", "Q88", 
                "Q899", "Q94", "Q999", "R00", "R012", "R07", "R071", "R079", "R31", "R319"),
    level_4 = c( "B169", "B64", "B82", "B829", "B839", "C69", "C699", "C911", "C914", "C915", 
                 "C917", "C919", "C927", "C929", "C932", "C935", "C937", "C939", 
                 "E12", "E149", "G00", "G009", "G028", "G039", "I379", "I42", "I420", 
                 "I429", "I515", "I64", "I649", "I67", 
                 "I678", "I68", "I688", "I69", "I694", "I699", 
                 "J07", "J08", "J159", "J176", "J196", "J22", "J29", "J64", 
                 "J649", "P23", "P235", "P239", "P373", "P374", 
                 "R73", "R739", 
                 "V87", "V871", "V874", "V881", "V884", "V899", "V990", 
                 "X84", "X849", "Y09", "Y099", "Y85", "Y859"))
  
  # Verificar qual nível de Garbage Code o valor da causabas pertence
  if (causabas %in% garbage_codes$level_1) {
    return(c(1, "Level 1"))
  } else if (causabas %in% garbage_codes$level_2) {
    return(c(2, "Level 2"))
  } else if (causabas %in% garbage_codes$level_3) {
    return(c(3, "Level 3"))
  } else if (causabas %in% garbage_codes$level_4) {
    return(c(4, "Level 4"))
  } else {
    return(c(NA, "Not a Garbage Code"))
  }
}

# Aplicar a função para gerar as duas novas variáveis no banco de dados
infant_mort <- infant_mort %>%
  mutate(
    garbage_code = sapply(causabas, function(x) categorize_garbage_code(x)[1]), # Códigos de lixo
    garbage_severity = sapply(causabas, function(x) categorize_garbage_code(x)[2]) # Nível de gravidade
  )

infant_mort = infant_mort %>% filter(!(code_muni=="5"))





# Mapeamento de estados para regiões
region_codes <- c("RO" = "Norte", "AC" = "Norte", "AM" = "Norte", "RR" = "Norte", "PA" = "Norte", 
                  "AP" = "Norte", "TO" = "Norte", "MA" = "Nordeste", "PI" = "Nordeste", "CE" = "Nordeste", 
                  "RN" = "Nordeste", "PB" = "Nordeste", "PE" = "Nordeste", "AL" = "Nordeste", 
                  "SE" = "Nordeste", "BA" = "Nordeste", "MG" = "Sudeste", "ES" = "Sudeste", 
                  "RJ" = "Sudeste", "SP" = "Sudeste", "PR" = "Sul", "SC" = "Sul", "RS" = "Sul", 
                  "MS" = "Centro-Oeste", "MT" = "Centro-Oeste", "GO" = "Centro-Oeste", "DF" = "Centro-Oeste")

# Aplicando a transformação
df <- df %>%
  mutate(
    natural_cond = ifelse(substr(code_muni, 1, 2) %in% names(uf_codes),
                          uf_codes[substr(code_muni, 1, 2)],
                          'Foreign'),
    region_cond = ifelse(natural_cond != 'Foreign', region_codes[natural_cond], 'Desconhecido')
  )

# Calcular a soma dos nascimentos por data
brasil_summary <- df %>%
  group_by(data) %>%
  summarize(nascimentos = sum(nascimentos, na.rm = TRUE)) %>%
  mutate(
    ano = year(data),
    code_muni = "Brasil",
    nome = "Brasil",
    natural_cond = "Brasil",
    region_cond = "Brasil"
  )

# Adicionar a categoria "Brasil" diretamente ao dataframe original
df <- bind_rows(df, brasil_summary)

# Exibir os primeiros registros do dataframe atualizado
head(df)

snis$cobertura_agua = snis$populacao_urbana_atendida_agua/snis$populacao_urbana*100
snis$cobertura_esgoto = snis$populacao_urbana_atendida_esgoto/snis$populacao_urbana*100




# Mapeamento de estados para regiões
region_codes <- c("RO" = "Norte", "AC" = "Norte", "AM" = "Norte", "RR" = "Norte", "PA" = "Norte", 
                  "AP" = "Norte", "TO" = "Norte", "MA" = "Nordeste", "PI" = "Nordeste", "CE" = "Nordeste", 
                  "RN" = "Nordeste", "PB" = "Nordeste", "PE" = "Nordeste", "AL" = "Nordeste", 
                  "SE" = "Nordeste", "BA" = "Nordeste", "MG" = "Sudeste", "ES" = "Sudeste", 
                  "RJ" = "Sudeste", "SP" = "Sudeste", "PR" = "Sul", "SC" = "Sul", "RS" = "Sul", 
                  "MS" = "Centro-Oeste", "MT" = "Centro-Oeste", "GO" = "Centro-Oeste", "DF" = "Centro-Oeste")

# Aplicando a transformação
snis <- snis %>%
  mutate(
    natural_cond = ifelse(substr(code_muni, 1, 2) %in% names(uf_codes),
                          uf_codes[substr(code_muni, 1, 2)],
                          'Foreign'),
    region_cond = ifelse(natural_cond != 'Foreign', region_codes[natural_cond], 'Desconhecido')
  )


# Aplicando a transformação
dfbf <- dfbf %>%
  mutate(
    natural_cond = ifelse(substr(code_muni, 1, 2) %in% names(uf_codes),
                          uf_codes[substr(code_muni, 1, 2)],
                          'Foreign'),
    region_cond = ifelse(natural_cond != 'Foreign', region_codes[natural_cond], 'Desconhecido')
  )


snis = snis %>% filter(!(ano<2000))
snis$code_muni = as.numeric(snis$code_muni)
snis$ano = as.numeric(snis$ano)
