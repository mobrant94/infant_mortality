#####load de databases and final treatment of infant_mort
pacman::p_load(readxl,tidyverse,janitor, geobr)

setwd("C:/Users/Usuario/Downloads/Tese-20250310T183802Z-001/Tese")



snis = read.csv2("snis.csv")
load("dfbf.RData")
load("im.RData")
infant_mort = df
infant_mort = infant_mort %>% filter(year<=2022)
load("nascimentos_summary.RData")
df <- df %>% filter(year(data) != 2023)
load("pop.RData")
load("indicadores.RData")
load("vacinas.RData")
load("cnes.RData")
cnes = df_final
rm(df_final)

snis = snis %>%
  arrange(id_municipio, ano)
snis$id_municipio <- as.numeric(substr(snis$id_municipio, 1, nchar(snis$id_municipio) - 1))
snis = snis %>% rename("code_muni"="id_municipio")

pop_expanded <- pop_expanded %>%
  mutate(code_muni = substr(municipio_codigo, 1, nchar(municipio_codigo) - 1)) %>%
  select(-municipio_codigo)

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

# Exemplo para incorporação de uma lista maior de causas evitáveis

causas_evitaveis <- c(
  # Causas evitáveis - Reduzíveis pelas ações de imunização
  "A17" = "Tuberculose do sistema nervoso",
  "A19" = "Tuberculose miliar",
  "A33" = "Tétano neonatal",
  "A35" = "Tétano",
  "A36" = "Difteria",
  "A37" = "Coqueluche",
  "A80" = "Poliomielite aguda",
  "B05" = "Sarampo",
  "B06" = "Rubéola",
  "B16" = "Hepatite aguda B",
  "B26" = "Caxumba",
  "G000" = "Meningite por Haemophilus",
  "P350" = "Síndrome da rubéola congênita",
  "P353" = "Hepatite viral congênita",
  
  # Reduzíveis por adequada atenção à mulher na gestação
  "A50" = "Sífilis congênita",
  "B20" = "Doenças pelo vírus da imunodeficiência humana [HIV]",
  "B21" = "Doenças pelo vírus da imunodeficiência humana [HIV]",
  "B22" = "Doenças pelo vírus da imunodeficiência humana [HIV]",
  "B23" = "Doenças pelo vírus da imunodeficiência humana [HIV]",
  "B24" = "Doenças pelo vírus da imunodeficiência humana [HIV]",
  "P022" = "Algumas situações de feto e recém-nascido afetados por complicações da placenta e das membranas",
  "P023" = "Algumas situações de feto e recém-nascido afetados por complicações da placenta e das membranas",
  "P027" = "Algumas situações de feto e recém-nascido afetados por complicações da placenta e das membranas",
  "P028" = "Algumas situações de feto e recém-nascido afetados por complicações da placenta e das membranas",
  "P029" = "Algumas situações de feto e recém-nascido afetados por complicações da placenta e das membranas",
  "P00" = "Feto e recém-nascido afetados por afecções maternas, não obrigatoriamente relacionadas com a gravidez atual, e por influências nocivas transmitidas ao feto via placenta ou leite materno",
  "P04" = "Feto e recém-nascido afetados por afecções maternas, não obrigatoriamente relacionadas com a gravidez atual, e por influências nocivas transmitidas ao feto via placenta ou leite materno",
  "P01" = "Feto e recém-nascido afetados por complicações maternas da gravidez",
  "P05" = "Crescimento fetal retardado e desnutrição fetal",
  "P07" = "Transtornos relacionados com a gestação de curta duração e peso baixo ao nascer, não classificados em outra parte",
  "P220" = "Síndrome da angústia respiratória do recém-nascido",
  "P26" = "Hemorragia pulmonar originada no período perinatal",
  "P52" = "Hemorragia intracraniana não traumática do feto e do recém-nascido",
  "P550" = "Isoimunização Rh ou ABO do feto e do recém-nascido",
  "P551" = "Isoimunização Rh ou ABO do feto e do recém-nascido",
  "P558" = "Outras doenças hemolíticas do feto e do recém-nascido devido a isoimunização",
  "P559" = "Outras doenças hemolíticas do feto e do recém-nascido devido a isoimunização",
  "P56" = "Outras doenças hemolíticas do feto e do recém-nascido devido a isoimunização",
  "P57" = "Outras doenças hemolíticas do feto e do recém-nascido devido a isoimunização",
  "P77" = "Enterocolite necrotizante do feto e do recém-nascido",
  
  # Reduzíveis por adequada atenção à mulher no parto
  "P020" = "Feto e recém-nascido afetados por placenta prévia e por outras formas de descolamento da placenta e hemorragia",
  "P021" = "Feto e recém-nascido afetados por placenta prévia e por outras formas de descolamento da placenta e hemorragia",
  "P024" = "Feto e recém-nascido afetados por afecções do cordão umbilical",
  "P025" = "Feto e recém-nascido afetados por afecções do cordão umbilical",
  "P026" = "Feto e recém-nascido afetados por afecções do cordão umbilical",
  "P03" = "Feto e recém-nascido afetados por outras complicações do trabalho de parto e do parto",
  "P08" = "Transtornos relacionados com a gestação prolongada e peso elevado ao nascer",
  "P10" = "Traumatismo de parto",
  "P11" = "Traumatismo de parto",
  "P12" = "Traumatismo de parto",
  "P13" = "Traumatismo de parto",
  "P14" = "Traumatismo de parto",
  "P15" = "Traumatismo de parto",
  "P20" = "Hipóxia intrauterina e asfixia ao nascer",
  "P21" = "Hipóxia intrauterina e asfixia ao nascer",
  "P240" = "Síndrome de aspiração neonatal, exceto de leite e alimento regurgitados",
  "P241" = "Síndrome de aspiração neonatal, exceto de leite e alimento regurgitados",
  "P242" = "Síndrome de aspiração neonatal, exceto de leite e alimento regurgitados",
  "P248" = "Síndrome de aspiração neonatal, exceto de leite e alimento regurgitados",
  "P249" = "Síndrome de aspiração neonatal, exceto de leite e alimento regurgitados",
  
  # Reduzíveis por adequada atenção ao recém-nascido
  "P221" = "Transtornos respiratórios específicos do período neonatal",
  "P228" = "Transtornos respiratórios específicos do período neonatal",
  "P229" = "Transtornos respiratórios específicos do período neonatal",
  "P23" = "Transtornos respiratórios específicos do período neonatal",
  "P25" = "Transtornos respiratórios específicos do período neonatal",
  "P27" = "Transtornos respiratórios específicos do período neonatal",
  "P28" = "Transtornos respiratórios específicos do período neonatal",
  "P351" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P352" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P354" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P355" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P356" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P357" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P358" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P359" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P36" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P37" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P38" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P39" = "Infecções específicas do período neonatal, exceto síndrome da rubéola congênita e hepatite viral congênita",
  "P50" = "Hemorragia neonatal, exceto intracraniana não-traumática",
  "P51" = "Hemorragia neonatal, exceto intracraniana não-traumática",
  "P53" = "Hemorragia neonatal, exceto intracraniana não-traumática",
  "P54" = "Hemorragia neonatal, exceto intracraniana não-traumática",
  "P58" = "Outras icterícias neonatais",
  "P59" = "Outras icterícias neonatais",
  "P70" = "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido",
  "P71" = "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido",
  "P72" = "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido",
  "P73" = "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido",
  "P74" = "Transtornos endócrinos e metabólicos transitórios específicos do feto e do recém-nascido",
  "P60" = "Outros transtornos hemotológicos do feto e do recém-nascido",
  "P61" = "Outros transtornos hemotológicos do feto e do recém-nascido",
  "P75" = "Transtornos do aparelho digestivo do feto ou do recémnascido, exceto enterocolite necrotizante",
  "P76" = "Transtornos do aparelho digestivo do feto ou do recémnascido, exceto enterocolite necrotizante",
  "P78" = "Transtornos do aparelho digestivo do feto ou do recémnascido, exceto enterocolite necrotizante",
  "P80" = "Afecções que comprometem o tegumento e a regulação térmica do feto e do recém-nascido",
  "P81" = "Afecções que comprometem o tegumento e a regulação térmica do feto e do recém-nascido",
  "P82" = "Afecções que comprometem o tegumento e a regulação térmica do feto e do recém-nascido",
  "P83" = "Afecções que comprometem o tegumento e a regulação térmica do feto e do recém-nascido",
  "P90" = "Outros transtornos originados no período perinatal (exceto P95 e P969)",
  "P91" = "Outros transtornos originados no período perinatal (exceto P95 e P969)",
  "P92" = "Outros transtornos originados no período perinatal (exceto P95 e P969)",
  "P93" = "Outros transtornos originados no período perinatal (exceto P95 e P969)",
  "P94" = "Outros transtornos originados no período perinatal (exceto P95 e P969)",
  "P960" = "Outros transtornos originados no período perinatal (exceto P95 e P969)",
  "P968" = "Outros transtornos originados no período perinatal (exceto P95 e P969)",
  "P969" = "Transtornos originados no período perinatal",
  
  # Reduzíveis por ações de diagnóstico e tratamento adequado
  "A15" = "Tuberculose respiratória, com confirmação bacteriológica e histológica",
  "A16" = "Tuberculose das vias respiratórias, sem confirmação bacteriológica ou histológica",
  "A18" = "Tuberculose de outros órgãos",
  "G001" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G002" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G003" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G004" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G005" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G006" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G007" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G008" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G009" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "G03" = "Meningite bacteriana, não classificada em outra parte (exceto por Haemophilus) ou devida a outras causas e a causas não especificadas",
  "J00" = "Infecções agudas das vias aéreas superiores",
  "J01" = "Infecções agudas das vias aéreas superiores",
  "J02" = "Infecções agudas das vias aéreas superiores",
  "J03" = "Infecções agudas das vias aéreas superiores",
  "J04" = "Infecções agudas das vias aéreas superiores",
  "J05" = "Infecções agudas das vias aéreas superiores",
  "J06" = "Infecções agudas das vias aéreas superiores",
  "J12" = "Pneumonia",
  "J13" = "Pneumonia",
  "J14" = "Pneumonia",
  "J15" = "Pneumonia",
  "J16" = "Pneumonia",
  "J17" = "Pneumonia",
  "J18" = "Pneumonia",
  "J20" = "Outras infecções agudas das vias aéreas inferiores",
  "J21" = "Outras infecções agudas das vias aéreas inferiores",
  "J22" = "Outras infecções agudas das vias aéreas inferiores",
  "J384" = "Edema da laringe",
  "J40" = "Doenças crônicas das vias aéreas inferiores, exceto enfisema e outras doenças pulmonares obstrutivas crônicas",
  "J41" = "Doenças crônicas das vias aéreas inferiores, exceto enfisema e outras doenças pulmonares obstrutivas crônicas",
  "J42" = "Doenças crônicas das vias aéreas inferiores, exceto enfisema e outras doenças pulmonares obstrutivas crônicas",
  "J45" = "Doenças crônicas das vias aéreas inferiores, exceto enfisema e outras doenças pulmonares obstrutivas crônicas",
  "J46" = "Doenças crônicas das vias aéreas inferiores, exceto enfisema e outras doenças pulmonares obstrutivas crônicas",
  "J47" = "Doenças crônicas das vias aéreas inferiores, exceto enfisema e outras doenças pulmonares obstrutivas crônicas",
  "J68" = "Afecções respiratórias devidas a inalação de produtos químicos, gases, fumaças e vapores e pneumonite devida a sólidos e líquidos",
  "J69" = "Afecções respiratórias devidas a inalação de produtos químicos, gases, fumaças e vapores e pneumonite devida a sólidos e líquidos",
  "A70" = "Outras doenças causadas por clamídias",
  "A71" = "Outras doenças causadas por clamídias",
  "A72" = "Outras doenças causadas por clamídias",
  "A73" = "Outras doenças causadas por clamídias",
  "A74" = "Outras doenças causadas por clamídias",
  "A30" = "Outras doenças bacterianas",
  "A31" = "Outras doenças bacterianas",
  "A32" = "Outras doenças bacterianas",
  "A38" = "Outras doenças bacterianas",
  "A39" = "Outras doenças bacterianas",
  "A40" = "Outras doenças bacterianas",
  "A41" = "Outras doenças bacterianas",
  "A46" = "Outras doenças bacterianas",
  "A49" = "Outras doenças bacterianas",
  "E030" = "Hipotireoidismo congênito",
  "E031" = "Hipotireoidismo congênito",
  "E10" = "Diabetes mellitus",
  "E11" = "Diabetes mellitus",
  "E12" = "Diabetes mellitus",
  "E13" = "Diabetes mellitus",
  "E14" = "Diabetes mellitus",
  "E700" = "Fenilcetonúria clássica",
  "E730" = "Deficiência congênita de lactase",
  "G40" = "Epilepsia e estado de mal epiléptico",
  "G41" = "Epilepsia e estado de mal epiléptico",
  "Q90" = "Síndrome de Down",
  "N390" = "Infecção do trato urinário de localização não especificada",
  "I00" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I01" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I02" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I03" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I04" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I05" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I06" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I07" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I08" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  "I09" = "Febre reumática aguda e doenças reumáticas crônicas do coração",
  
  # Reduzíveis por ações promoção à saúde vinculadas a ações de atenção
  "A00" = "Doenças infecciosas intestinais",
  "A01" = "Doenças infecciosas intestinais",
  "A02" = "Doenças infecciosas intestinais",
  "A03" = "Doenças infecciosas intestinais",
  "A04" = "Doenças infecciosas intestinais",
  "A05" = "Doenças infecciosas intestinais",
  "A06" = "Doenças infecciosas intestinais",
  "A07" = "Doenças infecciosas intestinais",
  "A08" = "Doenças infecciosas intestinais",
  "A09" = "Doenças infecciosas intestinais",
  "A20" = "Algumas doenças bacterianas zoonóticas",
  "A21" = "Algumas doenças bacterianas zoonóticas",
  "A22" = "Algumas doenças bacterianas zoonóticas",
  "A23" = "Algumas doenças bacterianas zoonóticas",
  "A24" = "Algumas doenças bacterianas zoonóticas",
  "A25" = "Algumas doenças bacterianas zoonóticas",
  "A26" = "Algumas doenças bacterianas zoonóticas",
  "A27" = "Algumas doenças bacterianas zoonóticas",
  "A28" = "Algumas doenças bacterianas zoonóticas",
  "A90" = "Febres por arbovírus e febres hemorrágicas virais",
  "A91" = "Febres por arbovírus e febres hemorrágicas virais",
  "A92" = "Febres por arbovírus e febres hemorrágicas virais",
  "A93" = "Febres por arbovírus e febres hemorrágicas virais",
  "A94" = "Febres por arbovírus e febres hemorrágicas virais",
  "A95" = "Febres por arbovírus e febres hemorrágicas virais",
  "A96" = "Febres por arbovírus e febres hemorrágicas virais",
  "A97" = "Febres por arbovírus e febres hemorrágicas virais",
  "A98" = "Febres por arbovírus e febres hemorrágicas virais",
  "A99" = "Febres por arbovírus e febres hemorrágicas virais",
  "A75" = "Rickettsioses",
  "A76" = "Rickettsioses",
  "A77" = "Rickettsioses",
  "A78" = "Rickettsioses",
  "A79" = "Rickettsioses",
  "A82" = "Raiva",
  "B50" = "Doenças devidas a protozoários",
  "B51" = "Doenças devidas a protozoários",
  "B52" = "Doenças devidas a protozoários",
  "B53" = "Doenças devidas a protozoários",
  "B54" = "Doenças devidas a protozoários",
  "B55" = "Doenças devidas a protozoários",
  "B56" = "Doenças devidas a protozoários",
  "B57" = "Doenças devidas a protozoários",
  "B58" = "Doenças devidas a protozoários",
  "B59" = "Doenças devidas a protozoários",
  "B60" = "Doenças devidas a protozoários",
  "B61" = "Doenças devidas a protozoários",
  "B62" = "Doenças devidas a protozoários",
  "B63" = "Doenças devidas a protozoários",
  "B64" = "Doenças devidas a protozoários",
  "B65" = "Helmintíases", 
  "B66" = "Helmintíases", 
  "B67" = "Helmintíases", 
  "B68" = "Helmintíases",
  "B69" = "Helmintíases", 
  "B70" = "Helmintíases", 
  "B71" = "Helmintíases", 
  "B72" = "Helmintíases", 
  "B73" = "Helmintíases", 
  "B74" = "Helmintíases", 
  "B75" = "Helmintíases", 
  "B76" = "Helmintíases", 
  "B77" = "Helmintíases", 
  "B78" = "Helmintíases", 
  "B79" = "Helmintíases", 
  "B80" = "Helmintíases", 
  "B81" = "Helmintíases", 
  "B82" = "Helmintíases", 
  "B83" = "Helmintíases",
  "B99" = "Doenças infecciosas, outras e as não especificadas",
  "D50" = "Anemias nutricionais",
  "D51" = "Anemias nutricionais",
  "D52" = "Anemias nutricionais",
  "D53" = "Anemias nutricionais",
  "E40" = "Desnutrição e outras deficiências nutricionais",
  "E41" = "Desnutrição e outras deficiências nutricionais",
  "E42" = "Desnutrição e outras deficiências nutricionais",
  "E43" = "Desnutrição e outras deficiências nutricionais",
  "E44" = "Desnutrição e outras deficiências nutricionais",
  "E45" = "Desnutrição e outras deficiências nutricionais",
  "E46" = "Desnutrição e outras deficiências nutricionais",
  "E50" = "Desnutrição e outras deficiências nutricionais",
  "E51" = "Desnutrição e outras deficiências nutricionais",
  "E52" = "Desnutrição e outras deficiências nutricionais",
  "E53" = "Desnutrição e outras deficiências nutricionais",
  "E54" = "Desnutrição e outras deficiências nutricionais",
  "E55" = "Desnutrição e outras deficiências nutricionais",
  "E56" = "Desnutrição e outras deficiências nutricionais",
  "E57" = "Desnutrição e outras deficiências nutricionais",
  "E58" = "Desnutrição e outras deficiências nutricionais",
  "E59" = "Desnutrição e outras deficiências nutricionais",
  "E60" = "Desnutrição e outras deficiências nutricionais",
  "E61" = "Desnutrição e outras deficiências nutricionais",
  "E62" = "Desnutrição e outras deficiências nutricionais",
  "E63" = "Desnutrição e outras deficiências nutricionais",
  "E64" = "Desnutrição e outras deficiências nutricionais",
  "E86" = "Depleção de volume",
  "V01" = "Acidentes de transporte",
  "V02" = "Acidentes de transporte",
  "V03" = "Acidentes de transporte",
  "V04" = "Acidentes de transporte",
  "V05" = "Acidentes de transporte",
  "V06" = "Acidentes de transporte",
  "V07" = "Acidentes de transporte",
  "V08" = "Acidentes de transporte",
  "V09" = "Acidentes de transporte",
  "V10" = "Acidentes de transporte",
  "V11" = "Acidentes de transporte",
  "V12" = "Acidentes de transporte",
  "V13" = "Acidentes de transporte",
  "V14" = "Acidentes de transporte",
  "V15" = "Acidentes de transporte",
  "V16" = "Acidentes de transporte",
  "V17" = "Acidentes de transporte",
  "V18" = "Acidentes de transporte",
  "V19" = "Acidentes de transporte",
  "V20" = "Acidentes de transporte",
  "V21" = "Acidentes de transporte",
  "V22" = "Acidentes de transporte",
  "V23" = "Acidentes de transporte",
  "V24" = "Acidentes de transporte",
  "V25" = "Acidentes de transporte",
  "V26" = "Acidentes de transporte",
  "V27" = "Acidentes de transporte",
  "V28" = "Acidentes de transporte",
  "V29" = "Acidentes de transporte",
  "V30" = "Acidentes de transporte",
  "V31" = "Acidentes de transporte",
  "V32" = "Acidentes de transporte",
  "V33" = "Acidentes de transporte",
  "V34" = "Acidentes de transporte",
  "V35" = "Acidentes de transporte",
  "V36" = "Acidentes de transporte",
  "V37" = "Acidentes de transporte",
  "V38" = "Acidentes de transporte",
  "V39" = "Acidentes de transporte",
  "V40" = "Acidentes de transporte",
  "V41" = "Acidentes de transporte",
  "V42" = "Acidentes de transporte",
  "V43" = "Acidentes de transporte",
  "V44" = "Acidentes de transporte",
  "V45" = "Acidentes de transporte",
  "V46" = "Acidentes de transporte",
  "V47" = "Acidentes de transporte",
  "V48" = "Acidentes de transporte",
  "V49" = "Acidentes de transporte",
  "V50" = "Acidentes de transporte",
  "V51" = "Acidentes de transporte",
  "V52" = "Acidentes de transporte",
  "V53" = "Acidentes de transporte",
  "V54" = "Acidentes de transporte",
  "V55" = "Acidentes de transporte",
  "V56" = "Acidentes de transporte",
  "V57" = "Acidentes de transporte",
  "V58" = "Acidentes de transporte",
  "V59" = "Acidentes de transporte",
  "V60" = "Acidentes de transporte",
  "V61" = "Acidentes de transporte",
  "V62" = "Acidentes de transporte",
  "V63" = "Acidentes de transporte",
  "V64" = "Acidentes de transporte",
  "V65" = "Acidentes de transporte",
  "V66" = "Acidentes de transporte",
  "V67" = "Acidentes de transporte",
  "V68" = "Acidentes de transporte",
  "V69" = "Acidentes de transporte",
  "V70" = "Acidentes de transporte",
  "V71" = "Acidentes de transporte",
  "V72" = "Acidentes de transporte",
  "V73" = "Acidentes de transporte",
  "V74" = "Acidentes de transporte",
  "V75" = "Acidentes de transporte",
  "V76" = "Acidentes de transporte",
  "V77" = "Acidentes de transporte",
  "V78" = "Acidentes de transporte",
  "V79" = "Acidentes de transporte",
  "V80" = "Acidentes de transporte",
  "V81" = "Acidentes de transporte",
  "V82" = "Acidentes de transporte",
  "V83" = "Acidentes de transporte",
  "V84" = "Acidentes de transporte",
  "V85" = "Acidentes de transporte",
  "V86" = "Acidentes de transporte",
  "V87" = "Acidentes de transporte",
  "V88" = "Acidentes de transporte",
  "V89" = "Acidentes de transporte",
  "V90" = "Acidentes de transporte",
  "V91" = "Acidentes de transporte",
  "V92" = "Acidentes de transporte",
  "V93" = "Acidentes de transporte",
  "V94" = "Acidentes de transporte",
  "V95" = "Acidentes de transporte",
  "V96" = "Acidentes de transporte",
  "V97" = "Acidentes de transporte",
  "V98" = "Acidentes de transporte",
  "V99" = "Acidentes de transporte",
  "X40" = "Envenenamento [intoxicação] acidental por exposição a drogas, medicamentos e substâncias biológicas",
  "X41" = "Envenenamento [intoxicação] acidental por exposição a drogas, medicamentos e substâncias biológicas",
  "X42" = "Envenenamento [intoxicação] acidental por exposição a drogas, medicamentos e substâncias biológicas",
  "X43" = "Envenenamento [intoxicação] acidental por exposição a drogas, medicamentos e substâncias biológicas",
  "X44" = "Envenenamento [intoxicação] acidental por exposição a drogas, medicamentos e substâncias biológicas",
  "X45" = "Envenenamento [intoxicação] acidental por exposição a outras substâncias nocivas",
  "X46" = "Envenenamento [intoxicação] acidental por exposição a outras substâncias nocivas",
  "X47" = "Envenenamento [intoxicação] acidental por exposição a outras substâncias nocivas",
  "X48" = "Envenenamento [intoxicação] acidental por exposição a outras substâncias nocivas",
  "X49" = "Envenenamento [intoxicação] acidental por exposição a outras substâncias nocivas",
  "R95" = "Síndrome da morte súbita na infância",
  "W00" = "Quedas",
  "W01" = "Quedas",
  "W02" = "Quedas",
  "W03" = "Quedas",
  "W04" = "Quedas",
  "W05" = "Quedas",
  "W06" = "Quedas",
  "W07" = "Quedas",
  "W08" = "Quedas",
  "W09" = "Quedas",
  "W10" = "Quedas",
  "W11" = "Quedas",
  "W12" = "Quedas",
  "W13" = "Quedas",
  "W14" = "Quedas",
  "W15" = "Quedas",
  "W16" = "Quedas",
  "W17" = "Quedas",
  "W18" = "Quedas",
  "W19" = "Quedas",
  "X00" = "Exposição ao fumo, ao fogo e às chamas",
  "X01" = "Exposição ao fumo, ao fogo e às chamas",
  "X02" = "Exposição ao fumo, ao fogo e às chamas",
  "X03" = "Exposição ao fumo, ao fogo e às chamas",
  "X04" = "Exposição ao fumo, ao fogo e às chamas",
  "X05" = "Exposição ao fumo, ao fogo e às chamas",
  "X06" = "Exposição ao fumo, ao fogo e às chamas",
  "X07" = "Exposição ao fumo, ao fogo e às chamas",
  "X08" = "Exposição ao fumo, ao fogo e às chamas",
  "X09" = "Exposição ao fumo, ao fogo e às chamas",
  "X30" = "Exposição às forças da natureza",
  "X31" = "Exposição às forças da natureza",
  "X32" = "Exposição às forças da natureza",
  "X33" = "Exposição às forças da natureza",
  "X34" = "Exposição às forças da natureza",
  "X35" = "Exposição às forças da natureza",
  "X36" = "Exposição às forças da natureza",
  "X37" = "Exposição às forças da natureza",
  "X38" = "Exposição às forças da natureza",
  "X39" = "Exposição às forças da natureza",
  "W65" = "Afogamento e submersão acidentais",
  "W66" = "Afogamento e submersão acidentais",
  "W67" = "Afogamento e submersão acidentais",
  "W68" = "Afogamento e submersão acidentais",
  "W69" = "Afogamento e submersão acidentais",
  "W70" = "Afogamento e submersão acidentais",
  "W71" = "Afogamento e submersão acidentais",
  "W72" = "Afogamento e submersão acidentais",
  "W73" = "Afogamento e submersão acidentais",
  "W74" = "Afogamento e submersão acidentais",
  "W75" = "Outros riscos acidentais à respiração",
  "W76" = "Outros riscos acidentais à respiração",
  "W77" = "Outros riscos acidentais à respiração",
  "W78" = "Outros riscos acidentais à respiração",
  "W75" = "Outros riscos acidentais à respiração",
  "W76" = "Outros riscos acidentais à respiração",
  "W77" = "Outros riscos acidentais à respiração",
  "W78" = "Outros riscos acidentais à respiração",
  "W79" = "Outros riscos acidentais à respiração",
  "W80" = "Outros riscos acidentais à respiração",
  "W81" = "Outros riscos acidentais à respiração",
  "W82" = "Outros riscos acidentais à respiração",
  "W83" = "Outros riscos acidentais à respiração",
  "W84" = "Outros riscos acidentais à respiração",
  "W85" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W86" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W87" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W88" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W89" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W90" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W91" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W92" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W93" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W94" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W95" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W96" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W97" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W98" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "W99" = "Exposição a corrente elétrica, a radiação e a temperatura e pressão extremas do ar ambiental",
  "X85" = "Agressões",
  "X86" = "Agressões",
  "X87" = "Agressões",
  "X88" = "Agressões",
  "X89" = "Agressões",
  "X90" = "Agressões",
  "X91" = "Agressões",
  "X92" = "Agressões",
  "X93" = "Agressões",
  "X94" = "Agressões",
  "X95" = "Agressões",
  "X96" = "Agressões",
  "X97" = "Agressões",
  "X98" = "Agressões",
  "X99" = "Agressões",
  "Y00" = "Agressões",
  "Y01" = "Agressões",
  "Y02" = "Agressões",
  "Y03" = "Agressões",
  "Y04" = "Agressões",
  "Y05" = "Agressões",
  "Y06" = "Agressões",
  "Y07" = "Agressões",
  "Y08" = "Agressões",
  "Y09" = "Agressões",
  "Y10" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y11" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y12" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y13" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y14" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y15" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y16" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y17" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y18" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y19" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y20" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y21" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y22" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y23" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y24" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y25" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y26" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y27" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y28" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y29" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y30" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y31" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y32" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y33" = "Eventos (fatos) cuja intenção é indeterminada",
  "Y34" = "Eventos (fatos) cuja intenção é indeterminada",
  "W20" = "Exposição a forças mecânicas inanimadas",
  "W21" = "Exposição a forças mecânicas inanimadas",
  "W22" = "Exposição a forças mecânicas inanimadas",
  "W23" = "Exposição a forças mecânicas inanimadas",
  "W24" = "Exposição a forças mecânicas inanimadas",
  "W25" = "Exposição a forças mecânicas inanimadas",
  "W26" = "Exposição a forças mecânicas inanimadas",
  "W27" = "Exposição a forças mecânicas inanimadas",
  "W28" = "Exposição a forças mecânicas inanimadas",
  "W29" = "Exposição a forças mecânicas inanimadas",
  "W30" = "Exposição a forças mecânicas inanimadas",
  "W31" = "Exposição a forças mecânicas inanimadas",
  "W32" = "Exposição a forças mecânicas inanimadas",
  "W33" = "Exposição a forças mecânicas inanimadas",
  "W34" = "Exposição a forças mecânicas inanimadas",
  "W35" = "Exposição a forças mecânicas inanimadas",
  "W36" = "Exposição a forças mecânicas inanimadas",
  "W37" = "Exposição a forças mecânicas inanimadas",
  "W38" = "Exposição a forças mecânicas inanimadas",
  "W39" = "Exposição a forças mecânicas inanimadas",
  "W40" = "Exposição a forças mecânicas inanimadas",
  "W41" = "Exposição a forças mecânicas inanimadas",
  "W42" = "Exposição a forças mecânicas inanimadas",
  "W43" = "Exposição a forças mecânicas inanimadas",
  "W44" = "Exposição a forças mecânicas inanimadas",
  "W45" = "Exposição a forças mecânicas inanimadas",
  "W46" = "Exposição a forças mecânicas inanimadas",
  "W47" = "Exposição a forças mecânicas inanimadas",
  "W48" = "Exposição a forças mecânicas inanimadas",
  "W49" = "Exposição a forças mecânicas inanimadas",
  "Y60" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y61" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y62" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y63" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y64" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y65" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y66" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y67" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y68" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y69" = "Acidentes ocorridos em pacientes durante a prestação de cuidados médicos e cirúrgicos",
  "Y83" = "Reação anormal em paciente ou complicação tardia causadas por procedimentos cirúrgicos e outros procedimentos médicos sem menção de acidente ao tempo do procedimento",
  "Y84" = "Reação anormal em paciente ou complicação tardia causadas por procedimentos cirúrgicos e outros procedimentos médicos sem menção de acidente ao tempo do procedimento",
  "Y40" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y41" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y42" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y43" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y44" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y45" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y46" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y47" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y48" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y49" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y50" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y51" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y52" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y53" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y54" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y55" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y56" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y57" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y58" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "Y59" = "Efeitos adversos de drogas, medicamentos e substâncias biológicas usadas com finalidade terapêutica",
  "R00" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R01" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R02" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R03" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R04" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R05" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R06" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R07" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R08" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R09" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R10" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R11" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R12" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R13" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R14" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R15" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R16" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R17" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R18" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R19" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R20" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R21" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R22" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R23" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R24" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R25" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R26" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R27" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R28" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R29" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R30" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R31" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R32" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R33" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R34" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R35" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R36" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R37" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R38" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R39" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R40" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R41" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R42" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R43" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R44" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R45" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R46" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R47" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R48" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R49" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R50" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R51" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R52" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R53" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R54" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R55" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R56" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R57" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R58" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R59" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R60" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R61" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R62" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R63" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R64" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R65" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R66" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R67" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R68" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R69" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R70" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R71" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R72" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R73" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R74" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R75" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R76" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R77" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R78" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R79" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R80" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R81" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R82" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R83" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R84" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R85" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R86" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R87" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R88" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R89" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R90" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R91" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R92" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R93" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R94" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R96" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R97" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R98" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "R99" = "Sintomas, sinais e achados anormais, exceto síndrome da morte súbita na infância",
  "P95" = "Morte fetal de causa não especificada",
  "P969" = "Afecções originadas no período perinatal, não especificadas")


infant_mort <- infant_mort %>%
  mutate(causas_evitaveis = ifelse(causabas %in% names(causas_evitaveis), "sim", "não"))

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

# Criar quintis balanceados para IDH
social <- social %>%
  mutate(quintis_idhm = ntile(idhm, 5),  # Dividir IDH em quintis
         quintis_idhm = factor(quintis_idhm, levels = c(1, 2, 3, 4,5), 
                               labels = c("Muito Baixo","Baixo", "Intermediário", "Alto", "Muito Alto")))

# Verificar os pontos de corte para os quintis
quantile(social$idhm, probs = seq(0, 1, by = 0.2), na.rm = TRUE)


load("aps.RData")
load("cnes.RData")
load("mais_med.RData")
load("vacinas.RData")
favelas = read_excel("favelas.xlsx")
load("pop.RData")


mais_med <- mais_med %>%
  rename(data = dt_referencia)

vacinas <- vacinas %>%
  mutate(code_muni = substr(as.character(code_muni), 1, 6))

favelas <- favelas %>%
  mutate(code_muni = substr(as.character(code_muni), 1, 6))

pop_expanded <- pop_expanded %>%
  rename(code_muni = municipio_codigo)

pop_expanded <- pop_expanded %>%
  mutate(code_muni = substr(as.character(code_muni), 1, 6))


vacinas <- vacinas %>%
  pivot_wider(
    names_from = nome_vacina,
    values_from = cobertura_vacinal,
    values_fn = mean, # Calcula a média dos valores duplicados
    values_fill = list(cobertura_vacinal = 0) # Preenche com 0 onde não houver dados
  )
cnes <- df_final %>%
  mutate(code_muni = substr(as.character(code_muni), 1, 6))

cnes <- cnes %>%
  rename(data = comp)

cnes <- cnes %>%
  group_by(code_muni,data) %>%
  summarise(
    uti_pediatrico_exist = sum(uti_pediatrico_exist, na.rm = TRUE),
    uti_pediatrico_sus = sum(uti_pediatrico_sus, na.rm = TRUE),
    uti_neonatal_exist = sum(uti_neonatal_exist, na.rm = TRUE),
    uti_neonatal_sus = sum(uti_neonatal_sus, na.rm = TRUE)
  )

mais_med <- mais_med %>%
  group_by(code_muni,data) %>%
  summarise(
    prof_crm_brasil_pmmb = sum(prof_crm_brasil_pmmb, na.rm = TRUE),
    prof_inter_pmmb = sum(prof_inter_pmmb, na.rm = TRUE),
    prof_bolsista_pmpb = sum(prof_bolsista_pmpb, na.rm = TRUE),
    prof_tutor_pmpb = sum(prof_tutor_pmpb, na.rm = TRUE),
    prof_cooperados_pmmb = sum(prof_cooperados_pmmb, na.rm = TRUE),
    total_prof_ativos = sum(total_prof_ativos, na.rm = TRUE))

mais_med = mais_med %>% filter(!(total_prof_ativos=="-1"))

aps <- aps[!duplicated(aps), ]
cnes <- cnes[!duplicated(cnes), ]
dfbf <- dfbf[!duplicated(dfbf), ]
pop_expanded <- pop_expanded[!duplicated(pop_expanded), ]
mais_med <- mais_med[!duplicated(mais_med), ]
snis <- snis[!duplicated(snis), ]
favelas <- favelas[!duplicated(favelas), ]

dfbf = dfbf %>% group_by(code_muni, ano= year(data)) %>% summarise(cobertura_bf = mean(cobertura_bf, na.rm=TRUE),
                                                                   valor_repassado_bolsa_familia_def = sum(valor_repassado_bolsa_familia_def, na.rm=TRUE))

aps = aps %>% group_by(code_muni, ano= year(data)) %>% summarise(cobertura_aps = mean(cobertura_aps, na.rm=TRUE),
                                                                 gasto_aps = mean(custo, na.rm=TRUE))
pop_expanded = pop_expanded %>% group_by(code_muni, ano= year(data)) %>% summarise(populacao = mean(pop_all_years, na.rm=TRUE))

cnes = cnes %>% group_by(ano= year(data), code_muni) %>% summarise( uti_pediatrico_sus = sum(uti_pediatrico_sus, na.rm=TRUE),
                                                         uti_pediatrico_exist = sum(uti_pediatrico_exist, na.rm=TRUE),
                                                         uti_neonatal_exist = sum(uti_neonatal_exist, na.rm=TRUE),
                                                         uti_neonatal_sus = sum(uti_neonatal_sus, na.rm=TRUE))



