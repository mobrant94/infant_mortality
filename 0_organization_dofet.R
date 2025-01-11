# Anos dos arquivos
anos <- 2001:2022

# Nomes dos arquivos DBC com prefixo e sufixo
arquivos <- paste0("DOFET", sprintf("%02d", anos - 2000), ".DBC")

# Função para importar e ajustar o banco de dados
importar_e_ajustar <- function(arquivo, colunas_referencia) {
  # Importa o banco de dados
  dados <- read.dbc(arquivo)
  
  # Colunas que faltam
  colunas_faltando <- setdiff(colunas_referencia, names(dados))
  
  # Adiciona as colunas faltantes com valores NA
  for (coluna in colunas_faltando) {
    dados[[coluna]] <- NA
  }
  
  # Reordena as colunas para corresponder à referência
  dados <- dados[colunas_referencia]
  
  return(dados)
}

# Importa o banco de dados de referência
dados_referencia <- read.dbc("DOFET01.DBC")
colunas_referencia <- names(dados_referencia)

# Importa e ajusta todos os arquivos, exceto o de referência
bancos_dados <- lapply(arquivos, function(arquivo) {
  if (arquivo != "DOFET01.DBC") {
    importar_e_ajustar(arquivo, colunas_referencia)
  }
})

# Adiciona o banco de dados de referência à lista
bancos_dados <- c(list(dados_referencia), bancos_dados)

# Une todos os bancos de dados
dados_unidos <- do.call(rbind, bancos_dados)

save(dados_unidos,file = "dfet0122.RData")
