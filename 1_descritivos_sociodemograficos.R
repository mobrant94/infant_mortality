#############descritivos mortalidade infantil#########################################

# Carregar os dados geoespaciais dos municípios brasileiros
municipios <- read_municipality(year = 2020)

# Verificar a estrutura dos dados de 'municipios'
str(municipios)

# Modificar 'code_muni' para remover o sétimo dígito
municipios <- municipios %>%
  mutate(code_muni = as.numeric(substr(code_muni, 1, 6)))

# Juntar os dados de 'general' com os dados de 'municipios' por 'code_muni'
dados_mapa_social <- left_join(municipios, social, by = c("code_muni" = "code_muni"))

# Plotar o mapa de calor com escala personalizada
ggplot(data = dados_mapa_social) +
  geom_sf(aes(fill = idhm), color = NA) +  # 'geom_sf' para dados espaciais
  scale_fill_gradientn(colors = tim.colors(50), 
                       limits = c(0, 1), 
                       na.value = "white") +  # Definir a escala de preenchimento de 0 a 20
  labs(title = "",
       fill = "") +
  theme_classic() +
  theme(axis.title = element_blank(),  # Remover os títulos dos eixos
        axis.text = element_blank(),  # Remover os textos dos eixos
        axis.ticks = element_blank()) 

# Plotar o mapa de calor com escala personalizada
ggplot(data = dados_mapa_social) +
  geom_sf(aes(fill = gini), color = NA) +  # 'geom_sf' para dados espaciais
  scale_fill_gradientn(colors = tim.colors(50), 
                       limits = c(0, 1), 
                       na.value = "white") +  # Definir a escala de preenchimento de 0 a 20
  labs(title = "",
       fill = "") +
  theme_classic() +
  theme(axis.title = element_blank(),  # Remover os títulos dos eixos
        axis.text = element_blank(),  # Remover os textos dos eixos
        axis.ticks = element_blank()) 




esgoto = snis %>% select(code_muni, region_cond, ano, cobertura_esgoto)
esgoto[c("code_muni", "ano", "cobertura_esgoto")] <- lapply(esgoto[c("code_muni", "ano", "cobertura_esgoto")], function(x) ifelse(is.nan(x), NA, x))
esgoto = esgoto %>% group_by(region_cond) %>% summarise(esgoto = mean(cobertura_esgoto, na.rm=TRUE))


agua = snis %>% select(code_muni, region_cond, ano, cobertura_agua)
agua[c("code_muni", "ano", "cobertura_esgoto")] <- lapply(agua[c("code_muni", "ano", "cobertura_agua")], function(x) ifelse(is.nan(x), NA, x))
agua = agua %>% group_by(region_cond) %>% summarise(agua = mean(cobertura_esgoto, na.rm=TRUE))


# Juntar os datasets
dados_combinados <- left_join(agua, esgoto, by = "region_cond")

# Reshape dos dados para formato longo
dados_longos <- dados_combinados %>%
  pivot_longer(cols = c("agua", "esgoto"), names_to = "servico", values_to = "percentual")

ggplot(dados_longos, aes(x = region_cond, y = percentual, fill = servico)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Barras lado a lado com contorno preto
  geom_text(aes(label = round(percentual, 1)),  # Adicionar os valores
            position = position_dodge(width = 0.9),  # Ajustar posição dos textos
            vjust = -0.5,  # Posicionar os textos acima das barras
            color = "black") +  # Cor do texto
  scale_fill_manual(values = c("agua" = "skyblue", "esgoto" = "lightgreen"),
                    name = "Taxa de cobertura",
                    labels = c("Água", "Esgoto")) +  # Alterar os labels da legenda
  labs(title = "Cobertura de Água e Esgoto por Região",
       x = "Região",
       y = "Percentual de Cobertura") +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", color = "black"),  # Títulos dos eixos em negrito e preto
        axis.text = element_text(color = "black"),  # Textos dos eixos em preto
        legend.title = element_text(face = "bold"),  # Título da legenda em negrito
        legend.text = element_text(color = "black"))+  # Texto da legenda em preto
  ylim(0,103)












bf1 = dfbf %>% group_by(region_cond,data) %>% summarise(media_geral = mean(media_bf_nom, na.rm=TRUE))


p_smoothed_bf <- ggplot(bf1, aes(x = data, y = media_geral, color = region_cond, group = region_cond)) +
  # Adicionar a linha suavizada para 'Brasil' e as outras regiões
  geom_smooth(size = 1.5, method = "loess", se = FALSE, linetype = "solid") +
  # Definir a cor manualmente para as regiões
  scale_color_manual(values = c("Brasil" = "green4", 
                                "Centro-Oeste" = "orange2", 
                                "Nordeste" = "#8B0000", 
                                "Norte" = "yellow3", 
                                "Sudeste" = "#8B008B", 
                                "Sul" = "#00008B")) +
  labs(title = 'Tendência suavizada', 
       x = 'Período', 
       y = 'Média de valor por família - R$') +
  theme_classic() +   
  theme(legend.title = element_blank(),
        axis.title = element_text(face = "bold"))


bf2 = dfbf %>% group_by(region_cond,data) %>% summarise(media_geral = mean(media_bf_def, na.rm=TRUE))


p_smoothed_bf2 <- ggplot(bf2, aes(x = data, y = media_geral, color = region_cond, group = region_cond)) +
  # Adicionar a linha suavizada para 'Brasil' e as outras regiões
  geom_smooth(size = 1.5, method = "loess", se = FALSE, linetype = "solid") +
  # Definir a cor manualmente para as regiões
  scale_color_manual(values = c("Brasil" = "green4", 
                                "Centro-Oeste" = "orange2", 
                                "Nordeste" = "#8B0000", 
                                "Norte" = "yellow3", 
                                "Sudeste" = "#8B008B", 
                                "Sul" = "#00008B")) +
  labs(title = 'Tendência deflacionada', 
       x = 'Período', 
       y = 'Média de valor por família - R$') +
  theme_classic() +   
  theme(legend.title = element_blank(),
        axis.title = element_text(face = "bold"))


# Supondo que p_smoothed_bf e p_smoothed_bf2 já estejam definidos
(p_smoothed_bf / p_smoothed_bf2) + 
  plot_layout(guides = "collect", axis_titles = "collect")

