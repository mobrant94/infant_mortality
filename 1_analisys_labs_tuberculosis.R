

##############################CASOS LABORATORIAIS ###############################

# General table
tabela_geral = df1 %>%  
  mutate(data = floor_date(dt_diag, "month")) %>%
  group_by(data, extratos_idade) %>%
  summarise(cases = n())

# Pivot wider to spread age groups into columns
tabela_geral = tabela_geral %>%
  pivot_wider(names_from = extratos_idade, values_from = cases, values_fill = list(cases = 0))

# Calculate general cases by summing specific age groups
tabela_geral = tabela_geral %>%
  mutate(general = `entre 1 e 4` + `entre 10 e 14` + `entre 5 e 9` + `menos de 1`)

# Clean column names
tabela_geral = clean_names(tabela_geral)

# Join with population data
tabela_geral = left_join(tabela_geral, pop, by = "data")

# Calculate prevalence rates
tabela_geral <- tabela_geral %>%
  mutate(
    m_1 = (menos_de_1 / pop_0) * 100000,
    e1_e_4 = (entre_1_e_4 / pop_1_4) * 100000,
    e5_e_9 = (entre_5_e_9 / pop_5_9) * 100000,
    e10_e_14 = (entre_10_e_14 / pop_10_14) * 100000,
    general_cases = (general / brasil) * 100000
  )

# Convert to data frame
tabela_geral = as.data.frame(tabela_geral)

# Dummy variables for pandemic periods
tabela_geral <- tabela_geral %>%
  mutate(pandemic = case_when(
    data < as.POSIXct("2020-03-01") ~ 0,
    data >= as.POSIXct("2020-03-01") & data < as.POSIXct("2022-01-01") ~ 1,
    data >= as.POSIXct("2022-01-01") ~ 2
  ))

# Select relevant columns
tabela_geral <- tabela_geral %>%
  select(data, pandemic, m_1, e1_e_4, e5_e_9, e10_e_14, general_cases)


##############################with covid


# Select relevant columns
tabela_geral_analise <- tabela_geral %>%
  select(data, pandemic, m_1, e1_e_4, e5_e_9, e10_e_14, general_cases) 

# Add a counter
tabela_geral_analise$counter = 1:nrow(tabela_geral_analise)

# Fit GLM models
model_m1 <- glm(m_1 ~ factor(pandemic) + counter, data = tabela_geral_analise)
model_e1_e_4 <- glm(e1_e_4 ~ factor(pandemic) + counter, data = tabela_geral_analise)
model_e5_e_9 <- glm(e5_e_9 ~ factor(pandemic) + counter, data = tabela_geral_analise)
model_e10_e_14 <- glm(e10_e_14 ~ factor(pandemic) + counter, data = tabela_geral_analise)
model_general_cases <- glm(general_cases ~ factor(pandemic) + counter, data = tabela_geral_analise)

# Function to extract model results with confidence intervals
extract_model_results <- function(model, model_name) {
  # Usa broom::tidy para obter os coeficientes e seus intervalos de confiança
  tidy_model <- broom::tidy(model)
  conf_int <- confint(model)
  
  # Adiciona os intervalos de confiança ao tidy_model
  results <- cbind(tidy_model, conf_int)
  
  # Inclui o nome do modelo
  results <- results %>% mutate(model = model_name)
  
  return(results)
}

# Extract results and add model name
results_m1 <- extract_model_results(model_m1, "less than 1 year")
results_e1_e_4 <- extract_model_results(model_e1_e_4, "between 1 and 4 years")
results_e5_e_9 <- extract_model_results(model_e5_e_9, "between 5 and 9 years")
results_e10_e_14 <- extract_model_results(model_e10_e_14, "between 10 and 14 years")
results_general_cases <- extract_model_results(model_general_cases, "General cases")

# Combine all results into a single table
all_results <- bind_rows(
  results_m1,
  results_e1_e_4,
  results_e5_e_9,
  results_e10_e_14,
  results_general_cases
)

# Filter out intercepts
all_results <- all_results %>% filter(term != "(Intercept)")
all_results <- all_results %>% filter(term != "counter")

# Rename terms
all_results <- all_results %>%
  mutate(term = case_when(
    term == "analysis" ~ "post covid",
    term == "counter" ~ "months",
    TRUE ~ term
  ))

# Rename confidence interval columns
all_results <- all_results %>%
  rename(conf.low = `2.5 %`, conf.high = `97.5 %`)

# Select desired columns
final_table <- all_results %>%
  select(model, term, estimate, conf.low, conf.high, p.value)

writexl::write_xlsx(final_table,path = "casos_labs.xlsx")
writexl::write_xlsx(tabela_geral,path = "tabela_labs.xlsx")


# Add year column to the general table
tabela_geral <- tabela_geral %>%
  mutate(year = format(data, "%Y"))

# Group by 'year' and calculate the mean of the last five variables
annual_table <- tabela_geral %>%
  group_by(year) %>%
  summarise(
    m_1 = mean(m_1, na.rm = TRUE),
    e1_e_4 = mean(e1_e_4, na.rm = TRUE),
    e5_e_9 = mean(e5_e_9, na.rm = TRUE),
    e10_e_14 = mean(e10_e_14, na.rm = TRUE),
    general_cases = mean(general_cases, na.rm = TRUE)
  )

# View the resulting table
print(annual_table)
writexl::write_xlsx(annual_table, path = "annual_table.xlsx")
writexl::write_xlsx(tabela_geral, path = "general_table.xlsx")

# Add a column indicating if the date is outside the pandemic period
tabela_geral <- tabela_geral %>%
  mutate(outside_pandemic = ifelse(data < as.Date("2020-03-01") | data > as.Date("2021-12-31"), TRUE, FALSE))

# Convert to long format
tabela_geral <- tabela_geral %>%
  pivot_longer(cols = c(m_1, e1_e_4, e5_e_9, e10_e_14, general_cases), 
               names_to = "variable", 
               values_to = "value")

# Convert dates to Date class and set factor levels for variables
tabela_geral$data = as.Date(tabela_geral$data)
tabela_geral$variable <- factor(tabela_geral$variable, levels = c("general_cases", "m_1", "e1_e_4", "e5_e_9", "e10_e_14"))

# Filter data for pre-pandemic and post-pandemic periods
long_table_pre_pandemic <- tabela_geral %>% filter(data < as.Date("2020-03-01"))
long_table_post_pandemic <- tabela_geral %>% filter(data > as.Date("2021-12-31"))
long_table_pandemic <- tabela_geral %>% filter(data > as.Date("2020-02-29") & data < as.Date("2022-01-01"))

# Create the plot
ggplot(tabela_geral, aes(x = data, y = value)) +
  geom_line(color = "black", size = 0.7, aes(linetype = "Prevalence")) + # Line for prevalence
  geom_smooth(data = long_table_pre_pandemic, aes(color = "Moving Average"), method = "loess", se = FALSE, size = 1.2, linetype = "dashed") + # Loess smooth for pre-pandemic period
  geom_smooth(data = long_table_post_pandemic, aes(color = "Moving Average"), method = "loess", se = FALSE, size = 1.2, linetype = "dashed") + # Loess smooth for post-pandemic period
  geom_smooth(data = long_table_pandemic, aes(color = "Moving Average"), method = "loess", se = FALSE, size = 1.2, linetype = "dashed") + # Loess smooth for pandemic period
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype = "dashed", color = "red", size = 0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2022-01-01")), linetype = "dashed", color = "blue", size = 0.9) +
  annotate("text", x = as.Date("2020-02-10"), y = Inf, label = "Start of pandemic", color = "red", size = 2.5, vjust = 1.5, hjust = 1) +
  annotate("text", x = as.Date("2021-12-01"), y = Inf, label = "Relaxation\nof measures", color = "blue", size = 2.5, vjust = 1.5, hjust = 1) +
  facet_wrap(~ variable, scales = "free", labeller = as_labeller(c(
    m_1 = "<1 year",
    e1_e_4 = "1-4 years",
    e5_e_9 = "5-9 years",
    e10_e_14 = "10-14 years",
    general_cases = "General cases"
  ))) +
  labs(title = "",
       x = "Period",
       y = "Prevalence per 100,000",
       color = "",
       linetype = "") + # Add legend titles
  scale_color_manual(values = c("Moving Average" = "darkgreen")) + # Set the color for the moving average
  scale_linetype_manual(values = c("Prevalence" = "solid", "Moving Average" = "dashed")) + # Set line types
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2008-01-01", "2023-12-21"))) +
  theme_classic() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    plot.title = element_text(face = "bold", size = 14),  # Bold title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    axis.line.x = element_line(color = "black"),  # Black x-axis line
    axis.line.y = element_line(color = "black"),  # Black y-axis line
    axis.line = element_line(),  # Black axis line
    axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels by 90 degrees
  )
