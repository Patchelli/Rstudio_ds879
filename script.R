library(readr)
vaccination_data <- read_csv("C:/Users/User/Desktop/ds879/vaccination-data.csv")
View(vaccination_data)


library(ggplot2)

ggplot(vaccination_data, aes(x = DATE_UPDATED, y = TOTAL_VACCINATIONS)) +
  geom_line() +
  labs(title = "Vacinação Global ao Longo do Tempo",
       x = "Data Atualizada",
       y = "Total de Vacinações")


ggplot(vaccination_data, aes(x = WHO_REGION, y = TOTAL_VACCINATIONS, fill = WHO_REGION)) +
  geom_bar(stat = "identity") +
  labs(title = "Total de Vacinações por Região",
       x = "Região da OMS",
       y = "Total de Vacinações") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


install.packages("dplyr")
library(dplyr)

vaccine_data <- vaccination_data %>%
  group_by(VACCINES_USED) %>%
  summarize(total_vaccinations = sum(TOTAL_VACCINATIONS))

ggplot(vaccine_data, aes(x = "", y = total_vaccinations, fill = VACCINES_USED)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribuição de Vacinas Utilizadas",
       fill = "Tipo de Vacina") +
  theme_void() +
  theme(legend.position = "bottom")

vaccination_data <- vaccination_data %>%
  mutate(POPULATION = as.numeric(POPULATION)) %>%
  mutate(vaccination_rate = TOTAL_VACCINATIONS / POPULATION * 100) %>%
  filter(!is.na(vaccination_rate)) # Remove registros com população ausente

ggplot(vaccination_data, aes(x = WHO_REGION, y = vaccination_rate, fill = WHO_REGION)) +
  geom_bar(stat = "identity") +
  labs(title = "Taxa de Vacinação por Região",
       x = "Região da OMS",
       y = "Taxa de Vacinação (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vaccination_data, aes(x = WHO_REGION, y = PERSONS_VACCINATED_1PLUS_DOSE / POPULATION * 100, fill = WHO_REGION)) +
  geom_bar(stat = "identity") +
  labs(title = "Taxa de Pessoas Vacinadas (1+ Dose) por Região",
       x = "Região da OMS",
       y = "Taxa de Pessoas Vacinadas (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(vaccination_data, aes(x = DATE_UPDATED)) +
  geom_line(aes(y = TOTAL_VACCINATIONS, color = "Total de Vacinações")) +
  geom_line(aes(y = PERSONS_VACCINATED_1PLUS_DOSE, color = "Pessoas Vacinadas (1+ Dose)")) +
  labs(title = "Comparação de Doses Administradas e Pessoas Vacinadas",
       x = "Data Atualizada",
       y = "Quantidade") +
  scale_color_manual(values = c("Total de Vacinações" = "blue", "Pessoas Vacinadas (1+ Dose)" = "red")) +
  theme_minimal()

ggplot(vaccination_data, aes(x = VACCINES_USED, y = TOTAL_VACCINATIONS, fill = VACCINES_USED)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribuição de Doses por Tipo de Vacina",
       x = "Tipo de Vacina",
       y = "Total de Vacinações") +
  theme(legend.position = "none")

ggplot(vaccination_data, aes(x = DATE_UPDATED, y = TOTAL_VACCINATIONS, color = WHO_REGION)) +
  geom_line() +
  labs(title = "Análise Temporal de Vacinação por Região",
       x = "Data Atualizada",
       y = "Total de Vacinações") +
  scale_color_discrete(name = "Região da OMS") +
  theme_minimal()

# Filtrar dados para América Latina
latin_america_data <- vaccination_data %>%
  filter(WHO_REGION == "AMRO")

# Criar gráfico de barras para comparar a vacinação entre países
ggplot(latin_america_data, aes(x = COUNTRY, y = TOTAL_VACCINATIONS, fill = COUNTRY)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Comparação da Vacinação entre Países da América Latina",
       x = "País",
       y = "Total de Vacinações") +
  theme(legend.position = "none")

# Filtrar dados para o Brasil
brazil_data <- vaccination_data %>%
  filter(COUNTRY == "Brazil")

# Criar gráfico de linha temporal para a taxa de vacinação no Brasil
ggplot(brazil_data, aes(x = DATE_UPDATED, y = PERSONS_VACCINATED_1PLUS_DOSE / POPULATION * 100)) +
  geom_line() +
  labs(title = "Taxa de Vacinação no Brasil ao Longo do Tempo",
       x = "Data Atualizada",
       y = "Taxa de Pessoas Vacinadas (%)") +
  theme_minimal()
