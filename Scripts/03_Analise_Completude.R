# ============================================================================
# SCRIPT 03 - Avaliação de Completude das Bases SINAN e GAL
# Projeto: Avaliação da Qualidade dos Dados de Tuberculose - ILMD/FIOCRUZ
# Autor: Juarez Maciel da Silva
# Data: junho/2025
# Descrição: Calcula e visualiza a completude (preenchimento) dos campos
# ============================================================================

# ---------------------------
# 1. Carregar pacotes
# ---------------------------
if (!require("tidyverse")) install.packages("tidyverse");library(tidyverse)
if (!require("lubridate")) install.packages("lubridate");library(lubridate)

# ---------------------------
# 2. Carregar as bases originais (não tratadas)
# ---------------------------
sinan <- read_csv("data_clean/sinan_tratado.csv")
gal   <- read_csv("data_clean/gal_tratado.csv")

# ---------------------------
# 3. Função auxiliar para calcular completude (% preenchimento)
# ---------------------------
completude_variavel <- function(df) {
  df %>%
    summarise(across(everything(), ~ mean(!is.na(.)) * 100)) %>%
    pivot_longer(everything(), names_to = "variavel", values_to = "completude") %>%
    arrange(completude)
}

# ---------------------------
# 4. Completude geral (por variável)
# ---------------------------
sinan_completude <- completude_variavel(sinan) %>% mutate(base = "SINAN")
gal_completude   <- completude_variavel(gal)   %>% mutate(base = "GAL")

completude_total <- bind_rows(sinan_completude, gal_completude)

# ---------------------------
# 5. Gráfico de completude por base
# ---------------------------
plot_sinan <- sinan_completude %>%
  ggplot(aes(x = reorder(variavel, completude), y = completude)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Completude por variável - SINAN", x = NULL, y = "Completude (%)") +
  theme_minimal()

plot_gal <- gal_completude %>%
  ggplot(aes(x = reorder(variavel, completude), y = completude)) +
  geom_col(fill = "#D55E00") +
  coord_flip() +
  labs(title = "Completude por variável - GAL", x = NULL, y = "Completude (%)") +
  theme_minimal()

# ---------------------------
# 6. Completude temporal e geográfica (SINAN)
# ---------------------------
completude_por_ano_mun <- sinan %>%
  mutate(ano = year(dmy(dt_notific)),
         municipio = tolower(no_munic)) %>%
  group_by(ano, municipio) %>%
  summarise(across(everything(), ~ mean(!is.na(.)) * 100, .names = "comp_{.col}"), .groups = "drop")

# Exemplo: Completude da variável 'forma' por ano
comp_forma_ano <- sinan %>%
  mutate(ano = year(dmy(dt_notific))) %>%
  group_by(ano) %>%
  summarise(completude_forma = mean(!is.na(forma)) * 100)

graf_comp_forma <- comp_forma_ano %>%
  ggplot(aes(x = ano, y = completude_forma)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "black") +
  labs(title = "Completude da variável 'forma' por ano (SINAN)", x = "Ano", y = "Completude (%)") +
  theme_minimal()

# ---------------------------
# 7. Exportar tabelas e gráficos para outputs/
# ---------------------------

if (!dir.exists("outputs")) dir.create("outputs")

# Tabelas de completude
write_csv(sinan_completude, "outputs/completude_sinan.csv")
write_csv(gal_completude,   "outputs/completude_gal.csv")
write_csv(completude_total, "outputs/completude_total.csv")

# Gráficos
ggsave("outputs/grafico_completude_sinan.png", plot_sinan, width = 10, height = 8)
ggsave("outputs/grafico_completude_gal.png", plot_gal, width = 10, height = 8)
ggsave("outputs/grafico_completude_forma_ano.png", graf_comp_forma, width = 8, height = 5)

# ---------------------------
# 8. Avaliação extra: variáveis com completude crítica (< 70%)
# ---------------------------
variaveis_criticas <- completude_total %>%
  filter(completude < 70) %>%
  arrange(completude)

write_csv(variaveis_criticas, "outputs/variaveis_completude_baixa.csv")

# ---------------------------
# 9. Mensagem final
# ---------------------------
cat("✔️ Análise de completude concluída!\n")
cat("   → Tabelas salvas em 'outputs/completude_*.csv'\n")
cat("   → Gráficos salvos em 'outputs/*.png'\n")
cat("   → Variáveis críticas em 'outputs/variaveis_completude_baixa.csv'\n")
