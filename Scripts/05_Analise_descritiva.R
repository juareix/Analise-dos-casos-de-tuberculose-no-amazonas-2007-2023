# ============================================================================
# SCRIPT 05 - Análise Descritiva das Bases SINAN e GAL
# Projeto: Avaliação da Qualidade dos Dados de Tuberculose - ILMD/FIOCRUZ
# Autor: Juarez Maciel da Silva
# Data: junho/2025
# Descrição:
# - Estatísticas descritivas para variáveis qualitativas e quantitativas
# - Teste de normalidade
# - Tabelas cruzadas
# - Exportação de resultados
# ============================================================================

# ---------------------------
# 1. Carregar pacotes
# ---------------------------
library(tidyverse)
library(lubridate)

# ---------------------------
# 2. Importar bases tratadas
# ---------------------------
sinan <- read_csv("data_clean/sinan_tratado.csv")
gal   <- read_csv("data_clean/gal_tratado.csv")

dir.create("outputs/descritivas", showWarnings = FALSE)

# ---------------------------
# 3. VARIÁVEIS QUALITATIVAS (SINAN)
# ---------------------------
qualitativas <- c("cs_sexo", "cs_raca", "cs_escol_n", "forma", "hiv", "situa_ence", "faixa_etaria")

tab_qualitativas <- map_dfr(qualitativas, function(var) {
  sinan %>%
    filter(!is.na(.data[[var]])) %>%
    count(!!sym(var)) %>%
    mutate(
      variavel = var,
      percentual = round(n / sum(n) * 100, 1)
    ) %>%
    rename(categoria = 1)
})

write_csv(tab_qualitativas, "outputs/descritivas/qualitativas_sinan.csv")

# ---------------------------
# 4. VARIÁVEIS QUANTITATIVAS (SINAN)
# ---------------------------
quantitativas <- c("idade_anos")

resumo_quantitativas <- map_dfr(quantitativas, function(var) {
  dados <- sinan %>% filter(!is.na(.data[[var]]))
  vetor <- pull(dados, var)
  p_valor <- tryCatch(shapiro.test(vetor)$p.value, error = function(e) NA)
  
  if (!is.na(p_valor) && p_valor > 0.05) {
    tibble(
      variavel = var,
      tipo = "Normal",
      media = mean(vetor),
      dp = sd(vetor),
      mediana = NA,
      iiq = NA,
      p_normalidade = p_valor
    )
  } else {
    tibble(
      variavel = var,
      tipo = "Não normal",
      media = NA,
      dp = NA,
      mediana = median(vetor),
      iiq = IQR(vetor),
      p_normalidade = p_valor
    )
  }
})

write_csv(resumo_quantitativas, "outputs/descritivas/quantitativas_sinan.csv")

# ---------------------------
# 5. TABELAS CRUZADAS
# ---------------------------

# Exemplo 1: sexo × forma clínica
sexo_forma <- sinan %>%
  filter(!is.na(cs_sexo) & !is.na(forma)) %>%
  count(cs_sexo, forma) %>%
  group_by(cs_sexo) %>%
  mutate(percentual = round(n / sum(n) * 100, 1))

write_csv(sexo_forma, "outputs/descritivas/cruzada_sexo_forma.csv")

# Exemplo 2: faixa_etaria × situação de encerramento
idade_situacao <- sinan %>%
  filter(!is.na(faixa_etaria) & !is.na(situa_ence)) %>%
  count(faixa_etaria, situa_ence) %>%
  group_by(faixa_etaria) %>%
  mutate(percentual = round(n / sum(n) * 100, 1))

write_csv(idade_situacao, "outputs/descritivas/cruzada_idade_situacao.csv")

# ---------------------------
# 6. (Opcional) Análise descritiva para GAL
# ---------------------------

# Frequência de sexo e resultado
gal_qualitativas <- gal %>%
  count(sexo, resultado) %>%
  group_by(sexo) %>%
  mutate(percentual = round(n / sum(n) * 100, 1))

write_csv(gal_qualitativas, "outputs/descritivas/gal_qualitativas.csv")

# ---------------------------
# 7. Mensagem final
# ---------------------------
cat("✔️ Análise descritiva finalizada!\n")
cat("   → Tabelas salvas em 'outputs/descritivas/'\n")
