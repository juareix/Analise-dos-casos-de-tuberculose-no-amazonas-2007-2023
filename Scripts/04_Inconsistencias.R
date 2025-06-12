# ============================================================================
# SCRIPT 04 - Análise de Inconsistências Lógicas - SINAN e GAL
# Projeto: Avaliação da Qualidade dos Dados de Tuberculose - ILMD/FIOCRUZ
# Autor: Juarez Maciel da Silva
# Data: junho/2025
# Descrição: Identifica e exporta inconsistências nas bases tratadas
# ============================================================================

# ---------------------------
# 1. Carregar pacotes
# ---------------------------
library(tidyverse)
library(lubridate)

# ---------------------------
# 2. Carregar as bases tratadas
# ---------------------------
sinan <- read_csv("data_clean/sinan_tratado.csv")
gal   <- read_csv("data_clean/gal_tratado.csv")

# ---------------------------
# 3. Inconsistências na base SINAN
# ---------------------------

incons_sinan <- list()

# 3.1 Baciloscopia positiva em casos exclusivamente extrapulmonares
incons_sinan$bacilo_extrapulmonar <- sinan %>%
  filter(forma == "Extrapulmonar" & bacilosc_e == 1)

# 3.2 Sexo masculino grávido (quando aplicável)
incons_sinan$sexo_masculino_gestante <- sinan %>%
  filter(cs_sexo == "Masculino" & cs_gestant %in% c(1, 2, 3))


# 3.4 Idade fora do intervalo plausível
incons_sinan$idade_invalida <- sinan %>%
  filter(idade_anos < 0 | idade_anos > 120)

# 3.5 Desfecho encerrado mas cultura ainda "Em andamento"
incons_sinan$cultura_encerramento <- sinan %>%
  filter(situa_ence %in% c(1, 2, 3, 4, 6, 10) & cultura_es == 3)

# 3.6 HIV "Em andamento" com desfecho encerrado
incons_sinan$hiv_pendente_com_desfecho <- sinan %>%
  filter(situa_ence %in% c(1, 2, 3, 4, 6, 10) & hiv == "Em andamento")

# ---------------------------
# 4. Inconsistências na base GAL
# ---------------------------

incons_gal <- list()

# 4.1 Data de coleta após a liberação (cronologia errada)
incons_gal$data_incoerente <- gal %>%
  filter(data_da_coleta > data_da_liberacao)

# 4.2 Resultado positivo, mas DNA para TB "Não detectado"
incons_gal$resultado_incongruente <- gal %>%
  filter(resultado == "Positivo" &
           dna_para_o_complexo_mycobacterium_tuberculosis == "Não detectado")

# ---------------------------
# 5. Exportar inconsistências para arquivos
# ---------------------------

dir.create("outputs/inconsistencias", showWarnings = FALSE)

# Exportar cada inconsistência como CSV
walk2(
  .x = inconsistencias <- c(incons_sinan, inconsistencias_gal <- incons_gal),
  .y = names(inconsistencias),
  .f = ~ write_csv(.x, file = paste0("outputs/inconsistencias/", .y, ".csv"))
)

# ---------------------------
# 6. Relatório geral de inconsistências (quantitativo)
# ---------------------------
relatorio_incons <- tibble(
  inconsistencia = names(inconsistencias),
  total_casos = map_int(inconsistencias, nrow)
)

write_csv(relatorio_incons, "outputs/relatorio_inconsistencias.csv")

# ---------------------------
# 7. Mensagem final
# ---------------------------
cat("✔️ Análise de inconsistências finalizada!\n")
cat("   → Relatório salvo em 'outputs/relatorio_inconsistencias.csv'\n")
cat("   → Inconsistências detalhadas exportadas em 'outputs/inconsistencias/'\n")
