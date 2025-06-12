# ============================================================================
# SCRIPT 01 - Importação das Bases SINAN e GAL
# Projeto: Avaliação da Qualidade dos Dados de Tuberculose - ILMD/FIOCRUZ
# Autor: Juarez Maciel da Silva
# Etapa: Importação, padronização de nomes e salvamento
# Data: junho/2025
# ============================================================================

# ---------------------------
# 1. Carregar pacotes essenciais
# ---------------------------
# readr: para leitura eficiente de arquivos CSV
# janitor: para padronizar nomes de colunas (snake_case, sem espaços/acentos)

if (!require("readr")) install.packages("readr");library(readr)
if (!require("janitor")) install.packages("janitor");library(janitor)

# ---------------------------
# 2. Definir caminhos dos arquivos
# ---------------------------

# Diretórios de entrada (bases incicias)
sinan_path <- "data_raw/dados_sinan.csv"
gal_path   <- "data_raw/dados_gal.csv"

# Diretórios de saída (bases limpas)
sinan_out <- "data_clean/sinan_limpo.csv"
gal_out   <- "data_clean/gal_limpo.csv"

# ---------------------------
# 3. Importar os dados e padronizar nomes das variáveis
# ---------------------------

# Importar base SINAN
sinan <- read_csv(sinan_path)
sinan <- janitor::clean_names(sinan)  # padroniza nomes para snake_case

# Importar base GAL
gal <- read_csv(gal_path)
gal <- janitor::clean_names(gal) # padroniza nomes para snake_case

# ---------------------------
# 4. Salvar versões padronizadas para a próxima etapa
# ---------------------------

# Garante que a pasta de saída existe
if (!dir.exists("data_clean")) dir.create("data_clean")

# Salvar os arquivos tratados
write_csv(sinan, sinan_out)
write_csv(gal, gal_out)

# ---------------------------
# 5. Mensagem final
# ---------------------------
cat("✔️ Bases importadas, nomes padronizados e arquivos salvos em 'data_clean/'.\n")
