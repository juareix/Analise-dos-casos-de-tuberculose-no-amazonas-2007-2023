# ============================================================================
# SCRIPT 06 - Geração de Metadados e Codebook (SINAN e GAL)
# Projeto: Avaliação da Qualidade dos Dados de Tuberculose - ILMD/FIOCRUZ
# Autor: Juarez Maciel da Silva
# Data: junho/2025
# Descrição:
# - Atribuição de rótulos e valores válidos as principais variaveis de analise
# - Geração de codebook em HTML para documentação
# ============================================================================

# ---------------------------
# 1. Carregar pacotes
# ---------------------------
library(tidyverse)
library(codebook)
library(labelled)

# ---------------------------
# 2. Importar dados tratados
# ---------------------------
sinan <- read_csv("data_clean/sinan_tratado.csv")
gal   <- read_csv("data_clean/gal_tratado.csv")

# ---------------------------
# 3. Atribuir rótulos (label) e valores válidos esperados - SINAN
# ---------------------------

# Atribuição de rótulos (descrição das variáveis)
var_label(sinan$cs_sexo) <- "Sexo do paciente"
val_labels(sinan$cs_sexo) <- c(Masculino = 1, Feminino = 2)

var_label(sinan$cs_raca) <- "Raça/Cor"
var_label(sinan$cs_escol_n) <- "Escolaridade"
var_label(sinan$forma) <- "Forma clínica da tuberculose"
var_label(sinan$hiv) <- "Resultado do teste de HIV"
var_label(sinan$idade_anos) <- "Idade (em anos)"
var_label(sinan$faixa_etaria) <- "Faixa etária"
var_label(sinan$situa_ence) <- "Situação de encerramento do caso"
var_label(sinan$dt_notific) <- "Data de notificação"
var_label(sinan$dt_diag) <- "Data do diagnóstico"
var_label(sinan$dt_inic_tr) <- "Data de início do tratamento"

# Exemplo de valores esperados categóricos
val_labels(sinan$forma) <- c(
  `Pulmonar` = 1,
  `Extrapulmonar` = 2,
  `Pulmonar + Extrapulmonar` = 3
)

# ---------------------------
# 4. Atribuição básica de rótulos - GAL
# ---------------------------
var_label(gal$sexo) <- "Sexo do paciente"
var_label(gal$idade) <- "Idade (em anos)"
var_label(gal$resultado) <- "Resultado do exame laboratorial"
var_label(gal$data_da_coleta) <- "Data da coleta"
var_label(gal$data_da_liberacao) <- "Data da liberação do resultado"
var_label(gal$dna_para_o_complexo_mycobacterium_tuberculosis) <- "DNA para TB detectado"
var_label(gal$tecnica) <- "Técnica utilizada"
var_label(gal$faixa_etaria) <- "Faixa etária"

# ---------------------------
# 5. Gerar Codebooks (HTML) para cada base
# ---------------------------
dir.create("repo_docs", showWarnings = FALSE)

# SINAN
rmarkdown::render(
  system.file("rmarkdown", "templates", "codebook", "skeleton", "skeleton.Rmd", package = "codebook"),
  output_file = "codebook_sinan.html",
  output_dir = "repo_docs",
  params = list(data = sinan, metadata_table_title = "Codebook - SINAN")
)

# GAL
rmarkdown::render(
  system.file("rmarkdown", "templates", "codebook", "skeleton", "skeleton.Rmd", package = "codebook"),
  output_file = "codebook_gal.html",
  output_dir = "repo_docs",
  params = list(data = gal, metadata_table_title = "Codebook - GAL")
)

# ---------------------------
# 6. Mensagem final
# ---------------------------
cat("✔️ Codebooks gerados com sucesso e salvos em 'repo_docs/'\n")
