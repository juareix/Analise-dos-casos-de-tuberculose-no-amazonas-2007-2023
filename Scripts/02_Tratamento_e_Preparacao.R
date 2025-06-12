# ============================================================================
# SCRIPT 02 - Tratamento e Preparação das Bases SINAN e GAL
# Projeto: Avaliação da Qualidade dos Dados de Tuberculose - ILMD/FIOCRUZ
# Autor: Juarez Maciel da Silva
# Data: junho/2025
# ============================================================================

library(tidyverse)
library(lubridate)

# ---------------------------
# 1. Função para tratar valores ausentes (só em colunas character)
# ---------------------------
padronizar_ausentes <- function(x) {
  na_if(x, "9") %>% na_if("99") %>% na_if("999") %>% na_if("Ignorado") %>% na_if("IGNORADO")
}

# ---------------------------
# 1. Função para reclassificar e tratar corretamente os casos de escolaridade
# ---------------------------
reclassificar_escolaridade <- function(x) {
  x <- trimws(tolower(x))
  dplyr::case_when(
    x %in% c("0", "00", "sem escolaridade") ~ "Sem escolaridade",
    x %in% c("1", "01", "1ª a 4ª série incompleta") ~ "1ª a 4ª série incompleta",
    x %in% c("2", "02", "4ª série completa") ~ "4ª série completa",
    x %in% c("3", "03", "5ª a 8ª série incompleta") ~ "5ª a 8ª série incompleta",
    x %in% c("4", "04", "8ª série completa") ~ "8ª série completa",
    x %in% c("5", "05", "ensino médio incompleto") ~ "Ensino médio incompleto",
    x %in% c("6", "06", "ensino médio completo") ~ "Ensino médio completo",
    x %in% c("7", "07", "superior incompleto") ~ "Superior incompleto",
    x %in% c("8", "08", "superior completo") ~ "Superior completo",
    x %in% c("10", "não se aplica") ~ "Não se aplica",
    x %in% c("9", "09", "ignorado") ~ NA_character_,
    TRUE ~ NA_character_
  )
}


# ---------------------------
# 2. Carregar dados tratados da etapa anterior
# ---------------------------
sinan <- read_csv("data_clean/sinan_limpo.csv", col_types = cols(.default = col_character()))
gal   <- read_csv("data_clean/gal_limpo.csv")

# ---------------------------
# 3. Carregar dicionário de códigos de municípios
# ---------------------------
municipios <- read_csv("data_raw/codmunicipios_brasil_igbe.csv") %>%
  mutate(cod6 = substr(as.character(cod_mun), 1, 6)) %>%
  distinct(cod6, nome_mun)

# ---------------------------
# 4. TRATAMENTO DA BASE SINAN
# ---------------------------
sinan <- sinan %>%
  select(-c(nm_pacient, fonetica_n, nu_notific, id_unidade, id_pais, nduplic_n, dt_digita, dt_transus,
            dt_transdm, dt_transsm, dt_transrm, dt_transrs, dt_transse,
            nu_lote_v, nu_lote_h, nu_lote_ia, id_ocupa_n, outras, outras_des)) %>%
  mutate(id_municip = as.character(id_municip)) %>%
  mutate(
    dt_nasc    = suppressWarnings(parse_date_time(dt_nasc, orders = c("dmy", "ymd", "mdy"))),
    dt_notific = suppressWarnings(parse_date_time(dt_notific, orders = c("dmy", "ymd", "mdy"))),
    dt_diag    = suppressWarnings(parse_date_time(dt_diag, orders = c("dmy", "ymd", "mdy"))),
    dt_inic_tr = suppressWarnings(parse_date_time(dt_inic_tr, orders = c("dmy", "ymd", "mdy"))),
    
    idade_original = as.numeric(nu_idade_n),
    idade_anos = as.integer(interval(dt_nasc, dt_notific) / years(1)),
    
    cs_sexo = recode(as.character(cs_sexo), "M" = "Masculino", "F" = "Feminino", "I" = NA_character_),
    
    cs_raca = recode(as.character(cs_raca),
                     "1" = "Branca", "2" = "Preta", "3" = "Amarela",
                     "4" = "Parda", "5" = "Indígena", "9" = NA_character_),
    
    escolaridade = reclassificar_escolaridade(cs_escol_n),
    ,
    
    forma = recode(as.character(forma),
                   "1" = "Pulmonar", "2" = "Extrapulmonar", "3" = "Pulmonar + Extrapulmonar"),
    
    hiv = recode(as.character(hiv),
                 "1" = "Positivo", "2" = "Negativo",
                 "3" = "Em andamento", "4" = "Não realizado",
                 "9" = NA_character_)
  ) %>%
  mutate(across(where(is.character), ~ padronizar_ausentes(.))) %>%
  left_join(municipios, by = c("id_municip" = "cod6")) %>%
  rename(no_munic = nome_mun) %>%
  mutate(
    faixa_etaria = case_when(
      idade_anos < 5    ~ "0-4 anos",
      idade_anos < 15   ~ "5-14 anos",
      idade_anos < 25   ~ "15-24 anos",
      idade_anos < 45   ~ "25-44 anos",
      idade_anos < 65   ~ "45-64 anos",
      idade_anos >= 65  ~ "65+ anos",
      TRUE              ~ NA_character_
    ) %>% factor(levels = c("0-4 anos", "5-14 anos", "15-24 anos",
                            "25-44 anos", "45-64 anos", "65+ anos"))
  )


# ---------------------------
# 5. TRATAMENTO DA BASE GAL
# ---------------------------
gal <- gal %>%
  mutate(
    data_de_nascimento = suppressWarnings(parse_date_time(data_de_nascimento, orders = c("dmy", "ymd", "mdy"))),
    data_da_solicitacao = suppressWarnings(parse_date_time(data_da_solicitacao, orders = c("dmy", "ymd", "mdy"))),
    data_do_1_sintomas = suppressWarnings(parse_date_time(data_do_1_sintomas, orders = c("dmy", "ymd", "mdy"))),
    data_da_coleta = suppressWarnings(parse_date_time(data_da_coleta, orders = c("dmy", "ymd", "mdy"))),
    data_do_encaminhamento = suppressWarnings(parse_date_time(data_do_encaminhamento, orders = c("dmy", "ymd", "mdy"))),
    data_do_recebimento = suppressWarnings(parse_date_time(data_do_recebimento, orders = c("dmy", "ymd", "mdy"))),
    data_inicio_do_processamento = suppressWarnings(parse_date_time(data_inicio_do_processamento, orders = c("dmy", "ymd", "mdy"))),
    data_do_processamento = suppressWarnings(parse_date_time(data_do_processamento, orders = c("dmy", "ymd", "mdy"))),
    data_da_liberacao = suppressWarnings(parse_date_time(data_da_liberacao, orders = c("dmy", "ymd", "mdy"))),
    sexo = recode(sexo, "M" = "Masculino", "F" = "Feminino", "I" = NA_character_)
  ) %>%
  mutate(across(where(is.character), ~ padronizar_ausentes(.))) %>%
  select(-c(nome_da_pesquisa, observacao, complemento, tipo_doc_paciente_1,
            tipo_doc_paciente_2, hora_da_coleta)) %>%
  mutate(
    faixa_etaria = case_when(
      idade < 5    ~ "0-4 anos",
      idade < 15   ~ "5-14 anos",
      idade < 25   ~ "15-24 anos",
      idade < 45   ~ "25-44 anos",
      idade < 65   ~ "45-64 anos",
      idade >= 65  ~ "65+ anos",
      TRUE         ~ NA_character_
    ) %>% factor(levels = c("0-4 anos", "5-14 anos", "15-24 anos",
                            "25-44 anos", "45-64 anos", "65+ anos"))
  )

# ---------------------------
# 6. Salvar versões tratadas
# ---------------------------
write_csv(sinan, "data_clean/sinan_tratado.csv")
write_csv(gal,   "data_clean/gal_tratado.csv")

# ---------------------------
# 7. Mensagem final
# ---------------------------
cat("\u2714\ufe0f Bases tratadas salvas em:\n")
cat("   → data_clean/sinan_tratado.csv\n")
cat("   → data_clean/gal_tratado.csv\n")
