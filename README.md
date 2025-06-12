# 🧬 Análise da Qualidade dos Dados de Tuberculose no Amazonas (2007–2023)

Repositório do projeto de Iniciação Científica desenvolvido na Fiocruz Amazônia. O objetivo central é avaliar a qualidade dos dados dos sistemas SINAN e GAL relacionados à tuberculose no estado do Amazonas entre 2007 e 2023, com foco em completude, consistência, padronização e disponibilização dos resultados conforme os princípios FAIR.

---

## 🎯 Objetivos do Projeto

- Avaliar **completude e inconsistência** das variáveis nas bases SINAN e GAL;
- Executar **análises descritivas** com recorte temporal e geográfico;
- Documentar os dados com **dicionário de variáveis e metadados padronizados**;
- Desenvolver um **dashboard interativo** para exploração analítica;
- Promover **reprodutibilidade científica** com scripts automatizados e modularizados.

---

## 📁 Estrutura do Repositório

```bash
├── data/                   # Scripts de extração e pré-processamento (sem dados brutos)
├── scripts/                # Códigos para análise estatística e geração de outputs
├── outputs/
│   ├── dashboard/          # Dashboard interativo (HTML/Markdown)
│   └── relatorios/         # Relatórios analíticos e técnicos (PDF/HTML)
├── metadata/               # Dicionário de dados, esquemas e codebooks
├── README.md               # Arquivo de apresentação do projeto
└── LICENSE                 # Licença MIT

🧪 Tecnologias Utilizadas
Linguagem: R

IDE: RStudio

Principais pacotes:

tidyverse, dplyr, ggplot2, janitor, codebook, lubridate

shiny

📊 Acesso ao Dashboard
O dashboard interativo contém filtros e visualizações específicas para a tuberculose no Amazonas (2007–2024).

🔗 Link (): https:

🔐 Privacidade e Compartilhamento de Dados
Os dados sensíveis não são disponibilizados neste repositório em conformidade com os princípios de privacidade e confidencialidade. Apenas outputs agregados, scripts e metadados anonimizados são publicados.

📄 Documentação Técnica


📌 Resultados e Considerações Técnicas


♻️ Reprodutibilidade
Todos os scripts seguem boas práticas de organização, com modularização e uso de renv para controle de dependências (opcional). Recomenda-se clonar o repositório e executar os scripts sequencialmente a partir da pasta /scripts.

🧾 Licença
Este projeto está licenciado sob a MIT License.

👨‍🔬 Sobre
Projeto de Iniciação Científica desenvolvido no âmbito do ILMD/Fiocruz Amazônia, com foco em epidemiologia computacional, vigilância em saúde e transparência de dados.

📬 Contato
Dúvidas ou colaborações:

