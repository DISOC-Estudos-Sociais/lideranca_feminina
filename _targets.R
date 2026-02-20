# =============================================================================
# PIPELINE TARGETS - ANÁLISE DE GÊNERO NO MERCADO DE TRABALHO (PNADC)
# =============================================================================
# Outputs:
#   1. Tabela: distribuição % de ocupações por sexo (2019, 2024, 2025)
#   2. Gráfico: % mulheres em cargos de liderança - CE, NE, BR (2019, 2024, 2025)
#   3. Tabela: distribuição % de gerência/diretoria por sexo e setor (2019, 2024, 2025)
#   4. Gráfico: média de rendimentos por gênero - CE, NE, BR (2019, 2024, 2025)
# =============================================================================

library(targets)
library(tarchetypes)

# Pacotes necessários disponíveis em todos os targets
tar_option_set(
  packages = c(
    "PNADcIBGE",
    "srvyr",
    "survey",
    "dplyr",
    "tidyr",
    "ggplot2",
    "scales",
    "forcats",
    "stringr"
  )
)

# Carrega as funções de download e limpeza
source("R/functions.R")

# =============================================================================
# TARGETS
# =============================================================================

list(
  
  # ── 1. Download e processamento dos dados ──────────────────────────────────
  
  tar_target(survey_2019, baixar_e_processar(2019)),
  tar_target(survey_2024, baixar_e_processar(2024)),
  tar_target(survey_2025, baixar_e_processar(2025)),
  
  # Agrupa em lista nomeada para facilitar iteração
  tar_target(
    survey_list,
    list(
      "2019" = survey_2019,
      "2024" = survey_2024,
      "2025" = survey_2025
    )
  ),
  
  # ── 2. Cálculos analíticos ─────────────────────────────────────────────────
  
  tar_target(tab_dist_ocupacao,   calcular_dist_ocupacao(survey_list)),
  tar_target(tab_lideranca,       calcular_lideranca_mulheres(survey_list)),
  tar_target(tab_gerencia_setor,  calcular_gerencia_setor(survey_list)),
  tar_target(tab_rendimentos,     calcular_rendimentos(survey_list)),
  
  # ── 3. Visualizações ───────────────────────────────────────────────────────
  
  tar_target(
    plot_lideranca_mulheres,
    plot_lideranca(tab_lideranca)
  ),
  
  tar_target(
    plot_rend_genero,
    plot_rendimentos(tab_rendimentos)
  ),
  
  # ── 4. Exportação dos outputs ──────────────────────────────────────────────
  
  # Tabelas em CSV
  tar_target(
    export_tab_ocupacao,
    {
      readr::write_csv(tab_dist_ocupacao, "outputs/tab1_dist_ocupacao.csv")
      "outputs/tab1_dist_ocupacao.csv"
    },
    format = "file"
  ),
  
  tar_target(
    export_tab_gerencia,
    {
      readr::write_csv(tab_gerencia_setor, "outputs/tab3_gerencia_setor.csv")
      "outputs/tab3_gerencia_setor.csv"
    },
    format = "file"
  ),
  
  # Gráficos em PNG
  tar_target(
    export_plot_lideranca,
    {
      ggplot2::ggsave(
        "outputs/fig2_lideranca_mulheres.png",
        plot   = plot_lideranca_mulheres,
        width  = 10, height = 6, dpi = 150, bg = "white"
      )
      "outputs/fig2_lideranca_mulheres.png"
    },
    format = "file"
  ),
  
  tar_target(
    export_plot_rendimentos,
    {
      ggplot2::ggsave(
        "outputs/fig4_rendimentos_genero.png",
        plot   = plot_rend_genero,
        width  = 12, height = 7, dpi = 150, bg = "white"
      )
      "outputs/fig4_rendimentos_genero.png"
    },
    format = "file"
  )
)