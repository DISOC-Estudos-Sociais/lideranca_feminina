#==============================================================================
# CONSTANTES
#==============================================================================

variaveis = c("Ano",
              "Trimestre",
              "UF",
              "Capital",
              "RM_RIDE",
              "V1023",  # tipo de área
              "V2007",  # sexo
              "V2010",  # cor ou raça
              "V2009",  # idade
              "V3001",  # sabe ler e escrever
              "V3002",  # frequenta escola
              "V3002A", # escola que frequenta (publica x privada)
              "V3003",  # curso que frequenta
              "V3003A", # curso que frequenta
              "V4010",  # Código de ocupação
              "V4012",  # Posição na ocupação
              "VD3004", # etapa de ensino concluída
              "VD3005", # anos de estudo
              "VD4001", # Condição em relação à força de trabalho
              "VD4002", # Condição de ocupação
              "VD4009", # Tipo de Ocupação
              "VD4010", # Setor da economia
              "VD4011", # Grupamento por cargos 
              "VD4019", # Rendimento mensal habitual de todos os trabalhos
              "VD4020") # Rendimento mensal efetivo de todos os trabalhos

#==============================================================================
# 1. BAIXA ARQUIVOS RAW
#==============================================================================

get_data_pnadc <- function(ano) {
  PNADcIBGE::get_pnadc(
    year = ano,
    vars = variaveis,
    quarter = 4,
    design = TRUE,
    deflator = TRUE,
    label = FALSE
  )
}


#==============================================================================
# 2. LIMPA E ORGANIZA RÓTULOS
#==============================================================================
process_data <- function(design){
  design |> 
    # Utiliza o pacote syrvr()
    as_survey() |> 
    #filtra pop ocupada
    srvyr::filter (V2009 >= 14 & VD4002==1) |> 
    mutate(BR = 1,
           NE = ifelse(UF %in% 21:29, 1, NA),
           CE = ifelse(UF == 23, 1, NA),
           # variáveis de faixa etária dentro do grupo de jovens
           fetaria = factor(case_when(V2009 %in% 15:17 ~ "15-17",
                                      V2009 %in% 18:24 ~ "18-24",
                                      V2009 %in% 25:29 ~ "25-29",
                                      V2009 %in% 15:29 ~ "15-29")),
           sexo = factor(case_when(V2007==1 ~ "homem",
                                   V2007==2 ~ "mulher")),
           cor = factor(case_when(V2010==1 ~ "branco ",
                                  V2010==2 | V2010==4 ~ "preto ou pardo",
                                  V2010==5 | V2010==3 ~ "indigena ou asiático")),
           rec_geo = factor(case_when(Capital==23 ~ "Capital",
                                      V1023==2 ~ "RMF",
                                      V1023==4 ~ "Interior")),
           ## variáveis de analise educacional
           estuda = factor(case_when(V3002==1 ~ "estuda",
                                     V3002==2 ~ "nao estuda")),
           analfab = ifelse(V3001=="2", 1, 0),
           escolaridade = as.factor(VD3004),
           anos_estudo = as.numeric (VD3005),
           ## variáveis de mercado de trabalho
           forcatrab = factor(case_when(VD4001==1 ~ "forca trabalho",
                                        VD4001==2 ~ "fora da forca")),
           #ocupado = factor(case_when(VD4002==1 ~ "ocupado",
           #VD4002==2 ~ "desocupado")),
           ativ_ocup = factor(case_when(VD4001==1 & VD4002==1 ~ "ocupado",
                                        VD4001==1 & VD4002==2 ~ "desocupado",
                                        VD4001==2 ~ "inativo")),
           condicao = factor(case_when(
             V3002==1 & (VD4001==1 & VD4002==1) ~ "estuda e trabalha",
             (V3002==1 & (VD4001==1 & VD4002==2)) | (V3002==1 & VD4001==2) ~ "apenas estuda",
             V3002==2 & VD4002==1 ~ "apenas trabalha",
             (V3002==2 & (VD4001==1 & VD4002==2)) | (V3002==2 & (VD4001==2)) ~ "nem-nem")),
           #Condição de informalidade
           informal = factor (ifelse(VD4009=="02"| VD4009=="04" | VD4009=="06" | VD4009=="09", 1, 0)),
           #Setor da economia
           setor_economia = factor(case_when(VD4010=="01" ~ "Agricultura",
                                             VD4010=="02" ~ "Indústria",
                                             VD4010=="03" ~ "Construção",
                                             VD4010=="04" ~ "Comércio, reparação de veículos",
                                             VD4010=="05" ~ "Transportador, armazenagem e correio",
                                             VD4010=="06" ~ "Alojamento e alimentação",
                                             VD4010=="07" ~ "Atividades financeiras, imobiliarias e adm",
                                             VD4010=="08" ~ "Administração publica, seguridade social",
                                             VD4010=="09" ~ "Educação, Saúde e serviços sociais",
                                             VD4010=="10" ~ "Outros Serviços",
                                             VD4010=="11" ~ "Serviços domésticos",
                                             VD4010=="12" ~ "Mal definidas")),
           # rendimentos
           rendim_trabh = ifelse(!is.na(VD4019), (VD4019 * Habitual), NA),
           rendim_trabe = ifelse(!is.na(VD4020), (VD4020 * Efetivo), NA)
           ) |> 
    mutate (tipo_trab = factor(case_when( V4012==1 ~ "Trabalho Domestico",
                                          V4012==2 ~ "Militar",
                                          V4012==3 ~ "Empregado do setor privado",
                                          V4012==4 ~ "Empregado do setor publico",
                                          V4012==5 ~ "Empregador",
                                          V4012==6 ~ "Conta propria",
                                          V4012==7 ~ "Trabalhador familiar nao remunerado"))) |> 
    mutate (cod_ocupacao = factor(case_when(V4010 %in% 1111:1439 ~ "diretores e gerentes",
                                            V4010 %in% 2111:2659 ~ "profissionais da ciencia",
                                            V4010 %in% 3111:3522 ~ "profissionais lvl medio",
                                            V4010 %in% 4110:4419 ~ "apoio administrativo",
                                            V4010 %in% 5111:5419 ~ "comércio/serviços",
                                            V4010 %in% 6111:6225 ~ "trab. qualificados agro",
                                            V4010 %in% 7111:7549 ~ "construção, mecanicas e outros",
                                            V4010 %in% 8111:8350 ~ "instalações, maquinas e montadores",
                                            V4010 %in% 9111:9629 ~ "ocupações elementares",
                                            V4010 %in% c("0000",
                                                         "0110",
                                                         "0210",
                                                         "0411",
                                                         "0412",
                                                         "0511",
                                                         "0512") ~ "forças armadas, policiais e bombeiros")))
}

# =============================================================================
# FUNÇÕES AUXILIARES DA PIPELINE
# =============================================================================

anos_alvo <- c(2019, 2024, 2025)

# --- Download e processamento por ano ---

baixar_e_processar <- function(ano) {
  design <- get_data_pnadc(ano)
  process_data(design)
}

# --- Output 1: Distribuição % de sexo por ocupação ---

calcular_dist_ocupacao <- function(survey_list) {
  purrr::map_dfr(anos_alvo, function(ano) {
    sv <- survey_list[[as.character(ano)]]
    
    purrr::map_dfr(
      list(
        list(nome = "Brasil",   filtro = quote(BR == 1)),
        list(nome = "Nordeste", filtro = quote(NE == 1)),
        list(nome = "Ceará",    filtro = quote(CE == 1))
      ),
      function(recorte) {
        sv |>
          filter(!!recorte$filtro) |>
          filter(!is.na(cod_ocupacao), !is.na(sexo)) |>
          group_by(sexo, cod_ocupacao) |>
          summarise(n = survey_total(vartype = "ci"), .groups = "drop") |>
          group_by(sexo) |>
          mutate(pct = n / sum(n) * 100) |>
          ungroup() |>
          mutate(regiao = recorte$nome, ano = ano)
      }
    )
  })
}

# --- Output 2: % mulheres em liderança por recorte geográfico ---

calcular_lideranca_mulheres <- function(survey_list) {
  purrr::map_dfr(anos_alvo, function(ano) {
    sv <- survey_list[[as.character(ano)]]
    
    purrr::map_dfr(
      list(
        list(nome = "Brasil",   filtro = quote(BR == 1)),
        list(nome = "Nordeste", filtro = quote(NE == 1)),
        list(nome = "Ceará",    filtro = quote(CE == 1))
      ),
      function(recorte) {
        sv |>
          filter(!!recorte$filtro) |>
          filter(!is.na(cod_ocupacao), !is.na(sexo)) |>
          mutate(lideranca = cod_ocupacao == "diretores e gerentes") |>
          group_by(sexo, lideranca) |>
          summarise(n = survey_total(vartype = "ci"), .groups = "drop") |>
          filter(lideranca == TRUE) |>
          group_by(lideranca) |>
          mutate(pct_mulheres = n / sum(n) * 100) |>
          filter(sexo == "mulher") |>
          select(sexo, pct_mulheres) |>
          mutate(regiao = recorte$nome, ano = ano)
      }
    )
  })
}

# --- Output 3: Gerência por setor e sexo ---

calcular_gerencia_setor <- function(survey_list) {
  purrr::map_dfr(anos_alvo, function(ano) {
    sv <- survey_list[[as.character(ano)]]
    
    purrr::map_dfr(
      list(
        list(nome = "Brasil",   filtro = quote(BR == 1)),
        list(nome = "Nordeste", filtro = quote(NE == 1)),
        list(nome = "Ceará",    filtro = quote(CE == 1))
      ),
      function(recorte) {
        sv |>
          filter(!!recorte$filtro) |> 
          filter(cod_ocupacao == "diretores e gerentes",
                 !is.na(setor_economia), !is.na(sexo)) |>
          group_by(sexo, setor_economia) |>
          summarise(n = survey_total(vartype = "ci"), .groups = "drop") |>
          group_by(setor_economia) |>
          mutate(pct = n / sum(n) * 100) |>
          ungroup() |>
          mutate(regiao = recorte$nome, ano = ano)
      })
  })
}

# --- Output 4: Média de rendimentos por gênero e recorte ---

calcular_rendimentos <- function(survey_list) {
  purrr::map_dfr(anos_alvo, function(ano) {
    sv <- survey_list[[as.character(ano)]]
    
    purrr::map_dfr(
      list(
        list(nome = "Brasil",   filtro = quote(BR == 1)),
        list(nome = "Nordeste", filtro = quote(NE == 1)),
        list(nome = "Ceará",    filtro = quote(CE == 1))
      ),
      function(recorte) {
        sv |>
          filter(!!recorte$filtro) |>
          filter(cod_ocupacao == "diretores e gerentes") |> 
          filter(!is.na(sexo), !is.na(rendim_trabh)) |>
          group_by(sexo) |>
          summarise(media_rend = survey_mean(rendim_trabh,
                                             vartype = "ci",
                                             na.rm = TRUE),
                    .groups = "drop") |>
          mutate(regiao = recorte$nome, ano = ano)
      }
    )
  })
}

# --- Funções de visualização ---

plot_lideranca <- function(dados) {
  dados |>
    mutate(
      regiao = factor(regiao, levels = c("Brasil", "Nordeste", "Ceará")),
      ano    = factor(ano)
    ) |>
    ggplot(aes(x = ano, y = pct_mulheres, fill = regiao)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(aes(label = sprintf("%.1f%%", pct_mulheres)),
              position = position_dodge(width = 0.75),
              vjust = -0.4, size = 3.2, fontface = "bold") +
    scale_fill_manual(
      values = c("Brasil" = "#1d6fa4", "Nordeste" = "#f28b30", "Ceará" = "#2ca444")
    ) +
    scale_y_continuous(labels = label_percent(scale = 1),
                       limits = c(0, 60),
                       expand = expansion(mult = c(0, .05))) +
    labs(
      title    = "Participação Feminina em Cargos de Liderança",
      subtitle = "Diretores e Gerentes — 4º trimestre de cada ano",
      x        = NULL,
      y        = "% de mulheres",
      fill     = "Recorte geográfico",
      caption  = "Fonte: PNAD Contínua/IBGE. Valores deflacionados."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )
}

plot_rendimentos <- function(dados) {
  dados |>
    mutate(
      regiao = factor(regiao, levels = c("Brasil", "Nordeste", "Ceará")),
      ano    = factor(ano),
      sexo   = factor(sexo, levels = c("homem", "mulher"),
                      labels = c("Homens", "Mulheres"))
    ) |>
    ggplot(aes(x = ano, y = media_rend, fill = sexo)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_errorbar(
      aes(ymin = media_rend_low, ymax = media_rend_upp),
      position = position_dodge(width = 0.75),
      width = 0.25, linewidth = 0.5, colour = "grey40"
    ) +
    geom_text(aes(y     = media_rend,
                  label = scales::dollar(media_rend, prefix = "",
                                         big.mark = ".", decimal.mark = ",")),
              position = position_dodge(width = 0.75),
              hjust = 0, vjust = -0.5, size = 2.8) +
    facet_wrap(~regiao) +
    scale_fill_manual(values = c("Homens" = "#1d6fa4", "Mulheres" = "#e05c8a")) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "R$ ",
                                                      big.mark = ".",
                                                      decimal.mark = ","),
                       expand = expansion(mult = c(0, .1))) +
    labs(
      title    = "Média de Rendimentos Mensais dos Cargos de Liderança por Gênero",
      subtitle = "Rendimento habitual de todos os trabalhos — 4º trimestre de cada ano",
      x        = NULL,
      y        = "Rendimento médio (R$)",
      fill     = NULL,
      caption  = "Fonte: PNAD Contínua/IBGE. Valores deflacionados."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      strip.text    = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      legend.position = "top"
    )
}
