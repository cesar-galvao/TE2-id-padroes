


################################################################################
#            =====  Criação de lista de dicionários =====
################################################################################

# Funçao para criar uma lista com uma tabela para cada variável do bloco B
# com a tradução dos códigos utilizados nos microdados
gera_dicionario <-
  function(variavel, dicionario) {
    
    
    select(dicionario,-`Descrição da coluna`) |>
      dplyr::rename(Descricao = `Descrição do valor`) |>
      fill(Coluna) |>
      filter(Coluna == variavel) |>                    # Filtra a variável de interesse para criar o dicionário
      pivot_wider(values_from = Valor,                 # Códigos possíveis da variável
                  names_from  = Coluna) |>             # Coluna com o mesmo nome da variável no banco (para viabilizar o join)
      mutate(across(everything(), type.convert)) |>    # Transforma todas as colunas no melhor tipo (objetivo principal: em numéricas)
      rename(!!paste0(variavel, "_edit") := Descricao) # Renomeia a coluna com os valores da descrição para adicionar o sufixo '_edit'
    
  }



#       Cria o dicionário de interesse ====


cria_lista_dicionario <- function(dicionario, variavel){

  # Lista de dicionários (uma variável por elemento da lista)
lista_dicionarios <- 
  map(variavel,                     # Vetor de variáveis do bloco B para iterar no map
      gera_dicionario,                # Função para criar o dicionário
      dicionario = dicionario) |>  # Dicionário completo
  rlang::set_names(variavel)

  return(lista_dicionarios)   

}

################################################################################
#            =====  Criação de coluna com os valores das variáveis ====
################################################################################


# Função para criar coluna editada com os valores das variáveis
cria_coluna_edit <- function(data, dicionario, to_left_join){
  
  tryCatch({  
    
    # A função depende das tabelas de dicionários em formato de lista
    data |> 
      left_join(dicionario[[to_left_join]]) |> # Join com o dicionário da variável com nome a ser indicado no argumento to_left_join
      select(last_col())                     # Recupera só a coluna editada na ordem de linhas do banco
    
  },
  error = function(e) {
    
  }
  )
}




################################################################################
# Função para criar o banco com as colunas editadas da PDAD 2021 ----
################################################################################


cria_banco_coluna_edit <- function(data = pdad, dicionario, variaveis){


lista_dicionarios <- cria_lista_dicionario(dicionario, variaveis)


tabela_resultado <- 
  data %>%
  bind_cols(                          # Acrescenta as novas colunas ao banco de dados original
    map_dfc(
      variaveis,                      # Variáveis  a serem transformadas
      cria_coluna_edit,               # Função para criar as colunas editadas
      data       = data,              # Argumento da função cria_coluna_edit que indica o banco de dados
      dicionario = lista_dicionarios  # Argumento da função cria_coluna_edit que indica o dicionário
    )
  ) 

return(tabela_resultado)

}



################################################################################
# Função para gerar amostra de moradores da PDAD 2021 ----
################################################################################

gera_objeto_amostral <- function(data = pdad, domicilio = FALSE){


if(domicilio){


  # Declarar o desenho inicial para domicílios
amostra_pdad_dom <- survey::svydesign(
  id      = ~ A01nficha,  # Identificador único da unidade amostrada
  strata  = ~ A01setor,  # Identificação do estrato
  weights = ~ PESO_DOM,  # Probabilidade da unidade ser sorteada
  nest    = TRUE,        # Parâmetro de tratamento para dos IDs dos estratos
  data    = filter(data, E05 == 1)  # Declarar a base a ser utilizada
)


  # Ajustar estratos com apenas uma UPA (adjust = centered)
options(survey.lonely.psu = "adjust")


  # Ajusta o formato da amostra para uso das funções da família tidyverse
amostra_pdad_dom <- srvyr::as_survey(amostra_pdad_dom)

return(amostra_pdad_dom)

} else {
   
  ## Plano para domicílio ----

## Plano para moradores ----

  # Declarar o desenho inicial para moradores
sample <- survey::svydesign(
  id      = ~ A01nficha, # Identificador único da unidade amostrada
  strata  = ~ A01setor,  # Identificação do estrato
  weights = ~ PESO_PRE,  # Probabilidade da unidade ser sorteada
  nest    = TRUE,        # Parâmetro de tratamento para dos IDs dos estratos
  data    = data         # Declarar a base a ser utilizada
)

# Criar um objeto para pós-estrato pela população
post_pop <- data %>%
  group_by(POS_ESTRATO) %>%
  summarise(Freq = max(POP_AJUSTADA_PROJ))  # Capturar o total da população


# Declarar o objeto de pós-estrato
# Estamos dizendo nesse passo qual é a população alvo para cada pós-estrato considerado
sample <- survey::postStratify(sample, ~ POS_ESTRATO, post_pop)


  # Ajustar estratos com apenas uma UPA (adjust = centered)
options(survey.lonely.psu = "adjust")


  # Ajusta o formato da amostra pós-estratificada
amostra_pdad <- srvyr::as_survey(sample)

return(amostra_pdad)

}

}

################################################################################
# Função para gerar amostra parcial da PDAD ----
################################################################################

gera_objeto_amostral_parcial <- function(data = pdad, RA_selecionada, domicilio = FALSE){

if(is.null(RA_selecionada))
  stop("Indique, ao menos, uma RA em 2021!")

data <- data %>%
  filter(RA %in% RA_selecionada)


if(domicilio){

  ## Plano para domicílio ----
amostra_dom <- survey::svydesign(
  id      = ~ A01nficha,  # Identificador único da unidade amostrada
  strata  = ~ A01setor,  # Identificação do estrato
  weights = ~ PESO_DOM,  # Probabilidade da unidade ser sorteada
  nest    = TRUE,        # Parâmetro de tratamento para dos IDs dos estratos
  data    = filter(data, E05 == 1)  # Declarar a base a ser utilizada
)

  # Ajustar estratos com apenas uma UPA (adjust = centered)
options(survey.lonely.psu = "adjust")


  # Ajusta o formato da amostra para uso das funções da família tidyverse
amostra_dom <- srvyr::as_survey(amostra_dom)

return(amostra_dom)

  } else {

## Plano para moradores ----

  # Declarar o desenho inicial para moradores
sample <- survey::svydesign(
  id      = ~ A01nficha, # Identificador único da unidade amostrada
  strata  = ~ A01setor,  # Identificação do estrato
  weights = ~ PESO_PRE,  # Probabilidade da unidade ser sorteada
  nest    = TRUE,        # Parâmetro de tratamento para dos IDs dos estratos
  data    = data         # Declarar a base a ser utilizada
)

# Criar um objeto para pós-estrato pela população
post_pop <- data %>%
  group_by(POS_ESTRATO) %>%
  summarise(Freq = max(POP_AJUSTADA_PROJ))  # Capturar o total da população


# Declarar o objeto de pós-estrato
# Estamos dizendo nesse passo qual é a população alvo para cada pós-estrato considerado
sample <- survey::postStratify(sample, ~ POS_ESTRATO, post_pop)


  # Ajustar estratos com apenas uma UPA (adjust = centered)
options(survey.lonely.psu = "adjust")


  # Ajusta o formato da amostra pós-estratificada
amostra <- srvyr::as_survey(sample)

return(amostra)
 

}
}





################################################################################
#   =====  Gera tabelas de totais e percentuais com dados amostrais ====
################################################################################



# Função para gerar totais e proporções
gera_totais_e_percentuais <- 
  function(amostra, variavel, nome_variavel = NULL){
    
    # Transforma o string da variável em símbolo
    # Depois disso, basta usar !!variavel para indicar a variável como simbólica
    variavel <- rlang::sym(variavel)
    
    # Tabela para os estratos mais agregados e para o DF
    # Como a função cascade calcula os grupos em relação ao total, 
    # é necessário calcular os dois dados separadamente
    tb_total <-   
      amostra |>
      select(RA, !!variavel) |>                      # Seleciona as subpopulações e a variável de interesse
      srvyr::group_by(RA, !!variavel) |>   # Agrupa por RA e variável
      srvyr::summarise(n = srvyr::survey_total(vartype = c("ci")),  # Calcula os totais com intervalo de confiança e coeficiente de variação 
                       proporcao = srvyr::survey_mean(vartype = c("ci"))) |>
      srvyr::ungroup() |> 
      srvyr::mutate(
        n    = round(n),
        proporcao    = round(proporcao * 100, digits = 3)
      ) |>
      dplyr::arrange(desc(!!variavel)) |>
      srvyr::select(
        RA,
        !!nome_variavel := !!variavel,
        Total = n,
        "Proporção (%)" = proporcao,
        everything()
      ) |> 
      dplyr::arrange(nome_variavel)
    
    
    return(tb_total)
  }



################################################################################
# =====  Gera tabelas de totais e percentuais para o DF com dados amostrais ====
################################################################################



# Função para gerar totais e proporções
gera_totais_e_percentuais_df <- 
  function(amostra, variavel, nome_variavel = NULL){
    
    # Transforma o string da variável em símbolo
    # Depois disso, basta usar !!variavel para indicar a variável como simbólica
    variavel <- rlang::sym(variavel)
    
    # Tabela para os estratos mais agregados e para o DF
    # Como a função cascade calcula os grupos em relação ao total, 
    # é necessário calcular os dois dados separadamente
    tb_total <-   
      amostra |>
      select( !!variavel) |>                      # Seleciona as subpopulações e a variável de interesse
      srvyr::group_by( !!variavel) |>   # Agrupa por RA e variável
      srvyr::summarise(n = srvyr::survey_total(vartype = c("ci")),  # Calcula os totais com intervalo de confiança e coeficiente de variação 
                       proporcao = srvyr::survey_mean(vartype = c("ci"))) |>
      srvyr::ungroup() |> 
      srvyr::mutate(
        n    = round(n),
        proporcao    = round(proporcao * 100, digits = 3)
      ) |>
      dplyr::arrange(desc(!!variavel)) |>
      srvyr::select(
        !!nome_variavel := !!variavel,
        Total = n,
        "Proporção (%)" = proporcao,
        everything()
      ) |> 
      dplyr::arrange(nome_variavel)
    
    
    return(tb_total)
  }



################################################################################
#   =====  Gera dados de média e quantis com dados amostrais ====
################################################################################


gera_percentis_e_medias <- function(amostra,
                                    variavel,
                                    quantis = c(0, 0.25, 0.5, 0.75, 1)) {
  

  # Transforma o string da variável em símbolo
  # Depois disso, basta usar !!variavel para indicar a variável como simbólica
  variavel <- rlang::sym(variavel)

  
  # Tabela para os estratos mais agregados e para o DF
  # Como a função cascade calcula os grupos em relação ao total, 
  # é necessário calcular os dados de quantis e médias separadamente
  
#------------------------------------------------------------------------
  
  # Tabela dos quantis
tb_quantil <-   
  amostra |>
  srvyr::filter(!(!!variavel) %in% c(77777, 88888, 99999)) |> # Retira variáveis que são categóricas
  srvyr::select(RA, !!variavel) |> 
  srvyr::group_by(RA) |>  # Agrupa por RA
  srvyr::summarise(
      # Calcula os quantis com intervalo de confiança e coeficiente de variação 
    valor = srvyr::survey_quantile(!!variavel,  c(0, 0.25, 0.5, 0.75, 1), 
                                   vartype = c("cv", "ci"))
  ) |>
  srvyr::ungroup() |>
  pivot_longer(cols = contains("cv"),  # Hierarquiza os dados pelo coeficiente de variação
               names_to  = "medida",
               values_to = "cv")  |>
  srvyr::mutate(medida = str_remove(medida, "_cv")) |>  # Remove o sufixo _cv
  pivot_longer(cols = c(contains("_q")),                # Acumula informações
               names_to  = "valor_medida",              # de quantis
               values_to = "valor") |>
    # Separa a coluna valor_medida em duas colunas para retirar repetições
    # geradas anteriormente
  srvyr::mutate(coluna_ajuste = str_extract(valor_medida,
                                            "\\w*_q\\d*")) |>
  srvyr::filter(medida == coluna_ajuste) |> # Retira repetições
  srvyr::select(-coluna_ajuste) |>          # Descarta coluna auxiliar
  separate(valor_medida,
           into = c("residuo", "tipo_medida"), # Identifica quantis únicos
           sep = "r_") |>
  srvyr::select(-residuo,-medida) |>
  srvyr::rename("RA"      = RA,
                "Coef. de variação" = cv)

  # Tabelas para cada quantil e seus respectivos valores de intervalo de confiança
quantil <-
  tb_quantil |>
  srvyr::filter(str_detect(tipo_medida, "q\\d{2,3}$")) 

  # Valor mínimo do intervalo de confiança
quantil_low <-
  tb_quantil |>
  srvyr::filter(str_detect(tipo_medida, "low")) |>
  srvyr::mutate(tipo_medida = str_remove(tipo_medida, "_low")) |>
  srvyr::rename("low" = valor)

  # Valor máximo do intervalo de confiança
quantil_upp <-
  tb_quantil |>
  srvyr::filter(str_detect(tipo_medida, "upp")) |>
  srvyr::mutate(tipo_medida = str_remove(tipo_medida, "_upp")) |>
  srvyr::rename("upp" = valor)

  # Junta as tabelas de quantis e intervalos de confiança
tabela_quantil <-
  quantil |>
  left_join(quantil_low) |>
  left_join(quantil_upp) |> 
  srvyr::mutate(tipo_medida = str_replace(tipo_medida, "q", "Quantil "),
                tipo_medida = str_replace(tipo_medida, "Quantil 00", "Mínimo"),
                tipo_medida = str_replace(tipo_medida, "Quantil 100", "Máximo"),
                `Coef. de variação` = if_else(is.nan(`Coef. de variação`),
                                              NA,
                                              `Coef. de variação`),
                valor = round(valor)) 


#------------------------------------------------------------------------

  # Tabela das médias e intervalo de confiança
tabela_media <-
  amostra |>
  srvyr::filter(!(!!variavel) %in% c(77777, 88888, 99999)) |>
  srvyr::group_by(RA) |>  # Agrupa por RA
  srvyr::summarise(valor = srvyr::survey_mean(!!variavel,
                                            vartype = c("cv", "ci"))) |>
  srvyr::mutate(tipo_medida = "Média") |>
  select(
    "RA" = RA,
    tipo_medida,
    valor,
    "low" = valor_low,
    "upp" = valor_upp,
    "Coef. de variação"  = valor_cv
  )

#------------------------------------------------------------------------

tabela_total <- tabela_media |>
  bind_rows(tabela_quantil) |>
  arrange(RA, valor) |> 
  rename("Valor" = valor,
         "Medida" = tipo_medida) |> 
  ungroup()



return(tabela_total)

}


################################################################################
#            =====  Criação de coluna com os valores das variáveis ====
################################################################################



# Função para formatar a tabela resumo
gera_tabela_resumo <-
  
  function(tabela,
           coluna_principal = "Total",
           colunas_auxiliares = c("Proporção (%)"),
           title,
           subtitle = "Fonte: PDAD 2021, realizada pelo IPEDF.",
           each_n = 2,
           decimals = 0,
           decimals_auxiliaries = 2) {  # Decimais a serem apresentados nos valores principais
    
    # Indicará a última linha da tabela para aplicar cores distinhtas
    n_row <- nrow(tabela)
    
    # Limite para a sequência de linhas coloridas
    n_max <- n_row - each_n 
    
    # Cria o valor inicial de linhas coloridas
    n_initial <- seq(1, n_max, by = 2 * each_n)
    
    rows_to_gray <- map(n_initial, .f = seq, length = each_n) |> # Cria vetores a partir dos valores iniciais
      unlist() # Cria a sequência de dados saltando valores de interesse
    
    
    # Cria a tabela
    gt(tabela) |>
      tab_header(title    = md(title),                          # Ajuste do título
                 subtitle = subtitle)  |>
      tab_style(style     = cell_fill(color = "#f2f2f2"),
                # Alterna linhas entre cinza e branco
                locations = cells_body(rows = rows_to_gray)) |>
      tab_style(style     = list(style = cell_text(weight = "bold")),
                # Negrito nos títulos da tabela
                locations = cells_column_labels(columns = everything()))  |>
      fmt_number(
        columns = coluna_principal,   # Ajuste para formatação em português da exibição de números
        decimals = decimals,
        sep_mark = ".",
        dec_mark = ","
      ) |>
      fmt_number(
        columns = colunas_auxiliares,   # Ajuste para formatação em português da exibição de números
        decimals = decimals_auxiliaries,
        sep_mark = ".",
        dec_mark = ","
      ) |>
      tab_source_note(md("Estatística Aplicada 1-2024."))
    
  }


################################################################################
#            =====  Ajusta layouts dos gráficos ====
################################################################################

# Ajuste dos eixos ====

ajuste_variavel_eixo_x <- 
  theme(axis.text.x  = element_text(     # Ajustes nos labels do eixo x
      angle = 0,
      color = "black",
      size  = 16,
      face  = 1
    ),
    axis.text.y = element_text(     # Ajustes nos labels do eixo y
      angle = 0,
      color = "black",
      size  = 14,
      face  = 1
    ),
     plot.title   = element_text(    # Ajustes no título
      size  = 18,
      face  = "bold",
      hjust = 0.5,
      color = "black"
    ),
    strip.text = element_text(size = 20, face  = "bold")
    ) 



# Eixos gráfico da normal ====


grids_titulos_normal <-   theme(
  axis.text.y = element_blank(),
  axis.text.x = element_text(size = 30),
  axis.title.y =  element_blank(),
  axis.title.x = element_text(size = 30, face = "bold"),
  axis.ticks = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
) 

grids_titulos_normal_invertido <-   theme(
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 30),
  axis.title.x =  element_blank(),
  axis.title.y = element_text(size = 30, face = "bold"),
  axis.ticks = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
) 


# Configuração básica do fundo dos gráficos com o nome no eixo x e valores no eixo y
configuracao_graf_nome_eixo_x <-
  theme_bw() +             # Tema básico
  theme(
    axis.text.y  = element_blank(),  # Ajustes nos eixos
    axis.title.y = element_blank(),
    axis.ticks   = element_blank(),  # Remove axis' ticks
    axis.text.x  = element_text(     # Ajustes nos labels do eixo x
      angle = 0,
      color = "black",
      size  = 12,
      face  = 1
    ),
    legend.title = element_blank(), # Retira legenda
    plot.title   = element_text(    # Ajustes no título
      size  = 18,
      face  = "bold",
      hjust = 0.5,
      color = "black"
    ),
    strip.text = element_text(size = 11,      # Ajustes no título do facet_wrap
                              face = "bold"), 
    plot.caption = element_text(hjust = 0),   # Ajustes no caption
    plot.margin = unit(c(1.0, 1.0, 1.0, 0.5), "cm"),
    panel.grid.major = element_blank(),       # Remove gridlines do fundo
    panel.grid.minor = element_blank()
  )

# Configuração básica do fundo dos gráficos com o nome no eixo y e valores no eixo x
configuracao_graf_nome_eixo_y <-
  theme_bw() +             # Tema básico
  theme(
    axis.text.x  = element_blank(), # Ajustes nos eixos
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),   # Remove axis ticks
    axis.text.y  = element_text(
      angle = 0,
      color = "black",
      size  = 12,
      face  = 1
    ),
    legend.title = element_blank(),
    plot.title   = element_text(    # Ajustes no título
      size  = 18,
      face  = "bold",
      hjust = 0.5,
      color = "black"
    ),
    strip.text = element_text(size = 11,
                              face = "bold"),
    plot.caption = element_text(hjust = 0),  # Ajustes no caption
    plot.margin = unit(c(1.0, 1.0, 1.0, 0.5), "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )  



