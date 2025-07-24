# ===================================================================
# --- APLICATIVO SHINY PREDITOR (VERSÃO CORRIGIDA) ---
# Correção do erro no gráfico de clusters
# ===================================================================

# --- Carregando Pacotes Necessários ---
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(caret) 
library(xgboost)
library(class)
library(factoextra)
library(stringi) # Pacote para remover acentos

# ===================================================================
# --- FUNÇÃO DE LIMPEZA E PADRONIZAÇÃO DE DADOS ---
# Esta função DEVE SER IDÊNTICA à do script train_models.R
# ===================================================================
padronizar_dados <- function(df) {
  df %>%
    mutate(
      # Padroniza a coluna 'animal'
      animal_limpo = tolower(stri_trans_general(as.character(animal), "Latin-ASCII")),
      animal_limpo = trimws(animal_limpo),
      animal = case_when(
        animal_limpo == "aguia" ~ "Aguia",
        animal_limpo == "elefant" ~ "Elefante",
        TRUE ~ tools::toTitleCase(animal_limpo)
      ),
      
      # Padroniza a coluna 'Periodo'
      Periodo = tolower(stri_trans_general(as.character(Periodo), "Latin-ASCII")),
      Periodo = trimws(Periodo),
      Periodo = case_when(
        Periodo == "manha" ~ "Manha",
        Periodo == "tarde" ~ "Tarde",
        Periodo == "noite" ~ "Noite",
        TRUE ~ Periodo
      ),
      
      # Padroniza a coluna 'Diasemana'
      Diasemana = tolower(stri_trans_general(as.character(Diasemana), "Latin-ASCII")),
      Diasemana = trimws(Diasemana),
      Diasemana = case_when(
        Diasemana == "segunda" ~ "Segunda",
        Diasemana == "terca" ~ "Terca",
        Diasemana == "quarta" ~ "Quarta",
        Diasemana == "quinta" ~ "Quinta",
        Diasemana == "sexta" ~ "Sexta",
        Diasemana == "sabado" ~ "Sabado",
        Diasemana == "domingo" ~ "Domingo",
        TRUE ~ Diasemana
      )
    ) %>%
    select(-animal_limpo)
}

# ===================================================================
# --- Shiny App UI (User Interface) ---
# ===================================================================
ui <- dashboardPage(
  dashboardHeader(title = "Análise de Sorteios"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visão Geral", tabName = "dados", icon = icon("database")),
      menuItem("Análise de Clusters", tabName = "clusters", icon = icon("sitemap")),
      menuItem("Análise de Sequência", tabName = "sequencia", icon = icon("project-diagram")),
      menuItem("Comparação de Modelos", tabName = "modelos", icon = icon("chart-bar")),
      menuItem("Previsões Futuras", tabName = "previsao", icon = icon("magic")),
      menuItem("Tabela de Animais", tabName = "tabela_animais", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dados",
              h2("Estatísticas Descritivas dos Dados Históricos"),
              fluidRow(
                box(title = "Sorteios por Dia da Semana", status = "info", solidHeader = TRUE, plotOutput("freqDiasemanaPlot")),
                box(title = "Sorteios por Período", status = "info", solidHeader = TRUE, plotOutput("freqPeriodoPlot"))
              ),
              fluidRow(
                box(title = "Sorteios por Animal", status = "success", solidHeader = TRUE, plotOutput("freqAnimalPlot")),
                box(title = "Sorteios por Grupo", status = "success", solidHeader = TRUE, plotOutput("freqGrupoPlot"))
              )
      ),
      tabItem(tabName = "clusters",
              h2("Análise de Agrupamento (Clusters) de Animais por Proximidade"),
              p("Esta análise agrupa os animais com base em seus padrões de sorteios seguintes. Animais no mesmo grupo tendem a ser seguidos por um conjunto similar de outros animais."),
              uiOutput("cluster_main_ui")
      ),
      tabItem(tabName = "sequencia",
              h2("Análise de Sequência: Animal Sorteado vs. Próximo Animal"),
              fluidRow(box(title = "Animal Mais Frequente no Sorteio Seguinte", status = "primary", solidHeader = TRUE, width = 12, plotOutput("sequenciaPlot"))),
              fluidRow(box(title = "Tabela de Transição de Animais", status = "primary", solidHeader = TRUE, width = 12, DTOutput("sequenciaTable")))
      ),
      tabItem(tabName = "modelos",
              h2("Desempenho Comparativo dos Modelos de Machine Learning"),
              fluidRow(
                box(title = "Acurácia dos Modelos", status = "primary", solidHeader = TRUE, plotOutput("acuraciaModelosPlot")),
                box(title = "Tabela de Acurácia", status = "primary", solidHeader = TRUE, DTOutput("acuraciaModelosTable"))
              ),
              fluidRow(box(title = "Matriz de Confusão do Melhor Modelo", status = "warning", solidHeader = TRUE, width = 12, verbatimTextOutput("confusionMatrixOutput")))
      ),
      tabItem(tabName = "previsao",
              h2("Previsões para Novos Dados"),
              fluidRow(box(title = "Previsões do Melhor Modelo", status = "success", solidHeader = TRUE, width = 12, DTOutput("previsoesNovosDados")))
      ),
      tabItem(tabName = "tabela_animais",
              h2("Tabela de Referência de Animais e Dezenas"),
              fluidRow(
                box(title = "Tabela Completa", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("tabelaAnimaisCompleta")
                )
              )
      )
    )
  )
)

# ===================================================================
# --- Shiny App Server Logic ---
# ===================================================================
server <- function(input, output, session) {
  
  # --- Carregando TODOS os objetos PRÉ-TREINADOS ---
  modelo_xgb <- readRDS("modelo_xgb.rds")
  modelo_multinom <- readRDS("modelo_multinom.rds")
  modelo_rf <- readRDS("modelo_rf.rds")
  modelo_svm <- readRDS("modelo_svm.rds")
  modelo_nb <- readRDS("modelo_nb.rds")
  k_knn <- readRDS("k_knn.rds")
  
  sorteio <- readRDS("sorteio_data.rds")
  treino <- readRDS("treino_data.rds")
  teste <- readRDS("teste_data.rds")
  treino_scaled <- readRDS("treino_scaled.rds")
  treino_label_factor <- readRDS("treino_label_factor.rds")
  resultados_comparativos_ordenados <- readRDS("resultados_comparativos.rds")
  dummy_fit <- readRDS("dummy_fit.rds")
  preproc_values <- readRDS("preproc_values.rds")
  all_train_features_names <- readRDS("all_train_features_names.rds")
  
  # --- Mapa de Animais para Emojis ---
  animal_emoji_map <- data.frame(
    stringsAsFactors = FALSE,
    animal = c("Avestruz", "Águia", "Burro", "Borboleta", "Cachorro", "Cabra", "Carneiro", "Camelo", "Cobra", "Coelho", "Cavalo", "Elefante", "Galo", "Gato", "Jacaré", "Leão", "Macaco", "Porco", "Pavão", "Peru", "Touro", "Tigre", "Urso", "Veado", "Vaca"),
    grupo = 1:25,
    emoji = c("🐦", "🦅", "🐴", "🦋", "🐕", "🐐", "🐏", "🐫", "🐍", "🐇", "🐎", "🐘", "🐓", "🐈", "🐊", "🦁", "🐒", "🐖", "🦚", "🦃", "🐂", "🐅", "🐻", "🦌", "🐄")
  )
  
  # --- Tab CLUSTERS ---
  output$cluster_main_ui <- renderUI({
    if (!file.exists("cluster_info.rds")) {
      return(
        fluidRow(
          box(title = "Aviso", status = "warning", solidHeader = TRUE, width = 12,
              p("A análise de clusters não pôde ser executada durante o treinamento.")
          )
        )
      )
    }
    
    cluster_info <- readRDS("cluster_info.rds")
    choices <- c("Todos os Grupos", paste("Grupo", sort(unique(cluster_info$Grupo))))
    
    fluidRow(
      box(title = "Visualização dos Clusters", solidHeader = TRUE, status = "success", width = 12,
          selectInput("cluster_filter", "Filtrar por Grupo:", choices = choices),
          plotOutput("clusterPlot"),
          p("Cada ponto de texto representa um animal. Animais no mesmo cluster (mesma cor) são estatisticamente semelhantes em seus padrões de sorteio seguintes.")
      ),
      box(title = "Composição dos Grupos de Proximidade", solidHeader = TRUE, status = "primary", width = 12,
          DTOutput("clusterTable")
      )
    )
  })
  
  # === CORREÇÃO DO GRÁFICO DE CLUSTER ===
  output$clusterPlot <- renderPlot({
    req(file.exists("cluster_plot_data.rds"))
    
    # Carrega o gráfico salvo
    full_plot <- tryCatch({
      readRDS("cluster_plot_data.rds")
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(full_plot)) {
      # Se não conseguir carregar, cria um gráfico simples informativo
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "Dados de cluster não disponíveis", size = 6) +
        theme_void()
    } else {
      # Verifica se é necessário filtrar
      if (!is.null(input$cluster_filter) && input$cluster_filter != "Todos os Grupos") {
        
        # Carrega informações do cluster
        cluster_info <- tryCatch({
          readRDS("cluster_info.rds")
        }, error = function(e) {
          return(NULL)
        })
        
        if (!is.null(cluster_info)) {
          selected_group_num <- as.numeric(gsub("Grupo ", "", input$cluster_filter))
          
          # Pega os animais do grupo selecionado
          animals_in_group <- cluster_info$Animal[cluster_info$Grupo == selected_group_num]
          
          # Se o gráfico original for um ggplot, tenta modificá-lo
          if (inherits(full_plot, "ggplot")) {
            # Adiciona um título específico para o grupo
            full_plot + 
              ggtitle(paste("Animais no", input$cluster_filter)) +
              theme(plot.title = element_text(hjust = 0.5, size = 14))
          } else {
            # Se não for ggplot, apenas mostra o gráfico original
            full_plot
          }
        } else {
          # Se não conseguir carregar cluster_info, mostra o gráfico original
          full_plot
        }
      } else {
        # Mostra o gráfico completo
        if (inherits(full_plot, "ggplot")) {
          full_plot + 
            ggtitle("Análise de Clusters - Todos os Grupos") +
            theme(plot.title = element_text(hjust = 0.5, size = 14))
        } else {
          full_plot
        }
      }
    }
  })
  
  # Tabela de Cluster
  output$clusterTable <- renderDT({
    req(file.exists("cluster_info.rds"))
    
    cluster_info <- tryCatch({
      readRDS("cluster_info.rds")
    }, error = function(e) {
      return(data.frame(Erro = "Não foi possível carregar os dados de cluster"))
    })
    
    if ("Erro" %in% names(cluster_info)) {
      return(datatable(cluster_info, options = list(dom = 't'), rownames = FALSE))
    }
    
    summary_table <- cluster_info %>%
      group_by(Grupo) %>%
      summarise(Animais = paste(sort(Animal), collapse = ", "), .groups = 'drop') %>%
      rename(`Grupo de Proximidade` = Grupo)
    
    datatable(summary_table, options = list(searching = FALSE, pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  # --- Tab TABELA DE ANIMAIS ---
  output$tabelaAnimaisCompleta <- renderDT({
    tabela_data <- data.frame(
      Grupo = sprintf("%02d", 1:25),
      Animal = c("Avestruz", "Águia", "Burro", "Borboleta", "Cachorro", "Cabra", "Carneiro", "Camelo", "Cobra", "Coelho", "Cavalo", "Elefante", "Galo", "Gato", "Jacaré", "Leão", "Macaco", "Porco", "Pavão", "Peru", "Touro", "Tigre", "Urso", "Veado", "Vaca"),
      Dezenas = c("01-02-03-04", "05-06-07-08", "09-10-11-12", "13-14-15-16", "17-18-19-20", "21-22-23-24", "25-26-27-28", "29-30-31-32", "33-34-35-36", "37-38-39-40", "41-42-43-44", "45-46-47-48", "49-50-51-52", "53-54-55-56", "57-58-59-60", "61-62-63-64", "65-66-67-68", "69-70-71-72", "73-74-75-76", "77-78-79-80", "81-82-83-84", "85-86-87-88", "89-90-91-92", "93-94-95-96", "97-98-99-00")
    )
    datatable(tabela_data, options = list(pageLength = 25, searching = FALSE, dom = 't'), rownames = FALSE)
  })
  
  # --- Tab VISÃO GERAL ---
  output$freqDiasemanaPlot <- renderPlot({
    top_animal_per_day <- sorteio %>% 
      group_by(Diasemana) %>% 
      count(animal, sort = TRUE) %>% 
      slice_max(order_by = n, n = 1, with_ties = FALSE) %>% 
      ungroup() %>% 
      left_join(animal_emoji_map, by = "animal")
    
    day_counts <- sorteio %>% count(Diasemana, name = "total_sorteios")
    emoji_positions <- day_counts %>% left_join(top_animal_per_day, by = "Diasemana")
    
    ggplot(emoji_positions, aes(x = Diasemana, y = total_sorteios, fill = Diasemana)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = ifelse(is.na(emoji), "", emoji)), vjust = -0.5, size = 8) +
      labs(title = "Frequência de Sorteios por Dia da Semana", 
           subtitle = "Emoji representa o animal mais sorteado no dia", 
           x = "Dia da Semana", y = "Número de Sorteios") +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "none")
  })
  
  output$freqGrupoPlot <- renderPlot({
    data_to_plot <- sorteio %>% 
      count(grupo, sort = TRUE) %>% 
      left_join(animal_emoji_map, by = "grupo")
    
    ggplot(data_to_plot, aes(x = reorder(as.factor(grupo), n), y = n, fill = as.factor(grupo))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = ifelse(is.na(emoji), "", emoji)), vjust = 1.5, size = 6, color = "white") +
      labs(title = "Frequência dos Grupos Mais Sorteados", 
           subtitle = "Emoji representa o animal do grupo", 
           x = "Grupo", y = "Frequência") +
      theme_minimal(base_size = 14) + 
      theme(legend.position = "none") + 
      coord_flip()
  })
  
  output$freqPeriodoPlot <- renderPlot({ 
    sorteio %>% 
      ggplot(aes(x=Periodo, fill=Periodo)) + 
      geom_bar() + 
      labs(title = "Frequência de Sorteios por Período") + 
      theme_minimal() 
  })
  
  output$freqAnimalPlot <- renderPlot({ 
    sorteio %>% 
      count(animal, sort=T) %>% 
      ggplot(aes(x=reorder(animal,n),y=n,fill=animal)) + 
      geom_col(show.legend=F) + 
      labs(title="Frequência de Sorteios por Animal", x="Animal", y="Frequência") + 
      coord_flip() + 
      theme_minimal() 
  })
  
  # --- Tab ANÁLISE DE SEQUÊNCIA ---
  sequencia_data <- reactive({
    transitions <- sorteio %>% 
      mutate(animal_seguinte = lead(animal, n = 1)) %>% 
      filter(!is.na(animal_seguinte))
    
    most_frequent_successor <- transitions %>% 
      group_by(animal) %>% 
      count(animal_seguinte, name = "frequencia", sort = TRUE) %>% 
      slice_max(order_by = frequencia, n = 1, with_ties = FALSE) %>% 
      ungroup() %>% 
      rename(Animal_Sorteado = animal, Animal_Seguinte_Mais_Comum = animal_seguinte) %>% 
      left_join(animal_emoji_map %>% select(animal, emoji), by = c("Animal_Sorteado" = "animal")) %>% 
      rename(Emoji_Sorteado = emoji) %>% 
      left_join(animal_emoji_map %>% select(animal, emoji), by = c("Animal_Seguinte_Mais_Comum" = "animal")) %>% 
      rename(Emoji_Seguinte = emoji)
    
    return(most_frequent_successor)
  })
  
  output$sequenciaPlot <- renderPlot({
    ggplot(sequencia_data(), aes(x = reorder(Animal_Sorteado, -frequencia), y = frequencia, fill = Animal_Sorteado)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = ifelse(is.na(Emoji_Sorteado), "", Emoji_Sorteado)), vjust = -0.5, size = 8) +
      labs(
        title = "Se um animal é sorteado, qual o mais provável de vir em seguida?",
        subtitle = "A altura da barra indica a frequência da transição para o animal seguinte mais comum.",
        x = "Animal Sorteado",
        y = "Frequência da Transição"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$sequenciaTable <- renderDT({ 
    datatable(sequencia_data() %>% 
                select(Animal_Sorteado, Emoji_Sorteado, Animal_Seguinte_Mais_Comum, Emoji_Seguinte, frequencia), 
              options = list(pageLength = 25)) 
  })
  
  # --- Tab COMPARAÇÃO DE MODELOS ---
  output$acuraciaModelosPlot <- renderPlot({
    ggplot(resultados_comparativos_ordenados, aes(x = reorder(Modelo, Acuracia), y = Acuracia, fill = Modelo)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.4f", Acuracia)), vjust = -0.5, size = 4) +
      labs(title = "Comparação de Acurácia entre Modelos", x = "Modelo", y = "Acurácia") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$acuraciaModelosTable <- renderDT({ 
    datatable(resultados_comparativos_ordenados, options = list(pageLength = 10, dom = 't')) 
  })
  
  output$confusionMatrixOutput <- renderPrint({
    features <- c(levels(treino$Periodo), levels(treino$Diasemana), levels(treino$animal), "n", "Posicao", "grupo", "Centena", "numero", "Dezena_lag1", "n_lag1")
    factor_cols_to_encode <- c("Periodo", "Diasemana", "animal")
    numeric_features <- setdiff(features, factor_cols_to_encode)
    
    teste_encoded <- data.frame(predict(dummy_fit, newdata = teste))
    teste_processed <- teste %>% select(any_of(numeric_features)) %>% bind_cols(teste_encoded)
    
    missing_in_test <- setdiff(all_train_features_names, colnames(teste_processed))
    if (length(missing_in_test) > 0) { teste_processed[missing_in_test] <- 0 }
    teste_processed <- teste_processed[, all_train_features_names]
    teste_scaled <- predict(preproc_values, teste_processed)
    dtest <- xgb.DMatrix(data = as.matrix(teste_processed))
    
    melhor_modelo_nome <- resultados_comparativos_ordenados$Modelo[1]
    previsoes_melhor_modelo <- switch(
      melhor_modelo_nome,
      "XGBoost" = { previsoes_raw <- predict(modelo_xgb, dtest); as.factor(levels(sorteio$Dezena_factor)[previsoes_raw + 1]) },
      "Random Forest" = predict(modelo_rf, newdata = teste_processed, type = "class"),
      "Multinomial Regression" = predict(modelo_multinom, newdata = teste_processed, type = "class"),
      "SVM" = predict(modelo_svm, newdata = teste_scaled, type = "class"),
      "KNN" = knn(train = treino_scaled, test = teste_scaled, cl = treino_label_factor, k = k_knn),
      "Naive Bayes" = predict(modelo_nb, newdata = teste_processed, type = "class")
    )
    if (!is.null(previsoes_melhor_modelo)) { 
      print(paste("Matriz de Confusão para o melhor modelo:", melhor_modelo_nome))
      confusionMatrix(previsoes_melhor_modelo, teste$Dezena_factor) 
    }
  })
  
  # --- Tab PREVISÕES FUTURAS ---
  output$previsoesNovosDados <- renderDT({
    if (!file.exists("novos_resultados.xlsx")) {
      return(datatable(data.frame(Status = "Arquivo 'novos_resultados.xlsx' não encontrado.")))
    }
    
    novos_sorteios_raw <- read_excel("novos_resultados.xlsx")
    
    novos_sorteios_limpos <- padronizar_dados(novos_sorteios_raw)
    
    novos_sorteios <- novos_sorteios_limpos %>%
      select(-any_of("Emoji")) %>% 
      select(any_of(c("Diasemana", "Periodo", "animal", "Posicao", "grupo", "Dezena", "Centena", "numero"))) %>%
      mutate(
        grupo = as.numeric(grupo), 
        Dezena = as.numeric(Dezena), 
        Centena = as.numeric(Centena), 
        numero = as.numeric(numero),
        animal = factor(animal, levels = levels(treino$animal)),
        Periodo = factor(Periodo, levels = levels(treino$Periodo)),
        Diasemana = factor(Diasemana, levels = levels(treino$Diasemana))
      )
    
    temp_combined_for_freq_new <- bind_rows(sorteio %>% select(Dezena), novos_sorteios %>% select(Dezena))
    frequenciadezenas_para_novos <- temp_combined_for_freq_new %>% count(Dezena, sort = TRUE) %>% rename(n_recalc = n)
    novos_sorteios <- novos_sorteios %>% left_join(frequenciadezenas_para_novos, by = "Dezena") %>% mutate(n = coalesce(n_recalc, 0))
    novos_sorteios <- novos_sorteios %>% arrange(Diasemana, Periodo, Posicao) %>% group_by(Posicao) %>%
      mutate(Dezena_lag1 = lag(Dezena, n = 1), n_lag1 = lag(n, n = 1)) %>% ungroup()
    novos_sorteios$Dezena_lag1[is.na(novos_sorteios$Dezena_lag1)] <- 0
    novos_sorteios$n_lag1[is.na(novos_sorteios$n_lag1)] <- 0
    
    novos_sorteios_encoded <- data.frame(predict(dummy_fit, newdata = novos_sorteios))
    numeric_features_for_new <- setdiff(all_train_features_names, colnames(novos_sorteios_encoded))
    novos_sorteios_processed <- novos_sorteios %>% select(any_of(numeric_features_for_new)) %>% bind_cols(novos_sorteios_encoded)
    
    missing_cols_in_new_processed <- setdiff(all_train_features_names, colnames(novos_sorteios_processed))
    if (length(missing_cols_in_new_processed) > 0) { novos_sorteios_processed[missing_cols_in_new_processed] <- 0 }
    novos_sorteios_processed <- novos_sorteios_processed[, all_train_features_names]
    
    melhor_modelo_nome <- resultados_comparativos_ordenados$Modelo[1]
    previsoes_finais_dezena <- NULL
    
    if (melhor_modelo_nome == "XGBoost") {
      dnew_predict <- xgb.DMatrix(data = as.matrix(novos_sorteios_processed))
      previsoes_finais_raw <- predict(modelo_xgb, dnew_predict)
      previsoes_finais_dezena <- as.factor(levels(sorteio$Dezena_factor)[previsoes_finais_raw + 1])
    } else {
      novos_sorteios_scaled <- predict(preproc_values, novos_sorteios_processed)
      previsoes_finais_dezena <- switch(
        melhor_modelo_nome,
        "Random Forest" = predict(modelo_rf, newdata = novos_sorteios_processed, type = "class"),
        "Multinomial Regression" = predict(modelo_multinom, newdata = novos_sorteios_processed, type = "class"),
        "SVM" = predict(modelo_svm, newdata = novos_sorteios_scaled, type = "class"),
        "KNN" = knn(train = treino_scaled, test = teste_scaled, cl = treino_label_factor, k = k_knn),
        "Naive Bayes" = predict(modelo_nb, newdata = novos_sorteios_processed, type = "class")
      )
    }
    
    if (!is.null(previsoes_finais_dezena)) {
      novos_sorteios_raw$Previsao_Dezena_Final <- previsoes_finais_dezena
    }
    
    datatable(novos_sorteios_raw, options = list(pageLength = 25, scrollX = TRUE))
  })
}

# ===================================================================
# --- Run the application ---
# ===================================================================
shinyApp(ui = ui, server = server)