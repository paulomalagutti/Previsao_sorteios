# ===================================================================
# --- SCRIPT DE TREINAMENTO DE MODELOS (VERSÃO FINAL) ---
# Funcionalidade: Limpeza e padronização centralizada de dados
# para garantir consistência.
# ===================================================================

# --- Carregando Pacotes Necessários ---
message("Carregando pacotes...")
library(readxl)
library(dplyr)
library(caret)
library(xgboost)
library(nnet)
library(randomForest)
library(e1071)
library(class)
library(tidyr)
library(factoextra)
library(stringi) 
library(tibble)

# ===================================================================
# --- FUNÇÃO DE LIMPEZA E PADRONIZAÇÃO DE DADOS ---
# Esta função será usada tanto no treinamento quanto na previsão.
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
        # Adicione outras correções comuns aqui se necessário
        TRUE ~ tools::toTitleCase(animal_limpo) # Deixa a primeira letra maiúscula
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
    select(-animal_limpo) # Remove a coluna de limpeza auxiliar
}


# --- 1. Data Loading and Initial Preprocessing ---
message("Carregando e processando os dados de 'Pasta1.xlsx'...")
sorteio_raw <- read_excel("Pasta1.xlsx")

# Aplica a função de limpeza
sorteio <- padronizar_dados(sorteio_raw)

# Continua o processamento com os dados já limpos
sorteio <- sorteio %>%
  select(-any_of("Emoji")) %>%
  select(Diasemana, Periodo, animal, everything()) %>%
  select(-any_of("numero"), any_of("numero")) %>%
  mutate(
    grupo = as.numeric(grupo),
    Dezena = as.numeric(Dezena),
    Centena = as.numeric(Centena),
    numero = as.numeric(numero),
    animal = as.factor(animal),
    Periodo = factor(Periodo, levels = c("Manha", "Tarde", "Noite")),
    Diasemana = factor(Diasemana, levels = c("Domingo", "Segunda", "Terca", "Quarta", "Quinta", "Sexta", "Sabado"))
  )

frequenciadezenas_total <- sorteio %>%
  filter(!is.na(Dezena)) %>%
  count(Dezena, sort = TRUE) %>%
  rename(n_total = n)

sorteio <- sorteio %>%
  left_join(frequenciadezenas_total, by = "Dezena") %>%
  mutate(n = as.numeric(n_total))

sorteio$n[is.na(sorteio$n)] <- 0

sorteio <- sorteio %>%
  arrange(Diasemana, Periodo, Posicao) %>%
  group_by(Posicao) %>%
  mutate(
    Dezena_lag1 = lag(Dezena, n = 1),
    n_lag1 = lag(n, n = 1)
  ) %>%
  ungroup()

sorteio$Dezena_lag1[is.na(sorteio$Dezena_lag1)] <- 0
sorteio$n_lag1[is.na(sorteio$n_lag1)] <- 0
sorteio$Dezena_factor <- as.factor(sprintf("%02d", sorteio$Dezena))

# --- 2. Splitting Data for Training and Testing ---
message("Separando dados em treino e teste...")
set.seed(123)

# Filtra qualquer linha onde a dezena (nossa variável alvo) seja NA
sorteio_limpo <- sorteio %>% filter(!is.na(Dezena_factor))

# Cria a partição a partir dos dados limpos
trainIndex <- createDataPartition(sorteio_limpo$Dezena_factor, p = .7, list = FALSE)
treino <- sorteio_limpo[trainIndex, ]
teste <- sorteio_limpo[-trainIndex, ]

# --- 3. Preprocessing and Feature Engineering ---
message("Criando e salvando objetos de pré-processamento...")
factor_cols_to_encode <- c("Periodo", "Diasemana", "animal")

features_to_encode_final <- c()
for (col in factor_cols_to_encode) {
  if (nlevels(droplevels(treino[[col]])) >= 2) {
    features_to_encode_final <- c(features_to_encode_final, col)
  } else {
    message(paste("AVISO: A coluna '", col, "' tem menos de 2 níveis no conjunto de treino e será ignorada na criação de dummies."))
  }
}

formula_str <- if(length(features_to_encode_final) > 0) paste("~", paste(features_to_encode_final, collapse = " + ")) else "~ ."
features <- c(features_to_encode_final, "n", "Posicao", "grupo", "Centena", "numero", "Dezena_lag1", "n_lag1")

dummy_fit <- dummyVars(as.formula(formula_str), data = treino, fullRank = FALSE)
saveRDS(dummy_fit, "dummy_fit.rds")

treino_encoded <- data.frame(predict(dummy_fit, newdata = treino))
teste_encoded <- data.frame(predict(dummy_fit, newdata = teste))

numeric_features <- setdiff(features, features_to_encode_final)
treino_processed <- treino %>% select(all_of(numeric_features)) %>% bind_cols(treino_encoded)
teste_processed <- teste %>% select(all_of(numeric_features)) %>% bind_cols(teste_encoded)

all_train_features_names <- colnames(treino_processed)
missing_in_test <- setdiff(all_train_features_names, colnames(teste_processed))
if (length(missing_in_test) > 0) { teste_processed[missing_in_test] <- 0 }
extra_in_test <- setdiff(colnames(teste_processed), all_train_features_names)
if (length(extra_in_test) > 0) { teste_processed <- teste_processed %>% select(-all_of(extra_in_test)) }
teste_processed <- teste_processed[, all_train_features_names]

preproc_values <- preProcess(treino_processed, method = c("center", "scale"))
saveRDS(preproc_values, "preproc_values.rds")

# --- 4. Início do Treinamento dos 6 Modelos ---
message("\n--- INICIANDO TREINAMENTO DE TODOS OS 6 MODELOS (PODE DEMORAR) ---")
num_class_dezena <- length(levels(sorteio$Dezena_factor))
resultados_comparativos <- data.frame(Modelo = character(), Acuracia = numeric(), stringsAsFactors = FALSE)

treino_scaled <- predict(preproc_values, treino_processed)
teste_scaled <- predict(preproc_values, teste_processed)
treino_label_factor <- treino$Dezena_factor
teste_label_factor <- teste$Dezena_factor
treino_label_xgb <- as.numeric(treino_label_factor) - 1
teste_label_xgb <- as.numeric(teste_label_factor) - 1
dtrain <- xgb.DMatrix(data = as.matrix(treino_processed), label = treino_label_xgb)
dtest <- xgb.DMatrix(data = as.matrix(teste_processed), label = teste_label_xgb)

message("Treinando Modelo 1: XGBoost...")
params_xgb <- list(objective = "multi:softmax", num_class = num_class_dezena, eta = 0.1, max_depth = 6)
modelo_xgb <- xgb.train(params = params_xgb, data = dtrain, nrounds = 150, print_every_n = 50)
saveRDS(modelo_xgb, "modelo_xgb.rds")
previsoes_raw_xgb <- predict(modelo_xgb, dtest)
previsoes_dezena_xgb <- as.factor(levels(sorteio$Dezena_factor)[previsoes_raw_xgb + 1])
acuracia_xgb <- sum(previsoes_dezena_xgb == teste_label_factor) / nrow(teste)
resultados_comparativos[nrow(resultados_comparativos) + 1,] <- c("XGBoost", acuracia_xgb)

message("Treinando Modelo 2: Multinomial Regression...")
treino_multinom <- bind_cols(treino_processed, Dezena_factor = treino_label_factor)
modelo_multinom <- nnet::multinom(Dezena_factor ~ ., data = treino_multinom, MaxNWts = 20000, trace = FALSE)
saveRDS(modelo_multinom, "modelo_multinom.rds")
previsoes_multinom <- predict(modelo_multinom, newdata = teste_processed, type = "class")
acuracia_multinom <- sum(previsoes_multinom == teste_label_factor) / nrow(teste)
resultados_comparativos[nrow(resultados_comparativos) + 1,] <- c("Multinomial Regression", acuracia_multinom)

message("Treinando Modelo 3: Random Forest...")
treino_rf <- bind_cols(treino_processed, Dezena_factor = treino_label_factor)
modelo_rf <- randomForest(Dezena_factor ~ ., data = treino_rf, ntree = 500)
saveRDS(modelo_rf, "modelo_rf.rds")
previsoes_rf <- predict(modelo_rf, newdata = teste_processed, type = "class")
acuracia_rf <- sum(previsoes_rf == teste_label_factor) / nrow(teste)
resultados_comparativos[nrow(resultados_comparativos) + 1,] <- c("Random Forest", acuracia_rf)

message("Treinando Modelo 4: SVM...")
treino_svm <- bind_cols(treino_scaled, Dezena_factor = treino_label_factor)
modelo_svm <- e1071::svm(Dezena_factor ~ ., data = treino_svm, kernel = "radial")
saveRDS(modelo_svm, "modelo_svm.rds")
previsoes_svm <- predict(modelo_svm, newdata = teste_scaled, type = "class")
acuracia_svm <- sum(previsoes_svm == teste_label_factor) / nrow(teste)
resultados_comparativos[nrow(resultados_comparativos) + 1,] <- c("SVM", acuracia_svm)

message("Treinando Modelo 5: KNN...")
k_knn <- 5
saveRDS(k_knn, "k_knn.rds")
saveRDS(treino_scaled, "treino_scaled.rds")
saveRDS(treino_label_factor, "treino_label_factor.rds")
previsoes_knn <- class::knn(train = treino_scaled, test = teste_scaled, cl = treino_label_factor, k = k_knn)
acuracia_knn <- sum(previsoes_knn == teste_label_factor) / nrow(teste)
resultados_comparativos[nrow(resultados_comparativos) + 1,] <- c("KNN", acuracia_knn)

message("Treinando Modelo 6: Naive Bayes...")
treino_nb <- bind_cols(treino_processed, Dezena_factor = treino_label_factor)
modelo_nb <- e1071::naiveBayes(Dezena_factor ~ ., data = treino_nb)
saveRDS(modelo_nb, "modelo_nb.rds")
previsoes_nb <- predict(modelo_nb, newdata = teste_processed, type = "class")
acuracia_nb <- sum(previsoes_nb == teste_label_factor) / nrow(teste)
resultados_comparativos[nrow(resultados_comparativos) + 1,] <- c("Naive Bayes", acuracia_nb)

# --- 5. Finalizar e Salvar Objetos Adicionais ---
message("Salvando objetos de dados finais para o app...")
resultados_comparativos$Acuracia <- as.numeric(resultados_comparativos$Acuracia)
resultados_comparativos_ordenados <- resultados_comparativos %>% arrange(desc(Acuracia))

saveRDS(sorteio, "sorteio_data.rds")
saveRDS(treino, "treino_data.rds")
saveRDS(teste, "teste_data.rds")
saveRDS(resultados_comparativos_ordenados, "resultados_comparativos.rds")
saveRDS(all_train_features_names, "all_train_features_names.rds")

# --- 6. ANÁLISE DE CLUSTERS (LÓGICA DE PROXIMIDADE) ---
message("\n--- INICIANDO ANÁLISE DE CLUSTERS POR PROXIMIDADE ---")

transitions <- sorteio %>%
  filter(!is.na(animal)) %>%
  mutate(animal_seguinte = lead(animal)) %>%
  filter(!is.na(animal_seguinte))

transition_counts <- transitions %>%
  count(animal, animal_seguinte)

total_transitions <- transitions %>%
  count(animal, name = "total")

transition_probs <- transition_counts %>%
  left_join(total_transitions, by = "animal") %>%
  mutate(prob = n / total) %>%
  select(animal, animal_seguinte, prob)

proximity_profiles <- transition_probs %>%
  pivot_wider(names_from = animal_seguinte, values_from = prob, values_fill = 0)

if (nrow(proximity_profiles) < 2) {
  message("AVISO: Dados insuficientes para a análise de clusters de proximidade. Esta etapa será pulada.")
} else {
  message(paste("Perfis de proximidade criados para", nrow(proximity_profiles), "animais. Prosseguindo..."))
  
  animal_names <- proximity_profiles$animal
  proximity_profiles_numeric <- proximity_profiles %>% select(-animal)
  
  scaled_proximity_profiles <- scale(proximity_profiles_numeric)
  rownames(scaled_proximity_profiles) <- animal_names
  
  max_k <- min(10, nrow(scaled_proximity_profiles) - 1)
  
  if (max_k >= 2) {
    tryCatch({
      set.seed(123)
      elbow_plot <- fviz_nbclust(scaled_proximity_profiles, kmeans, method = "wss", k.max = max_k)
      saveRDS(elbow_plot, "elbow_plot_data.rds")
      
      k_ideal <- min(4, max_k)
      kmeans_result <- kmeans(scaled_proximity_profiles, centers = k_ideal, nstart = 25)
      
      cluster_plot <- fviz_cluster(kmeans_result, data = scaled_proximity_profiles,
                                   geom = "text",
                                   ellipse.type = "confidence",
                                   ggtheme = theme_minimal(),
                                   main = "Clusters de Animais por Proximidade de Sorteio")
      
      saveRDS(cluster_plot, "cluster_plot_data.rds")
      
      cluster_info <- data.frame(
        Animal = animal_names,
        Grupo = kmeans_result$cluster
      )
      saveRDS(cluster_info, "cluster_info.rds")
      
      message("Análise de clusters por proximidade concluída com sucesso!")
      
    }, error = function(e) {
      message("ERRO na análise de clusters: ", e$message)
    })
  } else {
    message("AVISO: Número insuficiente de animais para criar clusters significativos.")
  }
}

# --- Mensagens Finais ---
print("\n--- Resultados Comparativos de Acurácia ---")
print(resultados_comparativos_ordenados)

message("\n--- TREINAMENTO E ANÁLISE CONCLUÍDOS ---")
message("Todos os arquivos .rds necessários foram criados e estão prontos para o app Shiny.")
