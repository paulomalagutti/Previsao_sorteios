🎯 Projeto de Análise e Previsão de Sorteios
Este projeto é uma plataforma completa de análise de dados e Machine Learning, desenvolvida em R com Shiny, para explorar, entender e prever resultados de sorteios.

📌 Objetivo do Projeto
Transformar dados brutos de sorteios em insights acionáveis e previsões estatísticas.

A ferramenta foi criada para:

Analisar Tendências: Identificar padrões nos dados históricos de sorteios.

Descobrir Relações Ocultas: Usar clusterização para encontrar grupos com comportamentos semelhantes.

Prever Resultados: Treinar e comparar múltiplos modelos de Machine Learning para prever a dezena mais provável.

Apresentar Resultados: Disponibilizar tudo em um dashboard interativo e fácil de usar.

🔍 Coleta de Dados (Web Scraping)
Para garantir análises com dados reais e atualizados, foi implementado um processo de Web Scraping.

O que é? Técnica automatizada para extrair informações de sites.

Como foi feito? Script em Python que acessa páginas com resultados de sorteios, lê tabelas e extrai dados relevantes (animal, dezena, período etc.).

Resultado: Os dados são limpos, organizados e salvos em Pasta1.xlsx, que é a fonte principal para o projeto.

🧱 Estrutura do Projeto
O projeto é dividido em dois scripts principais:

train_models.R
Função: Responsável por todo o processamento e treinamento dos modelos.

Etapas:

Carrega e trata os dados do arquivo Pasta1.xlsx.

Treina 6 modelos de Machine Learning (XGBoost, Random Forest etc.).

Realiza análise de clusters.

Salva os resultados em arquivos .rds.

Recomenda-se executar este script uma única vez ou sempre que houver novos dados.

appBicho.R
Função: Inicia o dashboard interativo.

Detalhes:

Carrega os arquivos .rds gerados anteriormente.

Exibe os resultados de forma interativa (gráficos, tabelas e previsões).

Este é o script que você roda para abrir o aplicativo no navegador.

🤖 Modelos de Machine Learning
Seis modelos foram implementados para análise preditiva, visando prever a próxima dezena sorteada:

Modelo	Descrição
XGBoost	Modelo de alto desempenho baseado em árvores de decisão.
Random Forest	Conjunto de árvores que melhora estabilidade e reduz overfitting.
SVM	Encontra o melhor plano para separar as classes.
KNN	Classifica com base nos vizinhos mais próximos.
Naive Bayes	Modelo probabilístico rápido e eficiente.
Regressão Multinomial	Generalização da regressão logística para múltiplas classes.

🧪 Análise de Clusters (K-Means por Proximidade)
Uma abordagem inovadora para entender padrões sequenciais entre os animais:

Pergunta-chave: “Quando o animal X é sorteado, qual é o próximo mais provável?”

Como funciona:

Calcula a probabilidade de cada animal suceder outro.

Cria uma “assinatura de proximidade” para cada animal.

Agrupa os animais com assinaturas semelhantes usando K-Means.

Resultado: Identificação de “panelinhas” de animais que tendem a aparecer em sequência.

📊 Resultados e Desempenho
🏆 Melhor Modelo: O XGBoost apresentou a melhor performance nos testes, com a maior acurácia.

📈 Insights dos Clusters: Revelaram padrões não aleatórios, indicando possíveis tendências de sequência entre os sorteios.

🖥️ O Aplicativo (Dashboard Shiny)
O dashboard integra todos os componentes do projeto em uma interface amigável:

Visão Geral: Frequência por animal, dia e período.

Clusters: Grupos de proximidade com visualização interativa.

Sequência: Probabilidades de sucessão entre animais.

Comparação de Modelos: Acurácia de cada algoritmo.

Previsões Futuras: Previsão usando XGBoost com base em novos dados (novos_resultados.xlsx).

Tabela de Animais: Referência rápida dos códigos e nomes.

✅ Conclusão
Este projeto é uma demonstração prática do poder da ciência de dados:

Automatiza coleta de dados via Web Scraping.

Aplica algoritmos avançados de classificação e clusterização.

Oferece uma plataforma robusta para tomada de decisão.

Separa a lógica de treinamento e interface para garantir performance e usabilidade.

Explore os dados. Visualize padrões. Faça previsões.

🚀 Como Executar
Execute train_models.R para treinar os modelos (requer Pasta1.xlsx).

Execute appBicho.R para abrir o dashboard Shiny.
