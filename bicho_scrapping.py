#!/usr/bin/env python
# coding: utf-8

# In[2]:


# ===================================================================
# WEB SCRAPER MELHORADO - resultadofacil.com.br
#
# Versão final que extrai os resultados e adiciona a coluna 'Dezena'
# ===================================================================

import subprocess
import sys
import pandas as pd
import time
import logging
import os
from bs4 import BeautifulSoup
import undetected_chromedriver as uc
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, WebDriverException

def install_packages():
    """Instala os pacotes necessários."""
    packages = ["undetected-chromedriver", "selenium>=4.15.0", "beautifulsoup4", "pandas", "lxml"]
    print("🔧 Instalando pacotes necessários...")
    for package in packages:
        try:
            subprocess.check_call([sys.executable, "-m", "pip", "install", package])
        except subprocess.CalledProcessError as e:
            print(f"❌ Erro ao instalar {package}: {e}")

# Garante que os pacotes estão instalados
install_packages()

class ResultadoFacilScraper:
    def __init__(self, debug_mode=True):
        self.driver = None
        self.debug_mode = debug_mode
        self.setup_logging()

    def setup_logging(self):
        """Configura o logging para debug."""
        level = logging.DEBUG if self.debug_mode else logging.INFO
        logging.basicConfig(
            level=level, 
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.StreamHandler(),
                logging.FileHandler('scraper_debug.log')
            ]
        )
        self.logger = logging.getLogger(__name__)

    def criar_driver(self):
        """Cria uma instância do driver do Chrome com configurações otimizadas."""
        self.logger.info("🚀 Iniciando driver...")
        try:
            options = uc.ChromeOptions()

            # Configurações básicas
            options.add_argument('--no-sandbox')
            options.add_argument('--disable-dev-shm-usage')
            options.add_argument('--disable-gpu')
            options.add_argument('--disable-extensions')
            options.add_argument('--disable-plugins')

            # User agent personalizado
            options.add_argument('--user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36')

            # Configurações de rede
            options.add_argument('--disable-web-security')
            options.add_argument('--allow-running-insecure-content')

            self.driver = uc.Chrome(options=options, version_main=None)
            self.driver.maximize_window()

            # Configura timeouts
            self.driver.set_page_load_timeout(30)
            self.driver.implicitly_wait(10)

            self.logger.info("✅ Driver criado com sucesso.")
            return True
        except Exception as e:
            self.logger.error(f"❌ Erro ao criar driver: {e}")
            return False

    def salvar_debug_html(self, html_content, filename):
        """Salva o HTML para debug."""
        if self.debug_mode and html_content:
            try:
                with open(f"debug_{filename}.html", "w", encoding="utf-8") as f:
                    f.write(html_content)
                self.logger.debug(f"🔍 HTML salvo para debug: debug_{filename}.html")
            except Exception as e:
                self.logger.warning(f"⚠️ Não foi possível salvar HTML de debug: {e}")

    def aguardar_elemento(self, by, value, timeout=30, description="elemento"):
        """Aguarda um elemento aparecer na página com múltiplas tentativas."""
        self.logger.info(f"⏳ Aguardando {description}...")

        selectors_alternativos = {
            "result-card": [
                (By.CLASS_NAME, "result-card"),
                (By.CLASS_NAME, "card"),
                (By.CSS_SELECTOR, ".result-card"),
                (By.CSS_SELECTOR, "div[class*='result']"),
                (By.CSS_SELECTOR, "div[class*='card']"),
                (By.CSS_SELECTOR, "table"),
                (By.TAG_NAME, "main"),
                (By.TAG_NAME, "section")
            ]
        }

        # Se é o elemento principal, tenta múltiplos seletores
        if value == "result-card":
            for by_alt, value_alt in selectors_alternativos["result-card"]:
                try:
                    element = WebDriverWait(self.driver, 5).until(
                        EC.presence_of_element_located((by_alt, value_alt))
                    )
                    self.logger.info(f"✅ {description} encontrado com seletor: {by_alt}='{value_alt}'")
                    return element
                except TimeoutException:
                    self.logger.debug(f"🔍 Tentativa com {by_alt}='{value_alt}' falhou")
                    continue
        else:
            # Para outros elementos, usa o seletor original
            try:
                element = WebDriverWait(self.driver, timeout).until(
                    EC.presence_of_element_located((by, value))
                )
                self.logger.info(f"✅ {description} encontrado")
                return element
            except TimeoutException:
                self.logger.warning(f"⏰ Timeout aguardando {description}")
                return None

    def extrair_html(self, url):
        """Navega até a URL, lida com cookies e extrai o HTML."""
        self.logger.info(f"✈️ Navegando para {url}...")
        try:
            # Navega para a página
            self.driver.get(url)
            time.sleep(3)  # Pausa inicial

            # Salva HTML inicial para debug
            self.salvar_debug_html(self.driver.page_source, "inicial")

            # Tenta lidar com cookies
            self.lidar_com_cookies()

            # Aguarda o carregamento do conteúdo principal
            elemento_principal = self.aguardar_elemento(
                By.CLASS_NAME, "result-card", 30, "cards de resultado"
            )

            if elemento_principal:
                self.logger.info("✅ Conteúdo principal encontrado")
                time.sleep(2)  # Pausa para garantir carregamento completo
                html_content = self.driver.page_source
                self.salvar_debug_html(html_content, "final")
                return html_content
            else:
                self.logger.warning("⚠️ Conteúdo principal não encontrado, tentando extrair HTML mesmo assim...")
                html_content = self.driver.page_source
                self.salvar_debug_html(html_content, "sem_elemento_principal")

                # Analisa o HTML disponível
                self.analisar_estrutura_pagina(html_content)
                return html_content

        except Exception as e:
            self.logger.error(f"❌ Erro ao extrair HTML: {e}")
            # Ainda assim tenta salvar o HTML atual para análise
            try:
                html_content = self.driver.page_source
                self.salvar_debug_html(html_content, "erro")
                return html_content
            except:
                return None

    def lidar_com_cookies(self):
        """Tenta lidar com diferentes tipos de banners de cookies."""
        cookie_selectors = [
            (By.ID, "lgpd-accept"),
            (By.CLASS_NAME, "cookie-accept"),
            (By.CSS_SELECTOR, "button[id*='accept']"),
            (By.CSS_SELECTOR, "button[class*='accept']"),
            (By.CSS_SELECTOR, "button[onclick*='accept']"),
            (By.XPATH, "//button[contains(text(), 'Aceitar')]"),
            (By.XPATH, "//button[contains(text(), 'Accept')]"),
            (By.XPATH, "//a[contains(text(), 'Aceitar')]")
        ]

        self.logger.info("🍪 Tentando lidar com cookies...")

        for by, selector in cookie_selectors:
            try:
                cookie_button = WebDriverWait(self.driver, 3).until(
                    EC.element_to_be_clickable((by, selector))
                )
                cookie_button.click()
                self.logger.info(f"✅ Cookies aceitos usando: {by}='{selector}'")
                time.sleep(1)
                return True
            except TimeoutException:
                continue
            except Exception as e:
                self.logger.debug(f"🔍 Erro ao tentar clicar em cookies com {by}='{selector}': {e}")
                continue

        self.logger.warning("⚠️ Nenhum botão de cookies encontrado")
        return False

    def analisar_estrutura_pagina(self, html_content):
        """Analisa a estrutura da página para entender melhor o layout."""
        if not html_content:
            return

        self.logger.info("🔍 Analisando estrutura da página...")
        soup = BeautifulSoup(html_content, 'lxml')

        # Procura por diferentes elementos que podem conter os dados
        elementos_interesse = [
            ('div[class*="result"]', 'divs com "result" no nome'),
            ('div[class*="card"]', 'divs com "card" no nome'),
            ('table', 'tabelas'),
            ('.container', 'containers'),
            ('.content', 'elementos de conteúdo'),
            ('main', 'elemento main'),
            ('section', 'seções')
        ]

        for selector, descricao in elementos_interesse:
            elementos = soup.select(selector)
            if elementos:
                self.logger.info(f"📋 Encontrado(s) {len(elementos)} {descricao}")
                # Mostra uma amostra do primeiro elemento
                if elementos and len(str(elementos[0])) > 100:
                    sample = str(elementos[0])[:200] + "..."
                    self.logger.debug(f"🔸 Amostra: {sample}")

    def processar_dados_flexivel(self, html_content):
        """Processamento mais flexível que tenta diferentes estruturas."""
        self.logger.info("📊 Processando dados com abordagem flexível...")
        if not html_content:
            self.logger.error("❌ Nenhum conteúdo HTML para processar.")
            return None

        try:
            soup = BeautifulSoup(html_content, 'lxml')

            # Estratégia 1: Procura pela estrutura original
            cards = soup.find_all('div', class_='result-card')
            if cards:
                self.logger.info(f"✅ Encontrados {len(cards)} cards com estrutura original")
                return self.processar_cards_originais(cards)

            # Estratégia 2: Procura por qualquer div com "card" no nome
            cards = soup.find_all('div', class_=lambda x: x and 'card' in x.lower())
            if cards:
                self.logger.info(f"🔍 Encontrados {len(cards)} cards com estrutura alternativa")
                return self.processar_cards_alternativos(cards)

            # Estratégia 3: Procura por tabelas diretamente
            tabelas = soup.find_all('table')
            if tabelas:
                self.logger.info(f"📋 Encontradas {len(tabelas)} tabelas, tentando extrair dados")
                return self.processar_tabelas(tabelas)

            # Estratégia 4: Busca por qualquer estrutura que contenha números de 4 dígitos
            self.logger.info("🎯 Tentando identificar padrões de números do jogo do bicho...")
            return self.processar_por_padroes(soup)

        except Exception as e:
            self.logger.error(f"❌ Erro no processamento dos dados: {e}", exc_info=True)
            return None

    def processar_cards_originais(self, cards):
        """Processa cards com a estrutura original esperada."""
        todos_os_premios = []

        for card in cards:
            header = card.find('div', class_='card-header')
            extracao_nome = header.find('h3').text.strip() if header and header.find('h3') else 'N/A'
            data_hora = header.find('p', class_='text-muted').text.strip() if header and header.find('p') else 'N/A'

            tabela = card.find('table')
            if tabela:
                for linha in tabela.find('tbody').find_all('tr'):
                    celulas = linha.find_all('td')
                    if len(celulas) >= 3:
                        premio_num = celulas[0].text.strip()
                        milhar = celulas[1].text.strip()
                        animal = celulas[2].text.strip()

                        todos_os_premios.append({
                            'Extracao': extracao_nome,
                            'Data_Hora': data_hora,
                            'Premio': premio_num,
                            'Numero': milhar,
                            'Animal': animal
                        })

        return pd.DataFrame(todos_os_premios) if todos_os_premios else None

    def processar_cards_alternativos(self, cards):
        """Processa cards com estruturas alternativas."""
        todos_os_premios = []

        for i, card in enumerate(cards):
            # Extrai qualquer texto que pareça ser data/hora
            data_hora = 'N/A'
            for element in card.find_all(['p', 'span', 'div']):
                text = element.get_text().strip()
                if any(palavra in text.lower() for palavra in [':', 'h', '2024', '2025']):
                    data_hora = text
                    break

            # Procura por tabelas dentro do card
            tabela = card.find('table')
            if tabela:
                for linha in tabela.find_all('tr')[1:]:  # Skip header
                    celulas = linha.find_all(['td', 'th'])
                    if len(celulas) >= 2:
                        # Adapta conforme o número de colunas
                        if len(celulas) >= 3:
                            premio = celulas[0].text.strip()
                            numero = celulas[1].text.strip()
                            animal = celulas[2].text.strip()
                        else:
                            premio = f'Premio_{i}'
                            numero = celulas[0].text.strip()
                            animal = celulas[1].text.strip() if len(celulas) > 1 else 'N/A'

                        todos_os_premios.append({
                            'Extracao': f'Extração_{i+1}',
                            'Data_Hora': data_hora,
                            'Premio': premio,
                            'Numero': numero,
                            'Animal': animal
                        })

        return pd.DataFrame(todos_os_premios) if todos_os_premios else None

    def processar_tabelas(self, tabelas):
        """Processa tabelas diretamente."""
        todos_os_premios = []

        for i, tabela in enumerate(tabelas):
            linhas = tabela.find_all('tr')
            if len(linhas) < 2:  # Precisa ter pelo menos header + 1 linha de dados
                continue

            for j, linha in enumerate(linhas[1:]):  # Skip primeira linha (header)
                celulas = linha.find_all(['td', 'th'])
                if len(celulas) >= 2:
                    dados = [celula.text.strip() for celula in celulas]

                    todos_os_premios.append({
                        'Extracao': f'Tabela_{i+1}',
                        'Data_Hora': time.strftime('%Y-%m-%d %H:%M:%S'),
                        'Premio': f'{j+1}º Prêmio' if j < 5 else 'Outros',
                        'Numero': dados[0] if dados else 'N/A',
                        'Animal': dados[1] if len(dados) > 1 else 'N/A'
                    })

        return pd.DataFrame(todos_os_premios) if todos_os_premios else None

    def processar_por_padroes(self, soup):
        """Última tentativa: procura por padrões típicos do jogo do bicho."""
        import re

        # Padrão para números de 4 dígitos
        padrao_numero = re.compile(r'\b\d{4}\b')
        texto_completo = soup.get_text()
        numeros_encontrados = padrao_numero.findall(texto_completo)

        if numeros_encontrados:
            self.logger.info(f"🎯 Encontrados {len(numeros_encontrados)} números de 4 dígitos")

            todos_os_premios = []
            for i, numero in enumerate(numeros_encontrados[:10]):  # Pega apenas os primeiros 10
                todos_os_premios.append({
                    'Extracao': 'Extraído por padrão',
                    'Data_Hora': time.strftime('%Y-%m-%d %H:%M:%S'),
                    'Premio': f'{i+1}º',
                    'Numero': numero,
                    'Animal': 'A definir'
                })

            return pd.DataFrame(todos_os_premios)

        return None

    def processar_dados(self, html_content):
        """Método principal de processamento com fallbacks."""
        return self.processar_dados_flexivel(html_content)

    def fechar_driver(self):
        """Fecha o driver do navegador de forma segura."""
        if self.driver:
            try:
                self.driver.quit()
                self.logger.info("🔐 Driver fechado com segurança.")
            except Exception as e:
                self.logger.warning(f"⚠️ Erro ao fechar driver: {e}")

def main():
    print("\n" + "="*70)
    print("🚀 WEB SCRAPER MELHORADO - RESULTADO FÁCIL 🚀")
    print("="*70)

    url_alvo = "https://www.resultadofacil.com.br/resultado-do-jogo-do-bicho/RJ"

    scraper = ResultadoFacilScraper(debug_mode=True)

    if scraper.criar_driver():
        try:
            html = scraper.extrair_html(url_alvo)
            if html:
                df_final = scraper.processar_dados(html)

                if df_final is not None and not df_final.empty:
                    # ==========================================================
                    # ADICIONANDO A COLUNA 'DEZENA'
                    # Esta linha extrai os 2 últimos dígitos da coluna 'Numero'
                    # ==========================================================
                    df_final['Dezena'] = df_final['Numero'].str.strip().str[-2:]

                    timestamp = time.strftime('%Y%m%d_%H%M%S')
                    nome_arquivo = f"resultados_facil_RJ_{timestamp}.csv"
                    df_final.to_csv(nome_arquivo, index=False, encoding='utf-8-sig')

                    print("\n" + "="*60)
                    print("🎉 SUCESSO! DADOS EXTRAÍDOS 🎉")
                    print("="*60)
                    print(f"📁 Arquivo salvo: {nome_arquivo}")
                    print(f"📊 {len(df_final)} linhas de dados extraídas.")
                    # Atualiza a impressão para mostrar a nova coluna
                    print(f"📋 Colunas: {', '.join(df_final.columns.tolist())}")
                    print("\n📋 Preview dos dados:")
                    print("-" * 60)
                    print(df_final.head(15).to_string())
                    print("="*60)

                    # Salva também um arquivo de resumo atualizado
                    with open(f"resumo_extracao_{timestamp}.txt", "w", encoding="utf-8") as f:
                        f.write(f"RESUMO DA EXTRAÇÃO - {timestamp}\n")
                        f.write("="*50 + "\n")
                        f.write(f"URL: {url_alvo}\n")
                        f.write(f"Linhas extraídas: {len(df_final)}\n")
                        f.write(f"Colunas: {', '.join(df_final.columns.tolist())}\n\n")
                        f.write("PREVIEW DOS DADOS:\n")
                        f.write(df_final.head(15).to_string())

                else:
                    print("\n⚠️ ATENÇÃO: Dados extraídos mas DataFrame vazio ou inválido.")
                    print("📝 Verifique os arquivos de debug gerados para análise manual.")
            else:
                print("\n❌ FALHA: Não foi possível extrair o conteúdo da página.")
                print("📝 Verifique os logs em 'scraper_debug.log' para mais detalhes.")

        except Exception as e:
            print(f"\n💥 ERRO INESPERADO: {e}")
            scraper.logger.error(f"Erro inesperado no main: {e}", exc_info=True)

        finally:
            scraper.fechar_driver()
    else:
        print("\n❌ FALHA: Não foi possível inicializar o driver do navegador.")

    print("\n🏁 Processo finalizado.")
    print("📋 Arquivos gerados:")
    print("    - scraper_debug.log (logs detalhados)")
    print("    - debug_*.html (HTML das páginas para análise)")
    print("    - *.csv (dados extraídos, se houver)")
    print("    - resumo_extracao_*.txt (resumo da extração)")

if __name__ == "__main__":
    main()


# In[ ]:





# In[ ]:





# In[ ]:




