# ==============================================================================
# AUTOMAÇÃO DE GRÁFICOS MACROECONÔMICOS - LMF
# Versão: 2.0 (Correção de sobreposição de rótulos com ggrepel)
# ==============================================================================

# 1. Preparação do Ambiente e Pacotes
# Adicionando 'ggrepel' à lista de dependências
pacotes_necessarios <- c("tidyverse", "dplyr", "rbcb", "quantmod", 
                         "plotly", "htmlwidgets", "lubridate", "ggrepel")

pacotes_instalar <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[,"Package"])]
if(length(pacotes_instalar)) install.packages(pacotes_instalar)

# Carregando as bibliotecas explicitamente para evitar erros de namespace
library(tidyverse)
library(dplyr)
library(lubridate)
library(rbcb)
library(quantmod)
library(plotly)
library(htmlwidgets)
library(ggrepel) # <-- Essencial para resolver a sobreposição

# Cria a pasta onde os arquivos HTML serão salvos
dir.create("graficos_html_lmf", showWarnings = FALSE)

# 2. Definição do Padrão Visual da LMF
tema_lmf <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", color = "#003366", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold", color = "#333333"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E0E0E0", linetype = "dashed"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom"
    )
}

# 3. Extração dos Dados do Banco Central (SGS)
# IPCA (12 meses): 13522 | Selic: 432 | Câmbio: 1
# IBC-Br (com ajuste sazonal): Geral = 24364, Agro = 29602, Indústria = 29604, Serviços = 29606
codigos_sgs <- c(
  IPCA_Acumulado_12M = 13522, 
  Selic = 432, 
  Cambio = 1, 
  IBC_Br_Geral = 24364,
  IBC_Br_Agro = 29602,
  IBC_Br_Ind = 29604,
  IBC_Br_Serv = 29606
)

data_inicio <- Sys.Date() - years(5)

# Puxando os dados (pode demorar um pouco dependendo da API do BC)
message("Acessando Banco Central (SGS)...")
dados_sgs <- rbcb::get_series(codigos_sgs, start_date = data_inicio)

# 4. Extração e Tratamento do IBOV (Yahoo Finance)
message("Acessando Yahoo Finance para IBOV...")
getSymbols("^BVSP", src = "yahoo", from = data_inicio, auto.assign = TRUE)
dados_ibov <- data.frame(
  date = index(BVSP), 
  valor = as.numeric(BVSP$BVSP.Adjusted) 
) %>%
  mutate(indicador = "IBOV") %>%
  drop_na() # Remove datas sem negociação

# 5. Tratamento de Dados (Preparando para os gráficos)
message("Tratando os dados...")
# Padronizando o nome da segunda coluna para "valor" nas extrações do BC
for (i in seq_along(dados_sgs)) {
  colnames(dados_sgs[[i]]) <- c("date", "valor")
}

# Empilhando as séries do IBC-Br para o gráfico múltiplo
df_ibc_br <- bind_rows(
  dados_sgs$IBC_Br_Geral %>% mutate(Setor = "Geral"),
  dados_sgs$IBC_Br_Agro %>% mutate(Setor = "Agropecuária"),
  dados_sgs$IBC_Br_Ind %>% mutate(Setor = "Indústria"),
  dados_sgs$IBC_Br_Serv %>% mutate(Setor = "Serviços")
)

# Removendo as séries individuais para limpar a lista de processamento
dados_sgs$IBC_Br_Geral <- NULL
dados_sgs$IBC_Br_Agro <- NULL
dados_sgs$IBC_Br_Ind <- NULL
dados_sgs$IBC_Br_Serv <- NULL


# 6. Função para Gerar e Salvar os Gráficos Individuais
gerar_grafico_html <- function(dados, nome_indicador, titulo_grafico) {
  
  ultimo_dado <- dados %>% filter(date == max(date))
  
  grafico_estatico <- ggplot(dados, aes(x = date, y = valor)) +
    geom_line(color = "#003366", size = 1) + 
    labs(title = titulo_grafico, x = "Data", y = "Valor") +
    scale_x_date(expand = expansion(mult = c(0.05, 0.15))) + 
    tema_lmf()
  
  grafico_interativo <- ggplotly(grafico_estatico) %>% 
    layout(hovermode = "x unified") %>%
    add_annotations(
      x = as.numeric(ultimo_dado$date), # A conversão numérica que corrige a posição
      y = ultimo_dado$valor,
      text = paste0("<b>", round(ultimo_dado$valor, 2), "</b>"),
      xanchor = "left",
      yanchor = "middle",
      xshift = 10, 
      showarrow = FALSE,
      font = list(color = "#003366", size = 12),
      bgcolor = "rgba(255, 255, 255, 0.9)",
      bordercolor = "#003366",
      borderpad = 3
    ) %>%
    config(toImageButtonOptions = list(format = "png", width = 1200, height = 800))
  
  caminho_arquivo <- paste0("graficos_html_lmf/", nome_indicador, ".html")
  saveWidget(grafico_interativo, file = caminho_arquivo, selfcontained = TRUE)
}

# 7. Execução Final da Geração dos Gráficos
message("A gerar os gráficos individuais...")

for (nome in names(dados_sgs)) {
  gerar_grafico_html(dados_sgs[[nome]], nome, paste("Evolução -", nome))
}
gerar_grafico_html(dados_ibov, "IBOV", "Evolução - Ibovespa")

message("A gerar o gráfico do IBC-Br com empilhamento inteligente...")

# Isola e ordena os dados do menor para o maior valor para organizar as etiquetas
ultimos_dados_ibc <- df_ibc_br %>% 
  group_by(Setor) %>% 
  filter(date == max(date)) %>%
  arrange(valor)

# Define a distância mínima vertical entre as caixas (em pontos do índice)
distancia_minima <- 1.5

# Cria uma coluna dedicada apenas para a posição vertical da etiqueta no ecrã
ultimos_dados_ibc$y_label <- ultimos_dados_ibc$valor

# Algoritmo para empurrar a etiqueta para cima se estiver muito colada à de baixo
for (i in 2:nrow(ultimos_dados_ibc)) {
  if (ultimos_dados_ibc$y_label[i] - ultimos_dados_ibc$y_label[i-1] < distancia_minima) {
    ultimos_dados_ibc$y_label[i] <- ultimos_dados_ibc$y_label[i-1] + distancia_minima
  }
}

cores_setor <- c("Geral" = "#003366", "Agropecuária" = "#2ca02c", 
                 "Indústria" = "#d62728", "Serviços" = "#ff7f0e")

grafico_ibc_estatico <- ggplot(df_ibc_br, aes(x = date, y = valor, color = Setor)) +
  geom_line(size = 1) +
  labs(title = "Evolução - IBC-Br e Aberturas Setoriais", x = "Data", 
       y = "Índice (Com Ajuste Sazonal)", color = "") +
  scale_color_manual(values = cores_setor) +
  scale_x_date(expand = expansion(mult = c(0.05, 0.15))) +
  tema_lmf()

grafico_ibc_interativo <- ggplotly(grafico_ibc_estatico) %>% 
  layout(hovermode = "x unified") %>%
  add_annotations(
    x = as.numeric(ultimos_dados_ibc$date),
    y = ultimos_dados_ibc$y_label, # Utiliza a nova coluna ajustada para a posição
    text = paste0("<b>", round(ultimos_dados_ibc$valor, 2), "</b>"),
    xanchor = "left",
    yanchor = "middle",
    xshift = 10,
    showarrow = FALSE,
    font = list(color = unname(cores_setor[ultimos_dados_ibc$Setor]), size = 12),
    bgcolor = "rgba(255, 255, 255, 0.9)",
    bordercolor = unname(cores_setor[ultimos_dados_ibc$Setor]),
    borderpad = 3
  ) %>%
  config(toImageButtonOptions = list(format = "png", width = 1200, height = 800))

saveWidget(grafico_ibc_interativo, file = "graficos_html_lmf/IBC_Br_Completo.html", selfcontained = TRUE)

message("Processo finalizado com sucesso!")