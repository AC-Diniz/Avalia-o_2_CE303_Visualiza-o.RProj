# 1. SETUP: CARREGAMENTO DE PACOTES E PREPARAÇÃO DE DADOS -----------------

# Carregar pacotes --------------------------------------------------------
library(shiny)
library(tidyverse)
library(readr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(shinydashboard)

# Código de preparação de dados -------------------------------------------
#Carregar os dados
df_bruto <- read_csv("Life-Expectancy-Data-Updated.csv")
View(df_bruto)


#Seleção, limpeza e média histórica
df_medias <- df_bruto%>%
  #renomear colunas 
  rename(
    Expectativa_Vida = Life_expectancy,
    Mortalidade_Infantil = Infant_deaths,
    Escolaridade = Schooling,
    Regiao = Region,
    Pais = Country
  )%>%
  #Criar a coluna de Statis de Renda Categórica (usando colunas binárias)
  #Se o Status for "Developed", é Alta Renda, senão é Baixo/Médio renda
  mutate(
    Status_Renda_Simples = case_when(
      Economy_status_Developed == 1 ~ "Alta Renda",
      Economy_status_Developing == 1 ~ "Baixo/Média Renda",
      TRUE ~ "Não Classificado"
    )
  )%>%
  #selecionar apenas as colunas que serão utilizadas na análise das médias
  select(
    Pais, Regiao, Status_Renda_Simples,
    Expectativa_Vida, Mortalidade_Infantil, Escolaridade
  )%>%
  #remover observações com NA nas variáveis quantitativas
  drop_na(Expectativa_Vida, Mortalidade_Infantil, Escolaridade)%>%
  group_by(Pais, Regiao, Status_Renda_Simples)%>%
  summarise(
    Expectativa_Vida_Media = mean(Expectativa_Vida, na.rm=TRUE),
    Mortalidade_Infantil_Media = mean(Mortalidade_Infantil, na.rm=TRUE),
    Escolaridade_Media = mean(Escolaridade, na.rm=TRUE),
    .groups = 'drop'
  )

# Código de preparação geográfica -----------------------------------------

#Obtenção do mapa mundial (geometria)
mapa_mundial <- ne_countries(scale = "medium", returnclass = "sf")

#Selecionar apenas as colunas relevantes do mapa (nome do país e geometria)
mapa_mundial <- mapa_mundial%>%
  select(sovereignt, geometry)%>%
  rename(Pais_Mapa = sovereignt)

# Juntar os dados de médias (df_medias) com os dados do mapa (mapa_mundial)
# A chave de união é o nome do país. Isso exige limpeza posterior dos nomes.
df_mapa <- mapa_mundial%>%
  left_join(df_medias, by = c("Pais_Mapa" = "Pais"))



# 2. INTERFACE DO USUÁRIO (UI) --------------------------------------------


ui <- fluidPage(

    # Título
    titlePanel("DashBoard de Expectativa de Vida Global (OMS/Banco Mundial)"),

    # Barra lateral com controle para selecionar os intervalos
    sidebarLayout(
      
        sidebarPanel(
          h3("Filtros de Análise"),
          
          #FILTRO 1: Região
          selectInput("filtro_regiao",
                      "Filtrar por Região:",
                      choices = unique(df_medias$Regiao),
                      multiple = TRUE,
                      selected = unique(df_medias$Regiao)
                      ),
          
          #FILTRO 2: Status Renda
          selectInput("filtro_renda",
                      "Filtrar por Status de Renda:",
                      choices = unique(df_medias$Status_Renda_Simples),
                      multiple = TRUE,
                      selected = unique(df_medias$Status_Renda_Simples)
                      )
        ),

        # Painel principal para os gráficos
        mainPanel(
           
          #Layout para os KPIs (Análise 6)
          fluidRow(
            #criação de 3 value boxes (Expetativa, Mortalidade e Escolaridade)
            valueBoxOutput("kpi_expectativa", width=4),
            valueBoxOutput("kpi_mortalidade", width=4),
            valueBoxOutput("kpi_mortalidade", width=4)
          ),
          
          #Mapa (Análise 5)
          tabsetPanel(
            tabPanel("Mapa Interativo e Correlações",
                     h4("Mapa da Expectativa de Vida Média (2000-2015)"),
                     leafletOutput("mapa_expectativa"),
                     
                     hr(),
                     
                     #Gráfico de Bolhas (Análise 7)
                     h4("Análise Multivarida: Mortalidade vs. Escolaridade por Expectativa de Vida"),
                     plotOutput("grafico_bolhas")
                     ),
            tabPanel("Detalhes e Distribuições",
                     #Linha 1: Histrograma (Análise 1) e Box-plot (Análise 3)
                     fluidRow(
                       column(6, h4("Distribuição da Expectativa de Vida"), plotOutput("hist_expectativa")),
                       column(6, h4("Boxplot por Região"), plotOutput("boxplot_regiao"))
                     ),
                     
                     hr(),
                     
                     #Linha 2: Dispersão (Análise 2) e Dispersão (Análise 4)
                     fluidRow(
                       column(6, h4("Expectativa de Vida vs. Escolaridade"), plotOutput("scatter_escolaridade")),
                       column(6, h4("Mortalidade Infantil vs. Expectativa de Vida"), plotOutput("scatter_mortalidade"))
                     )
                     )
          )
        )
    )
)

# Lógica do servidor
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # gerar os intervalos
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # histograma com os intervalos
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Rodar
shinyApp(ui = ui, server = server)
