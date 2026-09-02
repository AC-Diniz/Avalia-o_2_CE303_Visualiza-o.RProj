
# Carregamento dos pacotes e dados ----------------------------------------
library(tidyverse)
library(readr)

df_bruto <- read_csv("Life-Expectancy-Data-Updated.csv")
View(df_bruto)
summary(df_bruto)


# Seleção, limpeza e Média histórica --------------------------------------
df_medias <- df_bruto%>%
  #renomear colunas 
  rename(
    Expectativa_Vida = Life_expectancy,
    Mortalidade_Infantil = Infant_deaths,
    Escolaridade = Schooling,
    Regiao = Region,
    Pais = Country
  )%>%
  #Criar a coluna de Statis de Renda Categórica
  #Se o Status for "Developed", é Alta renda, se não é Baixo/Médio renda - usar condicional
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
  ) #lembrar: drop remove os agrupamentos
#Verificando o novo data frame
head(df_medias)
nrow(df_medias)  #número de observações médias

# Preparação Geográfica para o Mapa ---------------------------------------
install.packages("rnaturalearth")
install.packages("sf")
install.packages("leaflet")
install.packages("rnaturalearthdata")

library(rnaturalearthdata)
library(rnaturalearth)
library(sf)
library(leaflet) #pacote para mapas interativos no shiny

#Obtenção do mapa mundial (geometria)
mapa_mundial <- ne_countries(scale = "medium", returnclass = "sf")

#Selecionar apenas as colunas relevantes do mapa (nome do país e geometria)
mapa_mundial <- mapa_mundial%>%
  select(sovereignt, geometry)%>%
  rename(Pais_Mapa = sovereignt)

# Juntar os dados de médias (df_medias) com os dados do mapa (mapa_mundial)
# A chave de união é o nome do país -> primary-key né??
df_mapa <- mapa_mundial%>%
  left_join(df_medias, by = c("Pais_Mapa" = "Pais"))

#Visualizar a união
head(df_mapa)
sum(!is.na(df_mapa$Expectativa_Vida_Media))
nrow(df_medias)
