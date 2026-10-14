library(fmsb)
library(tidyr)
library(xlsx)

data<- read.csv(file.choose(),header = TRUE)


dt<- data %>% pivot_wider(names_from = Defeitos, 
                          values_from = Frequencia.defeitos
                          )
dt <- rbind(rep(0.1926,28),rep(0,28),dt)


radarchart(dt,
           axistype = 3,
           seg = 4,
           cglty = 1, #tipo de linha de grade
           cglcol = "black", #Cor da linha de grade
           cglwd = 1, #largura da linha de grade
           plty = 16, #tipo de linha
           pcol = "black", #Cor da linha
           plwd = 1, #largura da linha
           pfcol = rgb(0, 0.4, 1, 0.25)
          )







#install.packages("devtools")
#devtools::install_github("ricardo-bion/ggradar")

library(ggradar)

ggradar(dt,
        values.radar = c("0%","25%","50%", "75%", "100%")
)



##############################################################################
# Instalar e carregar a biblioteca necessária
if (!require(fmsb)) install.packages("fmsb")
library(fmsb)
library(tidyr)


# Simulando o dataset que será usado no Power BI
# No Power BI, substitua pelo dataset importado automaticamente
 dataset <- data.frame(
   `Q/C` = c("QC1", "QC2", "QC3", "QC4", "QC5"),
   valor = c(80, 65, 70, 90, 85)
 )

# Calcular os percentuais em relação ao total
dataset$percentual <- dataset$valor / sum(dataset$valor) * 100

dt<- dataset %>% pivot_wider(names_from = Q.C, 
                             values_from = valor
)
 

dt <- dataset %>%
  pivot_longer(
    cols = c( Q.C, percentual,valor), # Selecionar as colunas a serem transpostas
    names_to = "Q.C",              # Nome da nova coluna com os nomes das variáveis
    values_to = "valor"                 # Nome da nova coluna com os valores
  )

# Preparar os dados para o gráfico de radar
# Criar os limites superior e inferior com a mesma estrutura que a tabela principal
max_min <- data.frame(
  percentual = c(100, 0) # Limites máximo e mínimo
)

# Adicionar os dados calculados
data_radar <- rbind(
  max_min,
  data.frame(percentual = dataset$percentual)
)

# Ajustar os rótulos das linhas
rownames(data_radar) <- c("Máximo", "Mínimo", as.character(dataset$`Q/C`))

# Criar o gráfico de radar
radarchart(
  data_radar,
  axistype = 1,
  pcol = "blue",          # Cor da linha
  pfcol = rgb(0, 0, 1, 0.3), # Cor do preenchimento com transparência
  plwd = 2,               # Largura da linha
  cglcol = "grey",        # Cor das linhas de grade
  cglty = 1,              # Tipo das linhas de grade
  cglwd = 0.8,            # Largura das linhas de grade
  axislabcol = "black",   # Cor dos rótulos dos eixos
  vlcex = 0.8             # Tamanho dos rótulos das variáveis
)

# Adicionar título ao gráfico
title("Gráfico de Radar no Power BI")
