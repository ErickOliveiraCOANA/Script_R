# O código a seguir para criar um dataframe e remover as linhas duplicadas sempre é executado e age como um preâmbulo para o script: 

# dataset <- data.frame(PALLET, Defeitos, QC, Cliente_1)
# dataset <- unique(dataset)

# Cole ou digite aqui seu código de script:

library(dplyr)
library(tidyr)
library(fmsb)

data <- dataset %>%
  group_by(Defeitos) %>%
  summarise(
    Cliente_1 = sum(Cliente_1, na.rm = TRUE),
    QC = sum(QC, na.rm = TRUE)
  )


data_norm <- data %>%
  mutate(clientes = round((Cliente_1 / sum(Cliente_1, na.rm = TRUE)) * 100, 2)) %>%
  mutate(relatorios = round((QC / sum(QC, na.rm = TRUE)) * 100, 2)) %>%
  select(-Cliente_1, -QC)


data_radar <- data_norm %>%
  pivot_longer(-Defeitos, names_to = "Métrica", values_to = "Valor") %>%
  group_by(Métrica, Defeitos) %>%
  summarise(Valor = mean(Valor, na.rm = TRUE), .groups = "drop") %>% # Evita valores duplicados
  pivot_wider(names_from = Defeitos, values_from = Valor)

# Converter todas as colunas (exceto a primeira) para numérico
data_radar[,-1] <- lapply(data_radar[,-1], as.numeric)

# Definir os valores máximo e mínimo para o fmsb
max_value <- max(data_radar[,-1], na.rm = TRUE)
min_value <- 0

# Criar a estrutura exigida pelo fmsb (primeira linha = máximo, segunda linha = mínimo)
data_radar_fmsb <- rbind(
  max_value,   # Máximo
  min_value,   # Mínimo
  data_radar[,-1]  # Dados reais
)

# Definir cores para cada grupo
cores <- c(rgb(1, 0.5, 0, 0.5), rgb(0, 0, 1, 0.5))
scale_values <- paste0(round(seq(0, max_value, length.out = 5), 0), "%") # Cria 5 valores intermediários

wrapped_labels <- sapply(names(data_radar)[-1], function(x) paste(strwrap(x, width = 15), collapse = "\n")) #quebra de texto dos rótulos

# Criar o gráfico de radar com rótulos ajustados
radarchart(
  data_radar_fmsb,
  axistype = 1,  # Mostra valores reais nos eixos
  pcol = c("orange", "blue"),  # Cores das linhas
  pfcol = cores,  # Cores de preenchimento
  plwd = 2,  # Espessura das linhas
  plty = 1,  # Tipo de linha (sólida)
  cglcol = "grey",  # Cor da grade
  cglty = 1,  # Tipo da grade (sólida)
  cglwd = 0.8,  # Espessura da grade
  axislabcol = "black",  # Cor dos rótulos dos eixos
  caxislabels = as.character(scale_values),  # Define os valores reais no eixo
  vlcex = 0.7,  # Tamanho dos rótulos das variáveis
  vlabels = wrapped_labels  # Aplicar rótulos com quebra de texto
)

legend("topright", legend = c("Relatório", "Cliente"), fill = cores, border = NA, bty = "n") # Adicionar legenda