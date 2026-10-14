library(tidyr)
library(dplyr)
library(AgroR)
library(readxl)
library(bestNormalize)
library(ggplot2)

setwd("R:/Controle de Qualidade/TESTES/2025/TESTE DE GUARDA AUTUMNCRISP - FAN - 15-10-2024/GRÁFICOS")


CBRIX <- read_excel("R:/Controle de Qualidade/TESTES/2025/TESTE DE GUARDA AUTUMNCRISP - FAN - 15-10-2024/ESTATÍSTICA/ESTATISTICA.xlsx", sheet = "BRIX_ACIDEZ")

with( CBRIX,
DIC( CAIXA, bn$x.t, mcomp = "sk", ylab = "Brix", width.bar = 0.1,  width.column = 0.6
     )
)

bn<-bestNormalize(CBRIX$BRIX)

ggplot(data = CBRIX, aes(x = factor(CAIXA), y = BRIX)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6, 
            position = "dodge2",
            color ="black") +
  #  geom_errorbar( stat = "summary",
  #                 fun.data = "mean_sd",
  #                 width = 0.1,
  #                 position = position_dodge(width =0.6)) +
  labs(y= ("Média de Defeitos"), x = "", fill = " ") +
  theme_classic() + theme(legend.position = "top") + 
  theme(legend.position = "top",
        legend.text = element_text(size = 16,family = "sans", color = "black"),
        axis.title = element_text(size = 16,family = "sans", color = "black"),
        axis.text = element_text(size = 16,family = "sans", color = "black")
        
  )


ggsave("6_CAIXAS_MEDIA_BRIX.svg", dpi = 400,units = "in", width = 15,height = 8)


################################
DS <- read_excel("R:/Controle de Qualidade/TESTES/2025/TESTE DE GUARDA AUTUMNCRISP - FAN - 15-10-2024/ESTATÍSTICA/ESTATISTICA.xlsx",
                 sheet = "PROBLEMAS_6_CAIXAS", range = "A10:K16")
DS <- DS %>% select(-"QUEIMA DE SOL") 

DS <- pivot_longer( DS, cols = 5:9, names_to = "Defeitos", values_to = "Valor")


with( DS,
      DIC( CLASSIFICACAO_BRIX, Valor, test = "noparametric", ylab = "Brix",p.adj = "bonferroni", width.bar = 0.1,  width.column = 0.6
      ))

with( DS,
      DIC( Defeitos, Valor, test = "noparametric", ylab = "Brix",p.adj = "bonferroni", width.bar = 0.1,  width.column = 0.6
      ))


with(DS,
FAT2DIC( CLASSIFICACAO_BRIX, Defeitos, bn1$oos_preds$arcsinh_x , names.fat = c("Classe_Brix", "Defeitos")) 
)


ggplot(data = DS, aes(x = factor(CLASSIFICACAO_BRIX), y = Valor, fill = CLASSIFICACAO_BRIX )) +
  geom_violin(trim= FALSE, alpha = 0.7) + # Violino com cor e transparência
  geom_errorbar( stat = "boxplot",
                 width = 0.1,
                 position = position_dodge(width = 0.3),
                 alpha = 0.5 ) +
  geom_boxplot(width = 0.3,
               fill= "white") +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7, color = "black") +
  labs(y= ("Percentual de Defeitos"),
       x = "", fill = " ") +
  theme_classic() + theme(legend.position = "top") + 
  theme(legend.position = "top",
        axis.title = element_text(size = 15,family = "sans", color = "black"),
        axis.text = element_text(size = 15,family = "sans", color = "black"))


ggsave("violino_6_CAIXAS_DEFEITOS.svg", dpi = 400,units = "in", width = 15,height = 8)



ggplot(data = DS, aes(x = factor(A), y = Valor+1  )) +
  geom_violin( trim= FALSE, alpha = 0.7, fill = "#FFFFF0") + # Violino com cor e transparência
  geom_jitter(aes(colour = CLASSIFICACAO_BRIX),
              shape= 19,
              width = 0.2,
              height = 0.01, 
              size = 2.5 ) +
  labs(title = "Dispersão de Problemas por Brix ",
       y= ("Percentual de Defeitos"),
       x = "", colour = "Classificação de Brix") +
  theme_classic() + theme(legend.position = "top") + 
  theme(legend.position = "top",
        axis.title = element_text(size = 15,family = "sans", color = "black"),
        axis.text = element_text(size = 15,family = "sans", color = "black")) + ylim(0,70)

ggsave("violao_6_CAIXAS_DEFEITOS.svg", dpi = 400,units = "in", width = 8,height = 8)


################################################################
PS <- read_excel("R:/Controle de Qualidade/TESTES/2025/TESTE DE GUARDA AUTUMNCRISP - FAN - 15-10-2024/ESTATÍSTICA/ESTATISTICA.xlsx",
                 sheet = "PS", range = "A9:J18")

PS <- PS %>% select(-"Queima de sol")  

PS <-  
  pivot_longer( PS, cols = 2:8, names_to = "Defeitos", values_to = "Valor")

#ANOVA_2_factor
with( PS,
      FAT2DIC( Defeitos, posicao, Valor, ylab = "Percentual de Defeitos",
               dec = 1, labelsize = 2, textsize = 16, ylim = c(0,50)
      ))

#ANOVA_1_factor
with( PS,
DIC( Defeitos, Valor, test = "noparametric", ylab = "Defeitos (%)",p.adj = "bonferroni", width.bar = 0.1,  width.column = 0.6
))

with( PS,
      DIC( posicao, Valor, test = "noparametric", ylab = "Defeitos (%)",p.adj = "bonferroni", width.bar = 0.1,  width.column = 0.6
      ))


bn2 <- bestNormalize(PS$Valor+1)
bn2

hist(bn2$other_transforms$boxcox$x.t)

ggplot(data = PS, aes(x = factor(Defeitos), y = Valor,  fill = posicao)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6, 
            position = "dodge2",
            color ="black") +
#  geom_errorbar( stat = "summary",
#                 fun.data = "mean_sd",
#                 width = 0.1,
#                 position = position_dodge(width =0.6)) +
  labs(y= ("Defeitos"), x = "", fill = " ") +
  theme_classic() + theme(legend.position = "top") + 
  theme(legend.position = "top",
        legend.text = element_text(size = 16,family = "sans", color = "black"),
        axis.title = element_text(size = 16,family = "sans", color = "black"),
        axis.text = element_text(size = 16,family = "sans", color = "black")
        
        )


ggsave("POSICAO_DEFEITOS3.svg", dpi = 400,units = "in", width = 25,height = 15)





# Instalar e carregar os pacotes necessários
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

# Simulando o dataset no Power BI
# No Power BI, substitua pelo dataset carregado automaticamente
dataset <- data.frame(
  Q.C = c("QC1", "QC2", "QC3", "QC4", "QC5"),
  valor = c(80, 65, 70, 90, 85)
)

# Calcular os percentuais em relação ao total
dataset <- dataset %>%
  mutate(percentual = valor / sum(valor) * 100)

# Adicionar ponto extra para fechar o gráfico de radar
data_long <- rbind(dataset, dataset[1, ])

# Criar o gráfico de radar
ggplot(data_long, aes(x = Q.C, y = percentual, group = 1)) +
  geom_polygon(fill = "blue", alpha = 0.2) +   # Área preenchida
  geom_line(color = "blue", linewidth = 1) +  # Linha do radar com linewidth
  geom_point(size = 3, color = "blue") +      # Pontos do radar
  coord_polar() +                             # Projeção polar para o gráfico de radar
  theme_minimal() +                           # Tema limpo
  labs(
    title = "Gráfico de Radar no Power BI",
    x = "Rótulos",
    y = "Percentual"
  ) +
  theme(
    axis.text.x = element_text(size = 12),    # Tamanho do texto dos rótulos
    axis.title.x = element_blank(),           # Remover título do eixo X
    axis.title.y = element_blank()            # Remover título do eixo Y
  )

