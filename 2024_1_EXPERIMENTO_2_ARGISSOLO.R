require(xlsx)
library(AgroR)
library(ggplot2)
library(ggpubr)
library(ExpDes.pt)
setwd("C:/Users/erick/Desktop/Projeto 2 (2023-2024)/Resultados") #local do arquivo

#filtrar argissolo
arg<-read.xlsx(file.choose(),sheetName="arg",h=T)


ggplot(data = arg, aes(x =factor(arg[,2]), y = arg[,8], fill = arg[,3])) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6,
            position = "dodge2",
            color ="black") +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.1,
                 position = position_dodge(width =0.6)) +
  labs(y= ("MSA (g)"), x = "Doses (mg/L)", fill = " ") + ylim (-0.3,2.7) +
  theme_classic() + theme(legend.position = "top") +
  theme(legend.position = "top",
        axis.title = element_text(size = 10,family = "sans", color = "black"),
        axis.text = element_text(size = 10,family = "sans", color = "black"))

polynomial2_color( arg[,2], arg[,3], arg[,8],grau = 1, ylab = "MSA (g)", xlab = "nano-NP (mg/L)", 
theme = theme_classic(), point = "mean_se")

polynomial2_color( arg[,2], arg[,3], arg[,8],grau = 1, ylab = "MSA (g)", xlab = "nano-NP (mg/L)")

View(arg)

polynomial2_color( arg[,2], arg[,8], arg[,3],grau = 1, ylab = "MSA (g)", xlab = "nano-NP (mg/L)")

polynomial2_color( arg[,2], arg[,8], arg[,3], grau = 1, ylab = "MSA (g)", xlab = "nano-NP (mg/L)")

polynomial2_color(arg[,2], arg[,8], arg[,3], ylab = "MSA (g)", xlab = "nano-NP (mg/L)", point = "mean", pointsize = 3)

ggsave("MSA2_r1_A.png", dpi = 600,units = "in", width = 6,height = 5)