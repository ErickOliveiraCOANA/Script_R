#neossolo
neo<-read.xlsx(file.choose(),sheetName="neo",h=T)#header=TRUE #;separa comandos #SheetName=nome da planilha e não do arquivo xlsx
FAT3DIC(neo[,1], neo[,2], neo[,3], neo[,5],names.fat = c("nano", "fma", "solo"))
ggplot(data = neo, aes(x =factor(Nano), y = arg[,5], color = solo)) +
  geom_errorbar( stat = "boxplot",
                 width = 0.2,
                 position = position_dodge(width = 0.6)) +
  geom_boxplot(width = 0.6) +
  # média##geom_point( stat = "summary", fun = "mean", position = position_dodge(width = 0.6) ) +
  labs(y= "A (%)", x = "Doses de nano-NP (mg/L)") +
  theme_classic() + theme(legend.position = "top",
                          axis.title = element_text(size = 10,family = "sans", color = "black"),
                          axis.text = element_text(size = 10,family = "sans", color = "black"))
ggplot(data = neo, aes(x =factor(Nano), y = neo[,5], color = solo)) +
  geom_errorbar( stat = "boxplot",
                 width = 0.2,
                 position = position_dodge(width = 0.6)) +
  geom_boxplot(width = 0.6) +
  # média##geom_point( stat = "summary", fun = "mean", position = position_dodge(width = 0.6) ) +
  labs(y= "A (%)", x = "Doses de nano-NP (mg/L)") +
  theme_classic() + theme(legend.position = "top",
                          axis.title = element_text(size = 10,family = "sans", color = "black"),
                          axis.text = element_text(size = 10,family = "sans", color = "black"))
ggplot(data = neo, aes(x =factor(Nano), y = neo[,5], color = solo)) +
  geom_errorbar( stat = "boxplot",
                 width = 0.2,
                 position = position_dodge(width = 0.6)) +
  geom_boxplot(width = 0.6) +
  # média##geom_point( stat = "summary", fun = "mean", position = position_dodge(width = 0.6) ) +
  labs(y= "F (%)", x = "Doses de nano-NP (mg/L)") +
  theme_classic() + theme(legend.position = "top",
                          axis.title = element_text(size = 10,family = "sans", color = "black"),
                          axis.text = element_text(size = 10,family = "sans", color = "black"))
FAT3DIC(neo[,1], neo[,2], neo[,3], neo[,6],names.fat = c("nano", "fma", "solo"))
FAT3DIC(neo[,1], neo[,2], neo[,3], neo[,7],names.fat = c("nano", "fma", "solo"))
FAT3DIC(neo[,1], neo[,2], neo[,3], neo[,8],names.fat = c("nano", "fma", "solo"))
require(xlsx)
library(AgroR)
library(ggplot2)
library(ggpubr)
setwd("C:/Users/erick/Desktop/Projeto 2 (2023-2024)/Resultados") #local do arquivo
#ETAPA2
#neossolo
neo<-read.xlsx(file.choose(),sheetName="neo",h=T)
ggplot(data = neo, aes(x = factor(neo[,2]), y = neo[,5] , fill = neo[,3])) +
  geom_errorbar( stat = "boxplot",
                 width = 0.2,
                 position = position_dodge(width = 0.6)) +
  geom_boxplot(width = 0.6) +
  # média##geom_point( stat = "summary", fun = "mean", position = position_dodge(width = 0.6) ) +
  labs(y= "Altura (cm)", x = "Doses de nano-NP (mg/L)") +
  theme_classic() + theme(legend.position = "top",
                          axis.title = element_text(size = 10,family = "sans", color = "black"),
                          axis.text = element_text(size = 10,family = "sans", color = "black"))
#ETAPA2
#neossolo
neo<-read.xlsx(file.choose(),sheetName="neo",h=T)
ggplot(data = neo, aes(x =factor(neo[,2]), y = neo[,5], fill = neo[,3])) +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.1,
                 position = position_dodge(width =0.6)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6,
            position = "dodge2",
            color ="black") +
  labs(y= "Altura (cm)", x = "Doses de nano-NP (mg/L)") +
  theme_classic() + theme(legend.position = "top") +
  theme(legend.position = "top",
        axis.title = element_text(size = 10,family = "sans", color = "black"),
        axis.text = element_text(size = 10,family = "sans", color = "black"))
ggsave("N_Alt.png", dpi = 600,units = "in", width = 6,height = 5)
ggplot(data = neo, aes(x =factor(neo[,2]), y = neo[,5], fill = neo[,3])) +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.1,
                 position = position_dodge(width =0.6)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6,
            position = "dodge2",
            color ="black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(y= "Altura (cm)", x = "Doses de nano-NP (mg/L)") +