require(xlsx)
library(ggplot2)
library(ggpubr)
library(ExpDes.pt)
library(dplyr)
library(tidyr)
library(AgroR)

setwd("C:/Users/erick/Downloads/Documents/charem") #local do arquivo

sorgo<-read.xlsx("C:/Users/erick/Downloads/Documents/charem/Charem_DADOS.xlsx", sheetName="SORGO",h=T)

sorgo <- separate(sorgo, 
                  "substrato_nano", 
                  into = c("substrato", "dose"), 
                  sep = "/", 
                  remove = FALSE)


AL <- subset(sorgo, FMA == "AL") 
EE <- subset(sorgo, FMA == "EE") 
GA <- subset(sorgo, FMA == "GA") 
  

#df_grupo_a <- df_grupo_a %>% select(-NG_20g)


#############################
#Biomassa Seca

fat2.dic(sorgo[,2], sorgo[,3], sorgo[,4], fac.names = c("substrsto","fma"), mcomp = "sk")


FAT2DIC(sorgo[,2], sorgo[,3], sorgo[,4], 
        names.fat = c("substrato","fma"), 
        mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
        legend = "", posi = "top", ylab = "Biomassa seca (g)", xlab = "Composição do substrato", 
        width.column = 0.7,  width.bar = 0.2, ylim = c(0,6)
        )
ggsave("BS.png", dpi = 400,units = "in", width = 15,height = 9)
ggsave("BS.svg", dpi = 400,units = "in", width = 15,height = 9)


#AL
DIC(AL[,2], AL[,4],
    mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
    posi = "top", 
    ylab = "Biomassa seca (g)", 
    xlab = "Composição do substrato", 
    width.column = 0.7,  width.bar = 0.2, fill = "#F8766D", CV = FALSE , ylim = c(0,2,4,6)
)
ggsave("BS_AL.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("BS_AL.svg", dpi = 400,units = "in", width = 10,height = 8)

#EE
DIC(EE[,2], EE[,4],
    mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
    posi = "top", 
    ylab = "Biomassa seca (g)", 
    xlab = "Composição do substrato", 
    width.column = 0.7,  width.bar = 0.2, fill = "#00BA38", CV = FALSE, ylim = c(0,2,4,6)
)
ggsave("BS_EE.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("BS_EE.svg", dpi = 400,units = "in", width = 10,height = 8)


#GA
DIC(GA[,2], GA[,4],
    mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
    posi = "top", 
    ylab = "Biomassa seca (g)", 
    xlab = "Composição do substrato", 
    width.column = 0.7,  width.bar = 0.2, fill = "#619CFF", CV = FALSE, ylim = c(0,2,4,6)
)
ggsave("BS_GA.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("BS_GA.svg", dpi = 400,units = "in", width = 10,height = 8)



ggplot(data = sorgo, aes(x = factor(substrato_nano), y = BS,  fill = FMA)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.6, 
            position = "dodge2",
            color ="black") +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.1,
                 position = position_dodge(width =0.6)) +
  labs(y= ("Biomassa seca"), x = "Substratos", fill = " ") +
  theme_classic() + theme(legend.position = "top") + 
  theme(legend.position = "top",
        axis.title = element_text(size = 12,family = "sans", color = "black"),
        axis.text = element_text(size = 10,family = "sans", color = "black")) +
  facet_wrap(~FMA)


ggsave("Bioma_seca.png", dpi = 400,units = "in", width = 15,height = 9)

###########################################################
#Numero glomerosporos
FAT2DIC(sorgo[,2], sorgo[,3], sorgo[,6],
         names.fat = c("substrato","fma"), 
         mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
         legend = "", posi = "top", 
         ylab = "Número de glomerosporos ( /g de solo)", 
         xlab = "Composição do substrato", 
         width.column = 0.7,  width.bar = 0.2, ylim = c(0,118)
         )
ggsave("GM.png", dpi = 400,units = "in", width = 15,height = 9)
ggsave("GM.svg", dpi = 400,units = "in", width = 15,height = 9)


#AL
DIC(AL[,2], AL[,6],
        mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
        posi = "top", 
        ylab = "Número de glomerosporos ( /g de solo)", 
        xlab = "Composição do substrato", 
        width.column = 0.7,  width.bar = 0.2, fill = "#F8766D", CV = FALSE
)
ggsave("GM_AL.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("GM_AL.svg", dpi = 400,units = "in", width = 10,height = 8)

#EE
DIC(EE[,2], EE[,6],
    mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
    posi = "top", 
    ylab = "Número de glomerosporos ( /g de solo)", 
    xlab = "Composição do substrato", 
    width.column = 0.7,  width.bar = 0.2, fill = "#00BA38", CV = FALSE
)
ggsave("GM_EE.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("GM_EE.svg", dpi = 400,units = "in", width = 10,height = 8)


#GA
DIC(GA[,2], GA[,6],
    mcomp = "sk", textsize = 12, addmean = FALSE,  labelsize = 4, 
    posi = "top", 
    ylab = "Número de glomerosporos ( /g de solo)", 
    xlab = "Composição do substrato", 
    width.column = 0.7,  width.bar = 0.2, fill = "#619CFF", CV = FALSE
)
ggsave("GM_GA.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("GM_GA.svg", dpi = 400,units = "in", width = 10,height = 8)



ggplot(data = sorgo, aes(x =factor(substrato_nano), y = NG_g, fill = FMA)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.7, 
            position = "dodge2",
            color ="black") +
  geom_errorbar( stat = "summary",
                 fun.data = "mean_sd",
                 width = 0.2,
                 position = position_dodge(width =0.6)) +
  labs(y= ("Número de glomerosporos ( /g de solo)"), x = "Composição do substrato", fill = "") +
  theme_classic() + theme(legend.position = "top") + 
  theme(legend.position = "top",
        axis.title = element_text(size = 12,family = "sans", color = "black"),
        axis.text = element_text(size = 12,family = "sans", color = "black")) + ylim(0,118)
ggsave("glomerosporos.png", dpi = 400,units = "in", width = 15,height = 9)



##########################################################
sorgo<-read.xlsx("C:/Users/erick/Downloads/Documents/charem/Charem_DADOS.xlsx", sheetName="SORGO",h=T)

#Kendall rank correlation test
kendall_test <- cor.test( sorgo[,4], sorgo[,6], alternative = "two.sided", method = "kendal")
kendall_test

ggscatter(sorgo, y = "BS", x = "NG_g", point = TRUE,color = "#A9A9A9",
          add = "reg.line", conf.int = FALSE, add.params = list ( color= "black"  ),
          cor.coef = TRUE, 
          cor.method = "kendall", 
          cor.coeff.args = list(digits = 4, label.x.npc = 0.5, label.y.npc = 1),  scientific = TRUE,
          ylab = "Biomassa seca (g) ", xlab = "Número de glomerosporos ( /g de solo) ",
          title = "", font.label = 12,  font.family = "sans")


ggsave("Corr_kendall.svg", dpi = 400,units = "in", width = 8,height = 6)



#####################################################
#MIP

AL_MIP<-read.xlsx("C:/Users/erick/Downloads/Documents/charem/MIP.xlsx", sheetName="AL_MIP",h=T)

ggplot(data = AL_MIP, aes(x =factor(Amostra), y = Resultados)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.7, 
            position = "dodge2",
            fill ="#F8766D", color = "black") +
  labs(y= ("Colonização micorrízica (%)"), x = "") +
  theme_classic() + 
  theme(axis.title = element_text(size = 12,family = "sans", color = "black"),
        axis.text = element_text(size = 12,family = "sans", color = "black"))

ggsave("MIP_AL.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("MIP_AL.svg", dpi = 400,units = "in", width = 10,height = 8)



EE_MIP<-read.xlsx("C:/Users/erick/Downloads/Documents/charem/MIP.xlsx", sheetName="EE_MIP",h=T)

ggplot(data = EE_MIP, aes(x =factor(Amostra), y = Resultados)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.7, 
            position = "dodge2",
            fill ="#00BA38", color = "black") +
  labs(y= ("Colonização micorrízica (%)"), x = "") +
  theme_classic() + 
  theme(axis.title = element_text(size = 12,family = "sans", color = "black"),
        axis.text = element_text(size = 12,family = "sans", color = "black"))

ggsave("MIP_EE.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("MIP_EE.svg", dpi = 400,units = "in", width = 10,height = 8)


GA_MIP<-read.xlsx("C:/Users/erick/Downloads/Documents/charem/MIP.xlsx", sheetName="GA_MIP",h=T)

ggplot(data = GA_MIP, aes(x =factor(Amostra), y = Resultados)) +
  geom_bar( stat = "summary",
            fun = "mean",
            width = 0.7, 
            position = "dodge2",
            fill ="#619CFF", color = "black") +
  labs(y= ("Colonização micorrízica (%)"), x = "") +
  theme_classic() + 
  theme(axis.title = element_text(size = 12,family = "sans", color = "black"),
        axis.text = element_text(size = 12,family = "sans", color = "black"))

ggsave("MIP_GA.png", dpi = 400,units = "in", width = 10,height = 8)
ggsave("MIP_GA.svg", dpi = 400,units = "in", width = 10,height = 8)


ggplot(sorgo, aes(BS, NG_g)) +
  #geom_point(color = "blue", size = 2) + # Adicionar pontos (dispersão)
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = FALSE) + # Adicionar linha de regressão
  annotate("text", 
           label = paste0(" R = - 0,3171, p = 3,758e-08"), 
           size = 12, color = "dark") +
  labs(
    title = "Regressão Linear",
    x = "Biomassa Seca",
    y = "Numero de glomerosporos"  ) +
  theme_classic() + ylim(0,30)
  #facet_wrap(~FMA)


# Sample data
set.seed(123)
x <- c(12, 25, 35, 47, 52, 68, 70, 85, 90, 100)
y <- c(15, 22, 37, 40, 48, 60, 67, 80, 95, 105)

data <- data.frame(x, y)

# Perform Kendall correlation test
kendall_test <- cor.test(data$x, data$y, method = "kendall")
kendall_test

# Load required library
library(ggpubr)

# Scatter plot with Kendall correlation coefficient
ggscatter(data, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "X Values", ylab = "Y Values",
          title = "Kendall Correlation Plot")


