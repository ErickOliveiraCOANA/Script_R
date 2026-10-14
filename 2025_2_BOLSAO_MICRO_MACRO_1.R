library(AgroR)
library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)
library(scales)

library(multcomp)
library(crayon)
library(emmeans)



setwd("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/GRÁFICOS")


#CONDESANCAO####################################################################

df <- read_excel("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/DADOS.xlsx", 
                 sheet = "ESTATÍSTICA - CONDEN.")

df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)

df$CONDENSAÇÃO <- ifelse(df$CONDENSAÇÃO == "SIM", 1, 0)

a = glm(CONDENSAÇÃO ~ BOLSÃO, data=df , family = binomial(link = "logit"))
summary(a)

anava1 = anova(modelo, test="Chisq")
summary(anova)

cld(regrid(emmeans(a, "BOLSÃO", level=1-0.05)),
    Letters=letters,reversed=TRUE,adjusted="tukey")


emmeans(a, "BOLSÃO", level=1-0.05)

regrid(emmeans(a, "BOLSÃO", level=1-0.05))

DIC.glm(
  df$BOLSÃO,
  df$CONDENSAÇÃO,
  glm.family = "binomial", ylab = "Probabilidade de ocorrer condensação (%)", xlab = "Bolsões",
  textsize = 12 , labelsize = 4
)

ggsave("bolsao.svg")


DIC.glm(
  df$POSIÇÃO,
  df$CONDENSAÇÃO,
  glm.family = "binomial",ylab = "Probabilidade de ocorrer condensação (%)", xlab = "Posição de caixas",
  textsize = 12 , labelsize = 4)

ggsave("posição.svg")


modelo <- glm(CONDENSAÇÃO ~ BOLSÃO, data = df, family = binomial)


df_pred <- data.frame(BOLSÃO = unique(df$BOLSÃO))  # Garantindo que BOLSÃO seja reconhecido
df_pred$Prob <- predict(modelo, newdata = df_pred, type = "response")

# Criando o gráfico
ggplot(df, aes(x = BOLSÃO, y = CONDENSAÇÃO)) +
  geom_jitter(width = 0.2, height = 0.02, alpha = 0.5) +  # Pontos da amostra
  geom_line(data = df_pred, aes(x = BOLSÃO, y = Prob), color = "blue", linewidth = 1.2) +  # Correção do tamanho da linha
  labs(x = "Bolsões", y = "Probabilidade de ocorrer condensação (%)") +
  theme_minimal(base_size = 12) +
  coord_cartesian(ylim = c(0, 1))  # Ajustando os limites do eixo y





#TERMOGRAFO#####################################################################

termografo <- read_excel("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/Termografos/termografos.xlsx", 
                 sheet = "BOLSAO")


termografo$Data = as.POSIXct(termografo$Data)


glimpse(termografo)



termografo %>% filter(Bolsão %in% c("MACRO 0,6%","MICRO 0,6%") ) #filtrar

help("ggplot2")

ggplot( data = termografo,
        aes( x = Data, y = `Temperatura (°C)`, color = "Temperatura (ºC)") ) +
  geom_line() +
  geom_line(aes(y = `UR (%)`*0.5, color = "UR (%)") ) +
  facet_wrap(~Bolsão, nrow = 4 ) +
  scale_y_continuous(
    limits = c(0,50),
    breaks = seq(0, 50, by = 5),
    labels = function(x) paste0(x, "º"),
    sec.axis = sec_axis(~./0.5,
                        labels = function(x) paste0(x, "%")) ) +
  labs(x= "Período do teste", y = "",
       color = "") +
  theme_bw() +
  theme(legend.position = "top",
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"))

ggsave( "termografos.png",
        width = 6 ,
        height = 15,
        dpi = 300,
        units = "in"
        )


#ENGACO#########################################################################

engaço <- read_excel("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/DADOS.xlsx", 
                 sheet = "NÍVEL DE DESIDRATAÇÃO", range = "A1:D121")

with( engaço,
  DIC(
    BOLSÃO,
    `NÍVEL DE DESIDRATAÇÃO`,
    test = "noparametric",
    mcompNP = "LSD",
    p.adj = "bonferroni",
    errorbar = TRUE,
    width.bar = 0.1,
    width.column = 0.6
  )
)


with(engaço, transf(`NÍVEL DE DESIDRATAÇÃO`,f1=BOLSÃO))


help("AgroR")

hist(engaço$`NÍVEL DE DESIDRATAÇÃO`)

engaço$CONDIÇÃO


engaço %>% summarise(BOLSÃO) %>% 
  mutate( "t" = count(CONDIÇÃO))

engaço2 <- read_excel("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/DADOS.xlsx", 
                     sheet = "NÍVEL DE DESIDRATAÇÃO", range = "I1:L5")
engaço2 %>% 
  pivot_longer(cols = c("leve","moderado","severo"),
               names_to = "Condição do Engaço",
               values_to = "perc") %>% 
ggplot(aes( x = bolsao, y = perc*100, fill = `Condição do Engaço`) ) +
  geom_bar( stat = "identity" ) +
  geom_text(  aes( label = paste0(round(perc*100,1),"%") ), position = position_stack(vjust = 0.5)  ) +
  xlab("Bolsões") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    name = "") +
  fill_palette(c("#48BC66","#ECE253","#EC5454"))+
  theme_classic() +
  theme(legend.position = "top",
        legend.text = element_text(size = 10),
        axis.title = element_text(color = "black", size = 10),
        axis.text = element_text(color = "black", size = 10),
        strip.text = element_text(size = 10))

ggsave( "oxidação do engaço.png")
#PERDA##########################################################################

perda <- read_excel("R:/Controle de Qualidade/TESTES/2025/BOLSÃO MICROPERFURADO  E MACRO - MARÇO 2025/DADOS.xlsx", 
                     sheet = "PERDA")

with( perda,
DIC(
  bolsao,
  -1*(perda_perc),
  transf = -0.5,
  constant = 1,
  mcomp = "tukey",
  CV = FALSE,
  width.bar = 0.1,
  width.column = 0.6,
  ylab = "Perda após 30 dias (%)",
  xlab = "Bolsões"
) )

ggsave("perda.svg")


ggplot(data = perda, aes( x = bolsao, y = -1*(perda_perc)) ) +
        geom_boxplot()


if (transf == 0) {
  resp = log(response + constant)
}
if (transf == 0.5) {
  resp = sqrt(response + constant)
}
if (transf == -0.5) {
  resp = 1/sqrt(response + constant)
}
if (transf == -1) {
  resp = 1/(response + constant)
}
if (transf == "angular") {
}

perda$bolsao

join = inner_join(engaço, perda, by = c("N° DO CACHO" = "N° Cacho"))

  ggplot(join, aes(x=`NÍVEL DE DESIDRATAÇÃO`, y= -1*perda_perc) ) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) +
  facet_wrap(~bolsao)
