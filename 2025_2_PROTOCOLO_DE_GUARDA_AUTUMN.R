#PROTOCO DE GUARDA AUTUMN CRISP

library(readxl)
library(ggplot2)
library(AgroR)
library(dplyr)
library(tidyr)


#PASTA DE DESTINO
setwd("R:/Controle de Qualidade/TESTES/2025/PROTOCOLO DE GUARDA AUTUMNCRISP")


#CARREGAR DADOS DE SHELF LIFE
SHELF = read_excel("DADOS_ESTATISTICA.xlsx", sheet = "SHELF LIFE - BRIX, DUREZA", range = "A2:U302" )%>%
  select(-...9,-...10, -`DUREZA DIA 0`, -`SUCO DIA 0`, -`NAOH DIA 0`, -`...16`, -`...17`,-`DUREZA 60 DIAS`,-`NAOH 60 DIAS`,-`SUCO 60 DIAS` )

#CARREGAR DADOS DE PESO
PESO = read_excel("DADOS_ESTATISTICA.xlsx", sheet = "PESO", range = "A2:I302" ) %>% 
  select(-`PESO DIA 0`, -`PESO 60 DIAS`)

#MESCLAR DADOS
DADOS = left_join(
  SHELF,
  PESO,
  by = join_by(FAZENDA, PARCELA, RAMO, REPETICAO)
) %>% filter(!PARCELA %in% c("O3", "K1", "u5B3", "8B", "06-V4"))

#REMOVER DADOS SEPARADOS
remove( SHELF, PESO )


################################################################################0
# TESTE DE T

#DUREZA
with(
  DADOS, 
  test_two(
    trat = RAMO,
    resp = `DUREZA MEDIA DIA 0`,
    paired = FALSE,
    test = "w",
    ylab = "Dureza (Shore)",
    xlab = "Ramo"
  )
)


ggsave(
  "shelf_dureza_dia0.png",
  dpi = 300
)

##point - dureza shelf life
ggplot( data = DADOS , aes( x= RAMO, y=`DUREZA MEDIA DIA 0` ) ) +
  geom_boxplot(
    stat = "boxplot",
    outliers = FALSE
  ) +
  geom_text( stat = "summary",
             fun = "mean", 
             aes(label = round(..y.., 1))
             #, vjust = -11
  ) +
  geom_jitter( width = 0.1,
               size = 2,
               alpha = 0.1
  ) +
  theme_classic() +
  labs( title = "Shelf Life",
        y = "Dureza (Shore)",
        x = "Ramo" )


#60 dias vs 0 dias

with(
  DADOS %>% filter(!is.na(`DUREZA MEDIA 60 DIAS`)) %>% 
    pivot_longer(
      cols = c( `DUREZA MEDIA DIA 0`, `DUREZA MEDIA 60 DIAS` ),
      names_to = "Tempo",
      values_to = "Valor"
    ) %>% mutate( Tempo = ifelse(Tempo == "DUREZA MEDIA DIA 0", "0", "60") ),
#   test_two(
#     trat = Tempo,
#     resp = Valor,
#     paired = FALSE,
#     test = "w",
#     ylab = "Dureza (Shore)",
#     xlab = "Tempo de Avaliação (dias)"
#   )
# )
  DIC(
    trat = Tempo,
    response = Valor,
    test = "noparametric",
    p.adj = "bonferroni",
    ylab = "Dureza (Shore)",
    xlab = "Tempo de Avaliação (dias)",
    geom = "box"
  )
)


ggsave(
  "shelf_dureza_0x60dias.png",
  dpi = 300
)


#BRIX
with(
  DADOS, 
  test_two(
    trat = RAMO,
    resp = `BRIX DIA 0`,
    paired = FALSE,
    test = "w",
    ylab = "º Brix",
    xlab = "Ramo"
  )
)


ggsave(
  "shelf_brix_dia0.png",
  dpi = 300
)



#60 dias vs 0 dias
with(
  DADOS %>% filter(!is.na(`BRIX 60 DIAS`)) %>% 
    pivot_longer(
      cols = c( `BRIX DIA 0`, `BRIX 60 DIAS` ),
      names_to = "Tempo",
      values_to = "Valor"
    ) %>% mutate( Tempo = ifelse(Tempo == "BRIX 60 DIAS", "0", "60") ),
  test_two(
    trat = Tempo,
    resp = Valor,
    paired = FALSE,
    test = "w",
    ylab = "º Brix",
    xlab = "Tempo de Avaliação (dias)"
  )
)

ggsave(
  "shelf_brix_0x60dias.png",
  dpi = 300
)

################################################################################
# shelf life 60 dias

#PERDA

DADOS = DADOS %>% drop_na(PERDA)

with(
  DADOS, 
  test_two(
    trat = RAMO,
    resp = PERDA*100,
    paired = FALSE,
    test = "w",
    ylab = "Perda ('%')",
    xlab = "Ramo"
  )
)

DIC(
  DADOS$PARCELA,
  DADOS$PERDA*100 ,
  test = "noparametric",
  p.adj = "bonferroni",
  geom = "box"
)



##box plot - perda shelf life
ggplot( data = DADOS , aes( x= RAMO, y=PERDA*100 ) )  +
  geom_boxplot(
    stat = "boxplot"
    # outliers = FALSE
  ) +
  stat_summary(
    fun = "mean",
    geom = "point",
    col = "black",
    size = 2,
    shape = 24,
    fill = "red"
  ) +
  # geom_jitter( 
  #   width = 0.1,
  #   height = 0,
  #   size = 2,
  #   alpha = 0.1
  # )  +
  facet_grid(~PARCELA)

################################################################################
#LABORATORIO
LAB = read_excel("DADOS_ESTATISTICA.xlsx", sheet = "ANALISE DE CALCIO", range = "A1:O31" )

with(
  LAB, 
  test_two(
    trat = RAMO,
    resp = `Ca-Ligado (mg/100g)`,
    paired = FALSE,
    test = "w",
    ylab = "Dureza (Shore)",
    xlab = "Ramo"
  )
)


ggsave(
  "lab_ramo_dureza.png",
  dpi = 300
)


library(ggpmisc)


ggplot( data = LAB, 
        aes(  x= LAB$`Diâmetro do Ramo (mm)`,
              y=LAB$`Ca - Total (mg/100g)` )  ) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  stat_poly_eq( formula = y ~ x,
                method = "lm", )
  # +  facet_grid(~RAMO)



cor.test(x= LAB$`Diâmetro do Ramo (mm)`,
         y=LAB$`Ca - Total (mg/100g)`,
         method = c( "pearson"  )
         )

#matriz de correlação

library(ggcorrplot)

matriz = LAB %>%
  select(-FAZENDA,-PARCELA,-RAMO )

ggcorrplot(
    cor(matriz, method = "spearman"),
    type = "lower",
    lab = TRUE,
    p.mat = cor_pmat(matriz),
    insig = "blank",
    sig.level = 0.04
  )
  
ggsave("matriz_correlacao_shelf.png",
       height = 7,
       width = 7)

  
  
  cor(matriz, method = "pearson") %>% View()

  cor_pmat(matriz) %>% View()

