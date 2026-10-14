# Input load. Please do not change #
`dataset` = read.csv('C:/Users/eric.oliveira/REditorWrapper_e11a9d56-c662-4edc-8781-d93bf63ddeea/input_df_d8a654dc-7eac-410f-8a6f-7ce9fbff763b.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
# O código a seguir para criar um dataframe e remover as linhas duplicadas sempre é executado e age como um preâmbulo para o script: 

# dataset <- data.frame(PERIODO, AMOSTRA, ID PROCESSO, PROCESSO, PERDA)
# dataset <- unique(dataset)

# Cole ou digite aqui seu código de script:
library(ggplot2)
library(ggpubr)
library(AgroR)
library(dplyr)
library(patchwork)

setwd("R:/Controle de Qualidade/TESTES/2026/DPV Processo")


#classificar por outra tabela
dataset <- dataset %>%  
  arrange(`ID PROCESSO`) %>% 
  mutate(
    PROCESSO = factor( PROCESSO,
                       levels = unique(PROCESSO)
    )
  )


p <- with( filter( dataset, PROCESSO != "TOTAL" ),
           FAT2DIC(
             PERIODO,
             PROCESSO,
             PERDA*100,
             mcomp = "sk",
             ylab = "Perda ('%')",
             transf = 0.5,
             constant = 1,
             CV = FALSE,
             angle=60
           )
)

p <- with( filter( dataset, PROCESSO != "TOTAL" ),
           DIC(
             PROCESSO,
             PERDA*100,
             test = "noparametric",
             p.adj = "bonferroni",
             ylab = "Perda ('%')",
             CV = FALSE,
             angle=60
           )
)


################################################################################
#PERDA POR FASE

dataset |>
  filter( PROCESSO != "TOTAL" ) |>
  group_by(PERIODO, `ID PROCESSO`, PROCESSO) |>
  arrange(`ID PROCESSO`) |> 
  mutate(
    PROCESSO = factor( PROCESSO,
                       levels = unique(PROCESSO)
    )
  ) |> 
  summarise(
    avg = mean(PERDA*100),
    sd = sd(PERDA*100),
    .groups = "drop"
  ) |> 
  ggplot( aes(x = PROCESSO, y = avg ) ) +
  geom_col(      
    fill = "lightblue", 
    colour = "black") +
  geom_errorbar(
    aes(
      ymax = avg + sd,
      ymin = avg - sd  ),
    width = 0.3
  ) +
  geom_text(
    aes(
      y = avg + sd + 0.05 ,
      label = scales::number(
        avg,
        accuracy = 0.01, 
        decimal.mark = ",", #troca separador decimal por virgula
        big.mark = ".",
        suffix = "%" #add sufixo
      )
    )
  ) +
  labs( x = "Etapa", y = "Perda (%)" ) +
  scale_y_continuous(
    labels =
      scales::label_number(
        decimal.mark = ",", #troca separador decimal por virgula
        big.mark = ".",
        suffix = "%" #add sufixo
      )
  ) +
  facet_wrap(~PERIODO) +
  theme_classic() +
  theme( axis.text.x = element_text(angle = 60, hjust = 1,color = "black", size = 11),
         axis.text.y = element_text(color = "black", size = 11),
         axis.title = element_text(color = "black", size = 11))

ggsave(
  "Perda_perc.png",
  width = 12,
  height = 8,
  dpi = 300,
  units = "in"
)




##################################################################
#PERDA POR HORA POR ETAPA

dataset |>
  filter( PROCESSO != "TOTAL" ) |>
  group_by(PERIODO, `ID PROCESSO`, PROCESSO) |>
  arrange(`ID PROCESSO`) |> 
  mutate(
    PROCESSO = factor( PROCESSO,
                       levels = unique(PROCESSO)
    )
  ) |> 
  summarise(
    avg = mean(`PERDA/HORA`*100),
    sd = sd(`PERDA/HORA`*100),
    .groups = "drop"
  ) |> 
  ggplot( aes(x = PROCESSO, y = avg ) ) +
  geom_col(      
    fill = "lightblue", 
    colour = "black") +
  geom_errorbar(
    aes(
      ymax = avg + sd,
      ymin = avg - sd  ),
    width = 0.3
  ) +
  geom_text(
    aes(
      y = avg + sd + 0.05 ,
      label = scales::number(
        avg,
        accuracy = 0.0001, 
        decimal.mark = ",", #troca separador decimal por virgula
        big.mark = ".",
        suffix = "%" #add sufixo
      )
    )
  ) +
  labs( x = "Etapa", y = "Perda por Hora (%)" ) +
  scale_y_continuous(
    labels =
      scales::label_number(
        decimal.mark = ",", #troca separador decimal por virgula
        big.mark = ".",
        suffix = "%" #add sufixo
      )
  ) +
  facet_wrap(~PERIODO) +
  theme_classic() +
  theme( axis.text.x = element_text(angle = 60, hjust = 1,color = "black", size = 11),
         axis.text.y = element_text(color = "black", size = 11),
         axis.title = element_text(color = "black", size = 11))

ggsave(
  "Perda_hora_perc.png",
  width = 15,
  height = 8,
  dpi = 300,
  units = "in"
)
