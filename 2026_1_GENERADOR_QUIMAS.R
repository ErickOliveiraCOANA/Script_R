library(ggplot2)
library(dplyr)
library(AgroR)


#classificar por outra tabela
dataset <- dataset %>% 
  arrange(ID) %>% 
  mutate(
    `T Generador` = factor(
      `T Generador`,
      levels = unique(`T Generador`)
    )
  )

#teste anova e post hoc
p <- 
  with( dataset,
        DIC(
          `T Generador`,
          `% Branqueamento`*100,
          mcomp = "sk",
          ylab = "Desgrana ('%')",
          CV = FALSE,
          angle=60
        )
  )

#grafico
ggplot( p[["dadosm"]], aes( x = trats, y = media ) ) +
  geom_col(fill = "lightblue", colour = "black") + #stat = identity
  geom_errorbar( 
    aes( 
      ymax = media + desvio,
      ymin = media - desvio
      ),
    width = 0.3 
    ) +
  geom_text(
    aes( 
      y = media + desvio + 1, #acima da errorbar
      label = paste0(
        scales::number(
          media,
          accuracy = 0.01, 
          decimal.mark = ",", #troca separador decimal por virgula
          big.mark = ".",
          suffix = "%" #add sufixo
        ), groups, sep = " " ) #concatenar com letra
    ),
    colour = "black", size = 4 ) +
  labs( x = "Trat", y = "response" ) +
  scale_y_continuous(
    labels =
      scales::label_number(
        decimal.mark = ",", #troca separador decimal por virgula
        big.mark = ".",
        suffix = "%" #add sufixo
      )
  ) +
  theme_classic() +
  theme( axis.text.x = element_text(angle = 60, hjust = 1,color = "black", size = 11),
         axis.text.y = element_text(color = "black", size = 11),
         axis.title = element_text(color = "black", size = 11))