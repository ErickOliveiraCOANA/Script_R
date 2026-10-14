
setwd("R:/Controle de Qualidade/TESTES/2025/GENERADOR DE CAMPO")

CBRIX <- read_excel("R:/Controle de Qualidade/TESTES/2025/GENERADOR DE CAMPO/DADOS - ATUALIZADOS.xlsx", 
                    sheet = "ESTATÍSTICA")

with(
  CBRIX,
  DIC(
    Tratamento,
    abs(CBRIX$`PERDA (%)`),
    errorbar = FALSE,
    addmean = FALSE,
    mcomp = "sk",
    ylab = "Percentual de desidratação ('%')",
    xlab = "Tratamento",
    width.column = 0.5,
    textsize = 16,
    labelsize = 6
  )
)


ggsave("grafico.svg", dpi = 400,units = "in", width = 25,height = 15)


