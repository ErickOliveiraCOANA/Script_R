# Load libraries
library(Ramf)
library(ggplot2)

devtools::install_github("mchiapello/Ramf",force = TRUE)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("BiocStyle")

test_check("Ramf")
# Read data in
setwd("C:/Users/erick/Desktop/Projeto 1 (2022-23)/Resultados/Data Sheet 1/supplemental figures codes and data")
tr <- readData("Table S6 trouvelotData.csv", type = c("trouvelot"))

# Summary
trs <- am_summary(tr);trs

# Statistics
trst <- am_stat(tr, method = c("none"));trst
 
summary(trst)

# Plots
p1<- am_dotplot(tr,alpha = 0.05,annot = c("none"),annot_size = 5,method = c("none"),legend = c("bottom"),main = "Trouvelot analysis")
p2<- am_boxplot(tr,alpha = 0.05, annot = c("none"),annot_size = 5,method = c("none"),legend = c("bottom"),main = "Trouvelot analysis")


# Save
## Summary
stat <- am_save(trs, "Exp001")
summary(stat)

## Statistics
am_save(trst, "Exp001")

## Plot
am_save(p1, "Exp001.jpeg", width = 29, height = 21, units = "cm", dpi = 300)
am_save(p2, "Exp002.jpeg", width = 29, height = 21, units = "cm", dpi = 300)

## Fig9
pp <- cowplot::plot_grid(p1, p2, labels = c("A","B"), ncol = 1)
pp
cowplot::save_plot("Fig9.jpeg", pp, base_width = 9, base_height = 9)

###################################################################################
# Load libraries
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("BiocStyle")
library(Ramf)
library(xlsx)
library(ggplot2)

# Read data in
#setwd("C:/Users/erick/Downloads")
#tr <- readData("trouvelot_modelo.csv", type = "trouvelot")
setwd("C:\\Users\\erick\\Desktop\\Projeto 1 (2022-23)\\Resultados") #local do arquivo

var1 <-read.csv(file.choose(), h = T, sep = ";",dec = ".")
tr <-file.choose(readData("erick_trouvelot_expII_1.csv", type =c ("trouvelot")))

# Summary
trs <- am_summary(tr)
trs
# Statistics
trst <- am_stat(tr, method = "fdr")
trst
summary(trst)

# Plots
p1 <- am_dotplot(tr, main = "Trouvelot analysis", annot = "letters")+ theme(plot.title = element_text(hjust = .5), axis.text = element_text(size = 14))
p2 <- am_boxplot(tr, main = "Trouvelot analysis", annot = "letters")+ theme(plot.title = element_text(hjust = .5), axis.text = element_text(size = 14))

# Save
## Summary
stat <- am_save(trs, "Exp001")
summary(stat)
## Statistics
am_save(trst, "Exp001")
## Plot
am_save(p1, "Exp001.jpeg", width = 29, height = 21, units = "cm", dpi = 300)
am_save(p2, "Exp002.jpeg", width = 29, height = 21, units = "cm", dpi = 300)
## Fig9
pp <- cowplot::plot_grid(p1, p2, labels = c("A","B"), ncol = 1)
pp
cowplot::save_plot("Fig9.jpeg", pp, base_width = 9, base_height = 9)
