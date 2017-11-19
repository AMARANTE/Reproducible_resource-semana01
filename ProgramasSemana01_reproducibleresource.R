##Reproducible Research
##Semana-01
##Geraldo Barbosa do Amarante
##NOVEMBRO/2017
#-------------------------------------
##Definição da pasta de trabalho
setwd("D:/coursera/ReproducibleResource/Semana01/ArquivoDeDados")
##Instalação dos pacotes
install.packages(c("ggplot2","dplyr"))
##Enunciado da questão-01 ----------------------------------------------------------------------
##Make a plot that answers the question : *what is the relationship between mean covered charges 
##(Average.Covered.Charges) and mean total payments (Average.Total.Payments) in New-York ?
##----------------------------------------------------------------------------------------------
##Ler o arquivo de dados : pagamentos.csv (payments.csv)
ArquivoPagamentos <- read.csv(file="pagamentos.csv",header=TRUE, sep=",")
#Selecionando as informações da cidade de NOVA YORK
cidadeNovaYork <- subset(ArquivoPagamentos, Provider.State=="NY")
##--
correlacao <- with(cidadeNovaYork, cor(Average.Covered.Charges, Average.Total.Payments))
##--
modelo <- lm(formula = Average.Total.Payments ~ Average.Covered.Charges, data = cidadeNovaYork)
par(mfrow = c(1,1), mar=c(4,4,2,1))
plot(Average.Total.Payments ~ Average.Covered.Charges, data = cidadeNovaYork,
     main="New York : Covered charges X Total payments",
     xlab = "Mean covered charges", ylab = "Mean total payments",col ="blue")
abline(reg = modelo, lwd = 2)
legend(x = "bottomright", legend = paste0("correlation=", round(correlacao,2)), bty="n",title.col = "green")
##-----------------------------------------------------------------------------------------------------------
##Enunciado da questão-02 -----------------------------------------------------------------------------------
##Make a plot (possibly multi-panel) that answers the question: how does the relationship between mean 
##covered charges (Average.Covered.Charges) and mean total payments (Average.Total.Payments) vary by 
##medical condition (DRG.Definition) and the state in which care was received (Provider.State)?
##-----------------------------------------------------------------------------------------------------------
ArquivoPagamentos %>%
  group_by(DRG.Definition, Provider.State) %>%
  summarize(correlation = cor(Average.Covered.Charges, Average.Total.Payments)) %>%
  xtabs(correlation ~ DRG.Definition + Provider.State, data = .)
##--
painel <- function(data) {
  State = unique(data$Provider.State)
  DRG = unique(data$DRG.Definition)
  correlacao <- with(data, cor(Average.Covered.Charges, Average.Total.Payments))
  modelo <- lm(formula = Average.Total.Payments ~ Average.Covered.Charges, data = data)
  with(data,
       plot(Average.Covered.Charges, Average.Total.Payments,
            pch = 20, xaxt='n', yaxt='n', cex = 0.5, col = DRG.Definition,
            main = paste0(paste0(State, collapse=","), " (DRG ", 
                          paste0(substr(DRG, 1 , 3), collapse=","), ")")))
  abline(reg = modelo, lwd = 1)
  with(data, legend("topright", bty = "n", legend = paste0("cor=", round(correlacao,3))))
  return(NULL)
}
##--
with(ArquivoPagamentos, par(mfcol = c(nlevels(DRG.Definition), nlevels(Provider.State)), mar = c(0, 0, 1, 0)))
invisible(lapply(split(ArquivoPagamentos, list(ArquivoPagamentos$DRG.Definition, ArquivoPagamentos$Provider.State)), myPanelPlot))
##-----------------------------------------------------------------------------------------------------------







