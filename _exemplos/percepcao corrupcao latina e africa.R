#######################
### Exemplo: percepção de corrupção na américa latina e africa
# subsahriana são estatísticamente iguais

# install.packages("devtools")
# install.packages("tidyverse")
# library(devtools)
# install_github("vinisousa04/DadosLivroR")
library(tidyverse)
library(DadosLivroR)

dados <- data_set1
anos <- unique(dados$Ano)

resultado <- data.frame(matrix(NA,
                               ncol = 3,
                               nrow = length(unique(dados$Ano))))
colnames(resultado) <- c("Latina e Caribe",
                         "África Sub",
                         "Estatisticamente...")


for(i in seq_along(anos)){
  africa <- dados %>% filter(Regiao=="Sub-Saharan Africa",
                             Ano==anos[i]) %>%
    select(CPI)
  africa <- as_vector(africa)
  
  latina <- dados %>% filter(Regiao=="Latin America & Caribbean",
                             Ano==anos[i]) %>%
    select(CPI)
  latina <- as_vector(latina)
  teste_t <- t.test(x = latina, y = africa,paired = F)
  
  resultado[i,1] <- round(teste_t$estimate[1],2)
  resultado[i,2] <- round(teste_t$estimate[2],2)
  
  if(teste_t$p.value <= 0.05){
    resultado[i,3] <- "Estatisticamente Diferente"
  } else {
    resultado[i,3] <- "Estatisticamente Igual"
  }
}