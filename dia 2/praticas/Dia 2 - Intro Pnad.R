#'----------------------------------------
#'Roteiro trabalhos iniciais da PNADC em R
#'----------------------------------------

# instalando os pacotes necessários
install.packages("PNADcIBGE")
install.packages("survey")
install.packages("writexl")

# carregar os pacotes necessários
library(PNADcIBGE)
library(survey)
library(writexl)

# Importacao dos dados ----------------------------------------------------

###
# Baixando o banco de dados trimestral do 3º trimestre de 2016
###

# Isso demoraria em torno de 4-10 min, a depender da internet

# pnad_2016_3t <- get_pnadc(
#   year = 2016,
#   quarter = 3,
#   design = TRUE,
#   vars = c("UF", "V2007","VD4009","VD4019")
# )
#
# class(pnad_2016_3t) # verificando a classe do objeto criado


###
# Importando os dados diretamente da pasta
###

# 1 - Atribuindo um Working Diretory (diretorio de trabalho)

setwd(file.path(here::here(),"dia 2","praticas"))

# 2 - Abrindo o banco do terceiro trimestre de 2016 que baixei no site do IBGE
pnad_2016_3t <- read_pnadc(
  microdata = "PNADC_032016.txt",
  input_txt = "input_PNADC_trimestral.txt"
)

# 3 - Atribuindo os labels ao banco de 2016
pnad_2016_3t <- pnadc_labeller(
  data_pnadc = pnad_2016_3t,
  dictionary.file = "dicionario_PNADC_microdados_trimestral.xls"
)

# 4 - Atribuindo o plano amostral ao banco de dados
pnad_2016_3t <- pnadc_design(pnad_2016_3t )

# Algumas analises diretas da PNADC ---------------------------------------

# Frequência de Tipo de vínculo do trabalho
vinculo_16_3t <- svytotal(~VD4009,design = pnad_2016_3t, na.rm = TRUE)

# Resultado da frequência
vinculo_16_3t

# intervalo de confiança
confint(vinculo_16_3t)

# coeficiente de variação
cv(vinculo_16_3t)*100

# Fazendo tabela com o intervalo de confiança
vinculo_16_3t_ic <- data.frame(
  vinculo_16_3t,
  confint(vinculo_16_3t),
  cv(vinculo_16_3t)*100
)

# mudando nomes das colunas

names(vinculo_16_3t_ic) <- c("N","ErroPadrao","ConfInt_2.5","ConfInt97.5","CV")

# mostrar tabela com o intervalo de confiança
vinculo_16_3t_ic

# Exportacao --------------------------------------------------------------

# Exportacao em csv
write.csv2(vinculo_16_3t_ic,"vinculo.csv")
# em excel
vinculo_final <- data.frame(rownames(vinculo_16_3t_ic),vinculo_16_3t_ic)

write_xlsx(vinculo_final, "vinculo.xlsx")
