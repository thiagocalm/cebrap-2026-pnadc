#'----------------------------------------
#'Roteiro para a solução do exercício com a PNADC
#'----------------------------------------

# Configuracoes gerais ----------------------------------------------------
options(timeout = 1200, scipen = 99999)
invisible(gc())

# definicao do diretorio de trabalho
setwd(file.path(here::here(),"dia 2","praticas"))

# Pacotes -----------------------------------------------------------------
library(PNADcIBGE)
library(survey)

# Importacao dos dados ----------------------------------------------------
###
# 1. Baixando os 4 bancos trimestrais de 2021 diretamente do site do IBGE
###

# Baixar diretamente
P_21_t1 <- get_pnadc(year = 2021, quarter = 1, design = TRUE, vars = c("VD4009", "VD4002"))
P_21_t2 <- get_pnadc(year = 2021, quarter = 2, design = TRUE, vars = c("VD4009", "VD4002"))
P_21_t3 <- get_pnadc(year = 2021, quarter = 3, design = TRUE, vars = c("VD4009", "VD4002"))
P_21_t4 <- get_pnadc(year = 2021, quarter = 4, design = TRUE, vars = c("VD4009", "VD4002"))

###
# 2. Importando dados desde o computador (rodar essa parte do código caso tenha baixado os microdados!)
###

# 2.1 - Primeiro voce deve baixar os microdados, dicionario e input no site do IBGE

# 2.2 - Importar a base

# Trimestre 1
P_21_t1 <- read_pnadc(
  microdata = "PNADC_012021.txt",
  input_txt = "input_PNADC_trimestral.txt"
)

# Trimestre 2
P_21_t2 <- read_pnadc(
  microdata = "PNADC_022021.txt",
  input_txt = "input_PNADC_trimestral.txt"
)

# Trimestre 3
P_21_t3 <- read_pnadc(
  microdata = "PNADC_032021.txt",
  input_txt = "input_PNADC_trimestral.txt"
)

# Trimestre 4
P_21_t4 <- read_pnadc(
  microdata = "PNADC_042021.txt",
  input_txt = "input_PNADC_trimestral.txt"
)

# 2.3 - Atribuindo os labels às bases

# Trimestre 1
P_21_t1 <- pnadc_labeller(
  data_pnadc = P_21_t1,
  dictionary.file = "dicionario_PNADC_microdados_trimestral.xls"
)

# Trimestre 2
P_21_t2 <- pnadc_labeller(
  data_pnadc = P_21_t2,
  dictionary.file = "dicionario_PNADC_microdados_trimestral.xls"
)

# Trimestre 3
P_21_t3 <- pnadc_labeller(
  data_pnadc = P_21_t3,
  dictionary.file = "dicionario_PNADC_microdados_trimestral.xls"
)

# Trimestre 4
P_21_t4 <- pnadc_labeller(
  data_pnadc = P_21_t4,
  dictionary.file = "dicionario_PNADC_microdados_trimestral.xls"
)

# 2.4 - Atribuindo o plano amostral

# Trimestre 1
P_21_t1 <- pnadc_design(P_21_t1)

# Trimestre 2
P_21_t2 <- pnadc_design(P_21_t2)

# Trimestre 3
P_21_t3 <- pnadc_design(P_21_t3)

# Trimestre 4
P_21_t4 <- pnadc_design(P_21_t4)

# Analises ----------------------------------------------------------------

###
# 1. Numero de empregados com carteira assinada
###

# Vínculo dos empregados (VD4009)
Vinculo_t1 <- svytotal(~VD4009, P_21_t1, na.rm = T)
Vinculo_t2 <- svytotal(~VD4009, P_21_t2, na.rm = T)
Vinculo_t3 <- svytotal(~VD4009, P_21_t3, na.rm = T)
Vinculo_t4 <- svytotal(~VD4009, P_21_t4, na.rm = T)

# Resultados
Vinculo_t1
Vinculo_t2
Vinculo_t3
Vinculo_t4

# Salvando em csv para abrir no excel e fazer o grafico
write.csv2(Vinculo_t1, "Vinculo_t1.csv")
write.csv2(Vinculo_t2, "Vinculo_t2.csv")
write.csv2(Vinculo_t3, "Vinculo_t3.csv")
write.csv2(Vinculo_t4, "Vinculo_t4.csv")

###
# 2. Condicao de ocupacao das pessoas com 14 anos ou mais
###

# Condição de ocupação das pessoas (VD4009)
Condicao_t1 <- svytotal(~VD4002, P_21_t1, na.rm = T)
Condicao_t2 <- svytotal(~VD4002, P_21_t2, na.rm = T)
Condicao_t3 <- svytotal(~VD4002, P_21_t3, na.rm = T)
Condicao_t4 <- svytotal(~VD4002, P_21_t4, na.rm = T)

# Resultados
Condicao_t1
Condicao_t2
Condicao_t3
Condicao_t4

# Intervalo de confianca dos resultados
confint(Condicao_t1)
confint(Condicao_t2)
confint(Condicao_t3)
confint(Condicao_t4)

# Salvando em csv para abrir no excel e fazer o grafico
write.csv2(Condicao_t1, "Vinculo_t1.csv")
write.csv2(Condicao_t2, "Vinculo_t2.csv")
write.csv2(Condicao_t3, "Vinculo_t3.csv")
write.csv2(Condicao_t4, "Vinculo_t4.csv")

# Fazendo graficos simples em R -------------------------------------------

# Caso prefira fazer graficos em R ao inves de exportar o arquivo e faze-los em excel, segue uma referencia

# Para isso, vamos usar o pacote ggplot2 e uma gramatica que se chama 'tidyverse'
# (o 'ggplot2' ja se encontra dentro dessa gramatica 'tidyverse')
ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))

###
# Vinculos
###

# incluir vinculos em um arquivo data.frame

vinculos <- data.frame(
  tipo_vinculo = names(Vinculo_t1),
  Trim1 = Vinculo_t1[1:10],
  Trim2 = Vinculo_t2[1:10],
  Trim3 = Vinculo_t3[1:10],
  Trim4 = Vinculo_t4[1:10]
)

# excluir nome das linhas

rownames(vinculos) <- NULL

# excluir variavel do tipo_vinculo - queremos excluir a parte "VD4009"

vinculos$tipo_vinculo <- str_replace_all(
  vinculos$tipo_vinculo,
  pattern = "VD4009",
  replacement = ""
)

# fazendo uma pequena manipulacao dos dados para plotar - vamos usar pipes para isso

vinculos <- vinculos |>
  # transformando os trimestres que estavam em colunas para linhas da base
  pivot_longer(
    cols = Trim1:Trim4,
    names_to = "trimestre",
    values_to = "N"
  ) |>
  # atribuindo uma nova classe (factor) para os trimestres
  mutate(
    trimestre = factor(trimestre, levels = c("Trim1","Trim2","Trim3","Trim4"))
  )

# grafico

vinculos |>
  ggplot() +
  aes(
    x = trimestre,
    y = N,
    color = tipo_vinculo,
    group = interaction(tipo_vinculo, tipo_vinculo)
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 4) +
  theme_light() +
  labs(
    title = "Tipos de vínculos no acumulado dos trimestres da PNADC - 2021",
    caption = "Fonte: IBGE, PNADC acumulado dos trimestres, 2021.",
    x = "Trimestres",
    y = "Número total de vínculos",
    color = ""
  )

###
# Condição de ocupação
###

# As mesmas etapas podem ser repetidas para a condição de ocupação, que tal testar?
