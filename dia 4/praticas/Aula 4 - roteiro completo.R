#' --------------------------------------------------
#' Aula 4 - PNADC em R - Aplicação: Imposto zero!
#' --------------------------------------------------

# ajustes gerais
options(timeout = 1200, scipen = 9999)
invisible(gc()) # liberar de espaco nao utilizado da memoria do computador

# Importacao de biblioteca ------------------------------------------------

#install.packages("PNADcIBGE")
#install.packages("survey")

library(PNADcIBGE)
library(survey)
library(tidyverse)

# Variáveis que utilizaremos
# UF
# VD4002 - Condicao de ocupacao (VD4002 == 1, população ocupada)
# VD4017 - Rendimento mensal efetivo do trabalho principal
# V2009 - Idade

# Importação da base de dados ---------------------------------------------

###
# Importação da base de dados 2024 concentrada na visita 1
###


# baixando os dados diretamente do ibge

pnad_2024_v1 <- get_pnadc(
  year = 2024,
  interview = 1,
  labels = FALSE,
  design = TRUE # ja baixar com o plano amostral implementado
)

# importando offline (lembre-se de configurar o working directory antes!!!)

getwd() # antigo diretorio
setwd(file.path(here::here(),"dia 4","praticas")) # configurando novo diretorio
getwd() # novo diretorio configurado!

pnad_2024_v1 <- read_pnadc(
  microdata = "PNADC_2024_visita1_20250822.zip", # importando com arquivo 'zipado'
  # microdata = "PNADC_2024_visita1_20250822.txt", # importando com arquivo 'unzipado'
  input_txt = "input_PNADC_2024_visita1_20250822.txt"
)

# Deve ser rodado quando importamos os dados 'manualmente'

class(pnad_2024_v1) # classe ainda é 'dataframe'

pnad_2024_v1 <- pnadc_design(pnad_2024_v1) # aplicando o plano amostral

class(pnad_2024_v1) # classe mudou para "svyrep.design"


# Filtro para trabalhar com Pop. Ocupada ----------------------------------

# Criando estrato da base da PNADc

class(pnad_2024_v1$variables$VD4002)

pnad_ocupados <- subset(pnad_2024_v1, VD4002 == "1")

# remover base de dados completos

rm(pnad_2024_v1)

# limpar um pouco do espaco da memoria
invisible(gc())

# Criacao de variaveis derivadas ------------------------------------------

# Potencial isento

pnad_ocupados$variables <- transform(
  pnad_ocupados$variables,
  renda_isencao = ifelse(
    test = VD4017 <= 5000,
    yes = "Potencial_Isencao",
    no = "Sem_isencao"
  )
)

class(pnad_ocupados$variables$renda_isencao) # verificando se a variavel foi criada

# Variavel de soma

pnad_ocupados$variables <- transform(
  pnad_ocupados$variables,
  valor = 1
)

class(pnad_ocupados$variables$valor) # verificando se a variavel foi criada

# Analises ----------------------------------------------------------------

###
# 1 - Numero de pessoas ocupadas
###

tabela_1 <- svytotal(
  x = ~valor, # argumento da variavel de interesse
  design = pnad_ocupados, # base de dados com o plano amostral
  na.rm = TRUE # excluindo valores ausentes, NA
)

tabela_1 # visualizando tabela

# Estrategia alternativa de gerar os mesmos valores sem aplicar o survey design

pnad_ocupados$variables |>
  summarise(
    n = sum(V1032)
  )

# BOA PRÁTICA!: Verificar se converge com estimativas de PO do SIDRA
# Ver tabela 4191 do Sidra
# Link: https://sidra.ibge.gov.br/tabela/4191

###
# 2 - Numero de pessoas potencialmente isentas do IRPF em 2024
###

tabela_2 <- svytotal(
  x = ~renda_isencao, # desagregar por pessoas potencialmente isentas
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_2 # visualizando tabela

confint(tabela_2) # incerteza em torno do valor médio obtido em 'tabela_2'

# Estrategia alternativa de gerar os mesmos valores sem aplicar o survey design

pnad_ocupados$variables |>
  summarise(
    n = sum(V1032),
    .by = c(renda_isencao)
  )

###
# 3 - Proporcao de pessoas potencialmente isentas do IRPF em 2024
###

tabela_3 <- svymean(
  x = ~renda_isencao,
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_3 # visualizando tabela

# 4 - Rendimento médio do trabalho principal por grupo de potencialmente isento

# Funcao 'svyby':
# Para utilizá-la são necessários os seguintes argumentos:
#  - A variável da qual se deseja calcular a quantidade;
#  - A variável que define os domínios;
#  - O objeto do plano amostral;
#  - A função utilizada para calcular a quantidade de interesse (svytotal, svymean, svyratio, svyquantile, …).

tabela_4 <- svyby(
  formula =~ VD4017, # variavel que eu tenho interesse de representar
  by =~ renda_isencao, # grupo de interesse para a desagregação
  FUN = svymean, # funcao de interesse relacionado à medida
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_4 # visualizando tabela

# 5 - Proporcao de pessoas potencialemnte isentas do IRPF em 2024 por UF

tabela_5 <- svyby(
  formula =~ renda_isencao,
  by =~ UF,
  FUN = svymean,
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_5 # visualizando tabela

# Avaliando nossas estimativas principais

confint(tabela_5) # intervalo de confianca

# forma de calculo do cv: erro padrão / media - para interpretacao, ver slides 'dia 2'

cv(object=tabela_5) * 100 # coeficiente de variacao

# Extra! ------------------------------------------------------------------
###
#' Vamos plotar um mapa com essas estimativas
#' ---
#' Essa parte do código contém um estilo de programção diferente do que temos feito até o momento.
#' Vamos usar a 'gramática' tidyverse, a qual inclui 'ggplot' para plotar os graficos
###

# bibliotecas necessarias
library(sf) # para trabalhar com dados espaciais em R. Se nao tiver instalado, "install.packages("sf")" antes
library(tidyverse) # se nao tiver instalado, rodar "install.packages("tidyverse")" antes
library(brazilmaps) # se nao tiver instalado, rodar "install.packages("brazilmaps")" antes

# importando camadas espaciais usando o pacote "brazilmaps"
uf_map <- get_brmap(
  geo = "State",
  class = "sf"
)

# tratando as estimativas como um banco de dados

tabela_mapa <- as_tibble(tabela_5)

# fazendo pequenas alteracoes na tabela

tabela_mapa <- select(tabela_mapa, c(UF,"Potencial_Isentos" = renda_isencaoPotencial_Isencao)) # selecionando variaveis

tabela_mapa <- mutate(tabela_mapa, Potencial_Isentos = round(Potencial_Isentos * 100,2)) # transformacao de variavel em %

tabela_mapa <- mutate(tabela_mapa, UF = as.numeric(UF)) # transformacao de variavel em classe numerica

##
# juntando dados espaciais e estimativas usando bind_cols()
##

# ordenando estados para terem a mesma sequencia

uf_map <- arrange(uf_map, State)
tabela_mapa <- arrange(tabela_mapa, UF)

estimativas_espaciais <- left_join(
  x = uf_map,
  y = tabela_mapa,
  by = c("State" = "UF"),
  keep = FALSE
)

estimativas_espaciais |>
  ggplot() +
  geom_sf(aes(fill = Potencial_Isentos),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.6) +
  scale_fill_viridis_c(option = 2,direction = -1) +
  guides(fill = guide_colourbar(title = "%")) +
  labs(
    title = "Potencial parcela (%) de isentos do IRPFM em 2024 se a lei estivesse em vigor, por UF.",
    caption = "Fonte: IBGE/PNADC, acumulado da primeira visita."
  ) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())


# Outras analises possiveis -----------------------------------------------

# Distribuicao etaria, genero, raca

# Por setor de atividade (agricultura, servico publico?)

# Por posicao na ocupacao (sao mais empregados, conta propria, etc?)

# ...
