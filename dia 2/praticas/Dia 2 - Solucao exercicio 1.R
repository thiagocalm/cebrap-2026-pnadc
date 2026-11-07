#--------------------------------
#  Dia 2
#  Exercicio 1 - Resolução
#--------------------------------
#' PERGUNTAS A SEREM RESPONDIDAS
#' 1. Qual a média salarial de cada trabalhador (média entre salário anterior e o salário atual)?
#' 2. Faça uma coluna dizendo se o salário atual é “MAIOR” ou “MENOR” que o salário anterior.
#' 3. Qual a média de idade de todos os trabalhadores?


# Configuração do diretorio de trabalho ------------------------

getwd() # verificando onde esta o diretorio de trabalho

here::here() # exemplo de como verificar onde se encontra o diretorio com pacote 'here'

file.path(here::here(),"dia 2","praticas") # configurando diretorio utilizando file.path()

setwd(file.path(here::here(),"dia 2","praticas")) # configura o nosso NOVO diretório de trabalho

getwd() # agora temos um diretorio de trabalho onde queremos trabalhar!

# Importação da base de dados ----------------------------------------------

trabalhadores <- read.csv("Trabalhadores.txt") # importação da base de dados

# comandos para entender a estrutura do banco de dados

View(trabalhadores) # visualizar base de dados como um todo

head(trabalhadores) # visualizar as primeiras linhas

tail(trabalhadores) # visualizar ultimas linhas

str(trabalhadores) # estrutura das variaveis

class(trabalhadores) # classe da base dados

class(trabalhadores$Nome) # classe da variável Nome na base de dados

class(trabalhadores$Idade) # classe da variável Idade na base de dados

# 1 - Media salarial dos trabalhadores ------------------------------------

# Criando coluna para a media dos salarios

trabalhadores$soma_salarios <- (trabalhadores$Salario_atual + trabalhadores$Salario_anterior)

head(trabalhadores) # ver se a variavel foi criada

# Criando variável para a média dos salarios

trabalhadores$media_salarios <- trabalhadores$soma_salarios/2

class(trabalhadores$media_salarios)

# Visualizar a media salarial dos trabalhadores
trabalhadores$media_salarios

head(trabalhadores) # primeiras linhas para a variavel de media de salario por trabalhador

# 2 - Coluna dizendo qual salario é maior ---------------------------------

trabalhadores$salario_categorico <- ifelse(
  trabalhadores$Salario_atual > trabalhadores$Salario_anterior,
  yes = "MAIOR",
  no = "MENOR"
)

# Visualizar a variavel de salario categorica criada
trabalhadores$salario_categorico

table(trabalhadores$salario_categorico) # tabela sintetica com os valores

head(trabalhadores)

# 3 - Qual a media de idade de todos os trabalhadores? --------------------

# Calculando media de idade de todos os trabalhadores

mean(trabalhadores$Idade, na.rm = TRUE)
