#'----------------------------------------
#'Roteiro trabalhos iniciais em R
#'----------------------------------------

# Manipulacoes gerais no R ------------------------------------------------

# Criando variáveis
x <- 18.7
x

y <- 2
print(y)

z <- 10
print(z)

# Cálculos
x + y
x - y
x/y
x*y

# Criando um objeto de texto
nome <- "João"
class(nome)

# Criando um vetor numerico
col_ex <- c(20,30,50,70,84,13,20.2)
col_ex
class(col_ex)

# criando variaveis a partir de cálculo
f <- x+col_ex
print(f)
class(f)

# Apaga variáveis
rm(f)

# Apagar todos os arquivos no environment
rm(list = ls())

# Manipulação de bases de dados -------------------------------------------

# Configurando diretorio de trabalho

# setwd("C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/") # configurando novo diretorio de trabalho

getwd() # verificar se o diretorio foi alterado

# Importacao
alunos <- read.delim(file.path("dia 2", "praticas","alunos.txt"), sep = ",")

# Explorando a estrutura da base de dados

View(alunos)

head(alunos)
tail(alunos)

str(alunos)

class(alunos)
class(alunos$nome)
class(alunos$nota1)

# Criando uma variável a partir do cálculo de 2 colunas

alunos$media <- (alunos$nota1 + alunos$nota2)/2
class(alunos$media)

# Tirando a média das médias de todos os alunos
mean(alunos$media)

# criando uma variável da ordem da coluna (index)
alunos$ordem <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
alunos$ordem2 <- seq(1:23)

# Criando uma variavel de sobrenomes
sobrenome <- c("Souza","Silva","Andrade","Souza","Pisteli","Rosiz","Levan","Auter","Costa","Meira","Mocca","Merci","Suarez","Veloso","Jerd","Cloud","Flert","Xacro","Sine","Serfi","Santos","Zefro","Calho")
alunos$sobrenome <- sobrenome

# Criando a variavel de nome completo a partir do nome e sobrenome
alunos$nome_completo <- paste(alunos$nome , alunos$sobrenome, sep = " ")

# Criar uma variavel categorica
alunos$resultado <-ifelse(test = alunos$media>5, yes = "Aprovado", no = "Reprovado")

#apagando uma coluna

alunos$ordem <- NULL
alunos$ordem2 <- NULL

# Atribuir valores de uma coluna da base a um vetor

i <- alunos$media

# Exportacao de um resultado ----------------------------------------------

# Em csv
write.csv2(alunos, "Dia2_exemplo_alterado.csv")
