#Autor: Igor Brito
#Data: 16/09/22

#o objetivo deste código é criar uma base com os dados do tamanho do 
#centro presidencial e da composição dos gabinetes brasileiros entre
# 1985 e 2016 para ser utilizada na lista 5 e no trabalho final da
#disciplina de Métodos Quantitativos em Ciência Política.

#Em relação aos dados do tamanho do centro presidencial, irei juntar
#os dados coletados por Inácio e Llanos (2015), que cobrem o
#período de 1985 a 2010, com os dados que eu coletei (2003-2022).

#No que se refere aos dados de composição do gabinete, utilizarei os
#dados apresentados por Amorim Neto (2019) e disponibilizados diretamente
#no texto do trabalho. Esses dados já foram extraídos e disponibilizados
#em formato csv em um momento anterior.

#Obs: os dados de Inácio e Llanos (2015) possuem granularidade
#anual, ou seja, os dados revelam o tamanho do centro presidencial
#em um determinado ano. Já os dados que eu coletei têm como unidade
#de análise a norma que fez a última modificação no centro presidencial
#incluindo a data de publicação da norma. Assim, meus dados possuem 
#granularidade diária, ou seja, os dados podem indincar qual o tamanho
#do centro presidencial em cada dia. Como irei juntar essas bases, acabou
#que os dados finais terão granularidade anual.

#Referências:
#AMORIM NETO, Octavio. Cabinets and coalitional presidentialism.
#In: AMES, Barry (ed.). Routledge Handbook of Brazilian
#Politics. New York: Routledge, 2019. p. 293-312.

#INÁCIO, Magna; LLANOS, Mariana. The institutional presidency
#from a comparative perspective: Argentina and Brazil since the
#1980s. Brazilian Political Science Review, São Paulo, v. 9, n. 1,
#p. 39-64, 2015. 



#### Procedimentos iniciais

# Limpa memória
rm(list = ls())
gc()

# Definindo o diretorio de trabalho como do arquivo local
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Carrega pacotes usados no código
library(readxl)
library(tidyverse)
library(magrittr)
library(zoo)
library(lubridate)

#### Dados de tamanho do centro presidencial ####
#Carrega dados
dados_inacio <- read_xlsx("INACIO_LLANOS_DATABASE.xlsx")
dados_igor <- read_csv2("dados_pe_brasil.csv")


#Existe um problema de codificação que não consegui resolver neste momneto.
#Como isso poderia demandar muito tempo de pesquisa e esse problema não afeta
#os resultados, resolvi deixar para tratar dele em outro momento.
#dados_igor <- read_csv2("dados_pe_brasil.csv", locale = locale(encoding = "UTF-16"))
#dados_igor <- read.csv(file = "dados_pe_brasil.csv", fileEncoding = "UTF-8")
#guess_encoding(charToRaw(dados_igor$norma))
#iconv("Ser� pesquisado e", from = "UTF-8", to = "ASCII")
#iconv("Ser� pesquisado e", from = "ASCII", to = "UTF-8")
#iconv("Ser� pesquisado e", from = "UTF-8", to = "UTF-16")

#Ajusta a data para o formato do R "aaaa-mm-dd" e filtra os dados a partir de 2003
dados_igor %<>% 
  mutate(data = as.Date(data, "%d/%m/%Y")) %>% 
  distinct(data, norma, tam_cp, obs) %>% 
  filter(data >= "2003-01-01") %>% 
  select(data, tam_cp, norma, obs)

#cria banco de dados com a primeira coluna sendo cada dia entre o início de 2003
#e o início de 2022.
dados_diarios <-
  tibble(data = seq(as.Date("2003/1/1"), as.Date("2022/1/1"), "days"))

#Faz o matching entre as normas (tamanho do CP) e as datas específicas da
#publicação da norma
dados_diarios <- left_join(dados_diarios, dados_igor, by = "data")

#Preenche as datas vazias. A ideia aqui é deixar claro a norma vigente e 
#o tamanho do CP em cada dia
dados_diarios %<>% 
  na.locf()

#Seleciona apenas o primeiro dia do ano
dados_anuais <- dados_diarios %>% 
  filter(month(data) == 1 & day(data) == 1) %>% 
  mutate(ano = year(data)) %>% 
  select(ano, tam_cp)

#Prepara os dados de Inácio e Llanos (2015) para a junção
dados_inacio %<>% 
  filter(country == 1 & years < 2003) %>% 
  mutate(ano = years, tam_cp = prunits) %>% 
  select(ano, tam_cp)

#Junta os dados
dados_anuais %<>% 
  bind_rows(dados_inacio) %>% 
  arrange(ano)



#### Dados de composição do gabinete ####
#Importa os dados de tamanho e composição dos gabinetes
gabinetes_brasil <- read_csv("gabinetes_Brasil_Amorim_mensal.csv")

#A referência para registro das informações do ano é o seu primeiro dia (01/jan)
#Contudo, essa definição excluiria o ano de 1985, visto que o governo Sarney 1
#começou apenas em março. Portanto, fiz a modificação abaixo apenas para garantir
#que o ano de 1985 fosse incluído na análise.
gabinetes_brasil[1,1] <- as.Date("1985-01-01")

#Pega o gabinete em cada ano (feito o ajuste acima para o ano de 1985).
gabinetes_brasil %<>%
  filter(month(inicio) == 1 & day(inicio) == 1) %>% 
  mutate(ano = year(inicio)) %>% 
  select(ano, gabinete, qtd_part, partidos)



#### Junta os dados de tamanho do centro presidencial com os dados de gabinete ####
dados_trab_final <- 
  left_join(dados_anuais, gabinetes_brasil, by = "ano") %>% 
  filter(!is.na(gabinete))

#Exporta os dados
write.csv2(dados_trab_final, "dados_trab_final.csv", na = "", row.names = F)
