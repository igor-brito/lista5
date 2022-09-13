#Autor: Igor Brito
#Data: 12/09/22

#o objetivo deste código é criar uma base com os dados do tamanho do 
#centro presidencial brasileiro entre 1985 e 2022. Para tanto, irei 
#juntar os dados coletados por Inácio e Llanos (2015), que cobrem o
#período de 1985 a 2010, com os dados que eu coletei (2003-2022).

#Obs: os dados de Inácio e Llanos (2015) possuem granularidade
#anual, ou seja, os dados revelam o tamanho do centro presidencial
#em um determinado ano. Já os dados que eu coletei têm como unidade
#de análise a norma que fez a última modificação no centro presidencial
#incluindo a data de publicação da norma. Assim, meus dados possuem 
#granularidade diária, ou seja, os dados podem indincar qual o tamanho
#do centro presidencial em cada dia. Como irei juntar essas bases, acabou
#que os dados finais terão granularidade anual.


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

#Exporta os dados
write.csv2(dados_anuais, "cp_Brasil_anual.csv", na = "", row.names = F)

