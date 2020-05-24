
#install.packages("rvest")
library(magrittr) 
library(rvest) # principal pacote para web-scraping
library(readr) # usado para extrair numeros de texto
library(stringr) # usado para o data cleaning
library(curl) # usado como suporte para o rvest
library(tidyr) # data cleaning
library(dplyr) # data cleaning

limparString <- function(x) {
  # x = string coletado do olx
  x %<>% str_replace_all("[\t]", "")
  x %<>% str_replace_all("[\n]", "")
  x %<>% str_replace_all("Apartamentos", "")
  x %<>% str_replace_all("Anúncio Profissional", "")
  x %<>% str_replace("-", "")
  x %<>% str_replace_all("[R$]", "")
  x %<>% str_replace_all("[.]", "")
  x %<>% str_trim()
  return(x)
}

url_imoveis <- "https://am.olx.com.br/imoveis/venda/casas"
number_pages <- 10 #hard coded
# Criar vetor com todos os urls para as páginas do olx
lista_urls <- paste0(url_imoveis, "?o=", 1:number_pages)
page = read_html(lista_urls[1])
page
#pegar o nome do anuncio
pagenode = html_nodes(x = page, xpath = "//div[1]//h2")
texts   = html_text(pagenode)
texts = str_replace_all(string = texts,pattern = "\t",replacement = "") #apagar \t
texts = str_replace_all(string = texts,pattern = "\n",replacement = "") #apagar \n
texts
idtext  = grep("AM", texts)
idtext
imoveis = texts[idtext+1:50]
imoveis = imoveis[complete.cases(imoveis)]
imoveis

# preços
precos <- html_nodes(x = page, css = ".col-3")
precos
precos %<>% lapply(html_text)
precos %<>% unlist()
precos
precos %<>% limparString()
#precos %<>% as.numeric()
#precos <- precos[complete.cases(precos)]
precos
col_precos <- precos

data.frame(descricao = imoveis, preço = col_precos)
