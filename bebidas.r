
install.packages("rvest")
library(magrittr) 
library(rvest) # principal pacote para web-scraping
library(readr) # usado para extrair numeros de texto
library(stringr) # usado para o data cleaning
library(curl) # usado como suporte para o rvest
library(tidyr) # data cleaning
library(dplyr) # data cleaning


url_bebidas <- "https://cosmos.bluesoft.com.br/ncms/22011000-aguas-minerais-e-aguas-gaseificadas/products"
number_pages <- 50 #hard coded

# Criar vetor com todos os urls para as páginas
lista_urls <- paste0(url_bebidas, "?page=", 1:number_pages)

texts = c()

for(k in 1:50){
  page = read_html(lista_urls[k])
  #pegar o nome do anuncio
  pagenode = html_nodes(x = page, xpath = "//div[1]//h5")
  texts2   = html_text(pagenode)
  texts2 = str_replace_all(string = texts2,pattern = "\n",replacement = "") #apagar \n
  texts = c(texts,texts2)

}
texts

url_cervejas <- "https://cosmos.bluesoft.com.br/ncms/22030000-cervejas-de-malte/produtos"
number_pages <- 107 #hard coded

# Criar vetor com todos os urls para as páginas
lista_urls <- paste0(url_cervejas, "?page=", 1:number_pages)

cervejas = c()
for(k in 1:107){
  page = read_html(lista_urls[k])
  #pegar o nome do anuncio
  pagenode = html_nodes(x = page, xpath = "//div[1]//h5")
  cervejas2   = html_text(pagenode)
  cervejas2 = str_replace_all(string = cervejas2,pattern = "\n",replacement = "") #apagar \n
  cervejas = c(cervejas,cervejas2)
  
}
cervejas

bebidas = c()
bebidas = c(texts,cervejas)
bebidas

