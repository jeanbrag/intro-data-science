
#install.packages("rvest")

library(rvest)
Mouse = data.frame(Product = NA, Price = NA)
URL = "https://www.americanas.com.br/categoria/informatica-e-acessorios/mouse/pagina-"
cond = 0
k = 1
while (cond == 0) {
  url = paste0(URL,k)
  
  
  page = read_html(url)
  
  # Get product names ----
  pagenode = html_nodes(x = page, xpath = "//div//h2")
  pagenode
  texts   = html_text(pagenode)
  texts
  idtext  = grep("Refinar", texts)
  idtext
  newprod = texts[1:(idtext-1)]
  newprod
  # Get product price ----
  pagenode = html_nodes(x = page, xpath = "//section//div//div//span")
  texts    = html_text(pagenode)
  idtext   = grep("R\\$", texts)
  texts    = texts[idtext]
  idtext   = grep("sem juros", texts)
  newprice = texts[-idtext]
  
  # ----
  Mouse = rbind.data.frame(Mouse,cbind.data.frame(Product = newprod,
                                                  Price = newprice))
  
  cat("Page: ", k ,"\t Products: ",nrow(Mouse)-1,"\n")
  if(length(newprod)<24){
    cond = 1
  }
  k = k + 1 
  Sys.sleep(runif(1,0,5))
}










