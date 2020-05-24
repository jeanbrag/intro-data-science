library(rvest)

limparString <- function(x) {
  x %<>% str_replace_all("R$","")
  return(x)
}

url= "C:\\users\\Aluno\\Downloads\\boticario.html"
page = read_html(url, encoding = "UTF-8")
# Get Product Name
node = html_nodes(page, css = ".shelf-product-name")
nome = html_text(node)

# Get Product Price
node2 = html_nodes(page, css = ".shelf-product-price")
preco = html_text(node2)
preco = str_replace_all(string = preco,pattern = "R$",replacement = "")

k=1
for(k in 1:length(preco)){
  x = preco[k] #ler linhas
  idmod= gregexpr(pattern ="x",x)
  idmod = unlist(idmod)
  preco[k] = substr(x, 1, (idmod-2))
}
# Get Produt ID
node = html_nodes(page, xpath = "//div//div//like-button")
prod_id = t(simplify2array(html_attrs(node)))[,1]

Boticario = data.frame(Id = prod_id,
                       Product = prod_name,
                       Price = prod_price)
write.csv(Boticario, file = "Boticario.csv")

######## ----

node = html_nodes(page, css = ".product__image>img")
prod_img = t(simplify2array(html_attrs(node)))[,"src"]

Boticario = data.frame(Id = prod_id,
                       Product = prod_name,
                       Price = prod_price,
                       Img_url = prod_img)
write.csv(Boticario, file = "Boticario.csv")


for(i in 1:nrow(Boticario)){
  download.file(url=as.character(Boticario[i,"Img_url"]),
                destfile = paste0(Boticario[i,"Id"],".jpg"),
                mode="wb")
  print(i)
}

