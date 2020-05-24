#### Parte 1 ----
br  <- read.table("~/breast.txt", header=FALSE, stringsAsFactors=FALSE)

dim(br)

# 10000 bytes

# Estimar Volume de Dados correspondente ao limite de memÃ³ria RAM (10000 bytes)
vol_data = 0
size     = 0
cont     = 1
while(size <= 10000){
  br2  <- read.table("~/breast.txt", header=FALSE, stringsAsFactors=FALSE,
                     nrows = cont)
  size = object.size(br2)
  cont = cont + 1
}

vol_data = cont - 2
br3  <- read.table("~/breast.txt", header=FALSE, stringsAsFactors=FALSE,
                   nrows = vol_data)
size = object.size(br3)

# NÃºmero de PartiÃ§Ãµes da Base de Dados
nPar = trunc(nrow(br)/vol_data) + ifelse(nrow(br)%%vol_data>0, 1, 0)

# Carregando dados em partiÃ§Ãµes

media.vec = rep(0,length=ncol(br)-1)

for(k in 1:nPar){
  br4  <- read.table("~/breast.txt", header=FALSE, stringsAsFactors=FALSE,
                     nrows = vol_data, skip = (k-1)*vol_data)
  br4 = as.matrix(br4[,-31])
  media.vec = media.vec + colSums(br4)
  gc()
  print(k)
  print(((k-1)*vol_data+1):(k*vol_data))
  print(object.size(br4))
  print(nrow(br4))
  
}
mediaPar = media.vec / 569
identical(mediaPar,colMeans(br[,-31]))
abs(mediaPar - colMeans(br[,-31]))


sig2.vec = rep(0,length=ncol(br[1:2])-1)
min.vec  = c()
max.vec  = c()
for(k in 1:nPar){
  br4  <- read.table("~/breast.txt", header=FALSE, stringsAsFactors=FALSE,
                     nrows = vol_data, skip = (k-1)*vol_data)
  br4 = as.matrix(br4[,-31])
  sig2.vec = sig2.vec + colSums(t(t(br4) - mediaPar)^2)
  min.vec  = apply(rbind(min.vec,br4),2,min)
  max.vec  = apply(rbind(max.vec,br4),2,max)
  print(k)
}

S2Par = sig2.vec / (569 - 1)
S2    = apply(br[,-31], 2, var)
identical(S2Par, S2)
abs(S2Par - S2) < 0.0001

#### Parte 2 ----

setwd("C:\\Users\\souza\\OneDrive\\Documentos\\docsis 30")

# Control
setwd("control")

fileNames = dir()
fileNames = fileNames[grep(".txt", fileNames)]

for(k in 1:length(fileNames)){
  Log  = readLines(fileNames[k])
  idFP = grep("Freq\tPower",Log)+1
  Log  = Log[idFP:length(Log)]
  
  if(k == 1){
    logsplitted = strsplit(Log, split = "\t")
    logarray    = t(simplify2array(logsplitted))
    DATA        = matrix(as.numeric(logarray[,2]), 1, nrow(logarray),
                         dimnames = list(1, logarray[,1]))
    DATA        = as.data.frame(DATA)
  }else{
    logsplitted = strsplit(Log, split = "\t")
    logarray    = t(simplify2array(logsplitted))
    DATA[k,]    = as.numeric(logarray[,2])
    
  }
}

DATA = data.frame(DATA, CLASS = "CONTROL", stringsAsFactors = FALSE)


setwd("C:\\Users\\souza\\OneDrive\\Documentos\\docsis 30")
# Error
setwd("error")
n = nrow(DATA)
fileNames = dir()
fileNames = fileNames[grep(".txt", fileNames)]

for(k in 1:length(fileNames)){
  Log  = readLines(fileNames[k])
  idFP = grep("Freq\tPower",Log)+1
  Log  = Log[idFP:length(Log)]
  logsplitted = strsplit(Log, split = "\t")
  logarray    = t(simplify2array(logsplitted))
  DATA[n+k,-ncol(DATA)]    = as.numeric(logarray[,2])
  DATA[n+k,ncol(DATA)]    = "ERROR"
}

View(DATA)
