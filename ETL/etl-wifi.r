setwd("C:\\users\\Aluno\\Downloads\\aa")
fileNames = dir()
fileNames = fileNames[grep(".txt", fileNames)]
fileNames
a=1
length(Teste)

  x = readLines(fileNames[1])
  idmod=grep("Teste",x)
  x= x[-c(1:(idmod-1))]
#pegar numero do teste
teste = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="Teste",y)
  idmod
  idmod = unlist(idmod)
  idmod
  if(idmod!=-1)
    teste[k] = substr(y, 7, 8)
  else
    teste[k] = NA
}
teste = as.numeric(teste)
Teste = teste[complete.cases(teste)]

#pegar valor da antena
Antena = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  y
  idmod= gregexpr(pattern ="Antena",y)
  idmod
  idmod = unlist(idmod)
  idmod
  if(idmod!=-1)
    Antena[k] = substr(y, (idmod+7), 18)
  else
    Antena[k] = NA
}
Antena = as.numeric(Antena)
Antena = Antena[complete.cases(Antena)]

#pegar valor do Channel
Channel = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  y
  idmod= gregexpr(pattern ="Channel",y)
  idmod
  idmod = unlist(idmod)
  idmod
  if(idmod!=-1)
    Channel[k] = substr(y, (idmod+8),(idmod+8)+1)
  else 
    Channel[k] = NA
}
Channel = as.numeric(Channel)
Channel = Channel[complete.cases(Channel)]
Channel

#pegar valor da Freq
Freq = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  y
  idmod= gregexpr(pattern ="Freq",y)
  idmod
  idmod = unlist(idmod)
  idmod
  if(idmod!=-1)
    Freq[k] = substr(y, (idmod+5),(idmod+5)+3 )
}
Freq = as.numeric(Freq)
Freq = Freq[complete.cases(Freq)]

#pegar valor da Occ Band
Occ_Band = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  y
  idmod= gregexpr(pattern ="Band",y)
  idmod = unlist(idmod)
  idmod2 = gregexpr(pattern="PASSED",y)
  idmod2 = unlist(idmod2)
  if(idmod!=-1)
    Occ_Band[k] = substr(y, (idmod+5),(idmod2[1]-2) )
  else
    Occ_Band[k] = NA
}
Occ_Band = Occ_Band[complete.cases(Occ_Band)]
Occ_Band

#pegar valor da Power
Power = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="Power",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="PASSED",y)
    idmod2 = unlist(idmod2)
    Power[k] = substr(y, (idmod+6),(idmod2[2]-2) )
  }
  else
    Power[k] = NA
}
Power = Power[complete.cases(Power)]
Power

#pegar valor do -Outsig
outsig = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="-outsig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    outsig[k] = substr(y, (idmod+8),(idmod2[1]-1) )
  }
  else
    outsig[k] = NA
}
outsig = outsig[complete.cases(outsig)]
outsig

#pegar valor do -3sig
sig_3 = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="-3sig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    sig_3[k] = substr(y, (idmod+7),(idmod2[2]-1) )
  }
  else
    sig_3[k] = NA
}
sig_3 = sig_3[complete.cases(sig_3)]
sig_3

#pegar valor do -2sig
sig_2 = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="-2sig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    sig_2[k] = substr(y, (idmod+6),(idmod2[3]-1) )
  }
  else
    sig_2[k] = NA
}
sig_2 = sig_2[complete.cases(sig_2)]
sig_2

#pegar valor do -1sig
sig_1 = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="-1sig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    sig_1[k] = substr(y, (idmod+6),(idmod2[4]-1) )
  }
  else
    sig_1[k] = NA
}
sig_1 = sig_1[complete.cases(sig_1)]
sig_1

#pegar valor do U
U = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="U",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    U[k] = substr(y, (idmod+2),(idmod2[5]-1) )
  }
  else
    U[k] = NA
}
U = U[complete.cases(U)]
U
#pegar valor do +1sig
sig1 = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="+1sig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    sig1[k] = substr(y, (idmod[2]+5),(idmod2[6]-1) )
  }
  else
    sig1[k] = NA
}
sig1 = sig1[complete.cases(sig1)]
sig1

#pegar valor do +2sig
sig2 = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="+2sig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    sig2[k] = substr(y, (idmod[2]+5),(idmod2[7]-1) )
  }
  else
    sig2[k] = NA
}
sig2 = sig2[complete.cases(sig2)]
sig2

#pegar valor do +3sig
sig3 = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="+3sig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    sig3[k] = substr(y, (idmod[2]+5),(idmod2[8]-1) )
  }
  else
    sig3[k] = NA
}
sig3 = sig3[complete.cases(sig3)]
sig3

#pegar valor do +outsig
outsig_mais = x
k=1
for(k in 1:length(x)){
  y = x[k] #ler linhas
  idmod= gregexpr(pattern ="+outsig",y)
  idmod = unlist(idmod)
  if(idmod!=-1){
    idmod2 = gregexpr(pattern="}",y)
    idmod2 = unlist(idmod2)
    outsig_mais[k] = substr(y, (idmod[2]+7),(idmod2[9]-1) )
  }
  else
    outsig_mais[k] = NA
}
outsig_mais = outsig_mais[complete.cases(outsig_mais)]
outsig_mais


DATA = data.frame(Teste = Teste,
                  antena = Antena,
                  channel = Channel,
                  freq = Freq,
                  Occ_BAND = Occ_Band,
                  power = Power,
                  negoutsig = outsig,
                  neg3sig = sig_3,
                  neg2sig = sig_2,
                  neg1sig = sig_1,
                  U_ = U,
                  pos1sig = sig1,
                  pos2sig = sig2,
                  pos3sig = sig3,
                  posoutsig = outsig_mais)

DATA
