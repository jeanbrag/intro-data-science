setwd("C:/Users/Aluno/Downloads/baleias")

imgnames = dir()
imgnames

#package
library(jpeg)

#read jpeg

for(k in 2:16){
img = readJPEG(imgnames[k])

#plot(c(0,1),c(0,1),col=0)
#rasterImage(img,0,0,1,1)

#Grid Split
deltax = 250
deltay = 250
r = 1
for(i in 1:trunc(dim(img)[1]/deltax)){
  for(j in 1:trunc(dim(img)[2]/deltay)){
    x = img[(deltax*(i-1)+1):(i*deltax),(deltay*(j-1)+1):(j*deltay),]
    writeJPEG(x,paste("p",r,imgnames[k],sep = ""))
    r = r + 1
    #print(r)
  }
}
}
#Baleia Sample

setwd("C:/Users/Aluno/Downloads/baleias/baleia")
whalesname = dir()

whal = readJPEG(whalesname[1])
plot(c(0,1),c(0,1),col=0)
rasterImage(whal,0,0,1,1)

DATAw = data.frame(R = c(whal[,,1]), G = c(whal[,,2]), B = c(whal[,,3]))

par(mfrow=c(1,3))
hist(DATAw[,1],col="red")
hist(DATAw[,2],col="green")
hist(DATAw[,3],col="blue")

pairs(DATAw)

#Parameters Estimation
library(mixsmsn)
modsn = smsn.mmix(DATAw, g=1, family = "Skew.normal")