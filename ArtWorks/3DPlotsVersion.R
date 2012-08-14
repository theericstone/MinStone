library(biOps)
setwd("C:/Users/Angela/Dropbox/Machine Learning Project/Art")
setwd("/Users/ericstone/Dropbox/Machine Learning Project/Art")
setwd("Y:/Dropbox/Machine Learning Project/Art")

scale <- 50
q <- scale/2
p.1 <- c(1:q)
p.2 <- c((q+1):scale)
p.3 <- c((((scale^2)/2)+1):(((scale^2)/2)+q))
p.4 <- c((((scale^2)/2)+1+q):(((scale^2)/2)+2*q))
quad.2 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.1+scale*x)))
quad.3 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.2+scale*x)))
quad.1 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.3+scale*x)))
quad.4 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.4+scale*x)))

quad.colors <- data.frame(index=c(1:scale^2), color=NA)
quad.colors <- sapply(quad.colors[,1],function(x) ifelse(x %in% quad.1,quad.colors[x,1]<-"#1B9E77",
										               ifelse(x %in% quad.2,quad.colors[x,1]<-"black",
										               ifelse(x %in% quad.3,quad.colors[x,1]<-"#7570B3",
										               quad.colors[x,1]<-"#E7298A"))))

load("art_hue_smaller.rda")
art <- art.1997.hue
art.pca <- prcomp(art, scale=TRUE)
library(fastICA)
art.PPDE <- fastICA(art, 3, alg.typ = "deflation", fun = "logcosh", alpha = 1,
                    method = "C", row.norm = FALSE, maxit = 1000,
                    tol = 0.0001, verbose = TRUE)
library(lle)
art.lle <- lle(X=art, m=3, k=12, reg = 2, ss = FALSE, id = TRUE, nnk = TRUE, iLLE = FALSE, v = .99)
##THREE DEE PLOTTZ##
library(rgl)
plot3d(art.lle$Y[,1],art.lle$Y[,2],art.lle$Y[,3], xlab="1st Component", ylab="2nd Component", zlab="3rd Component", col=quad.colors)
plot3d(art.PPDE$S[,1],art.PPDE$S[,2],art.PPDE$S[,3],xlab="1st Component",  ylab="2nd Component", zlab="3rd Component", col=quad.colors)
plot3d(art.pca$x[,1],art.pca$x[,2],art.pca$x[,3],xlab="1st Component",  ylab="2nd Component", zlab="3rd Component",col=quad.colors)



#file="comp_red_yellow.jpg"

######## DATA SET ############
artProc <- function(file="sunflowers.jpg", scale=50, property="hue") {
	require(biOps)
	image <- readJpeg(file)
  #scales the image based on whether it's wider or taller, crops edges of other dimension 
	if (dim(image)[1] < dim(image)[2]) {
		image2 <- imgAverageShrink(image, x_scale=(scale/(dim(image)[1])), y_scale=(scale/(dim(image)[1])))
    if (dim(image2)[2] %% 2 == 0){
      range <- (dim(image2)[2]-scale)/2 } else {
        range <- (dim(image2)[2]-(scale-1))/2 }
    image2 <- image2[,c(range:(range+(scale-1))),]
    } else {
      image2 <- imgAverageShrink(image, x_scale=(scale/(dim(image)[2])), y_scale=(scale/(dim(image)[2])))
      if (dim(image2)[1] %% 2 == 0){
        range <- (dim(image2)[1]-scale)/2 } else {
          range <- (dim(image2)[1]-(scale-1))/2 }
      image2 <- image2[c(range:(range+(scale-1))),,]
	}
  #converts the rgb values to hue, brightness, luma, or saturation
	image.vec <- vector(length=scale*scale)
  if(property=="hue"){
    image.h <- atan2((2*image2[,,1]-image2[,,2]-image2[,,3]),sqrt(3)*(image2[,,2]-image2[,,3]))
    image.vec <- c(image.h)
    }
  if(property=="brightness"){
    image.b <- .2126*image2[,,1] + .7152*image2[,,2] + .0722*image2[,,3]
    image.vec <- c(image.b)
    }
  if(property=="luma"){
    image.b <- .3*image2[,,1] + .59*image2[,,2] + .11*image2[,,3]
    image.vec <- c(image.b)
    }
  if(property=="saturation"){
    image.t <- (max(image2[,,c(1:3)]) - min(image2[,,c(1:3)])) / (max(image2[,,c(1:3)]))
    image.vec <- c(image.t)
    }
  image.vec
}

art.1997.hue <- data.frame(
	Starry_Night=artProc("Starry_Night.jpg"),
	sunflowers=artProc("sunflowers.jpg"),
	les_vessenots=artProc("les_vessenots.jpg"),
	the_effect_of_fog=artProc("the_effect_of_fog.jpg"),
	landscape_near_pontois=artProc("landscape_near_pontois.jpg"),
	peasants_houses=artProc("peasants_houses.jpg"),
	horta_de_ebro=artProc("horta_de_ebro.jpg"),
	three_musicians=artProc("three_musicians.jpg"),
	Les_Demoiselles=artProc("Les_Demoiselles.jpg"),
	popova=artProc("popova.jpg"),
	harp=artProc("harp.jpg"),
	clarinette_and_rum=artProc("clarinette_and_rum.jpg"),
	persistence=artProc("persistence.jpg"),
	soft_construction=artProc("soft_construction.jpg"),
	sleepface=artProc("sleepface.jpg"),
	personal_values=artProc("personal_values.jpg"),
	son_of_man=artProc("son_of_man.jpg"),
	difficult_crossing=artProc("difficult_crossing.jpg"),
	comp_red_yellow=artProc("comp_red_yellow.jpg"),
	comp_10=artProc("comp_10.jpg"),
	gray_brown=artProc("gray_brown.jpg"),
	no_17=artProc("no_17.jpg"),
	no_8=artProc("no_8.jpg"),
	no_44=artProc("no_44.jpg"))

save(art.1997.hue, file="art_hue_smaller.rda")

scale <- 50
q <- scale/2
p.1 <- c(1:q)
p.2 <- c((q+1):scale)
p.3 <- c((((scale^2)/2)+1):(((scale^2)/2)+q))
p.4 <- c((((scale^2)/2)+1+q):(((scale^2)/2)+2*q))
quad.2 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.1+scale*x)))
quad.3 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.2+scale*x)))
quad.1 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.3+scale*x)))
quad.4 <- as.vector(sapply((seq(1:q+1)-1),function(x)c(p.4+scale*x)))

quad.colors <- data.frame(index=c(1:scale^2), color=NA)
quad.colors <- sapply(quad.colors[,1],function(x) ifelse(x %in% quad.1,quad.colors[x,1]<-"#1B9E77",
										               ifelse(x %in% quad.2,quad.colors[x,1]<-"black",
										               ifelse(x %in% quad.3,quad.colors[x,1]<-"#7570B3",
										               quad.colors[x,1]<-"#E7298A"))))
##COLOR SCHEME##
Q1: "#1B9E77"
Q2: "black"
Q3: "#7570B3"
Q4: "#E7298A"
################

#########
load("art_hue_smaller.rda")
########### DATA MINING ################
#Regular old pca
art.pca <- prcomp(art, scale=TRUE)
art.t.pca <- prcomp(t(art),scale=TRUE)
par(mfrow=c(1,2))
plot(art.pca$x[,1], art.pca$x[,2], col=quad.colors, pch=16, cex=.85, legend=TRUE,
	  xlab="1st Principal Component",ylab="2nd Principal Component",main="Pixels")
legend("bottomleft", legend=c("Quad 1", "Quad 2", "Quad 3", "Quad 4"), 
       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"),
       xjust=1, yjust=1, title="Quantrants")
       
plot(art.t.pca$x[,1],art.t.pca$x[,2],col=c(rep(1,6),rep("#1B9E77",6),rep("#7570B3",6),rep("#E7298A",6)),pch=16,
     xlab="1st Principal Component",ylab="2nd Principal Component",main="Paintings")
     
lines(loess.smooth(art.t.pca$x[1:6,1],art.t.pca$x[1:6,2]),col=1, lty=2)
lines(loess.smooth(art.t.pca$x[7:12,1],art.t.pca$x[7:12,2]), col="#1B9E77", lty=1)
lines(loess.smooth(art.t.pca$x[13:18,1],art.t.pca$x[13:18,2]),col="#7570B3", lty=3)
lines(loess.smooth(art.t.pca$x[19:24,1],art.t.pca$x[19:24,2]), col="#E7298A",lty=4)


#lines(art.t.pca$x[1:6,1],art.t.pca$x[1:6,2],col=1, lty=2)
#lines(art.t.pca$x[7:12,1],art.t.pca$x[7:12,2], col="#1B9E77", lty=1)
#lines(art.t.pca$x[13:18,1],art.t.pca$x[13:18,2],col="#7570B3", lty=3)
#lines(art.t.pca$x[19:24,1],art.t.pca$x[19:24,2], col="#E7298A",lty=4)

legend("topleft", legend=c("Post-Impr", "Cubism", "Surrealism", "Abstract-Exp"), 
       text.width=strwidth("No Intercept"), pch=c(16,16,16,16), col=c("black","#1B9E77","#7570B3","#E7298A"),
       xjust=1, yjust=1, title="Art Genre")

install.packages(rge)


par(mfrow=c(1,1))  
biplot(art.t.pca)


#Kernel Pca
library(kernlab)
art.kpca.poly <- kpca(as.matrix(art), kernel="polydot", kpar=list(degree=3))
art.kpca.rad.5 <- kpca(as.matrix(art), kernel="rbfdot", kpar=list(sigma=.5), features=2)
art.kpca.rad.2 <- kpca(as.matrix(art), kernel="rbfdot", kpar=list(sigma=.2), features=2)
art.kpca.rad.1 <- kpca(as.matrix(art), kernel="rbfdot", kpar=list(sigma=.1), features=2)
art.kpca.rad.01 <- kpca(as.matrix(art), kernel="rbfdot", kpar=list(sigma=.01), features=2)
art.kpca.rad.001 <- kpca(as.matrix(art), kernel="rbfdot", kpar=list(sigma=.001), features=2)

art.kpca.rad.t <- kpca(t(as.matrix(art), kernel="rbfdot", sigma=.2, features=2))
save(art.kpca.rad, file="kpca.R")
summary(art.kpca.rad)

par(mfrow=c(2,2))
plot(rotated(art.kpca.rad.01),pch=16, col=c(quad.colors), cex=.6,xlab="1st Principal Component",ylab="2nd Principal Component", main=list(expression(sigma==.01),cex=1))
plot(rotated(art.kpca.rad.1),pch=16, col=c(quad.colors), cex=.6,xlab="1st Principal Component",ylab="2nd Principal Component", main=list(expression(sigma == .1) ,cex=1))
plot(rotated(art.kpca.rad.2),pch=16, col=c(quad.colors), cex=.6,xlab="1st Principal Component",ylab="2nd Principal Component", main=list(expression(sigma == .2), cex=1))
plot(rotated(art.kpca.rad.5),pch=16, col=c(quad.colors), cex=.6,xlab="1st Principal Component",ylab="2nd Principal Component",main=list(expression(sigma == .5), cex=1))
legend("topleft", legend=c("Quad 1", "Quad 2", "Quad 3", "Quad 4"), 
       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quantrants")


plot(rotated(art.kpca.rad),pch=16, cex=.3, col=quad.colors,xlab="1st Principal Component",ylab="2nd Principal Component")

#Projection Pursuit
art.PPDE <- fastICA(art, 3, alg.typ = "deflation", fun = "logcosh", alpha = 1,
                    method = "C", row.norm = FALSE, maxit = 1000,
                    tol = 0.0001, verbose = TRUE)
plot3d(art.PPDE$S[,1],art.PPDE$S[,2],art.PPDE$S[,3], col=quad.colors)
#art.PPDE.1 <- fastICA(art[,1:6], 2, alg.typ = "deflation", fun = "logcosh", alpha = 1,
#                    method = "C", row.norm = FALSE, maxit = 1000,
#                    tol = 0.0001, verbose = TRUE)
#par(mfrow = c(1, 2))
#plot(art.PPDE.1$X%*%art.PPDE.1$K, main = "Projection PCA", col=quad.colors, 
# xlab="1st Principal Component",ylab="2nd Principal Component",pch=16,cex=.8)
#legend("topright", legend=c("Q1", "Q2", "Q3", "Q4"), 
#       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quads", bty="n")
#       
#plot(art.PPDE.1$S, main = "ICA Components",col=quad.colors,pch=16,cex=.8,
# xlab="1st Component",ylab="2nd Component")
#legend("topright", legend=c("Q1", "Q2", "Q3", "Q4"), 
#       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quads")
#density(art.PPDE.1$S)


                    
par(mfrow = c(2, 2))
plot(art.PPDE$X%*%art.PPDE$K, main = "Projection PCA", col=quad.colors, 
 xlab="1st Principal Component",ylab="2nd Principal Component",pch=16,cex=.8)
legend("topright", legend=c("Q1", "Q2", "Q3", "Q4"), 
       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quads", bty="n")
       
plot(art.PPDE$S[,1],art.PPDE$S[,2], main = "ICA Components",col=quad.colors,pch=16,cex=.8,
 xlab="1st Component",ylab="2nd Component")
legend("topright", legend=c("Q1", "Q2", "Q3", "Q4"), 
       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quads")
density(art.PPDE$S)

plot(art.PPDE$S[,1],art.PPDE$S[,3], main = "ICA Components",col=quad.colors,pch=16,cex=.8,
 xlab="1st Component",ylab="3rd Component")
legend("topright", legend=c("Q1", "Q2", "Q3", "Q4"), 
       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quads")
density(art.PPDE$S)

plot(art.PPDE$S[,1],art.PPDE$S[,4], main = "ICA Components",col=quad.colors,pch=16,cex=.8,
 xlab="1st Component",ylab="4th Component")
legend("topleft", legend=c("Q1", "Q2", "Q3", "Q4"),bty="n", 
       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quads")
density(art.PPDE$S)

#Locally Linear Imbedding
library(lle)
#it's 12# optimal.k <- calc_k(X=art, m=2, kmin=5, kmax=15, plotres=TRUE, parallel=TRUE, cpus=2, iLLE=FALSE)
art.lle <- lle(X=art, m=3, k=12, reg = 2, ss = FALSE, id = TRUE, nnk = TRUE, iLLE = FALSE, v = .99)

# plot results and intrinsic dimension (manually)
par(mfrow=c(1,2))
plot( art.lle$Y, main="Embedded Data", xlab=expression(y[1]), ylab=expression(y[2]) ,
	col=quad.colors,pch=16,cex=.7)
legend("bottomleft", legend=c("Quad 1", "Quad 2", "Quad 3", "Quad 4"), 
       pch=c(16,16,16,16), col=c("#1B9E77","black","#7570B3","#E7298A"), title="Quantrants")
	
plot( art.lle$id, main="Intrinsic Dimension of Data", type="l", xlab=expression(x[i]), ylab="id", lwd=2 )

#Diffusion Maps

#Clustering various kinds thereof
#library(biclust)
#art.plaid <- biclust(as.matrix(art), method=BCPlaid(), cluster="c")
#art.spectral <- biclust(as.matrix(t(art)), method=BCSpectral())


#############################################
Starry_Night<-artProc("Starry_Night.jpg")
sunflowers<-artProc("sunflowers.jpg")
les_vessenots<-artProc("les_vessenots.jpg")
the_effect_of_fog<-artProc("the_effect_of_fog.jpg")
landscape_near_pontois<-artProc("landscape_near_pontois.jpg")
peasants_houses<-artProc("peasants_houses.jpg")
horta_de_ebro<-artProc("horta_de_ebro.jpg")
three_musicians<-artProc("three_musicians.jpg")
Les_Demoiselles<-artProc("Les_Demoiselles.jpg")
popova<-artProc("popova.jpg")
harp<-artProc("harp.jpg")
clarinette_and_rum<-artProc("clarinette_and_rum.jpg")
persistence<-artProc("persistence.jpg")
soft_construction<-artProc("soft_construction.jpg")
Sleep_1937<-artProc("Sleep_1937.jpg")
personal_values<-artProc("personal_values.jpg")
son_of_man<-artProc("son_of_man.jpg")
difficult_crossing<-artProc("difficult_crossing.jpg")
comp_red_yellow<-artProc("comp_red_yellow.jpg")
comp_10<-artProc("comp_10.jpg")
gray_brown<-artProc("gray_brown.jpg")
no_17<-artProc("no_17.jpg")
no_8<-artProc("no_8.jpg")
no_44<-artProc("no_44.jpg")
length(Starry_Night=artProc("Starry_Night.jpg"),)
length(sunflowers=artProc("sunflowers.jpg"),)
length(les_vessenots=artProc("les_vessenots.jpg"),)
length(the_effect_of_fog=artProc("the_effect_of_fog.jpg"),)
length(landscape_near_pontois=artProc("landscape_near_pontois.jpg"),)
length(peasants_houses=artProc("peasants_houses.jpg"),)
length(horta_de_ebro=artProc("horta_de_ebro.jpg"),)
length(three_musicians=artProc("three_musicians.jpg"),)
length(Les_Demoiselles=artProc("Les_Demoiselles.jpg"),)
length(popova=artProc("popova.jpg"),)
length(harp=artProc("harp.jpg"),)
length(clarinette_and_rum=artProc("clarinette_and_rum.jpg"),)
length(persistence=artProc("persistence.jpg"),)
length(soft_construction=artProc("soft_construction.jpg"),)
length(Sleep_1937=artProc("Sleep_1937.jpg"),)
length(personal_values=artProc("personal_values.jpg"),)
length(son_of_man=artProc("son_of_man.jpg"),)
length(difficult_crossing=artProc("difficult_crossing.jpg"),)
length(comp_red_yellow=artProc("comp_red_yellow.jpg"),)
length(comp_10=artProc("comp_10.jpg"),)
length(gray_brown=artProc("gray_brown.jpg"),)
length(no_17=artProc("no_17.jpg"),)
length(no_8=artProc("no_8.jpg"),)
length(no_44=artProc("no_44.jpg"))



	
#image2 <- as.data.frame(matrix(as.numeric(image), ncol=3))
#names(image2) <- c("red", "green", "blue")
#image.h <- atan(sqrt(3)*(image2[,2]-image2[,3])/(2*image2[,1]-image2[,2]-image2[,3]))

