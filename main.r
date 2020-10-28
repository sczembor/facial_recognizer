#loading packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,OpenImageR,reshape,tidyr,readr,splitstackshape)

#loading functions defined in another file
setwd("~/Desktop/facial_recognizer")
source('functions.r')
#setting working dir

# working_dir <- paste(getwd(),"Training",sep="/")
# setwd(working_dir)
# 
# #loading file names
# filenames <- list.files()
# length(filenames)
# #loading each image to one row in matrix A
# img <- readImage(filenames[1])
# dim(img)
# red <- as.vector(t(img[,,1]))
# green <- as.vector(t(img[,,2]))
# blue <- as.vector(t(img[,,3]))
# A <- matrix(c(red,green,blue),nrow = 1)
# B <- matrix(c(parse_number(filenames[1])),nrow = 1)
# for(i in 2:length(filenames))
# {
#   img <- readImage(filenames[i])
#   red <- as.vector(t(img[,,1]))
#   green <- as.vector(t(img[,,2]))
#   blue <- as.vector(t(img[,,3]))
#   A <- rbind(A, c(red,green,blue))
#   B <- rbind(B, c(parse_number(filenames[i])))
# }
#concatenating image data (A), with its ID (B)
#A <- cbind(A,B)
#nrow(A)
data = loading_img()
A = data$data
B = data$labels

results = pca(A)
eigenvalues = results$D
Eigenvectors = results$P
dim(Eigenvectors)
data.mean = results$data.mean
cum.var = cumsum(eigenvalues)/sum(eigenvalues)
cum.var[35]#first 35 principal components cover more than 95% of variance

data.scaled = A - data.mean
Eigenvectors = t(data.scaled)%*%Eigenvectors#Need for t(data.scaled) multiplication in order to recover Sigma long
dim(data.scaled)
length(eigenvalues)
data.new = data.scaled%*%Eigenvectors[,1:35]
data.labeled = cbind(data.new, B)
dim(data.new)
dim(data.labeled)
cov(data.new)
#TODO podziałzestawu danych na treningowy i validacyjny (każda klasa tj. każde zdj musi być równie liczna))
data.labeled.framed = as.data.frame(data.labeled)
str = stratified(data.labeled.framed, 'V36',4,bothSets = T)
train.data = str$SAMP1
#train.labels = str$SAMP1[,36]
test.data = str$SAMP2[,-36]
test.labels = str$SAMP2[,36]

#calling knn function
params = 4
vec = matrix(nrow = 50,ncol = 1)
trafienia = 0
for(p in 1:50)
{
  img = test.data[p,]
  vec[p,] = my_knn(img, train.data, params)
  if(test.labels[p] == vec[p,]) trafienia = trafienia + 1
}
#cbind(vec,test.labels[1:50,])
print(paste("dokladnosc testu:", trafienia/50))

#TODO pamietać o czyms jak thres hold stworoznym na podstawie macierzy odleglosci zdjec od siebie

  

rm(list = ls())
dev.off()
cat("\014")
