#loading packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,OpenImageR,reshape,tidyr,readr)

#setting working dir
working_dir <- paste(getwd(),"Training",sep="/")
setwd(working_dir)

#loading file names
filenames <- list.files()
parse_number(filenames[1])


#loading each image to one row in matrix A
img <- readImage(filenames[1])
red <- as.vector(t(img[,,1]))
green <- as.vector(t(img[,,2]))
blue <- as.vector(t(img[,,3]))
A <- matrix(c(red,green,blue),nrow = 1)
B <- matrix(c(parse_number(filenames[1])),nrow = 1)
B
for(i in 2:length(filenames))
{
  img <- readImage(filenames[i])
  red <- as.vector(t(img[,,1]))
  green <- as.vector(t(img[,,2]))
  blue <- as.vector(t(img[,,3]))
  A <- rbind(A, c(red,green,blue))
  B <- rbind(B, c(parse_number(filenames[i])))
}
#concatenating image data (A), with its ID (B)
A <- cbind(A,B)
nrow(A)

macierz_jednostkowa <- matrix(1.0, nrow = nrow(A), ncol = 1)
dim(macierz_jednostkowa)
typeof(macierz_jednostkowa)
macierz_srednie <- matrix(colMeans(A),nrow = 1)
dim(macierz_srednie)
typeof(macierz_srednie)
X_mean <- macierz_jednostkowa %*% macierz_srednie
X_mean[1:20,1:20]
dim(matrix(colMeans(A),nrow = 1))
#TODO podział zestawu danych na treningowy i validacyjny (każda klasa tj. każde zdj musi być równie liczna)





#TODO pamietać o czyms jak thres hold stworoznym na podstawie macierzy odleglosci zdjec od siebie

  

rm(list = ls())
dev.off()
cat("\014")
