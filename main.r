#loading packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(pacman,OpenImageR,reshape)

#loading data
img <- readImage("Training/1AT.jpg")
imageShow(img)
dim(img)
#TODO moj obrazek jest 3 wymiarowa tablica. Musze przekonwertowac to do jedno wymiarowej tablicy
#zapytać sie jak to zrobic????
#TODO zrobić pętle, lub funkcja która wczytuje wszystkie jpegi z folderu do macierzy
  
img <- readJPEG(system.file("img", "Training/1AT.jpg", package="jpeg"))
writePNG(img)
