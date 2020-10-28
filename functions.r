loading_img <- function()
{
  working_dir <- "~/Desktop/facial_recognizer/Training"
  setwd(working_dir)
  
  #loading file names
  filenames <- list.files()
  length(filenames)
  #loading each image to one row in matrix A
  img <- readImage(filenames[1])
  dim(img)
  red <- as.vector(t(img[,,1]))
  green <- as.vector(t(img[,,2]))
  blue <- as.vector(t(img[,,3]))
  A <- matrix(c(red,green,blue),nrow = 1)
  B <- matrix(c(parse_number(filenames[1])),nrow = 1)
  for(i in 2:length(filenames))
  {
    img <- readImage(filenames[i])
    red <- as.vector(t(img[,,1]))
    green <- as.vector(t(img[,,2]))
    blue <- as.vector(t(img[,,3]))
    A <- rbind(A, c(red,green,blue))
    B <- rbind(B, c(parse_number(filenames[i])))
  }
  results <- list("data" = A, "labels" = B)
}






pca <- function(data)
{
  #step 1. calculationg the mean of each column
  vector_ones <- matrix(1.0, nrow = nrow(data), ncol = 1)
  col_means <- matrix(colMeans(data),nrow = 1)
  data.mean <- vector_ones %*% col_means
  #substracting the mean
  data.scaled = data - data.mean
  #computing the cov matrix
  Sigma = data.scaled%*%t(data.scaled)/(ncol(data.scaled)-1)
  Eigen = eigen(Sigma)
  D = Eigen$values
  P = Eigen$vectors
  pca_results <- list('data.mean' = data.mean, 'P' = P, 'D' = D)
  return(pca_results)
}
#parameters shall contain: k, type of distance, mean of the original data, number of principal components to use
my_knn <- function(img, training_data, params)
{
  training_data = matrix(unlist(training_data), nrow = nrow(training_data))
  #step1. transforming an img to a 1 row matrix
  vector_ones <- matrix(c(1), nrow = nrow(training_data), ncol = 1)
  img_matrix <- vector_ones%*%matrix(unlist(img), nrow = 1)
  result1 <- img_matrix-training_data[,1:ncol(training_data)-1]
  result2 <- result1 %*% t(result1)
  distances <- matrix(nrow = nrow(training_data), ncol = 1)
  for(i in 1:nrow(training_data))
  {
    distances[i,] = sqrt(result2[i,i])
  }
  distances = cbind(distances, training_data[,ncol(training_data)])
  distances.sorted = distances[order(distances[,1],decreasing = F),]
  k_nn = distances.sorted[1:params, 2]
  img.label = names(which.max(table(k_nn)))
  
  return(img.label)
  
  #step2. running a pca on it
  
  #step3. projecting the data based on the number of principal components that i want to use
  
  #step4. calculate de distances between the img and the rest of the data based on the type of
  #distance I passed as a parameter and number o k-neighbours
  
  #step5. 
}