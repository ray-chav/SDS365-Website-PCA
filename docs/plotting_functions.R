## Misc. Plotting Functions
# Homework 2

# Input: image as a dataframe
# Output: Original image, plus 10
# PCA compression of that image
first_10_img <- function(img){
  
  # 1) Original image ---------
  image(as.matrix(img), sub = "Original image")
  
  # 2) First 10 PCA approximations ----
  for (i in 1:10){
    
    # Grab function output
    pca_k = compress_image(img, i)
    
    # Create compressed image
    image(pca_k[[1]], sub = paste("This is k value:", i))
    
  }
}

# Input: image as a dataframe
# Output: Made error
k_error <- function(img){
  
  # Calculate highest number of dimensions p
  p = dim(img)[2]
  
  # Set up vector of k errors
  k_errors = numeric(p)
  
  # Loop through to find each error
  for (i in 1:p){
    pca_k= compress_image(img, i)
    
    # Add erroro value to vector
    k_errors[i] = pca_k[[2]]
  }
  
  # Make plot
  plot(1:p, k_errors, 
       main = "Approximate Error for all k", 
       xlab = "k-value", 
       ylab = "Approx. Error", 
       pch = 16,
       cex = 0.5, # make points smaller
       col = "purple4")
  
}

# Input: image as dataframe
# Output: Plot of first eigenvector
eigenvector_plot <- function(img){
  
  # Grab first eigenvector --------
  cov_matrix = cov(img)
  first_eigen = eigen(cov_matrix)$vectors[,1]
  
  # Make plot
  barplot(first_eigen, 
          main = "First Eigenvector", 
          ylab = "Loading Value", 
          xlab = "Variables",
          col = 'lavender')
  
}

# Input: image as dataframe
# Output: Plot of first PC score
pcscore_plot <- function(img){
  
  # Grab first PC score -----------
  first_pcscore = prcomp(img)$x[,1]
  
  # Make plot
  barplot(first_pcscore,
          col = "lavender",
          main = "First PC Score", 
          ylab = "PC Score", 
          xlab = "Sample Indiex",)
  
}
