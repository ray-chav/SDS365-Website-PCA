# SDS365 - HW2 Pt. 1
# Image Compression Function

# Input: (n x p) matrix image as a df, k dimensions
# Output:
#   1) rank k approximation using prcomp(), eigen()
#   2) Error of rank k approx. using norm()
compress_image <- function(
                          image,
                          k){
  
  ## Rank k Approx. ===============
  # 1. Grab result of PCA (comes with eigenvectors)
  #cov_matrix = cov(image)
  pca_k = prcomp(image, rank. = k)
  
  # 2. Find U; (p x p) matrix of eigenvectors
  #U = eigen(cov_matrix)$vectors
  U = pca_k$rotation # argument containing eigenvectors
  
  # 3. Find Z; (n x p) transformed PC matrix
  #Z = prcomp(image, rank. = k)$x # ? Can we just only do first k
  Z = pca_k$x
  
  # 4. Compute rank k approximation of X
  k_X = Z %*% t(U) # Requires transpose of U
  # Rescale!
  k_X = scale(k_X, center = -pca_k$center, scale = FALSE)
  
  ## Rank k Error Approx. =========
  # 1. Define / Calculate error matrix
  # using formula:
  # X - rank X approx.
  error_matrix = as.matrix(image) - k_X
  
  # 2. Approximate the error
  # run: norm(error matrix, "F")
  err_norm = norm(error_matrix, "F")
  
  ## Return values ================
  return(list(k_X,
              err_norm))
  
}

# Test Code =======================

# Read in files -------------------
# Grab sample files
img1 = read.csv("image1.csv")
img2 = read.csv("image2.csv")
img3 = read.csv("image3.csv")
img4 = read.csv("image4.csv")

# Test function values 1 ----------

# Run function and get output
pca_k = compress_image(img4, 3)

# Grab k approx
k_approx = pca_k[[1]]
image(k_approx)

image(as.matrix(img4))

# Test function values 2 ----------
error = pca_k[[2]]

?eigen
# lowkey i think its like
# prcomp( DATA , rank. = RANK NUMBER)
# ??? do i need to specify k?