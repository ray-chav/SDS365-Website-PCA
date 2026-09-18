# Function that converts a JPG, JPEG, or PNG into a matrix
# For PCA compression Shiny app
library(imager)

# Input: PNG, JPEG, or JPG file
# Output: Image plotted, compressed by k
compress_image_from_imager <- function(img_path, k){

  # Read image
  img <- load.image(img_path)

  # Split into RGB channels
  channels <- imsplit(img, "c")

  # Apply your PCA compression function
  results <- lapply(channels, compress_image, k = k)

  # Extract the compressed image matrices
  compressed_channels <- lapply(results, function(x) {
    as.cimg(x[[1]])
  })

  # Put RGB channels back together
  compressed_img <- imappend(compressed_channels, "c")

  # Return compressed image and errors
  return(list(
    image = compressed_img,
    errors = sapply(results, function(x) x[[2]])
  ))
}

# read_image_as_mat <- function(img_path){
#   
#   # Read image
#   img <- load.image(img_path)
#   
#   # Split into RGB channels
#   channels <- imsplit(img, "c")
#   
#   # Return compressed image and errors
#   return(list(
#     max_p_value = dim(channels[[1]])[2],
#     channels = channels
#   ))
# }
# 
# compress_by_k <- function(channels, k){
#   
#   # Apply PCA compression function
#   results <- lapply(channels, compress_image, k = k)
#   
#   # Extract the compressed image matrices
#   compressed_channels <- lapply(results, function(x) {
#     as.cimg(x[[1]])
#   })
#   
#   # Put RGB channels back together
#   compressed_img <- imappend(compressed_channels, "c")
#   
#   # Return compressed image and errors
#   return(list(
#     image = compressed_img,
#     errors = sapply(results, function(x) x[[2]])
#   ))
# }

# path <- "../../../../WiDS/speed_dating_colopale.jpg"
# 
# result <- read_image_as_mat(path)
# 
# max_p <- result$max_p_value
# channels <- result$channels
# 
# output <- compress_by_k(channels, 237)
# 

#output <- compress_img(path, 236)[[1]]

# plot(output$image)

# Code source:
# https://dahtah.github.io/imager/imager.html#:~:text=imsplit(parrots%2C%22c%22)%20%25%3E%25%20llply(function(v)%20v/max(v)%20%25%3E%25%20imappend(%22c%22)%20%25%3E%25%20plot
