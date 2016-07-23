########################################################
# R Programming - Assignment 2
########################################################
## Author: vuiseng9@gmail.com
## Assignment: Caching the Inverse of a Matrix
## Two functions to cache the inverse of a Matrix

## Function 1: makeCacheMatrix
## This function stores a list of functions which are:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
makeCacheMatrix <- function(m = matrix()) {
  inv_m <- NULL
  
  ## function to store the matrix of interest (from inputs) in the main function
  setMat <- function(y) {
    m <<- as.matrix(y)
    inv_m <<- NULL
  }
  
  ## function to return the matrix stored in the main function
  getMat <- function() m
  
  ## function to store the inverse of a matrix in the main function
  setInvMat <- function(InvMat) inv_m <<- InvMat
  
  ## function to extract the inverse of a matrix in the main function
  getInvMat <- function() inv_m
  
  ## to store the 4 functions in the makeCacheMatrix function
  list(	setMat = setMat, 
        getMat = getMat,
        setInvMat = setInvMat,
        getInvMat = getInvMat)
}

## Function 2: This function computes the inverse of the  
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## getting the inverse of matrix x, x has to be an object of makeCacheMatrix
  inv_m <- x$getInvMat()
  
  ## if the inv of matrix x is not null,
  ## meaning the inv matrix is cached, 
  ## the cached inv matrix is thus returned
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  
  ## if evaluation reaches this point, it means the inv matrix is not stored
  ## following is the computation of the inverse using solve() of base package
  ## the resulted inv matrix is returned.
  data <- x$getMat()
  inv_m <- solve(data, ...)
  x$setInvMat(inv_m)
  inv_m
}

## Example of Usage
## Define a matrix
## > A = matrix(c(2, 4, 3, 1),nrow=2,ncol=2,byrow = TRUE)
##
## create the special Matrix
## > H <- makeCacheMatrix(A)
##
## checking through the 4 functions, there is no inverse value yet
##
## caching the current H for the first time, it will return the inv mat
## > cacheSolve(H)
## 
## if we execute the command again, we will get the inv mat, 
## and also it's from cache as we know from the message print
## "getting cache data"
##
## Now, to validate that if we change the matrix,
## cacheSolve will re-compute the inv mat
##
## Let's make a 3 by 3 mat this time
## > A <- matrix(c(2, 4, 3, 1, 7, 8, 9, 0, 3),nrow=3,ncol=3,byrow = TRUE)
##
## H$setMat(A)
##
## when we try H$getInvMat(), it return NULL, 
## as the cached inv mat is being set null once a new definition of matrix
##
## the final step, inv mat based on the new matrix is evaluated
## > cacheSolve(H)
