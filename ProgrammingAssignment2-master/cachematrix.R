#The first function, makeVector creates a special "vector", which is really a list containing a function to
# 
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeVector<- function(x, ...){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

makeCacheMatrix<- function(x, ...){
  #This function creates a special "matrix" object that can cache its inverse.
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              
  setInverse <- function(inverse) invMatrix <<- inverse  
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve<- function(x, ...)
  {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       
    message("Getting Cached Invertible Matrix")   
    return(invMatrix)                            
  }
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     
  invMatrix <- solve(MatrixData, ...)             
  x$setInverse(invMatrix)                         
  return(invMatrix)   
}

makeCacheMatrix<- function(x, ...){
  #This function creates a special "matrix" object that can cache its inverse.
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              
  setInverse <- function(inverse) invMatrix <<- inverse  
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}
#########Testing##########

TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

