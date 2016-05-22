## The follwoing functions allow the inverse of a matrix to be stored in cache and retrieved when needed -
## without having to compute inverse repeatedly, thereby saving some computating resources.
## This code assumes the supplied matrix is always a square invertible matrix.
## The first function create a special matrix object that can cache its inverse.  

## This function creates a special "matrix" object that can cache its inverse. It is nothing but a list of functions that does-
# 1.set the value of the vector
# 2.get the value of the vector
# 3.set the value of the mean
# 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()){
## initialize cache value to NULL
  cachemx <- NULL
  setmx <- function(y) {
    x <<- y
    cachemx <<- NULL
  }
  ## retrieve store matrix
  getmx <- function() {
    x
  }
  ## cache the given value
  setInversemx <- function(solve) {
    cachemx <<- solve
  }
  ## get cached value  
  getInversemx <- function(){
    cachemx
  }
    
  list(setmx = setmx, getmx = getmx,
       setInversemx = setInversemx,
       getInversemx = getInversemx)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated -
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheInverse <- function(y, ...) {
## check for cached inverse, if exists return the value from cache and skip comuptation
  cachemx <- y$getInversemx()
  if(!is.null(cachemx)) {
    message("getting cached data")
    return(cachemx)
  }
  ## If inverse hasn't been cached before, calculate the inverse and store it in the cache
  data <- y$getmx()
  cachemx <- solve(data, ...)
  y$setInversemx(cachemx)
  ## retrun the inverse and if already cached return the cached inverse
  cachemx
}

