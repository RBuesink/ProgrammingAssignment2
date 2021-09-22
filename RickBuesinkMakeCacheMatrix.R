##example for vector

makeVector <- function(x = numeric()) {
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


## These functions cache an inverse of a matrix
##

## set value of matrix and get inverse matrix values

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function (y){ 
    x <<- y
    inv <<- NULL
  } 
  get <- function() {x}
  setInverse <- function(inv2) {inv <<- inv2}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

##solving the inverse of the matrix and returning its values

cacheSolve <- function(x, ...){
  inv <- x$getInverse()  
  if (!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
} 


