## Build an object that can cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 
  
  set <- function(y){  #Set new matrix
    x <<- y
    inv <<- NULL
  } 
  
  get <- function() x #Return original matrix
  
  setInverse <- function(inverse) #Save the inverse
    inv <<- inverse
  
  getInverse <- function() inv #Return the inverse
  
  list(set = set, #Return a list of the 4 methods
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Use cached matrix or calculate it if it doesn't exist

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #Check if inv is saved
  
  if(!is.null(inv)){
    message("Using cached inverse")
    return(inv)} #If we have inv we use it from the cache
  
  #If we have no inv saved:
  mat <- x$get() #Get original matrix
  
  inv <- solve(mat, ...) #Calculate the inv
  
  x$setInverse(inv) #Save (cache) the inv
  
  inv #Return the inv
}
