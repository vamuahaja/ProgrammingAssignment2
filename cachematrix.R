## Caching the Inverse of a Matrix

## This function creates and return a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      getMatrix <- function() x
      setInverse <- function(inverse){
            inv <<- inverse 
      }
      getInverse <- function() inv
      
      list(getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes and return the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)){
            return(inv)
      }else{
            sMatrix <- x$getMatrix()    #sMatrix means special matrix
            inv <- solve(sMatrix)
            x$setInverse(inv)
            inv
      }
}