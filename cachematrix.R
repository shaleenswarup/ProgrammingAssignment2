## Put comments here that give an overall description of what your
## functions do

## This function is used to create a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
                     }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse<- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This functions checks if the inverse of the matrix has already been caluclated
## If yes then it will retrive it and if not it will calculate the inverse
cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
        ## Return a matrix that is the inverse of 'x'
}
