## makeCacheMatrix is a function that creates a matrix and compute it's invese, 
## then stores it in cached data (global Enviroment)


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve uses the output of makeCacheMatrix to return the inverse of the matrix in 
## question, or compute and set it if it isn't found

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inversed")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
     ## Return a matrix that is the inverse of 'x'
}
