## makeCacheMatrix is used for scoping 
## cacheSolve is used to compute the inverse of the matrix

## The following function is used to cache the inverse of
## the matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
## Matrix is cached
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## the following function is used to compute the inverse 
## of the matrix and also checks if inverse is in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getmatrix()
	  if(!is.null(m)) {
	    message("getting cached data")
	    return(m)
	  }
	  data <- x$get()
	  m <- solve(data, ...)
  	## solve is used to compute the inverse of the input argument
	  x$setmatrix(m)
  m
}
