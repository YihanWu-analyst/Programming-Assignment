makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_matrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_matr?x <- function() x
  
  set_inverse <- function(inverse) m <<- inverse
  
  get_inverse <- function() m
  
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix data")
    return(m)
  }
  data <- x$get_m?trix()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}

