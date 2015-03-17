
# Take a matrix associate a list of functions. Includes get and set matrix as well as get and set matrix inverse
makeMatrix <- function(x = matrix()) {
  inv <- NULL

  # Set function. matrix is stored. inverse is not.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function
  get <- function() x
  
  # set inverse of matrix. allows some value to be stored as the inverse of x.
  setinv <- function(inverse) inv <<-inverse
  
  # return the stored inverse.
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# if inverse of a matrix object is present retrieve it. Otherwise set it.
cacheinverse <- function(x, ...) {
  inv <- x$getinv()
  
  # if inverse is already cached, retreive it a exit.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if inverse is not cached, retrieve x and calculate the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}