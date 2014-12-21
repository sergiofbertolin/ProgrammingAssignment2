## The following functions create an especial kind of
## matrix and then compute its inverse matrix
## If we had computed the matrix before, we retrieve the
## computed value from our "special" matrix created with
## makeCacheMatrix function

## With makeCacheMatrix we first create a list of matrices 
## With no computation of Inverse matrix the value of Inv is NULL by default
## With previous computation, we can recover the value stored on the variable Inv

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
      x <<- y
      Inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) Inv <<- solve
    getInv <- function() Inv
    list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## cacheSolve is in charge of evaluate the values of Inv
## to recover the previous computed value
## If there's no Inverse matrix computed, cacheSolve 
## is computing it and storing the value in the list created
## by makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    Inv <- x$getInv()
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInv(Inv)
    Inv
  
}
