## The following functions create an especial kind of
## matrix and then compute its inverse matrix
## If we had computed the matrix before, we retrieve the
## computed value from our "special" matrix created with
## makeCacheMatrix function

## With makeCacheMatrix we first create a list to store matrices and their corresponding
## inverse matrices
## With no computation of Inverse matrix the value of Inv is NULL by default
## With previous computation, we can recover the value stored on the variable Inv

makeCacheMatrix <- function(x = matrix()) {
    
    ## initially Inv is NULL (no inverse computations made)
    Inv <- NULL
    
    ## with set we store a matrix in our list
    set <- function(y) {
      x <<- y
      Inv <<- NULL
    }
    
    ## with get we can retrieve the value of a matrix in our list
    get <- function() x
    
    ## with setInv we store a Inverse matrix in our list
    setInv <- function(Inverse) Inv <<- Inverse
    
    ## with get we can retrieve the value of a Inverse matrix in our list
    getInv <- function() Inv
    
    ## Finally we define the list with all the needed parameters
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
    
    ## First we check if there´s a Inverse matrix computed for the matrix passed as a parameter of cacheSolve
    Inv <- x$getInv()
    
    ## If the Inverse matrix has been previously computed, recover the value from our list instead of re-compute it
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    
    ## If there's no computation, it's high time to do it
    data <- x$get()
    Inv <- solve(data, ...)
    
    ## It's important to store our new Inverse matrix in the list created by makeCacheMatrix
    x$setInv(Inv)
    
    ## Return the Inverse matrix required
    Inv
  
}
