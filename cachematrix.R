## To avoid costly matrix computations, the two functions below
## cache the inverse of a matrix

## The makeCacheMatrix function creates a special Matrix
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix is created in the global environment which is 
  ## where it will first seek any variables that it cannot locate within itself.
  
  ## makeCacheMatrix will create some new functions like setmatrix 
  ## which it will retrun as a list to the calling variable in the 
  ## global environment
  ## setmatrix() can be called asa function from the global environment
  
  ## makevector creates m as an empty or NULL matrix. 
  ## It also creates its four children functions and returns them as a 
  ## callable list of functions.
  ## First Function: set the value of the Matrix
  ## Second Function: get the value of the Matrix
  ## Third Function: set the inverse of the Matrix
  ## Fourth Functions: get the inverse of the Matrix
  
  ## On the first call of getmatrix(), getmatrix finds m stored in 
  ## makeCacheMatrix's environment and finds it is empty.  
  ## So it calls setmatrix from within getmatrix and does a rather 
  ## long calculation and saves the answer in m.  Finally it returns m.

  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## Returns a list of functions set, get, setInverse, and getInverse
  get <- function() x
  
  ## set the inverse of a matrix
  setInverse <- function(solve) m  <<- solve
  
  ## get the inverse of a matrix
  getInverse <- function() m
  
  ## this returns the concatenation of the list of formal arguments 
  ## and the function body
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## The function checks whether the object's inverse has been already 
  ## been set and just retrieves it in order to return it, if set. 
  ## Otherwise, it first calculates and sets the inverse, before returning it. 
  
  ## load the inversve value
  m <- x$getInverse()
  
  ## data is already cached.  No need to calcualte inverse
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## Calculates inverse (Solve) of a matrix, before returning it.
  matrixdata <- x$get()
  m <- solve(matrixdata, ...)
  x$setInverse(m)
  m
}