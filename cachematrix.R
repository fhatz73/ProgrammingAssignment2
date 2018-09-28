## this file's functions implement a method of caching the calculation of the inverse of a square matrix

## makeCacheMatrix creates a special object which caches the calculated inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  issquare <- function() dim(x)[1]==dim(x)[2]
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse,issquare=issquare)
}


## cacheSolve calculates the inverse of a square matrix if it is called for the first time for
## this particular matrix or returns the already calculated cached inverse result if the specific matrix
## has been already calculated during a previous call of cacheSolve. 
cacheSolve <- function(x=matrix(), ...) {
  ## return if matrix is not square and therefore not inversible
  if (!x$issquare()) {
    message ("matrix is not inversible") 
    return()
  }
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
