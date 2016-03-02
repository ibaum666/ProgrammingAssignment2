
## Put comments here that give an overall description of what your
## functions do

## Cached Matrix inverse
makeCacheMatrix<- function(x = matrix()) {
  mat <- NULL #initialize inverse as null
  set <- function(y) { #function that assigns a value to x
    x <<- y
    mat <<- NULL
  } 
  get <- function() x #get original value x
  setinv <- function(inv) mat <<- inv #set inverse of x 
  getinv <- function() mat #get inverse of x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) #return the object with a list of functions
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) { #If it is already calculated, return the already calculated value
    message("getting cached inverse")
    return(m)
  } #otherwise, get the data
  data <- x$get()
  m <- solve(data, ...) #caluculate inverse
  x$setinv(m) #set inverse
  m #return it
}
