## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix conversts the matrix into a matrix with special functions
## and CacheSolve caches the inverse and outputs when required


## Write a short comment describing this function

## Makecache Matrix creates a list that takes a matrix as input and adds "get","set", "getinv"
## and "setinv" functionalities to it


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  ## set function assigns the value to the matrix
  set<-function(y){
    x<<-y
    inv<<-matrix(data=NA)
  }
  ## get outputs the matrix  
  get<- function() x
  ## Setinv caclulates the inverse and assigns it to "inv"
  ## if input isn't anything it inverts the stored matrix
  setinv<-function(inverse=solve(x)) inv<<- as.matrix(inverse)
  ## outputs the 'inv'
  getinv<-function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function


## CacheSolve caches the inverse and outputs when required
## first it gets the cached inverese and if it is empty it calcluates the inverse 
## for the rquired matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
