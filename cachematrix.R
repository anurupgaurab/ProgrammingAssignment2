## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix conversts the matrix into a matrix with special functions
## and CacheSolve caches the inverse and outputs when required


## Makecache Matrix creates a list that takes a matrix as input and adds "get","set", "getinv"
## and "setinv" functionalities to it


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
      x<<-y
      inv<<-NULL
  }
  get<- function() x
  setinv<-function() inv<<- solve(x) #creating the inverse
  getinv<-function() inv
  
  list(set=set, get=get, setinverse=setinv, getinverse=getinv)
}


## CacheSolve caches the inverse and outputs when required
## first it gets the cached inverese and if it is empty it calcluates the inverse 
## for the rquired matrix

cacheSolve <- function(x, ...) {
  ## Getting the inverse
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse()
  inv
}
