## Put comments here that give an overall description of what your
## functions do

## This function sets the matrix 'x', puts it in cache, gets the matrix, calculates its inverse 
## and puts it in cache. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m<<- solve
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## This function gets the inverse of 'x' in cache. If the inverse exists returns its value, 
## else it calculates the inverse of 'x' and sets its value in the cache via the setinv 
## function

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinv(m)
  m
}
