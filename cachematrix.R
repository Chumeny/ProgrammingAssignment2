## This functions cache the inverse of a matrix to save computation time
## functions do

## Sets the values of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inv) i<<-inv
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}


## Inverts the matrix and seeks for cache value if already exits

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data matrix")
    return(i)
  }
  data<-x$get()
  
  i<-solve(data,...)
  x$setinverse(i)
  i
}