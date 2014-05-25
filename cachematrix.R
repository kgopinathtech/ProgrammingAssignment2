## makeCacheMatrix sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse, gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinv<-function(solve) i<<-solve
  getinv<-function()i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## calculates the inverse of the matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return i
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
  
}
