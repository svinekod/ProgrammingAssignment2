## creates a special "matrix" object that can cache the input matrix and its inverse

makeCacheMatrix <- function(x = getmatrix()) {
  m<-NULL##default setting before cacheSolve is executed
  set<-function(y){
    x<<-y
    m<<-NULL##<<- assignes values to an object in an environment, different from the current one
  }
  get<-function() x##getting the matrix value
  setmatrix<-function(solve) m<<- solve##setting the inverse matrix
  getmatrix<-function() m##getting the inverse matrix value
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  #making the list, used as an input for cacheSolve function
}



cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
## Return a matrix that is the inverse of 'x'. If the inverse has already been calculated (and the matrix has not changed), 
##cachesolve would return the cached inverse.
