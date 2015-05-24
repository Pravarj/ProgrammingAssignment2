## Programming assignment number 2
## the 2 functions cache inverse of a matrix
## test code:
## m <- makeCacheMatrix()
## m$set(matrix(1:4,2,2))
## cacheSolve(m)
##        [,1][,2]
## [1,]   -2  1.5
## [2,]    1 -0.5





## the matrix that is eing creates is a list containing a function that
## sets the value of the matrix, gets the value of the matrix, sets the
## value of the matrix, gets and then sets the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) 
{
  mat <- NULL
  set <-function(y){
    x <<-y
    mat <<- NULL
  }
  get <-function() x
  setInverse <-function(solve) mat <<- solve
  getInverse <-function() mat
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Calculate the inverse of the matrix that is created with the makeCacheMatrix 
## function and also reuse cached result (if there)

cacheSolve <- function(x=matrix(), ...) 
  
  {
  mat<-x$getInverse()
  if(!is.null(mat))
    {
    message ("cached data being received")
    return(mat)
  }
  else 
  {
  matrix <- x$get()
  mat <- solve(matrix, ...)
  x$setInverse(mat)
  mat
  }
}
