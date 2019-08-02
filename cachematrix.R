## The following functions, makeCacheMatrix() and cacheSolve(),
## are used for caching inverse  of a (square) matrix; 

## For demistifying what function 
## makeVector() does (which is very, very similar function
## - or almost identical - to our makeCacheMatrix(), 
## see Len Greski's great detailed step-by-step explanation at 
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


## The function makeCacheMatrix creates a list of four objects 
## (all four are functions), which help retrieve cached 
## matrix inverse

makeCacheMatrix <- function(x = matrix()) {

   xinv <- NULL
   set <- function(y) {
      x <<- y
      xinv <<- NULL
   }
   get <- function() x
   setinverse <- function(tocache) xinv <<- tocache
   getinverse <- function() xinv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   xinv <- x$getinverse()
   if(!is.null(xinv)) {
      message("getting cached matrix inverse")
      return(xinv)
   }
   data <- x$get()
   xinv <- solve(data, ...)
   x$setinverse(xinv)
   xinv
}
