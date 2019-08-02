## The following functions, makeCacheMatrix() and cacheSolve(),
## are used for caching inverse  of a (square) matrix; 

## For demistifying what function 
## makeVector() does - which is very, very similar
## or almost identical function - to our makeCacheMatrix(), 
## see Len Greski's great detailed step-by-step explanation at 
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


## The function makeCacheMatrix creates a list of four objects 
## (all four are functions), which are used to retrieve cached 
## matrix inverse

makeCacheMatrix <- function(x = matrix()) {

   xinv <- NULL
   set <- function(y) {
      x <<- y   ## changing the value of x, which is the matrix
                ## from the parental environment function makeCachMatrix,
                ## so we need <<- in order to change the value of 
                ## variable 'x' (which is NOT within the environment
                ## of set function)
                
      xinv <<- NULL
   }
   get <- function() x
   setinverse <- function(tocache) xinv <<- tocache
   getinverse <- function() xinv
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## function cacheSolve finds inverse (denoted "xinv") of the input 
## matrix IF the input  matrix's inverse was not computed earlier,
## or else, if it WAS computed, it just retrieves the stored value

cacheSolve <- function(x, ...) {
   
   ## Return matrix 'xinv', which is the inverse of 'x'
   xinv <- x$getinverse() ## we retrieve inverse of 'x'; (xinv may be
                          ## NULL, which is the case if inverse of 
                          ## this particular 'x' was never computed)
   
   if(!is.null(xinv)) { ## if 'xinv' is NOT NULL, the inverse of 'x'
                        ## was already computed earlier, so we retrieve/return it
      message("getting cached matrix inverse")
      return(xinv)  ## return inverse of 'x' and stop the execution 
                    ## of cacheSolve function (again, this is the case 
                    ## IF the inverse of 'x' was already stored earlier)
   }
   data <- x$get()  ## if xinv is NULL we continue with the code
                    ## by getting the value of matrix 'x' (in fact,
                    ## our 'x' is LIST, and the actual value of the 
                    ## input matrix is )
   xinv <- solve(data, ...)
   x$setinverse(xinv)
   xinv
}
