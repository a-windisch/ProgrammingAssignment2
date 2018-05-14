##==============================================================
##=    Lexical Scoping in R - Cache the inverse of a matrix    =
##==============================================================
## Assignment 02 for coursera.org course 'R Programming' 
##--------------------------------------------------------------
## Andreas Windisch, 2018, andreas.windisch@yahoo.com
##--------------------------------------------------------------
##              GENERAL DESCRIPTION
##--------------------------------------------------------------
##The two functions below exploit the lexical scoping 
##of R to cache the inverse of a matrix. Every time the
##inverse is requested, it will first check whether it has
##been cached. If so, the cached value is returned, otherwise
##the inverse will be computed, written into the cache and
##finally returned. A subsequent call without a change of the 
##initial matrix will then always result in a return of the 
##cached inverse. This can be useful to avoid costly computations 
##of (large) matrix inverses. 
##---------------------------------------------------------------
##USAGE: After sourcing the .R file, the two functions can be 
##used as follows:
##> mcm <- makeCacheMatrix(matrix(1:4,2,2))
##> cacheSolve(mcm) #will return the inverse
##> cacheSolve(mcm) #will now return the cached inverse
##---------------------------------------------------------------
##         DESCRIPTION OF makeCacheMatrix
##---------------------------------------------------------------
##This function takes a (square) matrix as an arument and 
##provides a parent environment in which four functions are
##defined. Those functions are dedicated to the purpose of
##setting and getting a matrix, as well as setting and 
##retrieving the matrix inverse. Apart from the four functions
##which live in the parent environment, the parent environment 
##also stores the object of the matrix that is to be inverted,
##along with its inverse. The function returns a list with 
##the four functions, which inherently also have the information
##about the matrix x the function has been called with.
##The functions make use of the <<- operator to assign values
##to the objects x and minv, which live in the parent environment
##of the function.
##---------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
   minv <- NULL
   set <- function(y){
      x <<- y
      minv <<- NULL
   }
   get <- function() x
   setinv <- function(solve) minv <<- solve
   getinv <- function() minv
   list( set = set, get = get, setinv = setinv, getinv = getinv)
   
}

##---------------------------------------------------------------
##       DESCRIPTION OF cacheSolve
##---------------------------------------------------------------
##This function takes the output of a makeCacheMatrix call as
##an argument. It thus has access to the matrix x that has been
##used with the makeCacheMatrix call, and it has also access
##to the four functions set, get, setinv and getinv. 
##First, it is tested whether an inverse has been cached. 
##If this is not the case, the matrix is retrieved through the
##get function, its inverse is computed, cached via the x$setinv
##function, and returned. If there is already a cached value 
##available, a message is produced that states that a cached 
##value has been found, and the value is returned without any
##further operations being executed in this function.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   minv <- x$getinv()
   if(!is.null(minv)){
      message("getting cached data")
      return(minv)
   }
   data <- x$get()
   minv <- solve(data,...)
   x$setinv(minv)
   minv
}
