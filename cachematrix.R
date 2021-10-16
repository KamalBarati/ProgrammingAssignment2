## I have just simply rename the example in the
## Assignment description from "mean" and "m"
## to "solve" and "s"
## We know that solve is a function in R to calculate inverse of a matrix

## The purpose here is Creating a matrix object, that can cache its inverse 
## The part below creates a matrix that caches its inverse. Hereby x is a function argument; s is created in the makeCacheMatrix environment and set at NULL.
# set is also created in the makeCacheMatrix environment and creates a function (y), wherein  both x and s are placed in the parent environment, here the input is set in x and s is set at NULL, resetting any value that may have been present for s in the global environment.
# besides x and s, get, setsolve, getsolve and list are created in the makeCacheMatrix environment. 
# In get a function is created that retrieves x from the parent environment of makeCacheMatrix.  
# In setsolve a function is created, whereby the inverse is set as s, and s is defined in the parent environment (meaning the inverse is set by retrieving s from the parent environment)
# In getsolve s - being the inverse of x is retrieved. 
# Finally in list all of the functions are assigned as an element and returned to the parent environment


makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Same as the previous!

## The purpose here is Caching inverse
# Cache inverse, by creating function cacheSolve in which a matrix is returned that is the inverse of x
# cacheSolve starts with a function, with argument x and ellipses in which extra arguments can be placed.
# Within the function s tries to retrieve the inverse of x by x$getsolve
# if there is no result (aka s is NULL), cached data is retrieved for s and returned to the parent environment  
# next, the inverse of x is calculated and set


cacheSolve <- function(x, ...) {
    s <- x$getsolve()
      if(!is.null(s)) {
        message("getting cached data")
        return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
