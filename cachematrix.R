## I have just simply rename the example in the
## Assignment description from "mean" and "m"
## to "solve" and "s"

## The purpose here is Creating a matrix object, that can cache its inverse 
## The part below creates a matrix that caches its inverse. Hereby x is a function argument; m is created in the makeCacheMatrix environment and set at NULL.
# set is also created in the makeCacheMatrix environment and creates a function (y), wherein  both x and m are placed in the parent environment, here the input is set in x and m is set at NULL, reseting any value that may have been present for m in the global environment.
# besides m and s, get, setinverse, getinverse and list are created in the makeCacheMatrix environment. 
# In get a function is created that retrieves x from the parent environment of makeCacheMatrix.  
# In setinverse a function is created, whereby the inverse is set as m, and m is defined in the parent enviroment (meaning the inverse is set by retrieving m from the parent environment)
# In getinverse m - being the inverse of x is retrieved. 
# Finally in list all of the functions are asigned as an element and returned to the parent environment


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
# Within the function m tries to retrieve the inverse of x by x$getinverse
# if there is no result (aka m is NULL), cached data is retrieved for m and returned to the parent environment  
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
