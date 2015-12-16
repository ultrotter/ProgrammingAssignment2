## cachematrix.R is a solution to "R Programming" assignment 2.
##
## makeCacheMatrix allows creating an object holding a matrix and optionally a
## cache to its solution.

## cacheSolve will use the object to return the solution, using the cache.

## Create a "CacheMatrix" object, which contains
##   - a matrix
##   - a cached solution
##   - getter and setter methods for the solution
##   - getter and setter methods for the matrix
##
## Note: setting a new matrix invalidates the cached solution
makeCacheMatrix <- function(x = matrix()) {
        # Cached solution
        solution <- NULL

        # Matrix setter, update matrix and invalidate solution
        set <- function(y) {
                x <<- y
                solution <<- NULL
        }

        # Read the matrix
        get <- function() x

        # Cache the solution
        setsol <- function(sol) solution <<- sol

        # Return cached solution
        getsol <- function() solution

        # List of methods, returned
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}


## Given a "CacheMatrix" return the solution
##   - if the solution is in the cache, it is returned
##   - if the cache is empty the solution is also cached for future use
cacheSolve <- function(x, ...) {
        sol <- x$getsol()
        if(!is.null(sol)) {
                message("using cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsol(sol)
        sol
}
