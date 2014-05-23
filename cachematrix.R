## Programming Assignment 2 - Lexical Scoping
##
## makeCacheMatrix(x = matrix())
##      Generates a list of functions with data for a cache
##      matrix inversion solver. Initial use will result in
##      working data with a NULL result.
##
##      Example:
##      x <- makeCacheMatrix(matrix(runif(100, 1, 100), 10, 10))
##
##      Sets 'x' with a list of functions and data, including a
##      10 x 10 matrix populated with random numbers between
##      1 and 100.
##      
##      The following functions are created fpr the list environment:
##
##      - $set(y) -> Resets computational output and loads new data
##      - $get() -> Retreives working data
##      - $setSolve(solve) -> Write computed data back to cache
##      - $getSolve() -> Retreives cached computed data
##
## cacheSolve(x, ...)
##      Runs a matrix inversion on data generated using the 
##      makeCacheMatrix() function, then caches the results back
##      to the list (x). This function will also check for pre
##      computed results before running solve(). If results are
##      already computed, it will return the results without running
##      solve().
##
##      Example:
##      cacheSolve(x)
##
##      The output will return the inverse matrix back to x$setSolve(),
##      then return the output. You can retrieve the output in 1 of 2
##      ways ('xSolve' will be the matrix containing the inversion):
##
##      1) xSolve <- x$getSolve()
##      2) xSolve <- cacheSolve(x)

makeCacheMatrix <- function(x = matrix()) {
        ## Resets computed data to NULL
        m <- NULL
        
        ## Function - Empties cache and sets new data
        set <- function(y) {
                x <<- y      ## Pass 'y' back up to 'x'
                m <<- NULL   ## Reset computed data to NULL
        }
        
        ## Function - Returns input matrix
        get <- function() x
        
        ## Function - Returns computed data back to 'm'
        setSolve <- function(solve) m <<- solve
        
        ## Function - Returns data from 'm'
        getSolve <- function() m

        ## Returns list of functions
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


cacheSolve <- function(x, ...) {
        
        ## Populate 'm' with computed data contained in the list environment
        m <- x$getSolve()
        
        ## If computed data exists in the list (value of 'm' is not null),
        ## then the data will be returned.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Pulls data for computing from the list environment
        data <- x$get()
        
        ## Computes inverse matrix on the data
        m <- solve(data, ...)
        
        ## Returns computed data back to list cache
        x$setSolve(m)
        
        ## Return the value 'm'
        m
}
