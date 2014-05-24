## Programming Assignment 2 - Lexical Scoping
##
## makeCacheMatrix(x = matrix())
##      Creates a list environment containing 4 functions for storing
##      working data and caching computed data. This function is
##      primarily used in conjunction with appropriate computational 
##      functions in order to reduce computing time where repeated
##      calculations will net the same results.
##
##      Example:
##      x <- makeCacheMatrix(matrix(runif(100, 1, 100), 10, 10))
##
##      Sets 'x' with a list of functions and data, including a
##      10 x 10 matrix populated with random numbers between
##      1 and 100.
##      
##      The following functions are created for the list environment:
##
##      - $set(y) -> Resets computational output and loads new data
##      - $get() -> Retreives working data
##      - $setCompData(compData) -> Write computed data back to cache
##      - $getCompData() -> Retreives cached computed data
##
##      Initial running of this function will set computed data to a
##      NULL value. It is recommended to only run $setCompData(compData) 
##      as part of computing a solution for your working data. Having 
##      a !NULL value returned may create problems with computed results.
##
## cacheSolve(x)
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
##      1) xSolve <- x$getCompData()
##      2) xSolve <- cacheSolve(x)
##
##      This function is a dumb function. It will only check for !NULL
##      results in the cached computed data. If $setCompData() is used
##      on the compatible list data prior to computing, the input matrix
##      will not be solved and may return an incorrect result. Assuming
##      your list data is 'x', run x$setCompData(NULL) to reset your cache.
##      You can also run the full makeCacheMatrix() function again with
##      your working data as an input.

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
        setCompData <- function(compData) m <<- compData
        
        ## Function - Returns data from 'm'
        getCompData <- function() m

        ## Returns list of functions
        list(set = set, get = get,
             setCompData = setCompData,
             getCompData = getCompData)
}


cacheSolve <- function(x) {
        
        ## Populate 'm' with computed data contained in the list environment
        m <- x$getCompData()
        
        ## If computed data exists in the list (value of 'm' is not null),
        ## then the data will be returned.
        if(!is.null(m)) {
                message("Using cached data.")
                return(m)
        }
        
        ## Pulls data for computing from the list environment
        data <- x$get()
        
        ## Computes inverse matrix on the data
        m <- solve(data)
        
        ## Returns computed data back to list cache
        x$setCompData(m)
        
        ## Return the value 'm'
        m
}
