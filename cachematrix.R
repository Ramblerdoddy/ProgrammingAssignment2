## These functions perform a matrix inversion on a matrix provided by the user. 
## If the calculation has been performed before then it will use the cached 
## result to save time.

## This function creates a special list containing functions that allow 
## manipulation of the provided matrix object

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() {
                x
        }
        setinverted <- function(inv) {
                inverted <<- inv
        }
        getinverted <- function() {
                inverted
        }
        list(
                set = set,
                get = get,
                setinverted = setinverted,
                getinverted = getinverted
        )
        
}


## This function will search to see if there is a cached version of the matrix 
## inversion. If so it will return it, otherwise it will first perform the 
## inversion.

cacheSolve <- function(x, ...) {
        inverted <- x$getinverted()
        if (!is.null(inverted)) {
                message("Using cached inversion")
                return(inverted)
        }
        matrix <- x$get()
        inverted <- solve(matrix)
        x$setinverted(inverted)
        inverted
}
