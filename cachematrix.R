## Put comments here that give an overall description of what your
## functions do

## Function which takes a matrix as input, and outputs a list of length four, containing functions which store and retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

	cachedInv <- NULL
						
        set <- function(y) {

                x <<- y

                cachedInv <<- NULL

        }

        get <- function() x

        setInv <- function(solve) cachedInv <<- solve

        getInv <- function() cachedInv

        list(set = set, get = get,

             setInv = setInv,

             getInv = getInv)

}


## Function which takes the output list from makeCacheMatrix as input, and computes and stores the inverse if it has not already been computed, or retrieves it and displays a message if it has, and then returns the inverse.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        cachedInv <- x$getInv()

        if(!is.null(cachedInv)) {

                message("getting cached data")

                return(cachedInv)

        }

        data <- x$get()

        cachedInv <- solve(data, ...)

        x$setInv(cachedInv)

        cachedInv
}