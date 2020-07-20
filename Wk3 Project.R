print("This file was created within RStudio")

## Below are a pair of functions that creates a special matrix, and then stores/
## caches the inverse of that matrix.  
## Caching of calculations that are repeatedly needed (especially ones that
## are long or time intensive, can help simplify the analysis.   

## This first function is creating an object where the matrix is stored

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y) {
                x <<- y
                z <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) z <<- inverse
        getInverse <- function() z
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This second function calculates the inverse of the matrix, and 
## caches the inverse for later recall/reference.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getInverse()
        if (!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        mat <- x$get()
        z <- solve(mat, ...)
        x$setInverse(z)
        z
}

print("And now it lives on GitHub")

