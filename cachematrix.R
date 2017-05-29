## THis function is able to cache potentially time-consuming
## computatios on the inverse of a Matrix


##The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion) m <<- inversion
        getinversion <- function() m
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}

## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix aove
cacheSolve <- function(x, ...) {
        m <- x$getinversion()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inversion <- function(x){
                rn <- nrow(x)
                cn <- ncol(x)
                y <- matrix(rep(0, rn*cn), cn, rn)
                for (i in 1:cn) {
                        y[i,] <- x[,i]
                }
                y
        }
        m <- inversion(data)
        x$setinversion(m)
        m
}











