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











