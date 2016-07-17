## uodw - set the value , wodw - get the value 
## (sorry for variable naming, am polish, using polish abbreviations makes it
## easier for me to conceptualize stuff that way)

## make the matrix that can store it's inverse in cache
makeCacheMatrix <- function(x = matrix()) {
        odw <- NULL
        u <- function(y) {
                x <<- y
                odw <<- NULL
        }
        w <- function() x
        uodw <- function(inverse) odw <<- inverse
        wodw <- function() odw
        list(u = u,
             w = w,
             uodw = uodw,
             wodw = wodw)
}

## compute the inverse of matrix. if already done, retrieve from cache
cacheSolve <- function(x, ...) {
        odw <- x$wodw()
        if (!is.null(odw)) {
                message("getting cached data")
                return(odw)
        }
        n_mat <- x$w()
        odw <- solve(n_mat, ...)
        x$uodw(odw)
        odw
}