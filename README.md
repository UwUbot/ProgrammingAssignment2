

<!-- -->

    makeVector <- function(x = numeric()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmean <- function(mean) m <<- mean
            getmean <- function() m
            list(set = set, get = get,
                 setmean = setmean,
                 getmean = getmean)
    }




    cachemean <- function(x, ...) {
            m <- x$getmean()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- mean(data, ...)
            x$setmean(m)
            m
    }


makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        get <- function() x
        getInverse <- function() {
                if(length(x)%%sqrt(length(x))==0) { ## make sure matrix is square
                        inv <<- solve(x)
                }
        }
        list(get = get, getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        if(!is.atomic(x)){      # check if the given parameter is atomic
                inv <- x$getInverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }     
        } else {        # if not atomic then make it
                message("getting the inverse-- no cached data found")
                return(makeCacheMatrix(x)$getInverse())
        } 

}

