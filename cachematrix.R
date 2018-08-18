## Matrix inversion with Caching

##Make Cache Matrix
make_cachematrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve
cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cache data")
                return(inver)
        }
        matrx <- x$get()
        inver <- solve(matrx, ...)
        x$setinverse(inver)
        inver
}
