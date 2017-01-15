#Es - Almacena una matriz y su inversa 
#En - Store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
		inversa <- NULL
		set <- function(y){
			x <<- y
			inversa <<- NULL
		}

		get <- function() x
    	setInversa <- function(inv) inversa <<- inv
    	getInversa <- function() inversa
    	list(set=set, get=get, setInversa=setInversa, getInversa=getInversa)
}

#Es - Obtiene la inversa de una matriz, si esta ya ha sido cacheada, devuelve esta en vez de recalcularla 
#En - Get the inverse of a matrix, if this is already cached, it is not calculated


cacheSolve <- function(x, ...) {
    inversa <- x$getInversa()
    if(!is.null(inv)) {
        return(inv)
    }
    
    matriz <- x$get()
    inversa <- solve(matriz)
    x$setInversa(inversa)
    inversa
}