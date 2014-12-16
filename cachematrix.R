
# Creates a special "matrix", which is really a list containing a function to

#     set the value of the matrix
#     get the value of the matrix
#     setsolve the value of the inverse of the matrix 
#     getsolve the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
	# init matrix
    mat <- NULL
    # set a matrix 
    set      <- function(y)
        {
        x    <<- y
        mat  <<- NULL
        }
    # get the value of the matrix
    get      <- function() x
    # set the value of the inverse of the matrix
    setsolve <- function(inverseMatrix) mat <<- inverseMatrix
    # get the value of the inverse of the matrix
    getsolve <- function() mat
    # add the funtions to a list
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve will retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...)
{
    # init result
    res <- NULL
    # get the cached matrix
    mat <- x$getsolve()
    # if it's not exist cached matrix
    if( is.null(mat) )
    	{
    	# get the matrix to calculate the inverse matrix
	    data <- x$get()
	    # It's a matrix ?
	    if (is.matrix(data))
	    	{
	    	# determine the dimension of the matrix
	   	    dimMatrix <- dim( data )
		    # it's a square matrix ?
			if ( dimMatrix[1] == dimMatrix[2] )
		    	{
		    	# figure out the inverse ot the matrix
			    mat <- solve(data, ...)
			    # cache the result
	   			x$setsolve(mat)
	   			# return the inverse of matrix
	   			mat
	   	 		}
	   	 	else
	   	 		# it's not a square matrix
	   	 		message( paste( str(data), "It's not a square matrix", sep = "") )
	   	 	}
	    else
	   	 	# it's not a matrix	    		
	   	 	message( paste( str(data), "It's not a matrix", sep = "") )
	    }
	else
    	{
    	# the data is getting from cache
        message("getting cached data")
	   	# return the inverse of matrix
        mat
    	}
}
