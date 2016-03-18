# Objective: Understanding the advantage of scoping rules of the R language
# <<- is used to assign a value to an object in a different environment from current environment

# The following two functions create and store a matrix and its inverse in cache 
# or read the matrix inverse from the cache.
# makeCacheMatrix creates a special matrix that can cache its inverse
	# makeCacheMatrix runs four functions
	# set function stores the matrix in the cache
	# get function read the matrix from the cache
	# setinverse function stores the inverse of matrix in the cache
	# getinverse function read the inverse of matrix from the cache
 
makeCacheMatrix <- function(x = matrix()) 
{	
	inv <- NULL
      set <- function(y) 
	{
		x <<- y
	      inv <<- NULL			# store matrix in the cache
	}
	get <- function() x 			# get matrix
	setinverse <- function(solve) inv <<- solve	# calculate and set inverse
	getinverse <- function() inv		# read the inverse matrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)	# return a list of the four fuctions
}


# cacheSolve take the special matrix created by the makeCacheMatrix function
	# and checks if the inverse has been calculated for the matrix
	# if it has been done before, it reads the data from the cache 
	# if it has not been done before (i.e. the matrix is changed)
	# it calculates the inverse for the matrix then store it in the cache

cacheSolve <- function(x, ...) 
{	
	inv <- x$getinverse()		# read the inverse matrix from getinverse
	if(!is.null(inv)) {
		message("Inversed already calculated. Getting cached data")	
						# if inverse is already calculated
	return(inv)				# return the cache
	}
	data <- x$get()			# read the matrix created by makeCacheMatrix
	inv <- solve(data, ...)		# calculate the inverse of the matrix
	x$setinverse(inv)			# store the inverse matrix in cache
	inv
}



# Testing of the function
# > source("matrixinverse.R")
# > test_matrix <- makeCacheMatrix(matrix(sample(1:20, 9), 3, 3))
# > test_matrix$get()
#      [,1] [,2] [,3]
# [1,]   11    5   15
# [2,]    3   19    1
# [3,]    7    4   20
# > test_matrix$getinverse()
# NULL
# > cacheSolve(test_matrix)
#             [,1]         [,2]        [,3]
# [1,]  0.18287938 -0.019455253 -0.13618677
# [2,] -0.02577821  0.055933852  0.01653696
# [3,] -0.05885214 -0.004377432  0.09435798
# > cacheSolve(test_matrix)
# Inversed already calculated. Getting cached data
#             [,1]         [,2]        [,3]
# [1,]  0.18287938 -0.019455253 -0.13618677
# [2,] -0.02577821  0.055933852  0.01653696
# [3,] -0.05885214 -0.004377432  0.09435798
# > test_matrix$getinverse()
#             [,1]         [,2]        [,3]
# [1,]  0.18287938 -0.019455253 -0.13618677
# [2,] -0.02577821  0.055933852  0.01653696
# [3,] -0.05885214 -0.004377432  0.09435798
# > test_matrix <- makeCacheMatrix(matrix(sample(1:20, 9), 3, 3))
# > test_matrix$get()
#      [,1] [,2] [,3]
# [1,]   17    8    6
# [2,]   20   14    9
# [3,]    1    7   12
# > test_matrix$getinverse()
# NULL
# > cacheSolve(test_matrix)
#            [,1]        [,2]        [,3]
# [1,]  0.1515152 -0.07792208 -0.01731602
# [2,] -0.3333333  0.28571429 -0.04761905
# [3,]  0.1818182 -0.16017316  0.11255411
# > cacheSolve(test_matrix)
# Inversed already calculated. Getting cached data
#           [,1]        [,2]        [,3]
# [1,]  0.1515152 -0.07792208 -0.01731602
# [2,] -0.3333333  0.28571429 -0.04761905
# [3,]  0.1818182 -0.16017316  0.11255411