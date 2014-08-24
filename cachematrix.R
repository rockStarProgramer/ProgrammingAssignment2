## makeCacheMatrix takes an ordinary matrix and makes a "matrix" object.
## The object has the functions 
## set(for assigning matrix object), 
## get(for retrieving matrix object), 
## getInverse(for retriving cached inverse of matrix),
## setInverse (for caching the inverse of matrix)


## makeCacheMatrix defines a matrix and other functions needed
## to store and retrieve the inverse of the matrix.

makeCacheMatrix <- function(mat = matrix())
{
	 invMat <- NULL
	
	set <- function (givenMat)
	{
		mat <<- givenMat
		invMat <<- NULL
	}
	get <- function() mat
	setInverse <- function(invOfMat) invMat <<- invOfMat
	getInverse <- function() invMat
	list(set = set, get = get, setInverse = setInverse, 
	getInverse = getInverse)
}

## cacheSolve takes a "matrix" object and checks if the object already has
## a calculated inverse. If not it calculates, caches, and prints the inverse.

cacheSolve <- function(mat)
{
	inverse <- mat$getInverse()
	if(!is.null(inverse))
	{
		message("getting cached inverse")
		return(inverse)
	}
	dataMat <- mat$get()
	inverse <- solve(dataMat)
	mat$setInverse(inverse)
	inverse
}