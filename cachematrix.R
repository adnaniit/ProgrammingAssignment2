## This function will create a matrix object that could cache its inverse
makeCacheMatrix<-function(m=matrix())
{
	inv=NULL
	set = function (y)
	{
		m<<-y
		inv<<-NULL
	}
	get = function()
		{m}
	setinverse = function (inverse)
		{inv<<-inverse}
	getinverse = function ()
		{inv}
	list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##Calculates inverse of Matrix returned by above function
##Checks for presence of inverse in cache and uses it if already present in cache
cacheSolve<-function(m,...)
{
	inv = m$getinverse()
	if (!is.null(inv))
	{
		print("Cached data is being used")
		return(inv)
	}
	matrix.data = m$get()
	inv = solve(matrix.data,...)
	m$setinverse(inv)
	return(inv)
}