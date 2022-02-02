\name{INTS}
\alias{INTS}


\title{
Create Range of Indices
}
\description{
A command such \code{INTS(i,j)} returns a one-dimensional array built of the integers \code{i, i+1, i+2, ..., j} when \code{i, j} are both scalars, and \code{j} is greater than \code{i}. When \code{j} is less than \code{i}, the command shown above defines a one-dimensional array built of the integers \code{i, i-1, i-2, ..., j}.\cr\cr Users can specify the \code{k} increment using a syntax like \code{INTS(i, j, k)} which defines a one-dimensional array built of the values \code{i, i+k, i+2*k, ..., i+N*k}.\cr\cr
The value of the last element of the array is the maximum value of \code{i+N*k} that is less than or equal to \code{j}, for positive \code{k}. For negative \code{k}, the value of the last element of the array is the minimum value of \code{i+N*k} that is greater than or equal to \code{j}.\cr\cr
The command can be used with one parameter by using a syntax like \code{INTS(i)} where \code{i} is a positive scalar. The result is a one-dimensional array built with the integers \code{1, 2, 3, ..., i}. When \code{i} is less than \code{1}, the array is built with the integers \code{-1, -2, ..., -i}. 
}
\usage{
INTS(FROM=NULL, TO=NULL, BY=NULL, ...)
}

\arguments{
  \item{FROM}{
The first integer of the sequence. If arguments  \code{TO} and \code{BY} are \code{NULL} and \code{FROM>0} the sequence will start from \code{1} and will end in \code{FROM}; If arguments  \code{TO} and \code{BY} are \code{NULL} and \code{FROM<0} the sequence will start from \code{-1}  and will end in \code{FROM} (see example).
}
  \item{TO}{
The last integer of the sequence.
}
  \item{BY}{
The increment between two elements of the sequence.
}

  \item{...}{
Backward compatibility.
}
  
}

\value{
This function returns an object of class \code{c()}.
}

\seealso{
\code{\link[=TSJOIN]{TSJOIN}}\cr
\code{\link[=TSEXTEND]{TSEXTEND}}\cr
\code{\link[=TSMERGE]{TSMERGE}}\cr
\code{\link[=MOVAVG]{MOVAVG}}\cr
\code{\link[=GETYEARPERIOD]{GETYEARPERIOD}}\cr
\code{\link[=TSLAG]{TSLAG}}\cr
\code{\link[=TSINFO]{TSINFO}}\cr
\code{\link[=TABIT]{TABIT}}\cr
\code{\link[=ELIMELS]{ELIMELS}}\cr
}


\examples{
	
	print(INTS(10,1,-2)) #... 10  8  6  4  2
	
	#...Error in INTS(10, 1, -0.5) : INTS(): inputs must be integers.
	tryCatch({print(INTS(10,1,-0.5));},error=function(e){cat(e$message)})
	
	print(INTS(10))  #...  1  2  3  4  5  6  7  8  9 10
	print(INTS(-10)) #  -1  -2  -3  -4  -5  -6  -7  -8  -9 -10
	
	# Error in INTS(0) : INTS(): magnitude must be >=1
	tryCatch({print(INTS(0));},error=function(e){cat(e$message)})
	
	print(INTS(-10,-45)) # -10 -11 -12 ... -41 -42 -43 -44 -45
	
	#...Error in seq.default(FROM, TO, BY) : wrong sign in 'by' argument
	tryCatch({print(INTS(-10,-45,3));},error=function(e){cat(e$message)})
	
	print(INTS(-10,-45,-3)) # -10 -13 -16 -19 -22 -25 -28 -31 -34 -37 -40 -43
}
