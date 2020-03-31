\name{get.color}
\alias{get.color}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get color Hex codes from palette.
%%  ~~function to do ... ~~
}
\description{Provides colors' Hex codes from a specified palette.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.color(artist, painting, color)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{artist}{A number indicating artist. Artists have a numeric order in the package. Check \link{palette.summary} to see artists numeric order.
}
  \item{painting}{A number indicating painting. Paintings have a numeric order in the artist category they belong to. Check \link{palette.summary} to see paintings numeric order in each artist category.
}
  \item{color}{A numeric vector indicating colors. Colors have a numeric order in the palette they belong to. Check \link{see.palette} to see colors numeric order in a specified palette.
}

}
\details{
Arguments artist and painting cannot be vectorized.
}
\value{
The output is a vector of colors' Hex codes as characters.
}

\author{
Federico A Casale, \email{casale@hhu.de}
}

\seealso{

\link{palette.summary}, \link{see.all.paintings}, \link{see.palette}

}

\examples{
get.color(artist=8, painting=2, color=c(1:5))
get.color(8, 2, c(1:5))
}
