\name{see.palette}
\alias{see.palette}

\title{See a color palette with the original painting.
}

\description{Displays a plot with the specified color palette along with the original painting.
}

\usage{
see.palette(artist, painting)
}

\arguments{
  \item{artist}{A number indicating artist. Artists have a number determined by their order in the package. Check \link{palette.summary} to see artists numeric order.
}
  \item{painting}{A number indicating painting. Paintings have a number determined by their order in the artist category. Check \link{palette.summary} to see paintings numeric order in each artist category.
}
}

\details{
Arguments artist and painting cannot be vectorized.
}

\author{
Federico A Casale, \email{casale@hhu.de}
}

\seealso{
\link{palette.summary}, \link{see.all.paintings}, \link{get.color}
}

\examples{
see.palette(artist=8, painting=2)
see.palette(8, 2)
}
