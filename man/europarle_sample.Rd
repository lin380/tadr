\name{europarle_sample}
\alias{europarle_sample}
\docType{data}
\title{
European Parliament Proceedings Parallel Corpus 1996-2011 (Spanish-English)
}
\description{
The Europarl Parallel Corpus is extracted from the proceedings of the European Parliament. This corpus is a sample from the Spanish-English pair.
}
\usage{data("europarle_sample")}
\format{
  A data frame with 200,000 observations on the following 3 variables.
  \describe{
    \item{\code{type}}{Either: Source or Target language}
    \item{\code{sentence_id}}{Id to index the sentence pairs}
    \item{\code{sentence}}{Each line from the proceedings, including comments}
  }
}
\details{
Version 7 release.
}
\source{
\url{https://www.statmt.org/europarl/}
}
\references{
Koehn, P. (2005, September). Europarl: A parallel corpus for statistical machine translation. In MT summit (Vol. 5, pp. 79-86).
}
\examples{
data(europarle_sample)
}
\keyword{datasets}
