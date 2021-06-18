\name{sms}
\alias{sms}
\docType{data}
\title{SMS Spam Collection v. 1}
\description{
The SMS Spam Collection v.1 is a public set of SMS labeled messages that
have been collected for mobile phone spam research. It has one collection
composed by 5,574 English, real and non-enconded messages, tagged according
being legitimate (ham) or spam.
}
\usage{data("sms")}
\format{
  A data frame with 4837 observations on the following 2 variables.
  \describe{
    \item{\code{sms_type}}{Type of message; either spam or ham}
    \item{\code{message}}{The sms message}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/smsspamcollection.zip
}
\references{
http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
}
\examples{
data(sms)
}
\keyword{datasets}
