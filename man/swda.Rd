\name{swda}
\alias{swda}
\docType{data}
\title{
Switchboard Dialog Act Corpus
}
\description{
A dataset containing 1,155 5-minute conversations of 441 speakers of American
English created in 1997 and tagged with a shallow discourse tagset of approximately 60 basic dialog act tags (DAMSL) and combinations.
}
\usage{data("swda")}
\format{
  A data frame with 223,606 observations on the following 16 variables.
  \describe{
    \item{\code{doc_id}}{ID for each conversation document}
    \item{\code{topic_num}}{Topic number associated with the conversation}
    \item{\code{topicality}}{Subjective rating of the annotator whether the callers conversed generally about what was suggested by the recorded prompt. Scale of 1 to 5, 1 being most on topic.}
    \item{\code{naturalness}}{Subjective rating of the annotator whether the the conversation sounded natural. Scale of 1 to 5, 1 being the most natural.}
    \item{\code{damsl_tag}}{DAMSL dialog act annotation labels}
    \item{\code{speaker}}{Label for each speaker in the conversation}
    \item{\code{turn_num}}{Number of contiguous utterance turns for a given speaker}
    \item{\code{utterance_num}}{The cumulative number of utterances in the conversation}
    \item{\code{utterance_text}}{The actual dialog utterance. Includes disfluency annotation (see details below)}
    \item{\code{speaker_id}}{ID for each speaker}
    \item{\code{sex}}{The biological sex of the speaker}
    \item{\code{birth_year}}{Year that the speaker was born}
    \item{\code{dialect_area}}{Region from the US where the speaker spent first 10 years}
    \item{\code{education}}{Highest educational level attained: values 0, 1, 2, 3, and 9}
    \item{\code{topic}}{Topic description}
    \item{\code{topic_prompt}}{Specific topic prompt for the conversation}
  }
}
\details{
More information on the metadata in this data can be found here: \url{https://catalog.ldc.upenn.edu/docs/LDC97S62/swb1_manual.txt}.

The SWBD-DAMSL manual can be found here: \url{https://web.stanford.edu/~jurafsky/ws97/manual.august1.html}.

The Dysfluency Annotation Stylebook for the Switchboard Corpus can be found here: \url{https://staff.fnwi.uva.nl/r.fernandezrovira/teaching/DM-materials/DFL-book.pdf}.
}
\source{
Switchboard-1 Release 2 \url{https://catalog.ldc.upenn.edu/docs/LDC97S62/}
}
\references{
Godfrey, John J., and Edward Holliman. Switchboard-1 Release 2 LDC97S62. Web Download. Philadelphia: Linguistic Data Consortium, 1993.

Jurafsky, Daniel, Elizabeth Shriberg, and Debra Biasca. 1997. "Switchboard SWBD-DAMSL Shallow-Discourse-Function Annotation Coders Manual, Draft 13"  University of Colorado, Boulder Institute of Cognitive Science Technical Report 97-02

Meteer, Marie and Ann Taylor. 1995.  Dysfluency Annotation Stylebook for the Switchboard Corpus
}
\examples{
data(swda)
}
\keyword{datasets}
