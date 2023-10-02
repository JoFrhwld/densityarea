#' Vowel Space Data
#'
#' This is the vowel space data from a single speaker, s01, whose
#' audio interview and transcription are part of the Buckeye Corpus
#' (Pitt et al. 2007). The transcript was realigned to the audio using the
#' Montreal Forced Aligner (McAullife et al. 2022) and vowel formant data
#' extracted with FAVE (Rosenfelder et al. 2022).
#'
#' @format ## `s01`
#' A dataframe with 4,245 rows and 10 columns
#' \describe{
#'  \item{name}{Speaker id}
#'  \item{age}{Speaker age (y=young, o=old)}
#'  \item{sex}{Speaker sex}
#'  \item{word}{Word in the transcription}
#'  \item{vowel}{Arpabet transcription of the measured vowel}
#'  \item{plt_vclass}{A modified Labov/Trager notation of the measured vowel}
#'  \item{ip_vclass}{An IPA-like transcription of the measured vowel}
#'  \item{F1}{The measured F1 frequency (Hz)}
#'  \item{F2}{The measured F2 frequency (Hz)}
#'  \item{dur}{The measured vowel duration}
#' }
#'
#' @source McAuliffe, M., Fatchurrahman, M. R., GalaxieT, NTT123,
#' Amogh Gulati, Coles, A., Veaux, C., Eren, E., Mishra, H., Paweł Potrykus,
#' Jung, S., Sereda, T., Mestrou, T., Michaelasocolof, & Vannawillerton. (2022).
#' MontrealCorpusTools/Montreal-Forced-Aligner: Version 2.0.1 (v2.0.1)
#' \doi{10.5281/ZENODO.6658586}

#'
#' @source Pitt, M. A., Dilley, L., Johnson, K., Kiesling, S., Raymond, W.,
#' Hume, E., & Fosler-Lussier, E. (2007). Buckeye Corpus of Conversational
#' Speech (2nd release). Department of Psychology, Ohio State University.
#' [https://buckeyecorpus.osu.edu/](https://buckeyecorpus.osu.edu/)
#'
#' @source Rosenfelder, I., Fruehwald, J., Brickhouse, C., Evanini, K.,
#' Seyfarth, S., Gorman, K., Prichard, H., & Yuan, J. (2022).
#' FAVE (Forced Alignment and Vowel Extraction) Program Suite v2.0.0
#' [https://github.com/JoFrhwld/FAVE](https://github.com/JoFrhwld/FAVE)
"s01"

