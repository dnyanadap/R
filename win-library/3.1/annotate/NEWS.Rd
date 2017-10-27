\name{annotate-NEWS}
\title{annotate News}

\section{CHANGES IN VERSION 1.43.}{

  \subsection{NEW FEATURES}{

    \itemize{

      \item \code{blastSequences} accepts an argument \code{timeout}
      limiting waiting time for a response; in an interactive session
      and after the timeout is reached, the user may opt to retry the
      query.

      \item \code{blastSequences} accepts an argument
      \code{as} controlling the representation of the return value,
      either a DNAMultipleAlignment, a data.frame, or the XML.

    }
  }
}
