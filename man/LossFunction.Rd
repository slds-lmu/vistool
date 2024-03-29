% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LossFunction.R
\name{LossFunction}
\alias{LossFunction}
\title{Loss Function}
\description{
This class is used to create loss functions.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{`character(1)`\cr
Unique identifier of the loss function.}

\item{\code{fun}}{`function(y_true, y_pred, ...)`\cr
Loss function.}

\item{\code{label}}{`character(1)`\cr
Label of the loss function.}

\item{\code{properties}}{`character()`\cr
Additional properties of the loss function.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LossFunction-new}{\code{LossFunction$new()}}
\item \href{#method-LossFunction-clone}{\code{LossFunction$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LossFunction-new"></a>}}
\if{latex}{\out{\hypertarget{method-LossFunction-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this [R6][R6::R6Class] class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LossFunction$new(id, label, properties, fun)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(`character(1)`)\cr
Unique identifier of the loss function.}

\item{\code{label}}{(`character(1)`)\cr
Label of the loss function.}

\item{\code{properties}}{(`character()`)\cr
Additional properties of the loss function.}

\item{\code{fun}}{(`function(y_true, y_pred, ...)`)\cr
Loss function.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LossFunction-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LossFunction-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LossFunction$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
