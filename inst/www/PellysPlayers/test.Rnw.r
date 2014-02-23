\documentclass[11pt]{article}
\usepackage[margin=1in]{geometry}   % set up margins
\usepackage{enumerate}              % fancy enumerate
\usepackage{amsmath}                % used for \eqref{} in this document
\usepackage{verbatim}               % useful for \begin{comment} and \end{comment}
\usepackage{comment}
\usepackage[pdftitle={Homework With knitr}, colorlinks=true, linkcolor=blue,
citecolor=blue, urlcolor=blue, linktocpage=true, breaklinks=true]{hyperref}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{document}
%%%%%%%%%%%%%%%%   Sweave Options  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\SweaveOpts{fig.path='./Graphs/alan-', comment=NA, prompt=FALSE}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\title{Tables with \textbf{xtable} and \textbf{knitr}}
\author{Alan T. Arnholt\\ STT 3851}
\date{Spring 2012}
\maketitle

<<setup, echo = FALSE, results= 'hide', message = FALSE>>=
require(xtable)
require(PASWR)
options(width = 98)
@

Here are a few examples using the function \texttt{xtable()} form the \textbf{R} package \texttt{xtable} I used to generate \LaTeX{} code for tabular output without manually entering the values in a tabular environment. To get beyond the basic examples in this document, read the documentation and customize until you are happy.  Consider a table created with \texttt{xtabs()}.

<<MyT1, echo = TRUE, results = 'markup'>>=
T1 <- xtabs(~ Ease + Treatment, data = EPIDURALf)
T1
@

Now we want this to appear in a ``pretty'' format.

<<MyLaTeXT1, echo = FALSE, results = 'asis'>>=
xtable(T1)
@

Creating a caption with \texttt{xtable()} for Table \ref{MyT1}.
<<MyLaTeXT1Caption, echo = FALSE, results = 'asis'>>=
xtable(T1, caption ="Table of Something", label = 'MyT1')
@

Consider the regression results below and those shown in Tables \ref{RR} and \ref{RR2}.

<<RA, echo = TRUE, results ='markup'>>=
mod <- lm(gpa ~ sat, data = Grades)
SR <- summary(mod)$coefficients
SR
AR <- anova(mod)
AR
@

<<Reg, echo = FALSE, results = 'asis'>>=
xtable(SR, caption ="Regression results", label ="RR")
@

<<Reg2, echo = FALSE, results = 'asis'>>=
xtable(SR, caption ="Regression results with 4 digits", label ="RR2", digits = 4)
@

<<ANOVA, echo = FALSE, results = 'asis'>>=
xtable(AR, caption ="ANOVA", label ="AR")
@

Suppose you want the label \texttt{Pr(>F)} in Table \ref{AR} to read $\wp$-value.

<<ANOVAfix, echo = FALSE, results = 'asis'>>=
colnames(AR) <- c("DOF", "Sum Sq", "Mean Sq", "F value", "$\\wp$-value")
AR <- xtable(AR, caption ="ANOVA with $\\wp$-value changed", label ="ARpvalue")
print(AR, sanitize.text.function = function(x){x})
@

\end{document}