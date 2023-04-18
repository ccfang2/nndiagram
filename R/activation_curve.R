#' Producing 'LaTeX' Code for Drawing Activation Functions
#' @description The \code{activation_curve} command is used to produce 'LaTeX' code for drawing well-formatted activation functions, which are crucial in neural network analysis.
#' To make the code work in a 'LaTeX' editor, users need to install and import two 'TeX' packages \href{https://www.overleaf.com/learn/latex/TikZ_package}{TikZ} and \href{https://ctan.org/pkg/pgfplots}{pgfplots} in the setting of 'TeX' file.
#' Syntax of importing these packages is included in the output of function.
#'
#' @param expr an optional character that specifies the activation function. It could be \code{"ReLU"}, \code{"sigmoid"} or \code{"step"}, which are commonly used activation functions.
#' Or, it can be a \link[base]{call} or an \link[base]{expression} written as a function of \code{x} that will evaluate to an object of the same length as \code{x}. Default is \code{"ReLU"}.
#' @param title an optional character that specifies the main title of diagram. Default is \code{NULL}.
#' @param xlabel an optional character that specifies the label of x-axis. Default is \code{NULL}.
#' @param ylabel an optional character that specifies the label of y-axis. Default is \code{NULL}.
#' @param xmin an optional numeric value that specifies the minimum value of x-axis. Default is \code{-10}.
#' @param xmax an optional numeric value that specifies the maximum value of x-axis. Default is \code{10}.
#' @param ymin an optional numeric value that specifies the minimum value of y-axis. Default is \code{NULL}.
#' @param ymax an optional numeric value that specifies the maximum value of y-axis. Default is \code{NULL}.
#' @param suppress an optional logical value that specifies whether \code{activation_curve} should suppress the output of 'LaTeX' code to be directly printed on console. Default is \code{FALSE}.
#'
#' @return \code{activation_curve} uses \code{cat()} to print out 'LaTeX' code on console, if not suppressed. Also, \code{activation_curve} saves the same output as a character vector invisibly, so users could use \code{cat()} to print it out later at their demand, as shown in Examples.
#' The \code{activation_curve} 'LaTeX' output can be directly copied and pasted to produce activation curve in any 'LaTeX' editor.
#' @seealso \link{nndiagram}; \link{nndiagram_oversize}; \link{nndiagram_nodeCoverup}.
#' @export
#'
#' @author Chencheng Fang, Bonn Graduate School of Economics, University of Bonn. Email: \email{ccfang@uni-bonn.de}
#'
#' @examples
#' # Rectified Linear Unit Function with all arguments default
#' activation_curve()
#'
#' # Sigmoid function with domain from -5 to 5.
#' activation_curve(expr="sigmoid", title="Sigmoid Function", xmin=-5, xmax=5)
#'
#' # Define a parametric ReLU in the argument of \code{expr}.
#' activation_curve(expr="(x>=0)*x+0.2*x*(x<0)", title="Parametric Rectified Linear Unit Function")
#'
#' # Suppress the output of 'LaTeX' code to be directly printed on the console and save the output
#' # to an object, which can be printed later at demand.
#' nnd_activation <- activation_curve(suppress=TRUE)
#' cat(paste(nnd_activation,"\n"))
activation_curve <- function(expr="ReLU", title=NULL, xlabel=NULL, ylabel=NULL,
                             xmin=-10, xmax=10, ymin=NULL, ymax=NULL, suppress=FALSE) {

  #---------------------------------------
  # Checking arguments and data preparation
  #---------------------------------------

  # checking if xlabel is empty or a character of length 1
  if (length(xlabel)>1) stop("'xlabel' should be either empty or a character of length 1")
  if (!is.null(xlabel) & !is.character(xlabel)) stop("'xlabel' should either be empty or character")

  # checking if ylabel is empty or a character of length 1
  if (length(ylabel)>1) stop("'ylabel' should be either empty or a character of length 1")
  if (!is.null(ylabel) & !is.character(ylabel)) stop("'ylabel' should either be empty or character")

  # checking if xmin and xmax are numeric or not
  if (!is.numeric(xmin)) stop("'xmin' should be numeric")
  if (!is.numeric(xmax)) stop("'xmax' should be numeric")
  if (length(xmin)>1) stop("'xmin' should be a single number")
  if (length(xmax)>1) stop("'xmax' should be a single number")

  # checking if ymin and ymax are numeric or not
  if (!is.numeric(ymin) & !is.null(ymin)) stop("'ymin' should be numeric")
  if (!is.numeric(ymax) & !is.null(ymax)) stop("'ymax' should be numeric")
  if (length(ymin)>1) stop("'ymin' should be a single number")
  if (length(ymax)>1) stop("'ymax' should be a single number")

  # checking if suppress is defined correctly
  if (!is.logical(suppress)) stop("'suppress' should be a logical value.")
  if (length(suppress) > 1) stop("'suppress' should be a single logical value.")

  #---------------------------------------
  # producing latex syntax
  #---------------------------------------

  expr.update <- dplyr::case_when(expr == "ReLU" ~ "(x>=0)*x",
                                  expr == "sigmoid" ~ "1/(1+exp(-x))",
                                  expr == "step" ~ "(x>=0)",
                                  TRUE ~ as.character(expr))

  title.update <- dplyr::case_when(expr == "ReLU" ~ "Rectified Linear Unit Function",
                                   expr == "sigmoid" ~ "Sigmoid Function",
                                   expr == "step" ~ "Step Function",
                                   TRUE ~ as.character(expr))

  # setting syntax
  setting <- c("\\usepackage{tikz} \n",
               "\\usepackage{pgfplots} \n",
               "\\pgfplotsset{compat=1.18} \n")

  # main syntax
  main <- c("\\begin{figure}[!ht] \n", "\\centering \n",
            "\\begin{tikzpicture}[scale=1] \n",
            "\40 \\begin{axis}[ \n",
            "\40 \40 \40 axis on top = true, \n",
            "\40 \40 \40 axis x line = bottom, \n",
            "\40 \40 \40 axis y line = left, \n",
            "\40 \40 \40 xlabel = $",xlabel,"$, \n",
            "\40 \40 \40 ylabel = $",ylabel,"$, \n",
            "\40 \40 \40 ymin = ",ymin,", \n",
            "\40 \40 \40 ymax = ",ymax," \n",
            "\40 ] \n",
            "\40 \\addplot[ \n",
            "\40 \40 \40 domain=",xmin,":",xmax,", \n",
            "\40 \40 \40 samples=100 \n",
            "\40 ] {",expr.update,"}; \n",
            "\40 \\end{axis} \n",
            "\\end{tikzpicture} \n",
            "\\caption{",title.update,"} \n",
            "\\label{} \n",
            "\\end{figure} \n"
            )

  comment <- c("% To make the code work in any 'LaTeX' editor, users need to install and import two 'TeX' packages in the setting, including 'TikZ'. \n",
               "% Users are recommended to try the output 'LaTeX' code in Overleaf. \n \n")

  # discussing whether to suppress the output or not
  if (suppress==FALSE) {
    cat(comment, setting, main, sep="")
  } else message("The output of 'LaTeX' code is designed in your command to be suppressed, but it is saved in the object, if assigned. See Examples to find out how to print out the saved output.")

  syntax <- utils::capture.output(cat(comment, setting, main, sep=""))

}

