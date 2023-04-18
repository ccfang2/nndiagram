#' Producing 'LaTeX' Code for Drawing Over-sized Neural Network Diagrams
#' @description The \code{nndiagram_oversize} command is used to produce 'LaTeX' code for drawing well-formatted neural network diagrams, some layers of which have excess neurons that users hope to leave out. To make the code work in a 'LaTeX' editor, users need
#' to install and import the 'TeX' package \href{https://www.overleaf.com/learn/latex/TikZ_package}{TikZ} in the setting of 'TeX' file. Syntax of importing this package is included in the output of function.
#'
#' @param input a positive integer that specifies the number of input neurons.
#' @param hidden a positive integer vector that specifies the number of neurons on each hidden layer. For example, c(6,6,6) specifies that there are 3 hidden layers and 6 neurons on each hidden layer. Non-positive and non-integer numbers are not allowed.
#' @param size.cutoff an optional numeric value that specifies the cutoff number of neurons. If the number of neurons on a certain layer is greater than this cutoff value, it is considered as an over-sized layer, which will be drawn with some neurons left out.
#' Otherwise, all neurons on the layer will be drawn. In \code{nndiagram_oversize}, the cutoff number is defined to be no less than \code{5}. Default is \code{7}.
#' @param title an optional character that specifies the main title of diagram. Default is \code{NULL}.
#' @param color an optional character that specifies the color of lines. Default is \code{"black"}.
#' @param alpha an optional numeric value between 0 and 1 that specifies the opacity of lines. \code{1} indicates lines to be opaque, and \code{0} indicates lines to be transparent. Default is \code{1}.
#' @param layer.sep an optional positive numeric value that specifies the distance between layers of a neural network. Default is \code{2.5}.
#' @param layer.label an optional character vector that specifies label for each layer, including input, hidden and output layers.
#' @param input.label an optional character vector that specifies label for each input neuron.
#' @param output.label an optional character that specifies label for output neuron.
#' @param suppress an optional logical value that specifies whether \code{nndiagram_oversize} should suppress the output of 'LaTeX' code to be directly printed on console. Default is \code{FALSE}.
#'
#' @return \code{nndiagram_oversize} uses \code{cat()} to print out 'LaTeX' code on console, if not suppressed. Also, \code{nndiagram_oversize} saves the same output as a character vector invisibly, so users could use \code{cat()} to print it out later at their demand, as shown in Examples.
#' The \code{nndiagram_oversize} 'LaTeX' output can be directly copied and pasted to produce neural network diagram in any 'LaTeX' editor.
#' @seealso \link{nndiagram}; \link{nndiagram_nodeCoverup}; \link{activation_curve}.
#' @export
#'
#' @note This package is an ongoing project, and more functions will be added in the future, such as those to produce pdf version of diagrams or convert handdrawing neural network diagrams to computerized ones. Collaborations are sincerely welcome.
#' Comments and suggestions are always highly appreciated.
#'
#' @author Chencheng Fang, Bonn Graduate School of Economics, University of Bonn. Email: \email{ccfang@uni-bonn.de}
#'
#' @examples
#' # A neural network with 3 neurons on input layer, 6 neurons on each of 3 hidden layers, and 1 neuron
#' # on output layer. All other arguments are default, so no layer is considered to be over-sized.
#' nndiagram_oversize(input=3, hidden=c(6,6,6))
#'
#' # Same as the first example but cutoff value of size is designed to be 5, so all hidden layers are
#' # considered to be over-sized.
#' nndiagram_oversize(input=3, hidden=c(6,6,6), size.cutoff=5)
#'
#' # Same as the second example but labels of input neurons are designed to be letters from a to c.
#' nndiagram_oversize(input=3, hidden=c(6,6,6), size.cutoff=5, input.label=letters[1:3])
#'
#' # Suppress the output of 'LaTeX' code to be directly printed on the console and save the output
#' # to an object, which can be printed later at demand.
#' nnd_oversize <- nndiagram_oversize(input=3, hidden=c(6,6,6), size.cutoff=5, suppress=TRUE)
#' cat(paste(nnd_oversize,"\n"))
nndiagram_oversize <- function(input, hidden, size.cutoff=7, title=NULL, color="black",alpha=1,
                               layer.sep = 2.5, layer.label=NULL, input.label=NULL, output.label=NULL, suppress=FALSE) {

  #---------------------------------------
  # Checking arguments and data preparation
  #---------------------------------------

  # checking if input is a positive single integer
  if (!is.numeric(input) | input <=0 | (input != floor(input))) stop("'input' should be a positive integer")
  if (length(input)>1) stop("'input' should be a single number")

  # checking if elements in hidden are positive integers
  if (!is.numeric(hidden) | any(hidden <= 0)) stop("Elements in 'hidden' should be positive numbers")
  if (any(hidden != floor(hidden))) stop("Elements in 'hidden' should be integers")

  # checking if out-off size is a positive integer no less than 5
  if (size.cutoff<5 | !is.numeric(size.cutoff) | (size.cutoff != floor(size.cutoff))) stop("'size.cutoff' should be a positive integer no less than 5")
  if (length(size.cutoff)>1) stop("'size.cutoff' should be a single number")

  # checking if alpha is between 0 and 1
  if (alpha<0 | alpha>1 | !is.numeric(alpha)) stop("'alpha' should be a number between 0 and 1")
  if (length(alpha)>1) stop("'alpha' should be a single number")

  # checking if layer.sep is a positive single number
  if (layer.sep<=0 | !is.numeric(layer.sep)) stop("'layer.sep' should be a positive number")
  if (length(layer.sep)>1) stop("'layer.sep' should be a single number")

  # checking if layer.label is defined correctly
  if (is.null(layer.label))
    layer.label <- c("Input layer", paste0("Hidden layer ",1:length(hidden)), "Output layer") else {
      if (!is.null(layer.label) & length(layer.label) != (length(hidden)+2))
        stop("Number of layer labels don't coincide with number of layers")
    }

  # checking if input.label is defined correctly
  if (!is.null(input.label) & length(input.label)!= input)
    stop("Number of input labels don't coincide with number of input")

  # checking if output label is defined correctly
  if (is.null(output.label))
    output.label <- "Output" else {
      if (!is.null(output.label) & length(output.label)!= 1)
        stop("There are more than 1 output labels")
    }

  # checking if suppress is defined correctly
  if (!is.logical(suppress)) stop("'suppress' should be a logical value.")
  if (length(suppress) > 1) stop("'suppress' should be a single logical value.")

  # selecting layers which are oversized and compressing oversized layers
  layers_original <- c(input, hidden)
  oversize.layers <- which(layers_original>size.cutoff)
  layers_compressed <- c(input, hidden)
  layers_compressed[oversize.layers] <- 5
  hidden_compressed <- layers_compressed[-1]

  # finding out layers with maximum neurons after compression
  max.layer <- which.max(layers_compressed)

  # updating input layer after compression
  input.update <- ifelse(max.layer == 1 & 1 %in% oversize.layers, 5, input)

  #---------------------------------------
  # producing latex syntax
  #---------------------------------------

  # setting syntax
  setting <- c("\\usepackage{tikz} \n",
               "\\def\\layersep{",layer.sep, "cm} \n",
               "\\newcommand*\\circled[1]{\\tikz[baseline=(char.base)]{ \n",
               "\40 \\node[shape=rectangle,inner sep=3pt, draw=",color,"!",alpha*100,", fill= ",color,"!",alpha*25,"] (char) {#1};}} \n \n")

  # head syntax
  head <- c("\\begin{figure}[!ht] \n", "\\centering \n",
            "\\begin{tikzpicture}[shorten >=1pt,draw=",color,"!",alpha*100,", node distance=\\layersep, scale=1] \n",
            "\40 \\tikzstyle{every pin edge}=[<-,shorten <=1pt]; \n",
            "\40 \\tikzstyle{neuron}=[circle, draw=",color,"!",alpha*100,", minimum size=17pt,inner sep=0pt]; \n",
            "\40 \\tikzstyle{invisible neuron}=[draw=none, scale=2]; \n",
            "\40 \\tikzstyle{input neuron}=[neuron]; \n",
            "\40 \\tikzstyle{output neuron}=[neuron]; \n",
            "\40 \\tikzstyle{hidden neuron}=[neuron]; \n",
            "\40 \\tikzstyle{annot} = [text width=4em, text centered, text=",color,"!",alpha*100,"] \n \n")

  # producing syntax for drawing neurons
  if (max.layer == 1) {
    ## when input layer is one of the layers which have the most neurons in compressed layers
    ## drawing neurons on input layer
    if (is.null(input.label)) {
      if (1 %in% oversize.layers) {
        input_layer <- c("\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input 1}] (I-1) at (0,-1) {}; \n",
                         "\40 \\node [invisible neuron] (I-2) at (0,-2) {}; \n",
                         "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input ", ceiling((1+input)/2), "}] (I-3) at (0,-3) {}; \n",
                         "\40 \\node [invisible neuron] (I-4) at (0,-4) {}; \n",
                         "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input ", input, "}] (I-5) at (0,-5) {};")
        } else {
          input_layer <- c("\40 \\foreach \\name / \\y in {1,...,",input,"} \n",
                           "\40 \40 \40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,-\\y) {};")
          }
      } else {
        if (1 %in% oversize.layers) {
          input_layer <- c("\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{",input.label[1],"}] (I-1) at (0,-1) {}; \n",
                           "\40 \\node [invisible neuron] (I-2) at (0,-2) {}; \n",
                           "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{",input.label[ceiling((1+input)/2)], "}] (I-3) at (0,-3) {}; \n",
                           "\40 \\node [invisible neuron] (I-4) at (0,-4) {}; \n",
                           "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{",input.label[input], "}] (I-5) at (0,-5) {};")
          } else {
            input_layer <- ""
            nextline <- c(rep("\n",length(input.label)-1),"")
            for (i in 1:length(input.label))
              input_layer <- c(input_layer, "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{", input.label[i],"}] (I-",i,") at (0,-",i,") {};", nextline[i])
          }
      }

    ## drawing neurons on hidden layers
    hidden_layers <- ""
    i<-1
    for (j in 1:length(hidden)) {
      h <- hidden[j]
      if ((j+1) %in% oversize.layers) {
        hidden_layers <- c(hidden_layers, "\n",
                           "\40 \\node[hidden neuron,yshift=0cm] (H-",i,") at (",j,"* \\layersep, -1 cm) {}; \n",
                           "\40 \\node[invisible neuron,yshift=0cm] (H-",i+1,") at (",j,"* \\layersep, ", ceiling((-3-input.update)/4), "cm) {}; \n",
                           "\40 \\node[hidden neuron,yshift=0cm] (H-",i+2,") at (",j,"* \\layersep, ", ceiling((-1-input.update)/2), "cm) {}; \n",
                           "\40 \\node[invisible neuron,yshift=0cm] (H-",i+3,") at (",j,"* \\layersep, ",ceiling((-3*input.update-1)/4)," cm) {}; \n",
                           "\40 \\node[hidden neuron,yshift=0cm] (H-",i+4,") at (",j,"* \\layersep, -", input.update, "cm) {};")
        i=i+5
      } else {
        hidden_layers <- c(hidden_layers,"\n",
                           "\40 \\foreach \\name / \\y in {",paste0(i:(i-1+h),"/",1:h,sep= rep(c(",",""),c(h-1,1))),"} \n",
                           "\40 \40 \40 \\path[yshift=",(h-input.update)/2,"cm] \n",
                           "\40 \40 \40 \40 \40 node[hidden neuron] (H-\\name) at (",j,"* \\layersep,-\\y cm) {};")
        i=i+h
      }
    }
    hidden_layers <- c(hidden_layers,"\n")

    ## drawing neuron on output layer
    output_layer <- c("\40 \\node[output neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{",output.label,"}}, right of=H-",
                      floor((sum(hidden_compressed[-length(hidden_compressed)])+1+sum(hidden_compressed))/2),", yshift=",
                      floor((sum(hidden_compressed[-length(hidden_compressed)])+1+sum(hidden_compressed))/2)-(sum(hidden_compressed[-length(hidden_compressed)])+1+sum(hidden_compressed))/2,"cm] (O) {}; \n")
  } else {
    ## when input layer is not one of layers which have the most neurons
    ## drawing neurons on input layer
    if (is.null(input.label)) {
      if (1 %in% oversize.layers) {
        input_layer <- c("\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input 1}] (I-1) at (0,",-1+(5-max(layers_compressed))/2,") {}; \n",
                         "\40 \\node [invisible neuron] (I-2) at (0,",-2+(5-max(layers_compressed))/2,") {}; \n",
                         "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input ", ceiling((1+input)/2), "}] (I-3) at (0,",-3+(5-max(layers_compressed))/2,") {}; \n",
                         "\40 \\node [invisible neuron] (I-4) at (0,",-4+(5-max(layers_compressed))/2,") {}; \n",
                         "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input ", input, "}] (I-5) at (0,",-5+(5-max(layers_compressed))/2,") {};")
      } else {
        input_layer <- c("\40 \\foreach \\name / \\y in {1,...,",input,"} \n",
                         "\40 \40 \40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,",(input-max(layers_compressed))/2,"-\\y) {};")
        }
      } else {
        if (1 %in% oversize.layers) {
          input_layer <- c("\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{",input.label[1],"}] (I-1) at (0,",-1+(5-max(layers_compressed))/2,") {}; \n",
                           "\40 \\node [invisible neuron] (I-2) at (0,",-2+(5-max(layers_compressed))/2,") {}; \n",
                           "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{",input.label[ceiling((1+input)/2)], "}] (I-3) at (0,",-3+(5-max(layers_compressed))/2,") {}; \n",
                           "\40 \\node [invisible neuron] (I-4) at (0,",-4+(5-max(layers_compressed))/2,") {}; \n",
                           "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{",input.label[input], "}] (I-5) at (0,",-5+(5-max(layers_compressed))/2,") {};")
        } else {
          input_layer <- ""
          nextline <- c(rep("\n",length(input.label)-1),"")
          for (i in 1:length(input.label))
            input_layer <- c(input_layer, "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{", input.label[i],"}] (I-",i,") at (0,",(input-max(layers_compressed))/2-i,") {};",nextline[i])
        }
      }

    ## drawing neurons on hidden layers
    hidden_layers <- ""
    i<-1
    for (j in 1:length(hidden)) {
      h <- hidden[j]
      if ((j+1) %in% oversize.layers) {
        hidden_layers <- c(hidden_layers, "\n",
                           "\40 \\node[hidden neuron,yshift=0cm] (H-",i,") at (",j,"* \\layersep, ",-1+(5-max(layers_compressed))/2," cm) {}; \n",
                           "\40 \\node[invisible neuron,yshift=0cm] (H-",i+1,") at (",j,"* \\layersep, ",-2+(5-max(layers_compressed))/2," cm) {}; \n",
                           "\40 \\node[hidden neuron,yshift=0cm] (H-",i+2,") at (",j,"* \\layersep, ",-3+(5-max(layers_compressed))/2," cm) {}; \n",
                           "\40 \\node[invisible neuron,yshift=0cm] (H-",i+3,") at (",j,"* \\layersep, ",-4+(5-max(layers_compressed))/2," cm) {}; \n",
                           "\40 \\node[hidden neuron,yshift=0cm] (H-",i+4,") at (",j,"* \\layersep, ",-5+(5-max(layers_compressed))/2," cm) {};")
        i=i+5
      } else {
        hidden_layers <- c(hidden_layers,"\n",
                          "\40 \\foreach \\name / \\y in {",paste0(i:(i-1+h),"/",1:h,sep= rep(c(",",""),c(h-1,1))),"} \n",
                           "\40 \40 \40 \\path[yshift=",(h-max(layers_compressed))/2,"cm] \n",
                           "\40 \40 \40 \40 \40 node[hidden neuron] (H-\\name) at (",j,"* \\layersep,-\\y cm) {};")
        i=i+h
      }
    }
    hidden_layers <- c(hidden_layers,"\n")

    ## drawing neuron on output layer
    output_layer <- c("\40 \\node[output neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{",output.label,"}}, right of=H-",
                      floor((sum(hidden_compressed[-length(hidden_compressed)])+1+sum(hidden_compressed))/2),", yshift=",
                      floor((sum(hidden_compressed[-length(hidden_compressed)])+1+sum(hidden_compressed))/2)-(sum(hidden_compressed[-length(hidden_compressed)])+1+sum(hidden_compressed))/2,"cm] (O) {}; \n")
  }

  # producing syntax for drawing connections
  ## connections with neurons on input layers as sources
  connections_input <- if (1 %in% oversize.layers & 2 %in% oversize.layers) {
    c("\40 \\foreach \\source in {1,3,5} \n",
      "\40 \40 \40 \\foreach \\dest in {1,3,5} \n",
      "\40 \40 \40 \40 \40 \\path [->] (I-\\source) edge (H-\\dest);")
  } else {
    if (1 %in% oversize.layers & !(2 %in% oversize.layers)) {
      c("\40 \\foreach \\source in {1,3,5} \n",
        "\40 \40 \40 \\foreach \\dest in {1,...,",hidden[1],"} \n",
        "\40 \40 \40 \40 \40 \\path [->] (I-\\source) edge (H-\\dest);")
    } else {
      if (!(1 %in% oversize.layers) & 2 %in% oversize.layers) {
        c("\40 \\foreach \\source in {1,...,",input,"} \n",
          "\40 \40 \40 \\foreach \\dest in {1,3,5} \n",
          "\40 \40 \40 \40 \40 \\path [->] (I-\\source) edge (H-\\dest);")
      } else {
        c("\40 \\foreach \\source in {1,...,",input,"} \n",
          "\40 \40 \40 \\foreach \\dest in {1,...,",hidden[1],"} \n",
          "\40 \40 \40 \40 \40 \\path [->] (I-\\source) edge (H-\\dest);")
      }
    }
  }

  ## connections with neurons on hidden layers as sources
  connections_hidden <- ""
  for (j in 1:length(hidden)) {
    neurons_source <- sum(layers_compressed[2:(j+1)])
    neurons_dest <- sum(layers_compressed[2:(j+2)])
    connections_hidden_j <- if ((j+1) %in% oversize.layers & (j+2) %in% oversize.layers & (j+2) <= length(layers_compressed)) {
      c("\40 \\foreach \\source in {",neurons_source-4,",",neurons_source-2,",",neurons_source,"} \n",
        "\40 \40 \40 \\foreach \\dest in {",neurons_dest-4,",",neurons_dest-2,",",neurons_dest,"} \n",
        "\40 \40 \40 \40 \40 \\path [->] (H-\\source) edge (H-\\dest);")
    } else {
        if ((j+1) %in% oversize.layers & !((j+2) %in% oversize.layers) & (j+2) <= length(layers_compressed)) {
          c("\40 \\foreach \\source in {",neurons_source-4,",",neurons_source-2,",",neurons_source,"} \n",
            "\40 \40 \40 \\foreach \\dest in {",neurons_source+1,",...,",neurons_dest,"} \n",
            "\40 \40 \40 \40 \40 \\path [->] (H-\\source) edge (H-\\dest);")
        } else {
          if (!((j+1) %in% oversize.layers) & (j+2) %in% oversize.layers & (j+2) <= length(layers_compressed)) {
            c("\40 \\foreach \\source in {",neurons_source-layers_compressed[j+1]+1,",...,",neurons_source,"} \n",
              "\40 \40 \40 \\foreach \\dest in {",neurons_source+1,",",neurons_source+3,",",neurons_source+5,"} \n",
              "\40 \40 \40 \40 \40 \\path [->] (H-\\source) edge (H-\\dest);")
          } else {
            if (!((j+1) %in% oversize.layers) & !((j+2) %in% oversize.layers) & (j+2) <= length(layers_compressed)) {
              c("\40 \\foreach \\source in {",neurons_source-layers_compressed[j+1]+1,",...,",neurons_source,"} \n",
                "\40 \40 \40 \\foreach \\dest in {",neurons_source+1,",...,",neurons_dest,"} \n",
                "\40 \40 \40 \40 \40 \\path [->] (H-\\source) edge (H-\\dest);")
            } else {
              ""
            }
          }
        }
      }
    connections_hidden <- c(connections_hidden, "\n", connections_hidden_j)
  }

  ## connections with neurons on the last hidden layer as sources
  connections_output <- if((length(hidden)+1) %in% oversize.layers) {
    c("\40 \\foreach \\source in {",sum(layers_compressed[2:length(layers_compressed)])-4,",",sum(layers_compressed[2:length(layers_compressed)])-2,",",sum(layers_compressed[2:length(layers_compressed)]),"} \n",
      "\40 \40 \40 \\path [->] (H-\\source) edge (O); \n \n")
  } else {
    c("\40 \\foreach \\source in {",sum(layers_compressed[2:length(layers_compressed)])-layers_compressed[length(layers_compressed)]+1,",...,",sum(layers_compressed[2:length(layers_compressed)]),"} \n",
      "\40 \40 \40 \\path [->] (H-\\source) edge (O); \n \n")
  }

  # producing syntax for drawing dotted line
  dotted_lines <- ""
  for (j in 1:length(c(input, hidden))) {
    dotted_lines_j <- if (j %in% oversize.layers & j == 1) {
      c("\40 \\draw[line width=3pt, loosely dotted, dash pattern=on 0.1pt off 10pt, line cap=round] ([yshift=1ex]I-2.north) -- ([yshift=-1ex]I-2.south); \n",
        "\40 \\draw[line width=3pt, loosely dotted, dash pattern=on 0.1pt off 10pt, line cap=round] ([yshift=1ex]I-4.north) -- ([yshift=-1ex]I-4.south); \n")
    } else {
      if (j %in% oversize.layers & j != 1) {
        c("\40 \\draw[line width=3pt, loosely dotted, dash pattern=on 0.1pt off 10pt, line cap=round] ([yshift=1ex]H-",sum(layers_compressed[1:j])-layers_compressed[1]-3,".north) -- ([yshift=-1ex]H-",sum(layers_compressed[1:j])-layers_compressed[1]-3,".south); \n",
          "\40 \\draw[line width=3pt, loosely dotted, dash pattern=on 0.1pt off 10pt, line cap=round] ([yshift=1ex]H-",sum(layers_compressed[1:j])-layers_compressed[1]-1,".north) -- ([yshift=-1ex]H-",sum(layers_compressed[1:j])-layers_compressed[1]-1,".south); \n")
      } else {
        ""
      }
    }
    dotted_lines<- c(dotted_lines, dotted_lines_j)
  }

  # producing syntax for annotation
  if (max.layer == 1) {
    ## when input layer is one of the layers which have the most neurons
    ## annotating neurons on input layer
    annotation_input <- c("\40 \\node[annot,above of=I-1, node distance=2cm] {",layer.label[1],"}; \n",
                          "\40 \\node[annot,above of=I-1, node distance=1cm] {$\\circled{",input,"}$}; \n")
    ## annotating neurons on hidden layers
    annotation_hidden <- ""
    h <- 0
    for (i in 1:length(hidden)) {
      annotation_hidden <- c(annotation_hidden, "\40 \\node[annot,above of=H-",h+1,", node distance=",(layers_compressed[1]-hidden_compressed[i])/2+2,"cm] (hl",i,") {",layer.label[i+1],"}; \n",
                             "\40 \\node[annot,above of=H-",h+1,", node distance=",(layers_compressed[1]-hidden_compressed[i])/2+1,"cm] (hl",i,") {$\\circled{",hidden[i],"}$}; \n")
      h <- h+hidden_compressed[i]
    }
    ## annotating neurons on output layers
    annotation_output <- c("\40 \\node[annot,above of =O, node distance=",(layers_compressed[1]-1)/2+2,"cm] {",layer.label[length(layer.label)],"}; \n",
                           "\40 \\node[annot,above of =O, node distance=",(layers_compressed[1]-1)/2+1,"cm] {$\\circled{1}$}; \n")
    annotation <- c(annotation_input, annotation_hidden, annotation_output)
  } else {
    ## when input layer is not one of the layers which have the most neurons
    ## annotating neurons on input layer
    annotation_input <- c("\40 \\node[annot,above of=I-1, node distance=",(max(layers_compressed)-layers_compressed[1])/2+2,"cm] {",layer.label[1],"}; \n",
                          "\40 \\node[annot,above of=I-1, node distance=",(max(layers_compressed)-layers_compressed[1])/2+1,"cm] {$\\circled{",input,"}$}; \n")
    ## annotating neurons on hidden layers
    annotation_hidden <- ""
    h <- 0
    for (i in 1:length(hidden)) {
      annotation_hidden <- c(annotation_hidden, "\40 \\node[annot,above of=H-",h+1,", node distance=",(max(layers_compressed)-hidden_compressed[i])/2+2,"cm] (hl",i,") {",layer.label[i+1],"}; \n",
                             "\40 \\node[annot,above of=H-",h+1,", node distance=",(max(layers_compressed)-hidden_compressed[i])/2+1,"cm] (hl",i,") {$\\circled{",hidden[i],"}$}; \n")
      h <- h+hidden_compressed[i]
    }
    ## annotating neurons on output layers
    annotation_output <- c("\40 \\node[annot,above of =O, node distance=",(max(layers_compressed)-1)/2+2,"cm] {",layer.label[length(layer.label)],"}; \n",
                           "\40 \\node[annot,above of =O, node distance=",(max(layers_compressed)-1)/2+1,"cm] {$\\circled{1}$}; \n")
    annotation <- c(annotation_input, annotation_hidden, annotation_output)
  }

  # tail syntax
  tail <- c("\n","\\end{tikzpicture} \n",
            "\\caption{",title,"} \n",
            "\\label{} \n",
            "\\end{figure} \n")

  # comment
  comment <- c("% To make the code work in any 'LaTeX' editor, users need to install and import two 'TeX' packages in the setting, including 'TikZ'. \n",
               "% Users are recommended to try the output 'LaTeX' code in Overleaf. \n \n")

  # discussing whether to suppress the output or not
  if (suppress==FALSE) {
    cat(comment, setting, head, "\40 % drawing neurons \n",input_layer, hidden_layers, output_layer, dotted_lines,
        "\n \40 % drawing arrows \n",connections_input, connections_hidden, connections_output,
        "\40 % annotations \n",annotation, tail, sep="")
  } else message("The output of 'LaTeX' code is designed in your command to be suppressed, but it is saved in the object, if assigned. See Examples to find out how to print out the saved output.")

  syntax <- utils::capture.output(cat(comment, setting, head, "\40 % drawing neurons \n",input_layer, hidden_layers, output_layer,dotted_lines,
                                      "\n \40 % drawing arrows \n",connections_input, connections_hidden, connections_output,
                                      "\40 % annotations \n",annotation, tail, sep=""))

}
