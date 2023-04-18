#' Producing 'LaTeX' Code for Drawing Neural Network Diagrams with Some Neurons being Covered-up
#' @description The \code{nndiagram_nodeCoverup} command is used to produce 'LaTeX' code for drawing well-formatted neural network diagrams, some neurons of which users hope to cover up with lighter color.
#' To make the code work in a 'LaTeX' editor, users need to install and import the 'TeX' package \href{https://www.overleaf.com/learn/latex/TikZ_package}{TikZ} in the setting of 'TeX' file. Syntax of importing this package is included in the output of function.
#'
#' @param input a positive integer that specifies the number of input neurons.
#' @param hidden a positive integer vector that specifies the number of neurons on each hidden layer. For example, c(6,6,6) specifies that there are 3 hidden layers and 6 neurons on each hidden layer. Non-positive and non-integer numbers are not allowed.
#' @param node.coverup an optional positive integer vector that specifies the index of neurons users hope to cover up with lighter color. Neurons are counted from top to bottom and from left to right. For example, in a neural network with \code{input=3}
#' and \code{hidden=c(6,6,6)}, \code{node.coverup=c(4,10)} means that the user needs to cover up the first neuron on both the first and second hidden layer. Default is \code{NULL}.
#' @param title an optional character that specifies the main title of diagram. Default is \code{NULL}.
#' @param color an optional character that specifies the color of lines. Default is \code{"black"}.
#' @param alpha an optional numeric value between 0 and 1 that specifies the opacity of lines. \code{1} indicates lines to be opaque, and \code{0} indicates lines to be transparent. Default is \code{1}.
#' @param layer.sep an optional positive numeric value that specifies the distance between layers of a neural network. Default is \code{2.5}.
#' @param layer.label an optional character vector that specifies label for each layer, including input, hidden and output layers.
#' @param input.label an optional character vector that specifies label for each input neuron.
#' @param output.label an optional character that specifies label for output neuron.
#' @param suppress an optional logical value that specifies whether \code{nndiagram_nodeCoverup} should suppress the output of 'LaTeX' code to be directly printed on console. Default is \code{FALSE}.
#'
#' @return \code{nndiagram_nodeCoverup} uses \code{cat()} to print out 'LaTeX' code on console, if not suppressed. Also, \code{nndiagram_nodeCoverup} saves the same output as a character vector invisibly, so users could use \code{cat()} to print it out later at their demand, as shown in Examples.
#' The \code{nndiagram_nodeCoverup} 'LaTeX' output can be directly copied and pasted to produce neural network diagram in any 'LaTeX' editor.
#' @seealso \link{nndiagram}; \link{nndiagram_oversize}; \link{activation_curve}.
#' @export
#'
#' @note This package is an ongoing project, and more functions will be added in the future, such as those to produce pdf version of diagrams or convert handdrawing neural network diagrams to computerized ones. Collaborations are sincerely welcome.
#' Comments and suggestions are always highly appreciated.
#'
#' @author Chencheng Fang, Bonn Graduate School of Economics, University of Bonn. Email: \email{ccfang@uni-bonn.de}
#'
#' @examples
#' # A neural network with 3 neurons on input layer, 6 neurons on each of 3 hidden layers,
#' # and 1 neuron on output layer. All other arguments are default, so no neuron is
#' # drawn with lighter color.
#' nndiagram_nodeCoverup(input=3, hidden=c(6,6,6))
#'
#' # Same as the first example but neurons indexed with 4 and 11 are designed to be drawn
#' # with lighter color.
#' nndiagram_nodeCoverup(input=3, hidden=c(6,6,6), node.coverup=c(4,10))
#'
#' # Same as the first example but distance between layers is defined to be smaller.
#' nndiagram_nodeCoverup(input=3, hidden=c(6,6,6), layer.sep=1.5)
#'
#' # Suppress the output of 'LaTeX' code to be directly printed on the console and save the output
#' # to an object, which can be printed later at demand.
#' nnd_nodeCoverup <- nndiagram_nodeCoverup(input=3, hidden=c(6,6,6), suppress=TRUE)
#' cat(paste(nnd_nodeCoverup,"\n"))
nndiagram_nodeCoverup <- function(input, hidden, node.coverup=NULL, title=NULL, color="black", alpha=1,
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

  # checking if node.coverup is positive, integer and smaller than the total number of nodes
  if (!is.numeric(node.coverup) & !is.null(node.coverup)) stop("Elements in 'node.coverup' should be numeric")
  if (any(node.coverup <= 0)) stop("Elements in 'node.coverup' should be positive numbers")
  if (!is.null(node.coverup)) {if (any(node.coverup != floor(node.coverup))) stop("Elements in 'node.coverup' should be integers")}
  if (!is.null(node.coverup)) {if (any(node.coverup > sum(c(input, hidden))+1)) stop("Elements in 'node.coverup' should not be greater than the total number of nodes in the network")}

  # checking if alpha is between 0 and 1
  if (alpha<0 | alpha>1 | !is.numeric(alpha)) stop("'alpha' should be a number between 0 and 1")
  if (length(alpha)>1) stop("'alpha' should be a single number")

  # checking if layer.sep is a positive single number
  if (layer.sep<=0 | !is.numeric(layer.sep)) stop("'layer.sep' should be a positive number")
  if (length(layer.sep)>1) stop("'layer.sep' should be a single number")

  # checking if layer.label is defined correctly
  if (is.null(layer.label)) {
    layer.label <- c("Input layer", paste0("Hidden layer ",1:length(hidden)), "Output layer")
    } else {
      if (!is.null(layer.label) & length(layer.label) != (length(hidden)+2))
        stop("Number of layer labels don't coincide with number of layers")
    }

  # checking if input.label is defined correctly
  if (!is.null(input.label) & length(input.label)!= input)
    stop("Number of input labels don't coincide with number of input")

  # checking if output label is defined correctly
  if (is.null(output.label)) {
    output.label <- "Output"
    } else {
      if (!is.null(output.label) & length(output.label)!= 1)
        stop("There are more than 1 output labels")
    }

  # checking if suppress is defined correctly
  if (!is.logical(suppress)) stop("'suppress' should be a logical value.")
  if (length(suppress) > 1) stop("'suppress' should be a single logical value.")

  # generating a vector or a list containing cover-up neurons on each layer
  ## output layer
  node.coverup.update <- ifelse(node.coverup<=input, node.coverup, node.coverup-input)
  output.coverup <- if ((sum(c(input,hidden))+1) %in% node.coverup)
    node.coverup.update[which(node.coverup==(sum(c(input,hidden))+1))] else NULL

  ## input layer
  node.coverup.ih <- if((sum(c(input,hidden))+1) %in% node.coverup)
    node.coverup.update[-which(node.coverup==(sum(c(input,hidden))+1))] else node.coverup.update
  input.coverup <- node.coverup.ih[which(node.coverup<=input)]

  ## hidden layer
  hidden.coverup <- if (length(which(node.coverup<=input))==0) node.coverup.ih else node.coverup.ih[-which(node.coverup<=input)]
  hidden.coverup.list <- list()
  hidden.cumsum <- c(0,cumsum(hidden))

  for (i in 2:(length(hidden)+1)) {
    sum.before <- hidden.cumsum[i-1]
    sum.now <- hidden.cumsum[i]
    node.coverup.each.layer <- vector()
    for (j in hidden.coverup) {
      if (j > sum.before & j <= sum.now)
        node.coverup.each.layer <- append(node.coverup.each.layer, j)
    }
    hidden.coverup.list[[i-1]] <- node.coverup.each.layer
  }

  # generating a vector or a list containing normal neurons on each layer
  ## output layer
  output.normal <- if ((sum(c(input,hidden))+1) %in% node.coverup) {NULL} else {sum(c(input,hidden))+1}

  ## input layer
  input.normal <- setdiff(1:input, input.coverup)

  ## hidden layer
  hidden.normal.list <- list()
  for (i in 1:length(hidden)) {
    all.nodes.each.layer <- (hidden.cumsum[i]+1):hidden.cumsum[i+1]
    node.normal.each.layer <- setdiff(all.nodes.each.layer, hidden.coverup.list[[i]])
    hidden.normal.list[[i]]<-node.normal.each.layer
  }

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
            "\40 \\tikzstyle{every pin edge}=[<-,shorten <=1pt, draw=",color,"!",alpha*100,"]; \n",
            "\40 \\tikzstyle{neuron}=[circle, draw=",color,"!",alpha*100,", minimum size=17pt,inner sep=0pt]; \n",
            "\40 \\tikzstyle{input neuron}=[neuron]; \n",
            "\40 \\tikzstyle{output neuron}=[neuron]; \n",
            "\40 \\tikzstyle{hidden neuron}=[neuron]; \n",
            "\40 \\tikzstyle{coveruped neuron}=[neuron, draw=",color,"!",alpha*100*0.25,"]; \n",
            "\40 \\tikzstyle{annot} = [text width=4em, text centered, text=",color,"!",alpha*100,"] \n \n")

  # producing syntax for drawing neurons
  max.layer <- which.max(c(input, hidden))

  if (max.layer == 1) {
    ## when input layer is one of the layers which have the most neurons
    ## drawing neurons on input layer
    if (is.null(input.label)) {
      ## normal neurons
      if (length(input.normal)!=0) {
        normal_input_layer <- c("\40 \\foreach \\name / \\y in {",paste(as.character(input.normal), collapse=","),"} \n",
                                "\40 \40 \40 \\node[input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,-\\y) {}; \n")
        } else {normal_input_layer <- ""}
      ## cover-up neurons
      if (length(input.coverup)!=0) {
        coverup_input_layer <- c("\40 \\foreach \\name / \\y in {",paste(as.character(input.coverup), collapse=","),"} \n",
                                  "\40 \40 \40 \\node[coveruped neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,-\\y) {}; \n")
        } else {coverup_input_layer <- ""}
      } else {
        ## normal neurons with label
        if (length(input.normal)!=0) {
          normal_input_layer <- ""
          normal_nextline <- c(rep("\n",length(input.normal)-1),"")
          for (i in 1:length(input.normal)) {
            k <- input.normal[i]
            normal_input_layer <- c(normal_input_layer, "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{", input.label[k],"}] (I-",k,") at (0,-",k,") {};", normal_nextline[i]) }
          } else { normal_input_layer <- ""}
        ## cover-up neurons with label
        if (length(input.coverup)!=0) {
          coverup_input_layer <- ""
          coverup_nextline <- c(rep("\n",length(input.coverup)-1),"")
          for (i in 1:length(input.coverup)) {
            k <- input.coverup[i]
            coverup_input_layer <- c(coverup_input_layer, "\40 \\node [coveruped neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{", input.label[k],"}] (I-",k,") at (0,-",k,") {};", coverup_nextline[i]) }
        } else {coverup_input_layer <- ""}
     }
    ## drawing neurons on hidden layers
    normal_hidden_layers <- ""
    coverup_hidden_layers <- ""
    for (i in 1:length(hidden)) {
      h <- hidden[i]
      if (length(hidden.normal.list[[i]])!=0) {
        normal_hidden_layers <- c(normal_hidden_layers,
                                  "\40 \\foreach \\name / \\y in {",paste0(hidden.normal.list[[i]],"/",hidden.normal.list[[i]]-hidden.cumsum[i],sep= rep(c(",",""),c(length(hidden.normal.list[[i]])-1,1))),"} \n",
                                  "\40 \40 \40 \\path[yshift=",(h-input)/2,"cm] \n",
                                  "\40 \40 \40 \40 \40 node[hidden neuron] (H-\\name) at (",i,"* \\layersep,-\\y cm) {}; \n")
      } else {
        normal_hidden_layers <- normal_hidden_layers
      }

      if (length(hidden.coverup.list[[i]])!=0) {
        coverup_hidden_layers <- c(coverup_hidden_layers,
                                    "\40 \\foreach \\name / \\y in {",paste0(hidden.coverup.list[[i]],"/",hidden.coverup.list[[i]]-hidden.cumsum[i],sep= rep(c(",",""),c(length(hidden.coverup.list[[i]])-1,1))),"} \n",
                                    "\40 \40 \40 \\path[yshift=",(h-input)/2,"cm] \n",
                                    "\40 \40 \40 \40 \40 node[coveruped neuron] (H-\\name) at (",i,"* \\layersep,-\\y cm) {}; \n")
      } else {
        coverup_hidden_layers <- coverup_hidden_layers
      }
      i=i+h
    }

    ## drawing neuron on output layer
    if (length(output.normal)!=0) {
      output_layer <- c("\40 \\node[output neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{",output.label,"}}, right of=H-",
                        floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2),", yshift=",
                        floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2)-(sum(hidden[-length(hidden)])+1+sum(hidden))/2,"cm] (O) {}; \n \n")
    } else {
      output_layer <- c("\40 \\node[coveruped neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{",output.label,"}}, right of=H-",
                        floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2),", yshift=",
                        floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2)-(sum(hidden[-length(hidden)])+1+sum(hidden))/2,"cm] (O) {}; \n \n")
    }
    } else {
      ## when input layer is not one of layers which have the most neurons
      ## drawing neurons on input layer
      if (is.null(input.label)) {
        ## normal neurons
        if (length(input.normal)!=0) {
          normal_input_layer <- c("\40 \\foreach \\name / \\y in {",paste0(input.normal,"/",input.normal,sep= rep(c(",",""),c(length(input.normal)-1,1))),"} \n",
                                  "\40 \40 \40 \\node[input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,",(input-max(c(input,hidden)))/2,"-\\y) {}; \n")
        } else {normal_input_layer <- ""}
        ## cover-up neurons
        if (length(input.coverup)!=0) {
          coverup_input_layer <- c("\40 \\foreach \\name / \\y in {",paste0(input.coverup,"/",input.coverup,sep= rep(c(",",""),c(length(input.coverup)-1,1))),"} \n",
                                    "\40 \40 \40 \\node[coveruped neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,",(input-max(c(input,hidden)))/2,"-\\y) {}; \n")
        } else {coverup_input_layer <- ""}
      } else {
        ## normal neurons with label
        if (length(input.normal)!=0) {
          normal_input_layer <- ""
          normal_nextline <- c(rep("\n",length(input.normal)-1),"")
          for (i in 1:length(input.normal)) {
            k <- input.normal[i]
            normal_input_layer <- c(normal_input_layer, "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{", input.label[k],"}] (I-",k,") at (0,",(input-max(c(input, hidden)))/2-k,") {};", normal_nextline[i]) }
        } else {normal_input_layer <- ""}
        ## coveruped neurons with label
        if (length(input.coverup)!=0) {
          coverup_input_layer <- ""
          coverup_nextline <- c(rep("\n",length(input.coverup)-1),"")
          for (i in 1:length(input.coverup)) {
            k <- input.coverup[i]
            coverup_input_layer <- c(coverup_input_layer, "\40 \\node [coveruped neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{", input.label[k],"}] (I-",k,") at (0,",(input-max(c(input, hidden)))/2-k,") {};", coverup_nextline[i])}
        } else {coverup_input_layer <- ""}
      }

      ## drawing neurons on hidden layers
      normal_hidden_layers <- ""
      coverup_hidden_layers <- ""
      for (i in 1:length(hidden)) {
        h <- hidden[i]
        if (length(hidden.normal.list[[i]])!=0) {
          normal_hidden_layers <- c(normal_hidden_layers,
                                    "\40 \\foreach \\name / \\y in {",paste0(hidden.normal.list[[i]],"/",hidden.normal.list[[i]]-hidden.cumsum[i],sep= rep(c(",",""),c(length(hidden.normal.list[[i]])-1,1))),"} \n",
                                    "\40 \40 \40 \\path[yshift=",(h-max(c(input, hidden)))/2,"cm] \n",
                                    "\40 \40 \40 \40 \40 node[hidden neuron] (H-\\name) at (",i,"* \\layersep,-\\y cm) {}; \n")
        } else {
          normal_hidden_layers <- normal_hidden_layers
        }

        if (length(hidden.coverup.list[[i]])!=0) {
          coverup_hidden_layers <- c(coverup_hidden_layers,
                                      "\40 \\foreach \\name / \\y in {",paste0(hidden.coverup.list[[i]],"/",hidden.coverup.list[[i]]-hidden.cumsum[i],sep= rep(c(",",""),c(length(hidden.coverup.list[[i]])-1,1))),"} \n",
                                      "\40 \40 \40 \\path[yshift=",(h-max(c(input, hidden)))/2,"cm] \n",
                                      "\40 \40 \40 \40 \40 node[coveruped neuron] (H-\\name) at (",i,"* \\layersep,-\\y cm) {}; \n")
        } else {
          coverup_hidden_layers <- coverup_hidden_layers
        }
      }

      ## drawing neurons on output layer
      if (length(output.normal)!=0) {
        output_layer <- c("\40 \\node[output neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{",output.label,"}}, right of=H-",
                          floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2),", yshift=",
                          floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2)-(sum(hidden[-length(hidden)])+1+sum(hidden))/2,"cm] (O) {}; \n \n")
      } else {
        output_layer <- c("\40 \\node[coveruped neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{",output.label,"}}, right of=H-",
                          floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2),", yshift=",
                          floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2)-(sum(hidden[-length(hidden)])+1+sum(hidden))/2,"cm] (O) {}; \n \n")
      }
    }
  # producing syntax for drawing connections
  normal.list <- append(hidden.normal.list, list(input.normal), 0)
  normal.list <- append(normal.list, list(output.normal),length(c(input, hidden)))

  coverup.list <- append(hidden.coverup.list, list(input.coverup), 0)
  coverup.list <- append(coverup.list, list(output.coverup),length(c(input, hidden)))

  ## connections with neurons on input layers as sources
  if(length(normal.list[[1]])!=0 & length(normal.list[[2]])!=0) {
    normal_connections_input <- c("\40 \\foreach \\source in {",paste(as.character(normal.list[[1]]), collapse=","),"} \n",
                                  "\40 \40 \40 \\foreach \\dest in {",paste(as.character(normal.list[[2]]), collapse=","),"} \n",
                                  "\40 \40 \40 \40 \40 \\path [->] (I-\\source) edge (H-\\dest); \n")
  } else {normal_connections_input <- ""}

  if(length(coverup.list[[1]])!=0) {
    coverup_connections_input <- c("\40 \\foreach \\source in {",paste(as.character(coverup.list[[1]]), collapse=","),"} \n",
                                    "\40 \40 \40 \\foreach \\dest in {1,...,",hidden[1],"} \n",
                                    "\40 \40 \40 \40 \40 \\path [->] (I-\\source) edge [draw=",color,"!",alpha*100*0.25,"] (H-\\dest); \n")
  } else {coverup_connections_input <- ""}

  if (length(coverup.list[[2]])!=0) {
    coverup_connections_input <- c(coverup_connections_input,
                                    "\40 \\foreach \\source in {1,...,",input,"} \n",
                                    "\40 \40 \40 \\foreach \\dest in {",paste(as.character(coverup.list[[2]]), collapse=","),"} \n",
                                    "\40 \40 \40 \40 \40 \\path [->] (I-\\source) edge [draw=",color,"!",alpha*100*0.25,"] (H-\\dest); \n")
  } else {coverup_connections_input <- coverup_connections_input}

  ## connections with neurons on hidden layers as sources
  normal_connections_hidden <- ""
  coverup_connections_hidden <- ""

  for (i in 1:(length(hidden)-1)) {
    if (length(normal.list[[i+1]])!=0 & length(normal.list[[i+2]])!=0) {
      normal_connections_hidden <- c(normal_connections_hidden,
                                     "\40 \\foreach \\source in {",paste(as.character(normal.list[[i+1]]), collapse=","),"} \n",
                                     "\40 \40 \40 \\foreach \\dest in {",paste(as.character(normal.list[[i+2]]), collapse=","),"} \n",
                                     "\40 \40 \40 \40 \40 \\path [->] (H-\\source) edge (H-\\dest); \n")
    } else {normal_connections_hidden <- normal_connections_hidden}

    if(length(coverup.list[[i+1]])!=0) {
      coverup_connections_hidden <- c(coverup_connections_hidden,
                                       "\40 \\foreach \\source in {",paste(as.character(coverup.list[[i+1]]), collapse=","),"} \n",
                                       "\40 \40 \40 \\foreach \\dest in {",hidden.cumsum[i+1]+1,",...,",hidden.cumsum[i+2],"} \n",
                                       "\40 \40 \40 \40 \40 \\path [->] (H-\\source) edge [draw=",color,"!",alpha*100*0.25,"] (H-\\dest); \n")
    } else {coverup_connections_hidden <- coverup_connections_hidden}

    if (length(coverup.list[[i+2]])!=0) {
      coverup_connections_hidden <- c(coverup_connections_hidden,
                                       "\40 \\foreach \\source in {",hidden.cumsum[i]+1,",...,",hidden.cumsum[i+1],"} \n",
                                       "\40 \40 \40 \\foreach \\dest in {",paste(as.character(coverup.list[[i+2]]), collapse=","),"} \n",
                                       "\40 \40 \40 \40 \40 \\path [->] (H-\\source) edge [draw=",color,"!",alpha*100*0.25,"] (H-\\dest); \n")
    } else {coverup_connections_hidden <- coverup_connections_hidden}
  }

  ## connections with neurons on the last hidden layer as sources
  if (length(normal.list[[length(c(input,hidden))]])!=0 & length(normal.list[[length(c(input,hidden))+1]])!=0) {
    normal_connections_output <- c("\40 \\foreach \\source in {",paste(as.character(normal.list[[length(c(input,hidden))]]), collapse=","),"} \n",
                                   "\40 \40 \40 \\path [->] (H-\\source) edge (O); \n")
  } else {normal_connections_output <- ""}

  if (length(coverup.list[[length(c(input,hidden))]])!=0) {
    coverup_connections_output <- c("\40 \\foreach \\source in {",paste(as.character(coverup.list[[length(c(input,hidden))]]), collapse=","),"} \n",
                                     "\40 \40 \40 \\path [->] (H-\\source) edge [draw=", color,"!", alpha*100*0.25,"] (O); \n")
  } else {coverup_connections_output <- ""}

  if (length(coverup.list[[length(c(input,hidden))+1]])!=0) {
    coverup_connections_output <- c("\40 \\foreach \\source in {",hidden.cumsum[length(hidden)]+1,",...,",hidden.cumsum[length(hidden)+1],"} \n",
                                     "\40 \40 \40 \\path [->] (H-\\source) edge [draw=", color,"!", alpha*100*0.25,"] (O); \n")
  } else {coverup_connections_output <- coverup_connections_output}

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
      annotation_hidden <- c(annotation_hidden, "\40 \\node[annot,above of=H-",h+1,", node distance=",(c(input,hidden)[1]-c(input,hidden)[i+1])/2+2,"cm] (hl",i,") {",layer.label[i+1],"}; \n",
                             "\40 \\node[annot,above of=H-",h+1,", node distance=",(c(input,hidden)[1]-c(input,hidden)[i+1])/2+1,"cm] (hl",i,") {$\\circled{",hidden[i],"}$}; \n")
      h <- h+hidden[i]
    }
    ## annotating neurons on output layers
    annotation_output <- c("\40 \\node[annot,above of =O, node distance=",(c(input,hidden)[1]-1)/2+2,"cm] {",layer.label[length(layer.label)],"}; \n",
                           "\40 \\node[annot,above of =O, node distance=",(c(input,hidden)[1]-1)/2+1,"cm] {$\\circled{1}$}; \n")
    annotation <- c(annotation_input, annotation_hidden, annotation_output)
  } else {
    ## when input layer is not one of the layers which have the most neurons
    ## annotating neurons on input layer
    annotation_input <- c("\40 \\node[annot,above of=I-1, node distance=",(max(c(input,hidden))-c(input,hidden)[1])/2+2,"cm] {",layer.label[1],"}; \n",
                          "\40 \\node[annot,above of=I-1, node distance=",(max(c(input,hidden))-c(input,hidden)[1])/2+1,"cm] {$\\circled{",input,"}$}; \n")
    ## annotating neurons on hidden layers
    annotation_hidden <- ""
    h <- 0
    for (i in 1:length(hidden)) {
      annotation_hidden <- c(annotation_hidden, "\40 \\node[annot,above of=H-",h+1,", node distance=",(max(c(input,hidden))-c(input,hidden)[i+1])/2+2,"cm] (hl",i,") {",layer.label[i+1],"}; \n",
                             "\40 \\node[annot,above of=H-",h+1,", node distance=",(max(c(input,hidden))-c(input,hidden)[i+1])/2+1,"cm] (hl",i,") {$\\circled{",hidden[i],"}$}; \n")
      h <- h+hidden[i]
    }
    ## annotating neurons on output layers
    annotation_output <- c("\40 \\node[annot,above of =O, node distance=",(max(c(input,hidden))-1)/2+2,"cm] {",layer.label[length(layer.label)],"}; \n",
                           "\40 \\node[annot,above of =O, node distance=",(max(c(input,hidden))-1)/2+1,"cm] {$\\circled{1}$}; \n")
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
    cat(comment, setting, head, "\40 % drawing neurons \n",normal_input_layer,coverup_input_layer, normal_hidden_layers, coverup_hidden_layers,output_layer,
        "\40 % drawing arrows \n",normal_connections_input, coverup_connections_input,normal_connections_hidden,
        coverup_connections_hidden, normal_connections_output, coverup_connections_output,
        "\n \40 % annotations \n",annotation, tail, sep="")
  } else message("The output of 'LaTeX' code is designed in your command to be suppressed, but it is saved in the object, if assigned. See Examples to find out how to print out the saved output.")

  syntax <- utils::capture.output(cat(comment, setting, head, "\40 % drawing neurons \n",normal_input_layer,coverup_input_layer, normal_hidden_layers, coverup_hidden_layers,output_layer,
                                      "\40 % drawing arrows \n",normal_connections_input, coverup_connections_input,normal_connections_hidden,
                                      coverup_connections_hidden, normal_connections_output, coverup_connections_output,
                                      "\n \40 % annotations \n",annotation, tail, sep=""))

}
