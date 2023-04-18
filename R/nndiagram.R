#' Producing 'LaTeX' Code for Drawing Neural Network Diagrams
#' @description The \code{nndiagram} command is used to produce 'LaTeX' code for drawing well-formatted neural network diagrams. To make the code work in a 'LaTeX' editor, users need
#' to install and import two 'TeX' packages, i.e., \href{https://www.overleaf.com/learn/latex/TikZ_package}{TikZ} and
#' \href{https://www.ctan.org/pkg/ifthen#:~:text=Ifthen%20is%20a%20separate%20package%20within%20the%20LaT.,always%20needed%20to%20load%20it.%20Sources.%20%2Fmacros%2Flatex%2Fbase.%20Documentation.}{ifthen}
#' in the setting of 'TeX' file. Syntax of importing these packages is included in the output of function.
#'
#' @param input a positive integer that specifies the number of input neurons.
#' @param hidden a positive integer vector that specifies the number of neurons on each hidden layer. For example, c(4,4,4) specifies that there are 3 hidden layers and 4 neurons on each hidden layer.
#' Non-positive and non-integer numbers are not allowed.
#' @param keep an optional character vector that specifies the connections of neurons to be kept. For example, c("1->4","5->8") specifies the connections from neuron 1 to 4 and from neuron 5 to 8. Neurons are counted from
#' top to bottom and from left to right. As special cases, "->4" specifies all connections with neuron 4 as destination, and "5->" specifies all connections with neuron 5 as source. No space in the string is allowed. Default is \code{NULL}.
#' @param omit an optional character vector that specifies the connections of neurons to be omitted. The specification of connections is the same as that in 'keep'. However, users are not allowed to assign values to 'keep'
#' and 'omit' simultaneously. Default is \code{NULL}.
#' @param title an optional character that specifies the main title of diagram. Default is \code{NULL}.
#' @param color an optional character that specifies the color of lines. Default is \code{"black"}.
#' @param alpha an optional numeric value between 0 and 1 that specifies the opacity of lines. \code{1} indicates lines to be opaque, and \code{0} indicates lines to be transparent. Default is \code{1}.
#' @param layer.sep an optional positive numeric value that specifies the distance between layers of a neural network. Default is \code{2.5}.
#' @param layer.label an optional character vector that specifies label for each layer, including input, hidden and output layers.
#' @param input.label an optional character vector that specifies label for each input neuron.
#' @param output.label an optional character that specifies label for output neuron.
#' @param suppress an optional logical value that specifies whether \code{nndiagram} should suppress the output of 'LaTeX' code to be directly printed on console. Default is \code{FALSE}.
#'
#' @return \code{nndiagram} uses \code{cat()} to print out 'LaTeX' code on console, if not suppressed. Also, \code{nndiagram} saves the same output as a character vector invisibly, so users could use \code{cat()} to print it out later at their demand, as shown in Examples.
#' The \code{nndiagram} 'LaTeX' output can be directly copied and pasted to produce neural network diagram in any 'LaTeX' editor.
#' @seealso \link{nndiagram_oversize}; \link{nndiagram_nodeCoverup}; \link{activation_curve}.
#' @export
#'
#' @note This package is an ongoing project, and more functions will be added in the future, such as those to produce pdf version of diagrams or convert handdrawing neural network diagrams to computerized ones. Collaborations
#' are sincerely welcome. Comments and suggestions are always highly appreciated.
#'
#' @author Chencheng Fang, Bonn Graduate School of Economics, University of Bonn. Email: \email{ccfang@uni-bonn.de}
#'
#' @examples
#' # A neural network with 3 neurons on input layer, 4 neurons on each of 3 hidden layers,
#' # and 1 neuron on output layer. No connection is omitted and all other arguments are default.
#' nndiagram(input=3, hidden=c(4,4,4))
#'
#' # Same as the first example but connections from neuron 1 to 4 and from neuron 5 to 8 are omitted.
#' nndiagram(input=3, hidden=c(4,4,4), omit=c("1->4","5->8"))
#'
#' # Same as the first example but connections with neuron 4 as destination are omitted.
#' nndiagram(input=3, hidden=c(4,4,4), omit=c("->4"))
#'
#' # Same as the first example but connections with neuron 5 as source are omitted.
#' nndiagram(input=3, hidden=c(4,4,4), omit=c("5->"))
#'
#' # Suppress the output of 'LaTeX' code to be directly printed on the console and save the output
#' # to an object, which can be printed later at demand.
#' nnd <- nndiagram(input=3, hidden=c(4,4,4), suppress=TRUE)
#' cat(paste(nnd,"\n"))
nndiagram <- function(input, hidden, keep=NULL, omit=NULL, title=NULL,color="black", alpha=1,
                      layer.sep=2.5, layer.label=NULL, input.label=NULL, output.label=NULL, suppress=FALSE) {

  #---------------------------------------
  # Checking arguments and data preparation
  #---------------------------------------

  # checking if input is a positive single integer
  if (!is.numeric(input) | input <=0 | (input != floor(input))) stop("'input' should be a positive integer")
  if (length(input)>1) stop("'input' should be a single number")

  # checking if elements in hidden are positive integers
  if (!is.numeric(hidden) | any(hidden <= 0)) stop("Elements in 'hidden' should be positive numbers")
  if (any(hidden != floor(hidden))) stop("Elements in 'hidden' should be integers")

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

  # checking if keep and omit are defined simultaneously
  if (!is.null(keep) & !is.null(omit)) stop("Arguments of 'keep' and 'omit' are exclusive. Assignment of values to both arguments is not allowed.")

  # checking if suppress is defined correctly
  if (!is.logical(suppress)) stop("'suppress' should be a logical value.")
  if (length(suppress) > 1) stop("'suppress' should be a single logical value.")

  # producing all possible connections
  s<-1
  total.connections <- matrix(ncol=2)
  for (i in 1:(length(hidden)+1)){
    sources<- rep(s:(s+c(input,hidden,1)[i]-1),each=c(input,hidden,1)[i+1])
    dests<-rep((s+c(input,hidden,1)[i]):(s+c(input,hidden,1)[i]+c(input,hidden,1)[i+1]-1),c(input,hidden,1)[i])
    total.connections <- rbind(total.connections,cbind(sources,dests))
    s<-s+c(input,hidden,1)[i]
  }
  total.connections<-total.connections[-1,]

  # designing a function to test the format of connections
  test <- function(x) {
    str <- strsplit(x, "->")[[1]]
    len <- length(str)
    ifelse(len >= 3,stop("Connections are not defined correctly."),
    ifelse(len == 2 & nchar(str[1])==0 & is.na(as.numeric(str[2]))==TRUE, stop("Connections are not defined correctly."),
    ifelse(len == 2 & nchar(str[1])==0 & nchar(str[2])==0, stop("Connections are not defined correctly."),
    ifelse(len == 2 & nchar(str[1])==0 & is.na(as.numeric(str[2]))==FALSE & as.numeric(str[2])<=0, stop("Neurons are only marked with positive integers."),
    ifelse(len == 2 & nchar(str[1])==0 & is.na(as.numeric(str[2]))==FALSE & (as.integer(str[2])==str[2])==FALSE, stop("Neurons are only marked with positive integers."),
    ifelse(len == 2 & nchar(str[1])==0 & is.na(as.numeric(str[2]))==FALSE & as.numeric(str[2])>0 & (as.integer(str[2])==str[2])==TRUE, x <- paste(0,x,sep=""),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==T & is.na(as.numeric(str[2]))==TRUE, stop("Connections are not defined correctly."),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==T & nchar(str[2])==0, stop("Connections are not defined correctly."),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==T & is.na(as.numeric(str[2]))==FALSE & as.numeric(str[2])<=0, stop("Neurons are only marked with positive integers."),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==T & is.na(as.numeric(str[2]))==FALSE & (as.integer(str[2])==str[2])==FALSE, stop("Neurons are only marked with positive integers."),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==T & is.na(as.numeric(str[2]))==FALSE & as.numeric(str[2])>0 & (as.integer(str[2])==str[2])==TRUE,  x <- paste(0,x,sep=""),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==F & any(is.na(as.numeric(str)))==TRUE, stop("Connections are not defined correctly."),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==F & any(is.na(as.numeric(str)))==FALSE & any(as.numeric(str)<=0)==TRUE, stop("Neurons are only marked with positive integers."),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==F & any(is.na(as.numeric(str)))==FALSE & all(as.integer(str)==str)==FALSE, stop("Neurons are only marked with positive integers."),
    ifelse(len == 2 & nchar(str[1])!=0 & all(nchar(strsplit(str[1], " ")[[1]])==0)==F & any(is.na(as.numeric(str)))==FALSE & any(as.numeric(str)>0)==TRUE & all(as.integer(str)==str)==TRUE, x <- x,
    ifelse(len == 1 & nchar(str)==0, stop("Connections are not defined correctly."),
    ifelse(len == 1 & nchar(str)!=0 & is.na(as.numeric(str))==T, stop("Connections are not defined correctly."),
    ifelse(len == 1 & nchar(str)!=0 & is.na(as.numeric(str))==F & (as.numeric(str)<=0)==TRUE, stop("Neurons are only marked with positive integers."),
    ifelse(len == 1 & nchar(str)!=0 & is.na(as.numeric(str))==F & (as.integer(str)==str)==FALSE, stop("Neurons are only marked with positive integers."),
    ifelse(len == 1 & nchar(str)!=0 & is.na(as.numeric(str))==F & (as.numeric(str)>0)==TRUE & (as.integer(str)==str)==TRUE, x<-paste(x,0,sep=""),
           stop("Connections are not defined correctly.")))))))))))))))))))))
    return(x)
}

  # data preparation when omit is not NULL
  if (!is.null(omit)) {
    ## using test function and transforming the input of omit
    omit <- sapply(omit, test)
    omit_mat <- matrix(as.numeric(unlist(sapply(omit, function(x) {strsplit(x,"->")}))), ncol=2, byrow = TRUE)

    ## checking if there is 0 in sources, and if so, replacing it
    if (0 %in% omit_mat[,1]) {
      loc.source <- which(omit_mat[,1]==0)
      replacement.source <- matrix(ncol=2)
      for (i in loc.source) {
        loc.total <- which(total.connections[,2]==omit_mat[i,2])
        replacement.source <- rbind(replacement.source,total.connections[loc.total,])
      }
      replacement.source <- replacement.source[-1,]
      omit_mat <- rbind(omit_mat[-which(omit_mat[,1]==0),], replacement.source)
      } else omit_mat <- omit_mat

    ## checking if there is 0 in destinations, and if so, replacing it
    if (0 %in% omit_mat[,2]) {
      loc.dest <- which(omit_mat[,2]==0)
      replacement.dest <- matrix(ncol=2)
      for (i in loc.dest) {
        loc.total <- which(total.connections[,1]==omit_mat[i,1])
        replacement.dest <- rbind(replacement.dest,total.connections[loc.total,])
      }
      replacement.dest <- replacement.dest[-1,]
      omit_mat <- rbind(omit_mat[-which(omit_mat[,2]==0),], replacement.dest)
      } else omit_mat <- omit_mat

    ## naming cols
    colnames(omit_mat) <- c("sources","dests")

    ## checking if there is any misspecified connections in omit input
    if (nrow(dplyr::setdiff(as.data.frame(omit_mat),as.data.frame(total.connections)))!=0) stop("There is at least one connection between nonadjacent layers or with backward direction.")
  }

  # data preparation when keep is not NULL
  if (!is.null(keep)) {
    ## using test function again and transforming the input of keep
    keep <- sapply(keep, test)
    keep_mat <- matrix(as.numeric(unlist(sapply(keep, function(x) {strsplit(x,"->")}))), ncol=2, byrow = TRUE)

    ## checking if there is 0 in sources, and if so, replacing it
    if (0 %in% keep_mat[,1]) {
      loc.source.k <- which(keep_mat[,1]==0)
      replacement.source.k <- matrix(ncol=2)
      for (i in loc.source.k) {
        loc.total.k <- which(total.connections[,2]==keep_mat[i,2])
        replacement.source.k <- rbind(replacement.source.k,total.connections[loc.total.k,])
      }
      replacement.source.k <- replacement.source.k[-1,]
      keep_mat <- rbind(keep_mat[-which(keep_mat[,1]==0),], replacement.source.k)
    } else keep_mat <- keep_mat

    ## checking if there is 0 in destinations, and if so, replacing it
    if (0 %in% keep_mat[,2]) {
      loc.dest.k <- which(keep_mat[,2]==0)
      replacement.dest.k <- matrix(ncol=2)
      for (i in loc.dest.k) {
        loc.total.k <- which(total.connections[,1]==keep_mat[i,1])
        replacement.dest.k <- rbind(replacement.dest.k,total.connections[loc.total.k,])
      }
      replacement.dest.k <- replacement.dest.k[-1,]
      keep_mat <- rbind(keep_mat[-which(keep_mat[,2]==0),], replacement.dest.k)
    } else keep_mat <- keep_mat

    ## naming cols
    colnames(keep_mat) <- c("sources","dests")

    ## checking if there is any misspecified connections in keep input
    if (nrow(dplyr::setdiff(as.data.frame(keep_mat),as.data.frame(total.connections)))!=0) stop("There is at least one connection between nonadjacent layers or with backward arrow.")

    ## producing omit matrix from keep matrix
    omit<-dplyr::setdiff(as.data.frame(total.connections), as.data.frame(keep_mat))
    omit_mat <- as.matrix(omit)
  }

  # checking if keep and omit are both NULL
  if (is.null(keep) & is.null(omit)) {
    omit_mat <- matrix(nrow = 0,ncol=0)
  }

  #---------------------------------------
  # producing latex syntax
  #---------------------------------------

  # setting syntax
  setting <- c("\\usepackage{tikz} \n",
               "\\usepackage{ifthen} \n",
               "\\def\\layersep{",layer.sep, "cm} \n",
               "\\newcommand*\\circled[1]{\\tikz[baseline=(char.base)]{ \n",
               "\40 \\node[shape=rectangle,inner sep=3pt, draw=",color,"!",alpha*100,", fill= ",color,"!",alpha*25,"] (char) {#1};}} \n \n")

  # head syntax
  head <- c("\\begin{figure}[!ht] \n", "\\centering \n",
            "\\begin{tikzpicture}[shorten >=1pt,->,draw=",color,"!",alpha*100,", node distance=\\layersep, scale=1] \n",
            "\40 \\tikzstyle{every pin edge}=[<-,shorten <=1pt]; \n",
            "\40 \\tikzstyle{neuron}=[circle, draw=",color,"!",alpha*100,", minimum size=17pt,inner sep=0pt]; \n",
            "\40 \\tikzstyle{input neuron}=[neuron]; \n",
            "\40 \\tikzstyle{output neuron}=[neuron]; \n",
            "\40 \\tikzstyle{hidden neuron}=[neuron]; \n",
            "\40 \\tikzstyle{annot} = [text width=4em, text centered, text=",color,"!",alpha*100,"] \n \n")

  # producing syntax for drawing neurons
  max.layer <- which.max(c(input,hidden))

  if (max.layer == 1) {
    ## when input layer is one of the layers which have the most neurons
    ## drawing neurons on input layer
    if (is.null(input.label)) {
    input_layer <- c("\40 \\foreach \\name / \\y in {1,...,",input,"} \n",
                     "\40 \40 \40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,-\\y) {};") } else {
                       input_layer <- ""
                       nextline <- c(rep("\n",length(input.label)-1),"")
                       for (i in 1:length(input.label))
                         input_layer <- c(input_layer, "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{",input.label[i],"}] (I-",i,") at (0,-",i,") {};", nextline[i])
                     }
    ## drawing neurons on hidden layers
    hidden_layers <- ""
    i<-1
    j<-1
    for (h in hidden) {
      hidden_layers <- c(hidden_layers,"\n",
                         "\40 \\foreach \\name / \\y in {",paste0(i:(i-1+h),"/",1:h,sep= rep(c(",",""),c(h-1,1))),"} \n",
                         "\40 \40 \40 \\path[yshift=",(h-input)/2,"cm] \n",
                         "\40 \40 \40 \40 \40 node[hidden neuron] (H-\\name) at (",j,"* \\layersep,-\\y cm) {};")
      i=i+h
      j=j+1
    }
    hidden_layers <- c(hidden_layers,"\n")
    ## drawing neuron on output layer
    output_layer <- c("\40 \\node[output neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{", output.label,"}}, right of=H-",
                      floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2),", yshift=",
                      floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2)-(sum(hidden[-length(hidden)])+1+sum(hidden))/2,"cm] (O) {}; \n")
  } else {
    ## when input layer is not one of layers which have the most neurons
    ## drawing neurons on input layer
    if (is.null(input.label)) {
      input_layer <- c("\40 \\foreach \\name / \\y in {1,...,",input,"} \n",
                       "\40 \40 \40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{Input \\y}] (I-\\name) at (0,",(input-max(c(input, hidden)))/2,"-\\y) {};") } else {
                         input_layer <- ""
                         nextline <- c(rep("\n",length(input.label)-1),"")
                         for (i in 1:length(input.label))
                           input_layer <- c(input_layer, "\40 \\node [input neuron, pin=left:\\textcolor{",color,"!",alpha*100,"}{", input.label[i],"}] (I-",i,") at (0,",(input-max(c(input, hidden)))/2-i,") {};",nextline[i])
                       }
    ## drawing neurons on hidden layers
    hidden_layers <- ""
    i<-1
    j<-1
    for (h in hidden) {
      hidden_layers <- c(hidden_layers,"\n",
                         "\40 \\foreach \\name / \\y in {",paste0(i:(i-1+h),"/",1:h,sep= rep(c(",",""),c(h-1,1))),"} \n",
                         "\40 \40 \40 \\path[yshift=",(h-max(c(input, hidden)))/2,"cm] \n",
                         "\40 \40 \40 \40 \40 node[hidden neuron] (H-\\name) at (",j,"* \\layersep,-\\y cm) {};")
      i=i+h
      j=j+1
    }
    hidden_layers <- c(hidden_layers,"\n")
    ## drawing neuron on output layer
    output_layer <- c("\40 \\node[output neuron,pin={[pin edge={->}]right:\\textcolor{",color,"!",alpha*100,"}{",output.label,"}}, right of=H-",
                      floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2),", yshift=",
                      floor((sum(hidden[-length(hidden)])+1+sum(hidden))/2)-(sum(hidden[-length(hidden)])+1+sum(hidden))/2,"cm] (O) {}; \n")
  }

  # producing syntax for drawing connections
  ## designing a function to delete connections which is sourced on input layer as given in the argument of omit
  omitted_input <- function(omit) {
    omit_connections <- "\n \40 \40 \40 \40 \40 \\path (I-\\source) edge (H-\\dest)"
    r <- 1
    while (r <= nrow(omit)) {
      omit_connections <- c("\n \40 \40 \40 \40 \\ifthenelse{\\source=",omit[r,1]," \\AND \\dest=",omit[r,2]-input,"}{}{",omit_connections,"}")
      r<-r+1
    }
    omit_connections<-c(omit_connections,"; \n")
    return(omit_connections)
  }
  ## designing a function to delete connections which is sourced on hidden layers as given in the argument of omit
  omitted_hidden <- function(omit) {
    omit_connections <- "\n \40 \40 \40 \40 \40 \\path (H-\\source) edge (H-\\dest)"
    r <- 1
    while (r <= nrow(omit)) {
      omit_connections <- c("\n \40 \40 \40 \40 \\ifthenelse{\\source=",omit[r,1]-input," \\AND \\dest=",omit[r,2]-input,"}{}{",omit_connections,"}")
      r<-r+1
    }
    omit_connections<-c(omit_connections,"; \n")
    return(omit_connections)
  }
  ## designing a function to delete connections which is sourced on input layer as given in the argument of omit
  omitted_output <- function(omit) {
    omit_connections <- "\n \40 \40 \40 \40 \40 \\path (H-\\source) edge (O)"
    r <- 1
    while (r <= nrow(omit)) {
      omit_connections <- c("\n \40 \40 \40 \40 \\ifthenelse{\\source=",omit[r,1]-input,"}{}{",omit_connections,"}")
      r<-r+1
    }
    omit_connections<-c(omit_connections,";")
    return(omit_connections)
  }
  ## connections with neurons on the input layer as sources
  if (nrow(omit_mat)==0) {omit_input <- omit_mat} else {omit_input<- omit_mat[omit_mat[,1]>=1 & omit_mat[,1]<=input,,drop=FALSE]}
  omit_connections_input <- omitted_input(omit=omit_input)
  connections_input <- c("\40 \\foreach \\source in {1,...,",input,"} \n",
                   "\40 \40 \40 \\foreach \\dest in {1,...,",hidden[1],"}")
  final_connections_input <- c(connections_input,omit_connections_input)
  ## connections with neurons on hidden layers as sources
  final_connections_hidden<-""
  j <- 1
  m <- hidden[1]
  for (i in 1:(length(hidden)-1)) {
    if (nrow(omit_mat)==0) {omit_hidden <- omit_mat} else {omit_hidden<- omit_mat[omit_mat[,1]>=j+input & omit_mat[,1]<=m+input,,drop=FALSE]}
    omit_connections_hidden <- omitted_hidden(omit_hidden)
    connections_hidden <- c("\40 \\foreach \\source in {",j,",...,",m,"} \n",
        "\40 \40 \40 \\foreach \\dest in {",j+hidden[i],",...,",m+hidden[i+1],"}")
    j<-j+hidden[i]
    m<-m+hidden[i+1]
    final_connections_hidden <- c(final_connections_hidden,connections_hidden,omit_connections_hidden)
  }
  ## connections with neurons on the last hidden layer as sources
  if (nrow(omit_mat)==0) {omit_output <- omit_mat} else {omit_output<- omit_mat[omit_mat[,1]>=sum(c(input,hidden[-length(hidden)]))+1 & omit_mat[,1]<=sum(c(input,hidden)),,drop=FALSE]}
  omit_connections_output <- omitted_output(omit=omit_output)
  connections_output <- c("\40 \\foreach \\source in {",sum(hidden[-length(hidden)])+1,",...,",sum(hidden),"}")
  final_connections_output <- c(connections_output, omit_connections_output)

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
      annotation_hidden <- c(annotation_hidden, "\40 \\node[annot,above of=H-",h+1,", node distance=",(input-hidden[i])/2+2,"cm] (hl",i,") {",layer.label[i+1],"}; \n",
                             "\40 \\node[annot,above of=H-",h+1,", node distance=",(input-hidden[i])/2+1,"cm] (hl",i,") {$\\circled{",hidden[i],"}$}; \n")
      h <- h+hidden[i]
    }
    ## annotating neurons on output layers
    annotation_output <- c("\40 \\node[annot,above of =O, node distance=",(input-1)/2+2,"cm] {",layer.label[length(layer.label)],"}; \n",
                           "\40 \\node[annot,above of =O, node distance=",(input-1)/2+1,"cm] {$\\circled{1}$}; \n")
    annotation <- c(annotation_input, annotation_hidden, annotation_output)
  } else {
    ## when input layer is not one of the layers which have the most neurons
    ## annotating neurons on input layer
    annotation_input <- c("\40 \\node[annot,above of=I-1, node distance=",(max(c(input, hidden))-input)/2+2,"cm] {",layer.label[1],"}; \n",
                          "\40 \\node[annot,above of=I-1, node distance=",(max(c(input, hidden))-input)/2+1,"cm] {$\\circled{",input,"}$}; \n")
    ## annotating neurons on hidden layers
    annotation_hidden <- ""
    h <- 0
    for (i in 1:length(hidden)) {
      annotation_hidden <- c(annotation_hidden, "\40 \\node[annot,above of=H-",h+1,", node distance=",(max(c(input, hidden))-hidden[i])/2+2,"cm] (hl",i,") {",layer.label[i+1],"}; \n",
                             "\40 \\node[annot,above of=H-",h+1,", node distance=",(max(c(input, hidden))-hidden[i])/2+1,"cm] (hl",i,") {$\\circled{",hidden[i],"}$}; \n")
      h <- h+hidden[i]
    }
    ## annotating neurons on output layers
    annotation_output <- c("\40 \\node[annot,above of =O, node distance=",(max(c(input, hidden))-1)/2+2,"cm] {",layer.label[length(layer.label)],"}; \n",
                           "\40 \\node[annot,above of =O, node distance=",(max(c(input, hidden))-1)/2+1,"cm] {$\\circled{1}$}; \n")
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
    cat(comment, setting, head, "\40 % drawing neurons \n", input_layer, hidden_layers, output_layer,
        "\n \40 % drawing arrows \n", final_connections_input, final_connections_hidden, final_connections_output,
        "\n \n \40 % annotations \n", annotation, tail, sep="")
  } else message("The output of 'LaTeX' code is designed in your command to be suppressed, but it is saved in the object, if assigned. See Examples to find out how to print out the saved output.")

  syntax <- utils::capture.output(cat(comment, setting, head, "\40 % drawing neurons \n", input_layer, hidden_layers, output_layer,
                                      "\n \40 % drawing arrows \n", final_connections_input, final_connections_hidden, final_connections_output,
                                      "\n \n \40 % annotations \n", annotation, tail, sep=""))
}
