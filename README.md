# nndiagram <img src="man/figures/badge.png" align="right" alt="" width="155" />

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](https://makeapullrequest.com)

> This package helps to generate LaTeX code for drawing well-formatted neural network diagrams. Users have to define number of neurons on each layer, and optionally define neuron connections they would like to keep or omit, layers they consider to be oversized and neurons they would like to draw with lighter color, as well as title of diagram, color of lines, opacity of lines, labels of layers, input and output neurons. Also, this package helps to produce LaTeX code for drawing activation functions which are crucial in neural network analysis. To make the code work in a LaTeX editor, users need to install and import some TeX packages in the setting of TeX file.

## Installation

You can install the development version of nndiagram from [GitHub](https://github.com/) with:
      
``` r
# install.packages("devtools")
devtools::install_github("ccfang2/nndiagram")
```

## Example 1

```r
# A neural network diagram with 3 neurons on input layer, 4 neurons on each of 3 hidden layers, 
# and 1 neuron on output layer. No connection is omitted and all other arguments are default.
nndiagram(input=3, hidden=c(4,4,4))
```
The output is given as follows.

```latex
% To make the code work in any LaTeX editor, users need to install and import two TeX packages in the setting, as shown below. 
% Also, users need to define the length of \layersep which is used in the LaTeX code. 
 
\usepackage{tikz} 
\usepackage{ifthen} 
\def\layersep{2.5cm} 
\newcommand*\circled[1]{\tikz[baseline=(char.base)]{ 
  \node[shape=rectangle,inner sep=3pt, draw=black!100, fill= black!25] (char) {#1};}} 
 
\begin{figure}[!ht] 
\centering 
\begin{tikzpicture}[shorten >=1pt,->,draw=black!100, node distance=\layersep, scale=1] 
  \tikzstyle{every pin edge}=[<-,shorten <=1pt]; 
  \tikzstyle{neuron}=[circle, draw=black!100, minimum size=17pt,inner sep=0pt]; 
  \tikzstyle{input neuron}=[neuron]; 
  \tikzstyle{output neuron}=[neuron]; 
  \tikzstyle{hidden neuron}=[neuron]; 
  \tikzstyle{annot} = [text width=4em, text centered, text=black!100] 
 
  % drawing neurons 
  \foreach \name / \y in {1,...,3} 
      \node [input neuron, pin=left:\textcolor{black!100}{Input \y}] (I-\name) at (0,-0.5-\y) {};
  \foreach \name / \y in {1/1,2/2,3/3,4/4} 
      \path[yshift=0cm] 
          node[hidden neuron] (H-\name) at (1* \layersep,-\y cm) {};
  \foreach \name / \y in {5/1,6/2,7/3,8/4} 
      \path[yshift=0cm] 
          node[hidden neuron] (H-\name) at (2* \layersep,-\y cm) {};
  \foreach \name / \y in {9/1,10/2,11/3,12/4} 
      \path[yshift=0cm] 
          node[hidden neuron] (H-\name) at (3* \layersep,-\y cm) {};
  \node[output neuron,pin={[pin edge={->}]right:\textcolor{black!100}{Output}}, right of=H-10, yshift=-0.5cm] (O) {}; 

   % drawing arrows 
  \foreach \source in {1,...,3} 
      \foreach \dest in {1,...,4}
           \path (I-\source) edge (H-\dest); 
  \foreach \source in {1,...,4} 
      \foreach \dest in {5,...,8}
           \path (H-\source) edge (H-\dest); 
  \foreach \source in {5,...,8} 
      \foreach \dest in {9,...,12}
           \path (H-\source) edge (H-\dest); 
  \foreach \source in {9,...,12}
           \path (H-\source) edge (O);
 
   % annotations 
  \node[annot,above of=I-1, node distance=2.5cm] {Input layer}; 
  \node[annot,above of=I-1, node distance=1.5cm] {$\circled{3}$}; 
  \node[annot,above of=H-1, node distance=2cm] (hl1) {Hidden layer 1}; 
  \node[annot,above of=H-1, node distance=1cm] (hl1) {$\circled{4}$}; 
  \node[annot,above of=H-5, node distance=2cm] (hl2) {Hidden layer 2}; 
  \node[annot,above of=H-5, node distance=1cm] (hl2) {$\circled{4}$}; 
  \node[annot,above of=H-9, node distance=2cm] (hl3) {Hidden layer 3}; 
  \node[annot,above of=H-9, node distance=1cm] (hl3) {$\circled{4}$}; 
  \node[annot,above of =O, node distance=3.5cm] {Output layer}; 
  \node[annot,above of =O, node distance=2.5cm] {$\circled{1}$}; 

\end{tikzpicture} 
\caption{} 
\label{} 
\end{figure} 
```

Users can copy and paste the output above to a LaTeX editor, such as [Overleaf](https://www.overleaf.com) to draw the neural network diagram as defined. Please be aware that two TeX packages, i.e., [TikZ](https://www.overleaf.com/learn/latex/TikZ_package) and [ifthen](https://www.ctan.org/pkg/ifthen#:~:text=Ifthen%20is%20a%20separate%20package%20within%20the%20LaT.,always%20needed%20to%20load%20it.%20Sources.%20%2Fmacros%2Flatex%2Fbase.%20Documentation.) need to be installed and imported in the setting of TeX file as instructed in the comment of output above. The resulting diagram in this example is as follows.

![Example 1](man/figures/nn.png)

If a user assigns the output to a variable, then he or she can still print out the LaTeX code by following syntax.

```r
nnd <- nndiagram(input=3, hidden=c(4,4,4))
cat(paste(nnd,"\n"))
```

## Example 2

If a user hopes to delete certain neuron connections or only keep certain connections, he or she could simply define the arguments `omit` or `keep` in `nndiagram`. For example,


## Note
- This package is a ongoing project, and more functions will be added in the future, such as those to produce pdf version of diagrams, combine existing diagrams or convert handdrawing neural network diagrams to computerized ones. 
- Collaborations are sincerely welcome. Comments and suggestions are always highly appreciated.
- This package will be available on [CRAN](https://cran.r-project.org) soon.

## Contact

Chencheng Fang, Email: [ccfang[at]uni-bonn.de](mailto:ccfang@uni-bonn.de),
Bonn Graduate School of Economics, University of Bonn, Germany
