# xfunctions package

take a look at my youtube playlist to see some of the functions in this package in action. my playlist with R Tutorials: 

---

https://www.youtube.com/playlist?list=PLRbCt61PaxX2d0_QXh6Qi6_jAQd66fmcI

---

## Some of my Defined Functions

* [cite_start]**XBarPlot(data1)**: Generates detailed frequency and relative frequency bar plots for categorical variables or histograms for numerical variables using `ggplot2`[cite: 1, 2].
* [cite_start]**XBarPlot2(data1, data2)**: Creates stacked bar plots or histograms to visualize the relationship between two variables, including contingency tables for relative frequencies[cite: 2, 3].
* [cite_start]**XBarPlot3(data1, data2, data3)**: Extends categorical visualization to three variables by using `facet_wrap` to create side-by-side plots for different levels of the third variable[cite: 4].
* [cite_start]**XColors(n)**: Returns a vector of `n` colors based on a "cyan" to "blue4" color ramp palette[cite: 4, 5].
* [cite_start]**XConfusionMatrix(predictions, targets)**: Provides an exhaustive evaluation of classification models by printing metrics like accuracy, sensitivity, specificity, F1-score, and generating a visual confusion matrix[cite: 5, 6, 7].
* [cite_start]**XDescribeRandomVariable(X, p)**: Calculates and prints the expected value (E), variance (var), and standard deviation (sd) for a discrete numerical random variable with optional associated probabilities[cite: 7].
* [cite_start]**XElectricForce(...)**: Calculates the resulting electric force at a primary point caused by other charges in a list using Coulomb's Law ($F = \frac{k_e Q_1 Q_2}{R^2}$)[cite: 8].
* [cite_start]**Vector Geometry Helpers**: A set of low-level mathematical functions including `norm` (vector magnitude), `unit` (unit vector), `dot` (dot product), `angle` (radians/degrees), `cross` (cross product), and `rotate` (2D rotation)[cite: 9, 10].
* [cite_start]**XEntropy(v)**: Computes the entropy of a vector, quantifying the expected value of information or uncertainty contained within the data[cite: 10, 11].
* [cite_start]**XExecSQL(subsys, query)**: Executes a SQL query read from an external file by connecting to a database via ODBC[cite: 11].
* [cite_start]**XFillMissingNumbers(df, formula)**: An automated data imputation tool that uses linear regression to predict and fill missing numerical values in a dataframe, excluding outliers for better accuracy[cite: 11, 12].
* [cite_start]**XFunctions(...)**: Searches for and displays all available functions within specified R packages, loading the results into a global dataframe called `Loaded.Functions`[cite: 12, 13].
* [cite_start]**XHistogram(distribution, ...)**: Plots a comprehensive histogram with a superimposed normal distribution curve, density plot, boxplot statistics, and z-score analysis for specific data points[cite: 14, 15, 16, 17, 18].
* [cite_start]**XHistogramDiscrete(v)**: Specifically designed for integer vectors, this function plots a histogram where each distinct value has its own centered bar[cite: 18, 19].
* [cite_start]**XIsNormalyDistributed(X)**: Performs a visual normality check using density plots, Q-Q plots, and logarithmic transformations to compare data against a normal distribution[cite: 19].
* [cite_start]**XJointCategorical(X, Y, p)**: Analyzes the joint distribution between two categorical variables, providing marginal and conditional probability tables[cite: 20, 21].
* [cite_start]**XJointNumerical(X, Y, p, a, b)**: Analyzes joint numerical distributions, calculates covariance and correlation, and performs portfolio analysis for linear combinations ($aX + bY$)[cite: 22, 23].
* [cite_start]**XLibrary(...)**: An automated package manager that installs missing packages, loads them, and creates global dataframes summarizing available datasets and vignettes[cite: 25, 26, 27, 28, 29, 31].
* [cite_start]**XListEnvironments(env)**: Recursively lists the chain of R environments starting from a specified environment (defaults to global) up to the empty environment[cite: 31, 32].
* [cite_start]**XMode(x)**: Identifies the statistical mode (most frequent value) for a vector, supporting both unimodal and multimodal data[cite: 32].
* [cite_start]**XPlot(f, x1, x2, x0)**: Plots a function, calculates derivatives (1st and 2nd), finds the tangent line at $x_0$, and identifies roots and extrema within a given interval[cite: 33, 34].
* [cite_start]**XPlotBinomial(n, p)**: Generates an interactive `plotly` bar chart of a Binomial distribution $B(n, p)$ with a superimposed normal curve[cite: 35, 36].
* [cite_start]**XPlotChiSquare(df, ConfidenceLevel)**: Visualizes the Chi-Squared distribution for specific degrees of freedom and highlights critical values for given confidence levels[cite: 36, 37].
* [cite_start]**XPlotGeometric(p)**: Creates an interactive `plotly` bar chart for a Geometric distribution $G(p)$, displaying the mean and standard deviation[cite: 37, 38].
* [cite_start]**XPlotMulti(myfunctions, x1, x2)**: Plots multiple functions simultaneously on a single graph with color coding and an automatic legend[cite: 38, 39].
