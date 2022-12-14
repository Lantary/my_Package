% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_processing.R
\name{my_normalized}
\alias{my_normalized}
\title{归一化函数}
\usage{
my_normalized(data)
}
\arguments{
\item{data}{\code{data} 是一个数据框格式的变量}

\item{...}{\code{...}}
}
\value{
dataframe， 返回一个数据框类型的变量，其中每个值均在c(0, 1)之间
}
\description{
归一化函数，将数据框中的所有值调整为c(0, 1)之间，归一化公式为 (Value(data) - min(data))/(max(data) - min(data))。
该函数时间复杂度为o(n^2)
}
\details{
输入一个数据框, 输出数据框的最小值
}
\author{
lantary
}
