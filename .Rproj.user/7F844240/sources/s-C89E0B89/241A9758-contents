#' max function
#'
#' This is a max function for dataframe.
#'
#' 输入一个数据框, 输出数据框的最大值
#'
#'
#' @param data data是一个数据框格式的变量 \code{data}
#' @param ...  \code{...}
#' @return 返回一个数值型变量，其值为数据框所有值的最大值
#' @author lantary
#' @seealso \code{\link[tools]{my_min}}
my_max <- function(data){
  # 取一个矩阵/数据框的最大值
  max = data[1, 1]
  for (i in 1:ncol(data)){
    max_i = max(data[, i], na.rm = T)
    if (max < max_i){
      max = max_i
    }
  }
  return(max)
}

#' min function
#'
#' This is a min function for dataframe.
#'
#' 输入一个数据框, 输出数据框的最小值
#'
#'
#' @param data data是一个数据框格式的变量 \code{data}
#' @param ...  \code{...}
#' @return 返回一个数值型变量，其值为数据框所有值的最小值
#' @author lantary
#' @seealso \code{\link[tools]{my_max}}
my_min <- function(data){
  # 取一个矩阵/数据框的最小值

  min = data[1, 1]
  for (i in 1:ncol(data)){
    min_i = min(data[, i], na.rm = T)
    if (min > min_i){
      min = min_i
    }
  }
  return(min)
}


#' 归一化函数
#'
#' 归一化函数，将数据框中的所有值调整为c(0, 1)之间，归一化公式为 (Value(data) - min(data))/(max(data) - min(data))。
#' 该函数时间复杂度为o(n^2)
#'
#' 输入一个数据框, 输出数据框的最小值
#'
#'
#' @param data \code{data} 是一个数据框格式的变量
#' @param ...  \code{...}
#' @return dataframe， 返回一个数据框类型的变量，其中每个值均在c(0, 1)之间
#' @author lantary
my_normalized <- function(data){
  # 归一化数据框数据
  if (is.data.frame(data) != TRUE){
    print('data must be a dataframe')
  }
  else{
    max = my_max(data)
    min = my_min(data)
    data_new <- data
    pb <- txtProgressBar(style=3)  # 进度条
    for (i in 1:ncol(data)){
      for (j in 1:nrow(data)){
        data_new[j, i] <- (data[j, i] - min)/(max - min)
      }
      # 设置进度条
      setTxtProgressBar(pb, i/ncol(data))
    }
    return(data_new)
  }
}




## this shows you how to write roxygen comments

#' First line is title
#'
#' This is a brief description.
#'
#' Anything else after the description goes to the Details section.
#'
#' You can write several paragraphs.
#' @param x explanation of \code{x}
#' @param ... explanation of \code{...}
#' @return The value returned by this function.
#' @author lantary
#' @seealso \code{\link[tools]{file_ext}}, \code{\link[tools]{file_path_sans_ext}}
#' @references \url{https://github.com/yihui/rmini}
#' @importFrom tools file_ext file_path_sans_ext
#' @export
#' @examples split_filename('foo.bar')
