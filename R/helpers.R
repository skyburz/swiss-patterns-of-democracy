# Helper functions for Swiss Patterns of Democracy app

#' Create a standard bar chart with ggplot2
#'
#' @param data The dataset to visualize
#' @param x_var The variable for the x-axis
#' @param y_var The variable for the y-axis
#' @param fill_var Optional variable for fill color
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @return A ggplot object
create_bar_chart <- function(data, x_var, y_var, fill_var = NULL, 
                            title = "", subtitle = "", 
                            x_label = NULL, y_label = NULL) {
  
  # Set default axis labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var))
  
  # Add fill if provided
  if (!is.null(fill_var)) {
    p <- p + geom_col(aes_string(fill = fill_var))
  } else {
    p <- p + geom_col(fill = "#1b6d80")
  }
  
  # Add styling
  p <- p + 
    theme_minimal() +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Create a line chart with ggplot2
#'
#' @param data The dataset to visualize
#' @param x_var The variable for the x-axis (typically time)
#' @param y_var The variable for the y-axis
#' @param group_var Optional variable for grouping lines
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @param x_label X-axis label
#' @param y_label Y-axis label
#' @return A ggplot object
create_line_chart <- function(data, x_var, y_var, group_var = NULL,
                             title = "", subtitle = "",
                             x_label = NULL, y_label = NULL) {
  
  # Set default axis labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var))
  
  # Add group if provided
  if (!is.null(group_var)) {
    p <- p + geom_line(aes_string(color = group_var), size = 1.2)
  } else {
    p <- p + geom_line(color = "#1b6d80", size = 1.2)
  }
  
  # Add styling
  p <- p + 
    theme_minimal() +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  return(p)
}

#' Convert ggplot to interactive plotly
#'
#' @param plot A ggplot object
#' @return A plotly object
make_interactive <- function(plot) {
  p <- plotly::ggplotly(plot) %>%
    plotly::layout(
      autosize = TRUE,
      margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial", size = 12)
      )
    )
  return(p)
}

#' Clean column names for better display
#'
#' @param names Vector of column names
#' @return Vector of cleaned column names
clean_names <- function(names) {
  # Check that names is a character vector
  if (!is.character(names)) {
    warning("clean_names expects a character vector, not ", class(names))
    return(names)
  }
  
  # Apply transformations
  names <- gsub("_", " ", names)
  names <- gsub("\\.", " ", names)
  names <- gsub("([a-z])([A-Z])", "\\1 \\2", names)
  
  # Apply title case - first letter uppercase
  names <- gsub("^(\\w)", function(x) toupper(x), names, perl = TRUE)
  
  return(names)
} 