####### Descriptive plot function ######

plot_fun  <- function(y) {
  if(is.numeric(analysis[[y]])){
    print(ggplot2::ggplot(analysis,
                          aes(x = as.factor(project), y = .data[[y]])) +
            ggplot2::geom_boxplot() +
            theme_bw()
    )} else {
      
      print(
        ggplot2::ggplot(analysis,
                        aes(
                          x = .data[[y]]
                        ),
                        na.rm = TRUE) +
          ggplot2::geom_bar(
            aes(y = ..count.. / sum(..count..), fill = project),
            position = "dodge"
          ) +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::ylab("Percentage")+
          ggplot2::ggtitle(paste('Distribution of ', y))
      )}
}



#######################################