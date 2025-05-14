#' @import dplyr
#' @import ggplot2

plot_initialize <- function(data,remove,xaxis_column_name,yaxis_column_name,selected_measurement,
                            color_variable,facet_row,facet_col,plot_mean){

  sample_map <- unique(data[,c('sample','id')])
  selected_sample <- sample_map$id[match(remove,sample_map$sample)]
  shape_column_name = 'name'

  if (is.null(levels(data$condition))){
    data$condition <- factor(data$condition, levels=unique(data$condition))
  }

  if (is.null(levels(data[,color_variable]))){
    data[,color_variable] <- factor(data[,color_variable], levels=unique(data[,color_variable]))
  }

  data$x_variable <- data[,xaxis_column_name]

  if(selected_measurement == ''){
    selected_measurement = unique(data$measurement)[1]
  }

  filtered_df <- data %>%
    filter(measurement %in% selected_measurement) %>%
    filter(!id %in% selected_sample)

  filtered_df$color_var <- filtered_df[,color_variable]

  filtered_df$facet_row_var <- 'a'
  filtered_df$facet_col_var <- 'b'

  if (facet_row != ''){
    filtered_df$facet_row_var <- filtered_df[,facet_row]
  }
  if (facet_col != ''){
    filtered_df$facet_col_var <- filtered_df[,facet_col]
  }

  orig <- filtered_df

  if(plot_mean==TRUE){
    n_colors_per_condition <- filtered_df %>%
                              select(condition,color_var) %>%
                              distinct() %>%
                              group_by(condition) %>%
                              distinct(color_var) %>%
                              count(condition)%>%
                              ungroup() %>%
                              select(n)

    if(sum(n_colors_per_condition>1)>0){
      stop('The color variable indicates that there are multiple subgroups within each condition.
           This will cause incorrect averaging and plotting of data. Make sure your condition variable
           adequately describes the grouping of data.')
    }

    filtered_df$Timepoint <- unname(unlist(lapply(sapply(unique(filtered_df$sample), function(x) sum(filtered_df$sample==x)), function(y) 1:y)))

    for (i in 1:length(yaxis_column_name)){
      sub_df2 <- filtered_df %>%
        group_by(condition,Timepoint) %>%
        summarize(y_sd = sd(.data[[yaxis_column_name[i]]]),
                  y = mean(.data[[yaxis_column_name[i]]]),
                  condition = unique(condition), x_variable = mean(x_variable), color_var = color_var[1],
                  facet_row_var = facet_row_var[1], facet_col_var = facet_col_var[1])

      sub_df2$y_upper <- sub_df2$y + sub_df2$y_sd
      sub_df2$y_lower <- sub_df2$y - sub_df2$y_sd

      colnames(sub_df2)[colnames(sub_df2)=='y_sd'] <- paste0(yaxis_column_name[i],'_sd')
      colnames(sub_df2)[colnames(sub_df2)=='y'] <- yaxis_column_name[i]
      colnames(sub_df2)[colnames(sub_df2)=='y_upper'] <- paste0(yaxis_column_name[i],'_upper')
      colnames(sub_df2)[colnames(sub_df2)=='y_lower'] <- paste0(yaxis_column_name[i],'_lower')

      if (i>1){
        sub_df <- cbind(sub_df,sub_df2[,c(paste0(yaxis_column_name[i],'_sd'),yaxis_column_name[i],
                                          paste0(yaxis_column_name[i],'_upper'),paste0(yaxis_column_name[i],'_lower'))])
      }else{
        sub_df <- sub_df2
      }
      #browser()

    }
    filtered_df <- sub_df
  }

  filtered_df$color_var <- factor(filtered_df$color_var, levels = levels(data[,color_variable]))

  return(list('plot_df' = filtered_df, 'filtered_df' = orig))
}

get_units <- function(str){
  a <- strsplit(str,'_')[[1]]
  ifelse(a[length(a)]=='1','',paste0(c('(',a[length(a)],')'),collapse = ''))
}

add_highlights <- function(highlight_intervals,p,y_scale){
  h_i <- list(sapply(1:length(highlight_intervals), function(x) highlight_intervals[[x]][1]),
              sapply(1:length(highlight_intervals), function(x) highlight_intervals[[x]][2]))

  if (y_scale=='linear'){
    ymin=-Inf
  }else{
    ymin=0
  }
  p + annotate('rect', xmin = h_i[[1]], xmax = h_i[[2]],
               ymin = ymin, ymax = Inf, fill='red', alpha=.1)
}

add_facets <- function(facet_col,facet_row,p){
  if(facet_col !='' & facet_row != ''){
    p + facet_grid(facet_row_var~facet_col_var,scale='free_y')
  }else if(facet_col !='' & facet_row == ''){
    p + facet_grid(.~facet_col_var,scale='free_y')
  }else if(facet_col =='' & facet_row != ''){
    p + facet_grid(facet_row_var~.,scale='free_y')
  }
}

add_sd <- function(p,yaxis_column_name,yaxis_lower,yaxis_upper,color_variable){
  p + geom_ribbon(aes(y = .data[[yaxis_column_name]],
                      ymin = .data[[yaxis_lower]], ymax = .data[[yaxis_upper]],
                      fill = color_var),#.data[[color_variable]]),
                      alpha = .2, color=NA)
}

set_scales <- function(x_scale,xlim,y_scale,ylim,p){
  if(y_scale == 'log10'){
    p <- p + scale_y_log10(limits = ylim,
                           breaks = 10^c(-8:8),#scales::trans_breaks("log10", function(x) 10^x),
                           labels = scales::trans_format("log10", scales::math_format(10^.x)))
  }else if(y_scale == 'linear'){
    p <- p + ylim(ylim)
  }else{print("Can't plot requested y scale")}

  if(x_scale == 'log10'){
    p <- p + scale_x_log10(limits = xlim,
                           breaks = 10^c(-8:8),#scales::trans_breaks("log10", function(x) 10^x),
                           labels = scales::trans_format("log10", scales::math_format(10^.x)))
  }else if(x_scale == 'linear'){
    p <- p + xlim(xlim)
  }else {print("Can't plot requested x scale")}

  p
}

set_y_scale <- function(y_scale,ylim,p){
  if(y_scale == 'log10'){
    p <- p + scale_y_log10(limits = ylim,
                           breaks = 10^c(-8:8),#scales::trans_breaks("log10", function(x) 10^x),
                           labels = scales::trans_format("log10", scales::math_format(10^.x)))
  }else if(y_scale == 'linear'){
    p <- p + ylim(ylim)
  }else{print("Can't plot requested y scale")}
}

set_theme <- function(cmap,n_col,x_label,y_label,legend_title,p){
  if (cmap==''){
      p + ylab(y_label)+
          xlab(x_label)+
          theme_classic()+
          theme(strip.background = element_blank(), axis.text.y = element_text(colour = "black"),
                axis.text.x = element_text(colour = "black"))+
          labs(color=legend_title,fill=legend_title)
  }else{
      p + scale_color_manual(values = rev(colorRampPalette(rev(cmap))(n_col))) +
          scale_fill_manual(values = rev(colorRampPalette(rev(cmap))(n_col)))+
          ylab(y_label)+
          xlab(x_label)+
          theme_classic()+
          theme(strip.background = element_blank(), axis.text.y = element_text(colour = "black"),
                axis.text.x = element_text(colour = "black"))+
          labs(color=legend_title,fill=legend_title)
  }
}

