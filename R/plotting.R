#' @import dplyr
#' @import ggplot2

#' @export
plot_general <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                             xaxis_column_name = 'Time_s', yaxis_column_name = c('Storage_Modulus_Pa','Loss_Modulus_Pa'),
                             color_variable = 'condition', alpha_variable = '', legend_title = '',
                             facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                             x_label = '', y_label = '', x_scale = 'linear', y_scale = 'log10',
                             highlight_intervals = list()){

  dfs <- plot_initialize(data,remove_samples,xaxis_column_name,yaxis_column_name,selected_measurement,
                                 color_variable,facet_row,facet_col,plot_mean)

  plot_df <- dfs[[1]]
  filtered_df <- dfs[[2]]

  if (x_label==''){
    x_label <- paste0(c(head(strsplit(xaxis_column_name,'_')[[1]],-1), get_units(xaxis_column_name)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c(head(strsplit(yaxis_column_name[1],'_')[[1]],-1), get_units(yaxis_column_name[1])), collapse = ' ')
  }

  p <- ggplot(data=plot_df, aes(x = x_variable, y = .data[[yaxis_column_name[1]]],
                                    color = color_var))

  if(length(highlight_intervals) > 0){
    p <- add_highlights(highlight_intervals,p,y_scale)
  }

  for (i in 1:length(yaxis_column_name)) {
  p <- p +
    geom_point(aes(y = .data[[yaxis_column_name[i]]]),
               alpha = seq(from=1,to=.2,length.out=length(yaxis_column_name))[i],
               size=1, show.legend = TRUE)
  }

  if(plot_sd == TRUE){
    if(plot_mean == TRUE){
      for (i in 1:length(yaxis_column_name)) {
        p <- add_sd(p,yaxis_column_name[i],
                    paste0(yaxis_column_name[i],'_lower'),
                    paste0(yaxis_column_name[i],'_upper'),
                    color_variable)
      }
    }
  }

  if(facet_col !='' | facet_row != ''){
    p <- add_facets(facet_col, facet_row, p)
  }

  p <- set_scales(x_scale,xlim,y_scale,ylim,p)

  p <- set_theme(cmap,length(levels(plot_df$color_var)),x_label,y_label,legend_title,p)

  return(list('g' = p, 'filtered_df' = filtered_df))
}


#' @export
plot_tan_delta <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                          xaxis_column_name = 'Time_s', color_variable = 'condition', alpha_variable = '', legend_title='',
                          facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                          x_label = '', y_label = 'Loss Factor', x_scale = 'linear', y_scale = 'log10',
                          highlight_intervals = list()){

  x_col = xaxis_column_name
  y_col = colnames(data)[sapply(colnames(data), function(x) grepl('Loss_Factor', x, fixed = TRUE))]

  if (x_label==''){
    x_label <- paste0(c(head(strsplit(x_col,'_')[[1]],-1), get_units(x_col)), collapse = ' ')
  }

  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)
}

#' @export
plot_time_sweep <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                            color_variable = 'condition', alpha_variable = '', legend_title = '',
                            facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                            x_label = '', y_label = '', x_scale = 'linear', y_scale = 'log10',
                            highlight_intervals = list()){

  x_col = colnames(data)[sapply(colnames(data), function(x) grepl('\\bTime_', x, fixed = FALSE))]
  y_col = c(colnames(data)[sapply(colnames(data), function(x) grepl('Storage_Modulus', x, fixed = TRUE))],
            colnames(data)[sapply(colnames(data), function(x) grepl('Loss_Modulus', x, fixed = TRUE))])

  if (x_label==''){
    x_label <- paste0(c('Time', get_units(x_col)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c('Modulus', get_units(y_col)), collapse = ' ')
  }

  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name = x_col, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)

}

#' @export
plot_strain_sweep <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                            color_variable = 'condition', alpha_variable = '', legend_title = '',
                            facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                            x_label = '', y_label = '', x_scale = 'linear', y_scale = 'log10',
                            highlight_intervals = list()){

  x_col = colnames(data)[sapply(colnames(data), function(x) grepl('Shear_Strain', x, fixed = TRUE))]
  y_col = c(colnames(data)[sapply(colnames(data), function(x) grepl('Storage_Modulus', x, fixed = TRUE))],
            colnames(data)[sapply(colnames(data), function(x) grepl('Loss_Modulus', x, fixed = TRUE))])

  if (x_label==''){
    x_label <- paste0(c('Strain', get_units(x_col)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c('Modulus', get_units(y_col)), collapse = ' ')
  }
  #browser()
  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name = x_col, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)
}

#' @export
plot_stress_sweep <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                              color_variable = 'condition', alpha_variable = '', legend_title = '',
                              facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                              x_label = '', y_label = '', x_scale = 'linear', y_scale = 'log10',
                              highlight_intervals = list()){

  x_col = colnames(data)[sapply(colnames(data), function(x) grepl('Shear_Stress', x, fixed = TRUE))]
  y_col = c(colnames(data)[sapply(colnames(data), function(x) grepl('Storage_Modulus', x, fixed = TRUE))],
            colnames(data)[sapply(colnames(data), function(x) grepl('Loss_Modulus', x, fixed = TRUE))])

  if (x_label==''){
    x_label <- paste0(c('Stress', get_units(x_col)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c('Modulus', get_units(y_col)), collapse = ' ')
  }

  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name = x_col, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)

}

#' @export
plot_freq_sweep <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                              color_variable = 'condition', alpha_variable = '', legend_title = '',
                              facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                              x_label = 'Time (s)', y_label = 'Modulus (Pa)', x_scale = 'linear', y_scale = 'log10',
                              highlight_intervals = list()){

  x_col = colnames(data)[sapply(colnames(data), function(x) grepl('Frequency', x, fixed = TRUE))]
  y_col = c(colnames(data)[sapply(colnames(data), function(x) grepl('Storage_Modulus', x, fixed = TRUE))],
            colnames(data)[sapply(colnames(data), function(x) grepl('Loss_Modulus', x, fixed = TRUE))])

  if (x_label==''){
    x_label <- paste0(c('Frequency', get_units(x_col)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c('Modulus', get_units(y_col)), collapse = ' ')
  }

  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name = x_col, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)

}

#' @export
plot_plateau <- function(data, selected_measurement='', remove_samples='', last_n_points=5,
                         xaxis_column_name = 'Condition', color_variable = 'Condition', alpha_variable = '',
                         yaxis_column_name = 'Storage_Modulus_Pa', dot_size = 1, condition_column = 'condition',
                         facet_row = '', facet_col = '', cmap = '', ylim = c(0,NA), xlim = c(0,NA),
                         x_label = '', y_label = '', x_scale = 'linear', y_scale = 'log10'){

  if (y_label==''){
    y_label <- paste0(c(head(strsplit(yaxis_column_name[1],'_')[[1]],-1), get_units(yaxis_column_name[1])), collapse = ' ')
  }

  dfs <- plot_initialize(data,remove_samples,xaxis_column_name,yaxis_column_name,selected_measurement,
                                    color_variable,facet_row,facet_col,plot_mean=FALSE)

  filtered_df <- dfs[[2]]

  filtered_df$Timepoint <- unname(unlist(lapply(sapply(unique(filtered_df$sample), function(x) sum(filtered_df$sample==x)), function(y) 1:y)))

  filtered_df$Condition <- filtered_df[,condition_column]

  color_levels <- levels(data[,condition_column])

  agg_df <- filtered_df %>%
    group_by(sample,.data[[xaxis_column_name]]) %>%
    do(tail(.,n=last_n_points)) %>%
    summarize(y_sd = sd(.data[[yaxis_column_name]]),
              y = mean(.data[[yaxis_column_name]]),
              #x = unique(.data[[xaxis_column_name]]),
              Condition = unique(Condition), Time_s = mean(Time_s), color = unique(.data[[color_variable]]),
              facet_row_var = facet_row_var[1], facet_col_var = facet_col_var[1])

  agg_df2 <- agg_df %>%
    group_by(Condition,.data[[xaxis_column_name]]) %>%
    summarize(y_sd = sd(y), y = mean(y),
              #x = unique(x),
              color = unique(color),
              Condition = factor(unique(Condition),levels=color_levels), Time_s = mean(Time_s),
              facet_row_var = facet_row_var[1], facet_col_var = facet_col_var[1])

  agg_df2$y_upper <- agg_df2$y + agg_df2$y_sd
  agg_df2$y_lower <- agg_df2$y - agg_df2$y_sd

  p <- agg_df2 %>%
    ggplot(aes(x = .data[[xaxis_column_name]], y = y), color='black')+
    geom_jitter(data=agg_df, width=.1,
                aes(x = .data[[xaxis_column_name]], y = y, color=color), size=dot_size)+
    geom_errorbar(data=agg_df2, aes(ymin = y_lower, ymax = y_upper), color='black', width = 0.2, show.legend = FALSE)+
    geom_point(size = dot_size*1.25, show.legend = FALSE)+
    ylab(y_label)+
    xlab(x_label)+
    theme_classic()+
    theme(strip.background = element_blank(),
          axis.text = element_text(color="black"),
          axis.ticks = element_line(color = "black"))+
    scale_color_manual(values = colorRampPalette(cmap)(length(unique(agg_df2$color))))

  p <- set_y_scale(y_scale,ylim,p)

  if(facet_col !='' | facet_row != ''){
    p <- add_facets(facet_col, facet_row, p)
  }
  return(list('g' = p, agg_data = agg_df2))
}

#' @export
plot_visc_vs_rate <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                            color_variable = 'condition', alpha_variable = '', legend_title = '',
                            facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                            x_label = '', y_label = '', x_scale = 'linear', y_scale = 'log10',
                            highlight_intervals = list()){

  x_col = colnames(data)[sapply(colnames(data), function(x) grepl('Shear_Rate', x, fixed = TRUE))]
  y_col = colnames(data)[sapply(colnames(data), function(x) grepl('\\bViscosity', x, fixed = FALSE))]

  if (x_label==''){
    x_label <- paste0(c('Shear Rate', get_units(x_col)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c('Viscosity', get_units(y_col)), collapse = ' ')
    y_label <- gsub('\\.','·',y_label)
  }

  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name = x_col, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)

}

#' @export
plot_stress_vs_rate <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd = FALSE,
                              color_variable = 'condition', alpha_variable = '', legend_title = '',
                              facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(-1,NA),
                              x_label = '', y_label = '', x_scale = 'linear', y_scale = 'log10',
                              highlight_intervals = list()){

  x_col = colnames(data)[sapply(colnames(data), function(x) grepl('Shear_Rate', x, fixed = TRUE))]
  y_col = colnames(data)[sapply(colnames(data), function(x) grepl('Shear_Stress', x, fixed = TRUE))]

  if (x_label==''){
    x_label <- paste0(c('Shear Rate', get_units(x_col)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c('Shear Stress', get_units(y_col)), collapse = ' ')
  }

  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name = x_col, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)

}

#' @export
plot_stress_relax <- function(data, normalize = FALSE, selected_measurement='', remove_samples='', plot_mean = FALSE, plot_sd=FALSE,
                              color_variable = 'condition', alpha_variable = '', dot_size = 1,
                              facet_row = '', facet_col = '', cmap = '', ylim = c(0.0001,NA), xlim = c(0,NA),legend_title='',
                              x_label = '', y_label = '', x_scale = 'log10', y_scale = 'log10',
                              highlight_intervals = list()){


  x_col = colnames(data)[sapply(colnames(data), function(x) grepl('\\bTime_', x, fixed = FALSE))]
  y_col = colnames(data)[sapply(colnames(data), function(x) grepl('Shear_Stress', x, fixed = TRUE))]

  if (x_label==''){
    x_label <- paste0(c('Time', get_units(x_col)), collapse = ' ')
  }

  if (y_label==''){
    y_label <- paste0(c('Shear Stress', get_units(y_col)), collapse = ' ')
  }

  if (normalize == TRUE){
    if(selected_measurement == ''){
      selected_measurement = unique(data$measurement)[1]
    }

    data <- data %>%
      group_by(sample) %>%
      filter(measurement %in% selected_measurement) %>%
      mutate(Norm_Shear_Stress=.data[[y_col]]/(.data[[y_col]][1])) %>%
      ungroup() %>%
      as.data.frame(.)

    y_label <- 'Normalized Shear Stress'
    y_col <- 'Norm_Shear_Stress'
  }

  plot_general(data, selected_measurement, remove_samples, plot_mean, plot_sd,
                   xaxis_column_name = x_col, yaxis_column_name = y_col,
                   color_variable, alpha_variable, legend_title,
                   facet_row, facet_col, cmap, ylim, xlim,
                   x_label, y_label, x_scale, y_scale,
                   highlight_intervals)
}

#' @export
plot_recovery <- function(data, selected_measurement='', remove_samples='', plot_mean = FALSE,
                          xaxis_column_name = 'condition', color_variable = 'condition', alpha_variable = '',
                          facet_row = '', facet_col = '', cmap = '', ylim = c(-1,NA), xlim = c(0,NA),
                          x_label = 'Group', y_label = 'Shear Stress (Pa)', x_scale = 'linear', y_scale = 'log10'){


  sample_map <- unique(df[,c('sample','id')])
  selected_sample <- sample_map$id[match(remove,sample_map$sample)]
  yaxis_column_name = 'Shear_Stress_Pa'
  shape_column_name = 'name'

  if(selected_measurement == ''){
    selected_measurement = unique(df$measurement)[1]
  }

  filtered_df <- df %>%
    filter(measurement %in% selected_measurement) %>%
    filter(!id %in% selected_sample)

  filtered_df <- filtered_df %>%
    filter(interval %in% unique(filtered_df$interval)[c(TRUE,FALSE)])

  filtered_df$Condition <- filtered_df[,xaxis_column_name]
  if(xaxis_column_name == color_variable){
    color_variable <- 'Condition'
  }
  xaxis_column_name <- 'Condition'


  filtered_df <- filtered_df %>%
    group_by(sample,interval) %>%
    do(tail(., n=5)) %>%
    group_by(sample, interval) %>%
    mutate(Storage_Modulus_Pa = mean(Storage_Modulus_Pa), sample=unique(sample),
           interval=unique(interval), Condition=unique(Condition)) %>%
    select(Storage_Modulus_Pa, sample, interval, Condition) %>%
    unique()

  filtered_df2 <- filtered_df %>%
    group_by(Condition,interval) %>%
    mutate(means = mean(Storage_Modulus_Pa)) %>%
    group_by(sample) %>%
    mutate(means2 = rep(first(means),length(unique(interval)))) %>%
    mutate(Storage_Modulus_Pa = Storage_Modulus_Pa/means2)

  norm_df2 <- filtered_df2 %>%
    group_by(sample) %>%
    mutate(Storage_Modulus_Pa = Storage_Modulus_Pa/first(Storage_Modulus_Pa))


  norm_df2 <- norm_df2 %>%
    group_by(Condition,interval) %>%
    mutate(Storage_Modulus_sd = sd(Storage_Modulus_Pa),
           Storage_Modulus_Pa = mean(Storage_Modulus_Pa)) %>%
    select(Storage_Modulus_Pa,Storage_Modulus_sd,interval,Condition) %>%
    unique()


  p <- norm_df2 %>%
    ggplot(aes(x=interval, y=Storage_Modulus_Pa, color=Condition, group=Condition))+
    geom_ribbon(aes(ymin = Storage_Modulus_Pa-Storage_Modulus_sd,
                    ymax = Storage_Modulus_Pa+Storage_Modulus_sd,
                    fill = Condition), color=NA, alpha=.2, show.legend = FALSE)+
    geom_point()+
    geom_line()+
    ylab(y_label)+
    xlab(x_label)+
    theme_classic()+
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text = element_text(color="black"),
          axis.ticks = element_line(color = "black"))+
    scale_color_manual(values = colorRampPalette(cmap)(length(unique(norm_df2$Condition))))+
    scale_fill_manual(values = colorRampPalette(cmap)(length(unique(norm_df2$Condition))))

  if(y_scale == 'log10'){
    p <- p + scale_y_log10(limits = ylim,
                           breaks = 10^c(-8:8),#scales::trans_breaks("log10", function(x) 10^x),
                           labels = scales::trans_format("log10", scales::math_format(10^.x)))
  }else if(y_scale == 'linear'){
    p <- p + ylim(ylim)
  }else {print("Can't plot requested y scale")}

  p

}
