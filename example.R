source('R/extract_data.R')
source('R/plotting.R')
source('R/plot_helpers.R')
library(ggplot2)
library(dplyr)

#devtools::document()
#devtools::install()


#library(APRheoPlotR)

col_pal <- c('#960162','#ED5315','#249FCA','#1D3367')

path <- './'

f <- 'norb_homopolymerization_same_formulations.csv'
dat <- readData(paste0(path,f),meta_from_file_name = TRUE)

df <- dat@long
meta <- dat@metadata

meta$condition <- meta$V2

meta$condition[meta$condition=='4armPEG'] = '4-arm PEG'
meta$condition[meta$condition=='10kNorHA'] = '19 kDa NorHA'
meta$condition[meta$condition=='40kNorHA'] = '37 kDa NorHA'
meta$condition[meta$condition=='60kNorHA'] = '82 kDa NorHA'

meta$condition <- factor(meta$condition, levels = c('4-arm PEG','19 kDa NorHA','37 kDa NorHA','82 kDa NorHA'))

df <- cbind(df, meta[match(df$sample,meta$sample),colnames(meta) != 'sample'])

.rs.restartR()
devtools::document();devtools::install();library(APRheoPlotR)
source('R/extract_data.R');source('R/plot_helpers.R');source('R/plotting.R')
#pdf(file = paste0(path,'Fig4_same_formulation_mean_curing.pdf'), width = 10, height = 2.5)#, units = 'in')#, res = 300)
g <- plot_time_sweep(data = df, selected_measurement = 'UV Cure', facet_col = 'condition',
                      ylim = c(1,NA), plot_mean=TRUE, plot_sd=TRUE, highlight_intervals = list(c(10,10+300)),
                      cmap = col_pal, y_scale='log10')
g+theme(axis.text.x = element_text(angle = 45,hjust=1))
#dev.off()

g <- plot_tan_delta(data = df, selected_measurement = 'UV Cure', facet_col = 'condition',
                   #ylim=c(0.01,2),
                   plot_mean=TRUE, plot_sd=TRUE, highlight_intervals = list(c(10,10+300)),
                   cmap = col_pal, y_scale='log10')
g+theme(axis.text.x = element_text(angle = 45,hjust=1))


out <- plot_plateau(df, last_n_points = 5, selected_measurement = 'UV Cure', xaxis_column_name = 'condition', cmap = col_pal,
                    yaxis_column_name = 'Loss_Factor_1', color_variable = 'condition',
                    y_scale = 'linear', ylim = c(0,NA), xlim = c(1,NA))
out$g+theme(axis.text.x = element_text(angle = 45,hjust=1))


g <- plot_general(data = df, selected_measurement = 'UV Cure',
                      xaxis_column_name = 'Loss_Factor_1', yaxis_column_name = 'Storage_Modulus_Pa',
                     ylim = c(1,NA), xlim = c(0,5), plot_mean=FALSE, plot_sd=FALSE,
                     cmap = col_pal, y_scale='log10')
g+theme(axis.text.x = element_text(angle = 45,hjust=1))


out <- plot_plateau(df, selected_measurement = 'Stress Relaxation', xaxis_column_name = 'condition', cmap = col_pal,
                    yaxis_column_name = 'Shear_Stress_Pa', color_variable = 'condition',
                    y_scale = 'linear', ylim = c(0,NA), xlim = c(1,NA))
out$g+theme(axis.text.x = element_text(angle = 45,hjust=1))

g <- plot_stress_relax(df, normalize = TRUE, selected_measurement = 'Stress Relaxation', cmap = col_pal,
                    color_variable = 'condition', plot_mean = TRUE, plot_sd = TRUE,
                    x_scale='linear', y_scale = 'linear')
g+theme(axis.text.x = element_text(angle = 45,hjust=1))


###############################
#Fibers
###############################
source('R/extract_data.R');source('R/plot_helpers.R');source('R/plotting.R')

f <- 'gelma_fibers.csv'#save as utf-8 csv
dat <- readData(paste0(path,f))

df <- dat@long
meta <- dat@metadata

df <- cbind(df, meta[match(df$sample,meta$sample),colnames(meta) != 'sample'])

df$condition <- 'test'

source('R/plot_helpers.R');source('R/plotting.R')

plot_strain_sweep(df, selected_measurement = 'Amplitude sweep',
                 x_scale = 'log10', plot_mean=TRUE, plot_sd = TRUE,
                 cmap = col_pal[c(1,3)], facet_col = 'condition', y_scale='log10')

plot_stress_sweep(df, selected_measurement = 'Amplitude sweep',ylim=c(1,250),
                  x_scale = 'log10', plot_mean=TRUE, plot_sd = TRUE,
                  cmap = col_pal[c(1,3)], facet_col = 'condition', y_scale='log10')

plot_tan_delta(df, selected_measurement = 'Amplitude sweep', xaxis_column_name = 'Shear_Strain_1',
                  x_scale = 'log10', plot_mean=TRUE, plot_sd = FALSE, ylim=c(.1,10),
                  cmap = col_pal[c(1,3)], facet_col = 'condition', y_scale='log10')



###############################
#Fibers
###############################
source('R/extract_data.R');source('R/plot_helpers.R');source('R/plotting.R')

f <- 'gelma_fibers_diff_size_visc_v_shear2.csv'#save as utf-8 csv
dat <- readData(paste0(path,f),meta_from_file_name = TRUE)

df <- dat@long
meta <- dat@metadata
meta$condition <- meta$V2

df <- cbind(df, meta[match(df$sample,meta$sample),colnames(meta) != 'sample'])


source('R/plot_helpers.R');source('R/plotting.R')

g <- plot_visc_vs_rate(df, selected_measurement = 'Measurement 1',
                  x_scale = 'log10', plot_mean=TRUE, plot_sd = TRUE,#facet_col = 'condition',
                  cmap = col_pal[c(1,3)],  y_scale='log10')
g+theme(axis.text.x = element_text(angle = 45,hjust=1))


g <- plot_stress_vs_rate(df, selected_measurement = 'Measurement 1',
                           x_scale = 'log10', plot_mean=TRUE, plot_sd = TRUE,#facet_col = 'condition',
                           cmap = col_pal[c(1,3)],  y_scale='log10')
g+theme(axis.text.x = element_text(angle = 45,hjust=1))


###############################
#Fibers
###############################
source('R/extract_data.R');source('R/plot_helpers.R');source('R/plotting.R')

f <- 'gelma_fibers_diff_size_recovery_osc.csv'#save as utf-8 csv
dat <- readData(paste0(path,f),meta_from_file_name = TRUE)

df <- dat@long
meta <- dat@metadata
meta$condition <- meta$V1

df <- cbind(df, meta[match(df$sample,meta$sample),colnames(meta) != 'sample'])


source('R/plot_helpers.R');source('R/plotting.R')

g <- plot_time_sweep(df, selected_measurement = 'Pre-UV recovery',facet_col = 'condition',
                       x_scale = 'linear', plot_mean=TRUE, plot_sd = FALSE,
                       cmap = col_pal[c(1,3)],  y_scale='log10')
g


out <- plot_plateau(df, last_n_points = 1, selected_measurement = 'Pre-UV recovery',facet_col = 'condition', color_variable = 'condition',
                xaxis_column_name = 'interval', x_scale = 'linear',
                cmap = col_pal[c(1,3)],  y_scale='linear')
out$g

out$g$data <- out$g$data %>%
  subset(color == '85um')

out$g %+% subset(out$g$data, interval==1)
