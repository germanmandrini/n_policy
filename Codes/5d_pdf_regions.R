# setwd('C:/Users/germa/Box Sync/My_Documents') #dell

# setwd("/home/germanm2")

rm(list=ls())

# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

  # grid10_tiles_sf7 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf7.rds") 
  # grid10_soils_dt5 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
  # grid10_fields_sf2 <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf2.rds')
  # reg_model_stuff <- readRDS("./n_policy_box/Data/files_rds/reg_model_stuff.rds")
  
perfomances_dt2 <- readRDS("./n_policy_box/Data/files_rds/perfomances_dt2.rds") #data by field x z
percent20_dt <- readRDS("./n_policy_box/Data/files_rds/percent20_dt.rds")
percent20_dt <- percent20_dt[,.(policy, NMS)]
percent20_dt <- rbind(data.table(policy = c('ratio_5', 'ratio_5'), NMS = c('static', 'dynamic')), 
                      percent20_dt)

pdf_dt <- filter_dt_in_dt(perfomances_dt2, filter_dt = percent20_dt, return_table = T)
pdf_dt[,policy_name := as.character(lapply(policy, function(x) str_split(x, pattern = '_')[[1]][1]))]
pdf_dt[policy == 'ratio_5' & NMS == 'static', policy_name := 'base+static']
pdf_dt[policy == 'ratio_5' & NMS == 'dynamic', policy_name := 'base+dynamic']
pdf_dt <- pdf_dt[ L < 100]


ggplot(data = pdf_dt) +
  geom_density(aes(x=L, color = z, fill = policy), alpha = 0.4)

pdf_dt_long <- melt(pdf_dt, id.vars = c('region','id_10', 'z','policy_name', 'NMS'), 
                    measure.vars = c('L')) # 'Y_corn', 'L', 'N_fert', 'P', 'G'

# #Smooth N_fert
pdf_dt_long[policy == 'ratio_10'& variable == 'N_fert',
            value:= lapply(pdf_dt_long[policy == 'ratio_10'& variable == 'N_fert']$value, function(x) x + sample(x = -5:5, 1)[[1]][1])]
table(pdf_dt_long[policy == 'ratio_11'& variable == 'N_fert']$value)

pdf_dt_long <- pdf_dt_long[!(variable == 'L1' & value > 100)]
pdf_dt_long <- pdf_dt_long[!(variable == 'L2' & value > 100)]
pdf_dt_long <- pdf_dt_long[!(variable == 'Y_corn' & value < 5000)]
pdf_dt_long <- pdf_dt_long[!(variable == 'Y_soy' & value < 2000)]
pdf_dt_long <- pdf_dt_long[!(variable == 'P' & value < 1000)]
pdf_dt_long <- pdf_dt_long[!(variable == 'W' & value < 1000)]

x_labels <- c('Corn yield (kg ha−1 yr−1)','Soy yield (kg ha−1 yr−1)', 'L1 (kg N ha−1 yr−1)', 'L2 (kg N ha−1 yr−1)', 
              'N (kg N ha−1 yr−1', 'P ($ ha−1 yr−1)', 'E ($ ha−1 yr−1)','W ($ ha−1 yr−1)')

plot_letters <- data.table(variable = unique(pdf_dt_long$variable),
                           x_lab = rep(-1, 8),
                           y_lab = rep(0.1, 8),
                           label = c("a)", "b)", "c)", "d)", "e)", "f)", "g)","h)")
                           )

ann_text <- data.frame(y = c(0.00075, 0.0005, 0.035, 0.05, 
                             0.43, 0.001, 0.003, 0.001), 
                       value = rep(0.5, 8),
                       lab = c("a)", "b)", "c)", "d)", "e)", "f)", "g)","h)"),
                       variable = unique(pdf_dt_long$variable))

# FROM https://stackoverflow.com/questions/37573155/showing-different-axis-labels-using-ggplot2-with-facet-wrap
(plot_1 <- ggplot() +
  geom_density(data = pdf_dt_long, aes(x=value, color = policy_name), alpha = 0.4) +
    facet_wrap(region ~ variable, ncol = 1)
  #scale_linetype_manual(values = c("dashed", "solid"))+
  #geom_hline(data = hline_dt, aes(yintercept = y_line), linetype = 'dashed', color = 'grey', size = 1)+
  #geom_text(data = hline_dt, aes(x = 18, y = y_line, label =y_label ))+
  # annotate("text", x = -35,
  #                  y = 0.1, 
  #               label = c("a", "b", "c", "d", "e", "f", "g","h")) +
  # scale_fill_manual(values=c("royalblue2", "tomato3", "red", "yellow"),
  #                    name="Policy",
  #                   #labels=c("NMS2-ratio", "NMS2-fee", "NMS2-target", "Base-level")
  #                   )+
  scale_color_manual(values=c("darkgreen", "blue", "red", "yellow4"),
                    name="Policy",
                    labels=c("NMS2-fee", "NMS2-target", "NMS2-ratio", "NMS1-Base-level"))+
  geom_text(data = ann_text, aes(y = y, x = value, label = lab), size = 5, hjust = 0)+
  # labs(tag = c("a", "b", "c", "d", "e", "f", "g","h")) +
  facet_wrap(region ~ variable, nrow =2,
             labeller = as_labeller(setNames(x_labels, unique(pdf_dt_long$variable))),
             scales="free",
             strip.position = "bottom") +
  #scale_x_continuous(breaks = seq(1,20,1), labels = seq(1,20,1)) + 
  #xlab('')+
  #geom_vline(xintercept = Pn/Pc, linetype = 'dashed', color = 'grey', size = 1)+
  theme_bw()+
  theme(panel.grid = element_blank(), 
        strip.placement = "outside",
        panel.spacing = unit(1.5, "lines"),
        strip.background = element_blank(),
        axis.title.x=element_blank(),
        #legend.position = c(0, 1), 
        legend.justification = c(0, 0),
        legend.direction = "horizontal",
        legend.position = "top"))


# I agree with most of the comments that it would be easier to make seperate plots. A painpoint of such an approach is often that the alignment of the axes is off. To counteract this, I'll point towards the patchwork package, that takes out most of this pain.
# Individual plots and arrange: https://stackoverflow.com/questions/57400516/adding-unique-axes-label-names-in-ggplot-with-facet-wrap

# Dimensions: 1239 x 592

ggsave(plot = plot_1, filename = "./n_policy_box/Data/figures/pdf_variables.jpg", width = 1239/300*3, height = 592/300*3,
       units = 'in')


p <- ggplot(mtcars, aes(disp, drat)) + geom_point()
p + facet_wrap(~am)

# Rename labels on the fly with a lookup character vector
to_string <- as_labeller(c(`0` = "Zero", `1` = "One"))
p + facet_wrap(~am, labeller = to_string)

p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
             colour = "red", size = 1.5)

p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
p <- p + facet_grid(. ~ cyl)
ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
                       cyl = factor(8,levels = c("4","6","8")))
p + geom_text(data = ann_text,label = "Text")

ann_text <- data.frame(mpg = 15,wt = 5,lab = "Text",
                       cyl = factor(8,levels = c("4","6","8")))
