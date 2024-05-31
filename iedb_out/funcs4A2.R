# Purpose: functions of A2 analysis
# Authors: wwz, jz
# Date: July 2022

# aesthetics
theme_local <- theme_bw() + theme(
    axis.text.x = element_text(face = "bold", color = "black", size = 10, angle = 45, vjust = 0.5),
    axis.text.y = element_text(face = "bold", color = "black", size = 10),
    text = element_text(size = 10, face = "bold"),
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    legend.position = "bottom",
    strip.background = element_rect(colour = "lightgray", fill = "white", size = 1.5, linetype = "solid"), strip.text.x = element_text(size = 10, color = "black", face = "bold"))

display_venn_local <- function(x, ...){
    xlim <- c(0, 500)
    ylim <- c(0, 500)
    grid.newpage()
    pushViewport(viewport( 
        x = 0.5, y = 0.5, # a centered viewport
        width = unit(min(1, diff(xlim)/diff(ylim)), "snpc"), # aspect ratio preserved
        height = unit(min(1,diff(ylim)/diff(xlim)), "snpc"),
        xscale = xlim, # cf. xlim
        yscale = ylim  # cf. ylim
    ))
    venn_object <- venn.diagram(x, filename = NULL, ...)
    grid.draw(venn_object)
}

# name: stats_local()
# input: dataframe with variables (in form of formula) ready for wilcox and adjust p calculation
# output : dataframe with stats and other values for plotting
stats_local <- function(dat.in, fm, y.pos) {
    dat.out <- dat.in %>%
        data.frame() %>%
        wilcox_test(formula = as.formula(fm)) %>% 
         adjust_pvalue(method = "bonferroni") %>%
         add_significance("p.adj") %>%
         add_x_position(x = "nsubject", dodge = 0.8) %>%
         mutate(#y.position = ceiling(max(n1, n2)/100) * 100, 
                y.position = y.pos,
                p.label = case_when(p.adj < 0.0001 ~ "****",
                                    p.adj < 0.001 ~ "***",
                                    p.adj < 0.01 ~ "**",
                                    p.adj < 0.05 ~ "*",
                                    TRUE ~ "ns"))
    
    return(dat.out)
}

# name: plots_local()
# input: dataframes for base barplot and manually p values, label of y and title of plots
# output: a gg object
plots_local <- function(dat.in, stat.in, lbl.y, title) {
out <- ggbarplot(
    dat.in,
    x = "nsubject", 
    y = lbl.y, 
    add = "mean_se", 
    fill = "nsubject", 
    palette = c("#45ADA8", "lightcoral", "lightblue"),
    position = position_dodge(0.8)) + 
    stat_pvalue_manual(stat.in, 
                       label = "p.label", 
                       tip.length = 0.001, 
                       size = 5, 
                       family = "Arial Rounded MT Bold") + 
    theme_bw() + 
    labs(x = "Nsubject", y = "mRNA level", fill = "nsubject") + 
    #ggtitle("Kidney") + 
    ggtitle(title) + 
    theme(axis.text.x = element_text(face = "bold", color = "black", size = 8, angle = 45, vjust = 0.5),
          axis.text.y = element_text(face = "bold", color = "black", size = 8),
          text = element_text(size = 10, face = "bold"),
          axis.line.x = element_line(size = 0.3, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.3, linetype = "solid", colour = "black"),
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          strip.background = element_rect(colour = "lightgray", fill = "white", size = 1.5, linetype = "solid"), strip.text.x = element_text(size = 10, color = "black", face = "bold"))

return(out)
}