# =============================
# 1. Load packages
# =============================
library(tidyverse)
library(ggrepel)
library(cowplot)

# =============================
# 2. Load data
# =============================
load('./data/Vdata.Rdata')

# =============================
# 3. Select top 10 genes per cluster
# =============================
clabel <- mresult %>%
  group_by(cluster) %>%
  arrange(desc(abs(logFC))) %>%
  dplyr::slice(1:10)

# =============================
# 4. Generate volcano plots
# =============================
spic <- list()

for (f01 in sname) {
  
  sdata <- mresult %>% filter(jcluster == f01)
  snum  <- which(sname == f01)
  
  p01 <- ggplot() +
    
    geom_point(
      data = sdata[sdata$group == 'not', ],
      alpha = 0.4,
      fill = '#CECECE',
      shape = 21,
      stroke = .3,
      aes(y = logFC, x = -log10(adj.P.Val), size = abs(logFC))
    ) +
    
    geom_point(
      data = subset(sdata, group != 'not'),
      alpha = 0.8,
      shape = 21,
      stroke = .3,
      aes(y = logFC, x = -log10(adj.P.Val),
          fill = abs(logFC), size = abs(logFC))
    ) +
    
    scale_size_continuous(limits = c(0, 1.4), range = c(.2, 6.5)) +
    
    scale_fill_gradientn(
      colors = c('white', wcolor[snum]),
      limits = c(0, 1.4)
    ) +
    
    scale_color_manual(values = wcolor[snum]) +
    
    geom_hline(
      yintercept = c(-.1, 0.1),
      lty = 4,
      col = "black",
      lwd = 0.5
    ) +
    
    labs(
      x = "-log10(P.adj)",
      y = "log2FC",
      size = '|log2FC|',
      title = f01
    ) +
    
    coord_cartesian(
      ylim = c(-1.3, 1.5),
      xlim = c(0, 321)
    ) +
    
    guides(fill = "none", color = "none", size = "none") +
    
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(
        color = "black",
        fill  = wcolor[snum]
      )
    ) +
    
    geom_text_repel(
      data = clabel %>% filter(jcluster == f01),
      aes(y = logFC, x = -log10(adj.P.Val), label = gene),
      color = 'grey50',
      box.padding = 0.3,
      size = 4
    )
  
  spic[[f01]] <- p01
}

# =============================
# 5. Combine plots
# =============================
vsplot <- plot_grid(
  spic[[1]], spic[[2]], spic[[3]], spic[[4]],
  ncol = 4,
  align = 'v',
  axis  = 'r'
)

# =============================
# 6. Save figure
# =============================
ggsave(
  './plot/Volcano_pic.pdf',
  plot   = vsplot,
  height = 6,
  width  = 13,
  units  = "in"
)
