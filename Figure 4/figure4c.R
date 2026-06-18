library(circlize)
pdf("./circos.pdf",width = 12,height = 10)
chordDiagram(mat, group = group, grid.col = grid.col,
             col=col_fun,
             big.gap = 5,transparency = 0.5,
             annotationTrack = c("grid"),
             preAllocateTracks = list(
               track.height = mm_h(1),
               track.margin = c(mm_h(1), 0))
)

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = T,  adj = c(-0.1, 0.5),cex = 0.8)
}, bg.border = NA)
legend("right",pch=20,legend=unique(variable_name_table$group),col=unique(variable_name_table$col),bty="n",cex=1,pt.cex=2,border="black")

circos.clear()
dev.off()
