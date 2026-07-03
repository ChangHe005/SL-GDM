par(pty = "s")

roc(ROC_trad[,1], ROC_trad[,2], plot=TRUE, legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Postive Percentage",
    col=roc_color[1], lwd=2.5,print.auc.y=55,
    print.auc=TRUE)

plot.roc(ROC_grs[,1],ROC_grs[,2],percent=TRUE,
         col=roc_color[2], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=50)

plot.roc(ROC_trad_grs[,1],ROC_trad_grs[,2],percent=TRUE,
         col=roc_color[3], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=45)

plot.roc(ROC_species_fdr[,1],ROC_species_fdr[,2],percent=TRUE,
         col=roc_color[4], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=40)

plot.roc(ROC_trad_species_fdr[,1],ROC_trad_species_fdr[,2],percent=TRUE,
         col=roc_color[5], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=35)

plot.roc(ROC_lipid_fdr[,1],ROC_lipid_fdr[,2],percent=TRUE,
         col=roc_color[6], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=30)

plot.roc(ROC_trad_lipid_fdr[,1],ROC_trad_lipid_fdr[,2],percent=TRUE,
         col=roc_color[7], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=25)

plot.roc(ROC_all[,1],ROC_all[,2],percent=TRUE,
         col=roc_color[8], lwd=2.5, print.auc=TRUE, add=TRUE, print.auc.y=20)

legend("bottomright", legend=c("Traditional", "Risk alleles","Traditional & Risk alleles","Species","Traditional & Species","Lipids","Traditional & Lipids","All combined"), 
       col=roc_color, lwd=2.5)