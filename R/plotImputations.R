# plot imputations on mcc tree
plotImputations <- function(trait, mcc, imp) {
  imp <-
    imp %>%
    # for main imputations only
    filter(id == 0) %>%
    # get heights for plot
    mutate(height = ifelse(Value == "?", Imputation, 1))
  # plot on tree
  out <-
    ggtree(mcc, size = 0.01, layout = "circular") +
    geom_fruit(aes(x = height, y = Language_ID, fill = Value), data = imp,
               geom = geom_bar, orientation = "y", stat = "identity",
               offset = 0) +
    scale_fill_manual(values = c("#619CFF", "#F8766D", "#D3D3D3")) +
    ggtitle(paste0("Trait: ", trait)) +
    theme(legend.title = element_blank())
  # save plot
  ggsave(
    plot = out,
    filename = paste0("plots/imputations/imp_", trait, ".pdf"),
    width = 7,
    height = 7
  )
  return(out)
}
