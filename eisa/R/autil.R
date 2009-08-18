
html.df <- function(df, label, digits, display) {
  if (nrow(df)==0) return("")
  if ("drive" %in% colnames(df)) { df <- df[,colnames(df) != "drive"] }
  xt <- xtable(df, label=label, digits=digits, display=display)
  tc <- textConnection("outp", open="w", local=TRUE)
  print(xt, type="html", file=tc)
  close(tc)
  fix.xtable(outp)
}
