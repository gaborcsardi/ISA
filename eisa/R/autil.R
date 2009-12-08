
fix.xtable <- function(str) {
  str <- gsub("&lt ", "<", str, fixed=TRUE)
  str <- gsub("&gt ", ">", str, fixed=TRUE)
  str <- gsub("<TR>", "<tr>", str, fixed=TRUE)
  str <- gsub("</TR>", "</tr>", str, fixed=TRUE)
  str <- gsub("<TD", "<td", str, fixed=TRUE)
  str <- gsub("</TD>", "</td>", str, fixed=TRUE)
  str <- gsub("<TH", "<th", str, fixed=TRUE)
  str <- gsub("</TH>", "</th>", str, fixed=TRUE)
  str <- gsub("<TABLE", "<table", str, fixed=TRUE)
  str <- gsub("</TABLE>", "</table>", str, fixed=TRUE)
  str <- gsub("<A NAME", "<a name", str, fixed=TRUE)
  str <- gsub("</A>", "</a>", str, fixed=TRUE)
  
  str
}

html.df <- function(df, link=NA, label=NULL, digits=NULL, display=NULL) {
  if (nrow(df)==0) return("")
  if ("drive" %in% colnames(df)) {
    df <- df[,colnames(df) != "drive"]
  }
  df <- cbind(Id=rownames(df), df)
  if (length(link)!=1 || !is.na(link)) {
    df$Id <- paste(sep="", '<a href="', link, '">', df$Id, '</a>')
  }
  xt <- xtable(df, label=label, digits=digits, display=display)
  tc <- textConnection("outp", open="w", local=TRUE)
  print(xt, type="html", file=tc, include.rownames=FALSE)
  close(tc)
  outp <- fix.xtable(outp)
  outp
}
