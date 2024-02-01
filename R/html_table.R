
simple_html_table = function(id="",df,header=colnames(df), extra=NULL) {
  restore.point("simple_html_table")
  str = paste.df.cols(df,sep="</td><td>")
  inner.tab = paste0("<tr><td>",str,"</td></tr>", collapse="\n")
  str = paste0(header, collapse="</th><th>")
  head.tab = paste0("<tr><th>",str,"</th></tr>", collapse="\n")
  tab = paste0('<table', if(!is.null(id)) paste0(' id="',id,'" '),extra,'>\n<thead>\n', head.tab,'\n</thead>\n<tbody>\n',inner.tab,'\n</tbody>\n</table>')
  HTML(tab)
}

