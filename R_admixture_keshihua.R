library(ggsci)
library(ggplot2)
library(reshape2)
library(optparse)

option_list <- list(
    make_option(c("-i", "--input"     ), type="character", default = NULL, metavar="path"  , help="Input Admixture Table file"  	  )
)

opt_parser <- OptionParser(option_list = option_list)
opt        <- parse_args(opt_parser)


infile <- opt$input


tbl <- read.table(infile)
colnames(tbl) <-c("strain" ,paste("Population", seq(ncol(tbl)-1 ), sep = ''))
order_dat <- data.frame(sort(colSums(tbl[,2:ncol(tbl)]),decreasing = T))
Population_order = sort(colSums(tbl[,2:ncol(tbl)]), index.return = T, decreasing = T )

tbl <- tbl[,c(1,Population_order$ix + 1 )]
tbl$strain <-  paste(apply(tbl[,2:ncol(tbl)], 1, function(t) colnames(tbl[,2:ncol(tbl)])[which.max(t)]),tbl$strain,sep = "_")
tbl$pop <- apply(tbl[,2:ncol(tbl)], 1, function(t) colnames(tbl[,2:ncol(tbl)])[which.max(t)])
tbl <- tbl[with(tbl, order(tbl$pop)),]
tab_group <- tbl
tbl$strain <- factor(tbl$strain, levels = tab_group$strain)
data_long <- melt(data = tbl,
                  id.vars=c("strain","pop"),
                  variable.name="Population",
                  value.name="Ancestry")

colpalettes<-unique(c(pal_npg("nrc")(10),pal_aaas("default")(10),pal_nejm("default")(8),pal_lancet("lanonc")(9),
                      pal_jama("default")(7),pal_jco("default")(10),pal_ucscgb("default")(26),pal_d3("category10")(10),
                      pal_locuszoom("default")(7),pal_igv("default")(51),
                      pal_uchicago("default")(9),pal_startrek("uniform")(7),
                      pal_tron("legacy")(7),pal_futurama("planetexpress")(12),pal_rickandmorty("schwifty")(12),
                      pal_simpsons("springfield")(16),pal_gsea("default")(12)))

set.seed(1234567)
col_list <- sample(colpalettes,ncol(tbl),replace = F)
p <- ggplot(data = data_long, mapping = aes(x = strain  , y = Ancestry, fill = Population)) +
geom_bar(stat = 'identity', position = 'fill',color = "black",width = 0.9 )+
scale_fill_manual(values = col_list)+ scale_y_continuous(expand = c(0, 0) ) + 
theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 1, angle = 90))
pdf(file =paste(infile,".pdf",sep = ""),width=0.2*nrow(tbl), height=8.27)
print(p)
dev.off()

