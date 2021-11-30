
args <- list( 
	source_dir= '~/Documents/GitHub/covid19_orphans/Pediatrics_US_estimates_disparities_2021',
	rep=0
)

## command line parsing if any
args_line <-  as.list(commandArgs(trailingOnly=TRUE))
if(length(args_line) > 0) 
{
	stopifnot(args_line[[1]]=='-source_dir')
	stopifnot(args_line[[3]]=='-rep')
	
	args <- list()
	args[['source_dir']] <- args_line[[2]]
	args[['rep']] <- as.integer(args_line[[4]])
} 

setwd(args$source_dir)
source(file.path("Pediatrics_US_estimates_disparities_2021","R","extraction_excess.R"))
source(file.path("Pediatrics_US_estimates_disparities_2021","R","process_fertility.R"))
source(file.path("Pediatrics_US_estimates_disparities_2021","R","process_number_children.R"))
source(file.path("Pediatrics_US_estimates_disparities_2021","R","calculate_orphans.R"))
source(file.path("Pediatrics_US_estimates_disparities_2021","R","process_skip_generation.R"))
source(file.path("Pediatrics_US_estimates_disparities_2021","R","summary_orphans.R"))


####  USA by state ########################################################################################################################
cat(sprintf("Running USA by state======\n"))
cat(sprintf("Processing Death data\n"))
process_usa_state(args$rep)

cat(sprintf("Processing number of children rates\n"))
process_number_children_usa_bystate(args$rep)

cat(sprintf("Processing number of orphans\n"))
d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
states <- unique(d_deaths$State)
states <- states[!(grepl("New York City",states) | grepl("Puerto Rico",states))]
rcat <- unique(d_deaths$Race.and.Hispanic.Origin.Group)
# exclude some groups we don't have complete data for
rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Other",rcat) | grepl("Unknown",rcat))]

ds = list()
dsr = list()
for(s in states){
	cat(paste0("processing ",s))
	for(r in rcat){
		cat(paste0("processing ",r))
		group <- paste0("usa","_",gsub(' ','',s),"_",gsub(' ','',r))
		process_orphans_usa_bystate(group,s,r)
		ds[[group]] <- t(data.frame(combine_orphans(group, process_usa_bystate_skip_generation(group,s,r))))
		dsr[[group]] <- data.frame(state=s,race.eth=r)
	}
}
df <- do.call(rbind,ds)
dlab <- do.call(rbind,dsr)

colnames(df) = c("deaths", "mother", "father", "both", "sg_grandmother", "sg_grandfather", 
								 "sg_both","cc_grandmother", "cc_grandfather", "cc_both", "primary_loss", "mg_grandmother", "mg_grandfather", "mg_both", "all", "ratio")
rownames(df) <- NULL
df <- cbind(df, dlab)

saveRDS(df, file = paste0('orphans_usa_allstates_',args$rep,'.RDS'))

source(file.path("Pediatrics_US_estimates_disparities_2021","R","make-manuscript-figures.R"))
source(file.path("Pediatrics_US_estimates_disparities_2021","R","make-manuscript-tables.R"))
source(file.path("Pediatrics_US_estimates_disparities_2021","R","SM_figures_tables.R"))

