### submit job to hpc
require(data.table)

## args
if(1)
{	
	args <- data.table(			
		source_dir= '~/git/covid19orphans',
		out_dir= '~/orphans',	
		bs_reps = 1000,
		job_tag = '1000_reps'
	)
}
# generate a list of commands of length 1000 for each bootstrap replicate

df <- data.table(REP=seq_len(args$bs_reps))
# make commands
cmds <- vector('list', args$bs_reps)
for(i in seq_len(args$bs_reps))
{
	cmd    <- ''			
	cmd    <- paste0( cmd, 'echo "----------- Copy repo into tmp directory ------------"\n')
	#	general housekeeping - make tmp folder to save analysis outputs
	cmd    <- paste0(cmd,"CWD=$(pwd)\n")
	cmd    <- paste0(cmd,"echo $CWD\n")	
	job_id <- as.character(abs(round(rnorm(1) * 1e6)))
	tmpdir.prefix	<- paste0('orph_',format(Sys.time(),"%y-%m-%d-%H-%M-%S"),job_id)
	tmpdir    <- paste0("$CWD/",tmpdir.prefix)
	#	copy repo to tmpdir
	cmd	<- paste0(cmd, 'cp -R ',args$source_dir,' ',tmpdir, ' \n')
	
	cmd    <- paste0( cmd, 'echo "----------- Run analysis for sample ',i,': ------------"\n')
	
	tmp    <- paste0('Rscript ', file.path(tmpdir,'scripts','run_US.R'), 
									 ' -source_dir "', file.path(tmpdir),'"',
								   ' -rep ', i,''
	)
	cmd    <- paste0(cmd, tmp, '\n')
	
	cmd    <- paste0( cmd, 'echo "----------- Copy files to out directory: ------------"\n')
	tmpdir2	<- file.path(args$out_dir,args$job_tag)
	if(i==1)
	{
		dir.create(tmpdir2)		  		
	}
	cmd    <- paste0(cmd,"mkdir -p ",tmpdir2,'\n')
	cmd    <- paste0(cmd, 'cp -v --no-preserve=mode,ownership ',file.path(tmpdir,paste0('orphans_usa_allstates_',i,'.RDS ')),'"', tmpdir2,'"\n')
	cmd    <- paste0(cmd, 'mv -v ',file.path(tmpdir,paste0('data/fertility/usa_states_fertility_f.csv  ')),file.path(tmpdir2,paste0('usa_states_fertility_f_',i,'.csv')),'\n')
	cmd    <- paste0(cmd, 'chmod -R g+rw ', tmpdir2,'\n')
	cmd    <- paste0( cmd, 'echo "----------- Summarise BS results: ------------"\n')
	
	tmp    <- paste0('Rscript ', file.path(tmpdir,'scripts','summarise_bs_results.R'), 
	                 ' -outdir "', file.path(tmpdir2),'"'
	)
	cmd    <- paste0(cmd, tmp, '\n')
	
	# delete tmp folders
	cmd    <- paste(cmd, "rm -rf $CWD/", tmpdir,'\n',sep='')
	
	cmds[[i]] <- cmd
}
df[, CMD:= unlist(cmds)]

#   submit jobs like this one:
cat(df[1,CMD])

#   run on HPC as array job
df[, CASE_ID:= 1:nrow(df)]

#   make PBS header
#hpc.load    <- "module load R/3.4.0"
hpc.load    <- "module load anaconda3/personal"
r.activate  <- "source activate covid19model"
hpc.select  <- 1                        # number of nodes
hpc.nproc   <- 1                        # number of processors on node
hpc.walltime<- 23                      # walltime
hpc.q       <- NA                       # PBS queue
hpc.mem     <- "6gb"                    # RAM
hpc.array   <- length(unique(df$CASE_ID))   # number of runs for job array
pbshead     <- "#!/bin/sh"
tmp         <- paste("#PBS -l walltime=", hpc.walltime, ":00:00,pcput=", hpc.walltime, ":00:00", sep = "")
pbshead     <- paste(pbshead, tmp, sep = "\n")
tmp         <- paste("#PBS -l select=", hpc.select, ":ncpus=", hpc.nproc,":mem=", hpc.mem, sep = "")
pbshead     <- paste(pbshead, tmp, sep = "\n")
pbshead     <- paste(pbshead, "#PBS -j oe", sep = "\n")
if(!is.na(hpc.array))
	pbshead <- paste(pbshead, "\n#PBS -J 1-", hpc.array, sep='')
if(!is.na(hpc.q))
	pbshead <- paste(pbshead, paste("#PBS -q", hpc.q), sep = "\n")
pbshead     <- paste(pbshead, hpc.load, r.activate, sep = "\n")
#   make array job
cmd     <- df[, list(CASE=paste0(CASE_ID,')\n',CMD,';;\n')), by='CASE_ID']
cmd     <- cmd[, paste0('case $PBS_ARRAY_INDEX in\n',paste0(CASE, collapse=''),'esac')]
cmd     <- paste(pbshead,cmd,sep='\n')
#   submit job
outfile     <- gsub(':','',paste("orph",paste(strsplit(date(),split=' ')[[1]],collapse='_',sep=''),'sh',sep='.'))
outfile     <- file.path(args$out_dir, outfile)
cat(cmd, file=outfile)
cmd         <- paste("qsub", outfile)
cat(cmd)
cat(system(cmd, intern= TRUE))
