library(ggplot2)
library(doBy)
library(data.table)


data_loader = function(load, ident, cleanup=function(x){x}){
	function(verbose=T, reload=F){
		if(!exists(ident, envir=.GlobalEnv) | reload){
			if(verbose){cat("Could not find cached", ident, "\n")}
			assign(ident, NULL, envir=.GlobalEnv)
		}
		if(is.null(get(ident, envir=.GlobalEnv))){
			if(verbose){cat("Loading", ident, "\n")}
			assign(ident, load(verbose, reload), envir=.GlobalEnv)
		}
		cleanup(get(ident, envir=.GlobalEnv))
	}
}

tsv_loader = function(filename, ident, cleanup=function(x){x}){
	data_loader(
		function(verbose=T, reload=F){
			if(verbose){cat("Loading ", filename, "...")}
			d = read.table(
				filename,
				header=T, sep="\t",
				quote="", comment.char="",
				na.strings="NULL"
			)
			d = data.table(d)
			if(verbose){cat("DONE!\n")}
			d
		},
		ident,
		cleanup
	)
}

wikits.extend.timestamp = function(ts){
	if(is.na(ts[1])){
		NA
	}else if(nchar(ts[1]) < 4){
		warning("Not enough characters for wiki timestamp.")
		NA
	}else if(nchar(ts[1]) >= 14){
		ts[1]
	}else{
		year = substr(ts, 1, 4)

		if(nchar(ts[1]) >= 6){
			month = substr(ts, 5, 6)
		}else{
			month = "01"
		}

		if(nchar(ts[1]) >= 8){
			day = substr(ts, 7, 8)
		}else{
			day = "01"
		}

		if(nchar(ts[1]) >= 10){
			hour = substr(ts, 9, 10)
		}else{
			hour = "00"
		}

		if(nchar(ts[1]) >= 12){
			minute = substr(ts, 11, 12)
		}else{
			minute = "00"
		}

		if(nchar(ts[1]) == 14){
			second = substr(ts, 13, 14)
		}else{
			second = "00"
		}

		paste(year, month, day, hour, minute, second, sep="")
	}
}

wikits.extend = function(wikits){
	unname(
		sapply(
			as.character(wikits),
			wikits.extend.timestamp
		)
	)
}

wikits.as.Date = function(wikits){
	if(!is.numeric(wikits) & !is.character(wikits)){
		warning("Non wiki timestamp string provided.")
		wikits
	}else{
		as.Date(
			wikits.extend(wikits),
			format="%Y%m%d",
			origin="1970-01-01",
			tz="UTC"
		)
	}
}

wikits.as.POSIXct = function(wikits){
	if(!is.numeric(wikits) & !is.character(wikits)){
		warning("Non wiki timestamp string provided.")
		wikits
	}else{
		as.POSIXct(
			wikits.extend(wikits),
			format="%Y%m%d%H%M%S",
			origin="1970-01-01",
			tz="UTC"
		)
	}
}

convert.factor = function(f, map){
	chars = as.character(f)
	new_f = factor(
		sapply(
			chars,
			function(val){
				if(is.null(map[[val]])){
					NA
				}else{
					map[[val]]
				}
			}
		),
		levels=unlist(map)
	)
	names(new_f) <- NULL
	new_f
}
#convert.factor(factor(c("foo", "bar", "herp")), list(foo="Foo", bar="Bar"))

geo.mean = function(x, ...){
	exp(mean(log(x), ...))
}

geo.se.upper = function(x, ...){
	log_mean = mean(log(x), ...)
	log_sd = sd(log(x), ...)
	log_se = log_sd/sqrt(length(x))
	exp(log_mean + log_se)
}
geo.se.lower = function(x, ...){
	log_mean = mean(log(x), ...)
	log_sd = sd(log(x), ...)
	log_se = log_sd/sqrt(length(x))
	exp(log_mean - log_se)
}

geo.mean.plus.one = function(x, ...){
	geo.mean(x+1, ...)-1
}
geo.se.lower.plus.one = function(x){
	geo.se.lower(x+1)-1
}
geo.se.upper.plus.one = function(x){
	geo.se.upper(x+1)-1
}

ifor = function(x, ifval, orval){
	sapply(
		x,
		function(xval){
			if(xval){
				ifval
			}else{
				orval
			}
		}
	)
}

clean.monthly_creations = function(dt){
	dt$month_created = as.Date(dt$month_created)
	dt$account_creation = sapply(
		dt$account_type,
		function(type){
			if(is.na(type)){
				"self"
			}else if(type == "anon"){
				"anon"
			}else if(type == "autocreate"){
				"autocreated"
			}else{
				"self"
			}
		}
	)
	dt$experience_type = factor(
		mapply(
			function(experience, account_type){
				if(!is.na(account_type)){
					if(account_type == "anon"){
						"anon"
					}else if(account_type == "autocreate"){
						"autocreate"
					}else{
						as.character(experience)
					}
				}else{
					as.character(experience)
				}
			},
			dt$experience,
			dt$account_type
		),
		levels=c("anon", "autocreate", "day", "week", "month", "oldtimer")
	)
	dt
}

wiki.table = function(dt){
	cat("{|\n")
	for(name in names(dt)){
		cat("! ", name, "\n")
	}

	for(row in 1:dim(dt)[1]){
		row = c(as.matrix(dt[row]))
		cat("|-\n")
		cat("|", row[1])
		for(val in row[2:length(row)]){
			cat(" || ", val)
		}
		cat("\n")
	}

	cat("|}")
}
