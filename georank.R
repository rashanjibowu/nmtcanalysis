# Rank NMTC investments by zipcode, state, or city
# Returns a data frame listing the amount invested in each geography in descending order
# geotype argument accepts "city", "state", or "zipcode" inputs
georank <- function(geotype = "city") {

	# validate inputs
	if (geotype != "state" && geotype != "zipcode" && geotype != "city") {
		stop("Invalid geography")
	}

	data <- NULL

	# loads the data
	load <- function() {
		data <<- read.csv("nmtc/data/projects.csv", colClasses = "character")
	}

	# load data
	load()

	if (is.null(data)) stop("Trouble loading data")

	# drop unneccesary data
	data <- data[,c(5, 7, 8, 9)]

	# change column names
	colnames(data) <- c("investment", "city", "state", "zipcode")

	# only include data for which we have complete information
	ok <- complete.cases(as.numeric(data$investment), data$city, data$state, data$zipcode)
	data <- data[ok,]

	# set up vectors to hold geo and investment data
	df.geo <- character(0)
	df.state <- character(0)
	df.investment <- numeric(0)

	# split by specified geography
	s <- split(data, data[geotype])

	# for each geography, find the sum
	for (geo in s) {
		df.investment <- c(df.investment, sum(as.numeric(geo$investment)))
		df.geo <- c(df.geo, geo[1, geotype])

		# include state, if a zip or city is specified
		if (geotype == "zipcode" || geotype == "city") {
			df.state <- c(df.state, geo[1, "state"])
		}
	}

	# prepare the returned data and headers
	if (geotype == "zipcode" || geotype == "city") {
		overview <- data.frame(df.geo, df.state, df.investment)
		colnames(overview) <- c(geotype, "state", "investment")
	} else {
		overview <- data.frame(df.geo, df.investment)
		colnames(overview) <- c(geotype, "investment")
	}

	# return data frame in descending order of sum
	overview[order(overview$investment, decreasing = TRUE),]
}