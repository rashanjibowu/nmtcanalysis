
# Makes a simple barchart
makeBarChart <- function(values, labels, title) {
	barplot(values, names.arg = labels, main = title, border = "dark blue")
}

# Returns a chronological history of annual investment activity for a specified geography
# Optionally specify the range of years, the type of geography("state", "city", or "zipcode", and the actually geo input)
geohistory <- function(years = 2005:2011, geoType = "all", geoName = NULL) {

	# validate inputs
	if (geoType != "state" && geoType != "zipcode" && geoType != "city" && geoType != "all") {
		stop("Invalid geoType argument. Use 'state', 'zipcode', 'city', or 'all'")
	}

	# if geoType is all, no geoname is necessary
	if (is.null(geoName) && geoType != "all") {
		stop("invalid geoName argument")
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
	data <- data[,c(3, 5, 7, 8, 9)]

	# change column names
	colnames(data) <- c("year", "investment", "city", "state", "zipcode")

	# only include data for which we have complete information
	ok <- complete.cases(as.numeric(data$year), as.numeric(data$investment), data$city, data$state, data$zipcode)
	data <- data[ok,]

	# subset for the geography specified
	geoSubSet <- if (geoType == "all") {
		data[,]
	} else {
		data[data[geoType] == geoName,]
	}

	# set up vectors to hold investment and report year data
	df.investment <- numeric(0)
	df.year <- numeric(0)

	# for each year specified, calculate and store the sum
	for (yr in years) {

		yearSubset <- geoSubSet[geoSubSet$year == yr,]

		# make the amount 0 if there was no investment that year
		annualInvestment <- if (nrow(yearSubset) > 0) {
			sum(as.numeric(yearSubset$investment))
		} else {
			0
		}

		df.investment <- c(df.investment, annualInvestment)
		df.year <- c(df.year, yr)
	}

	# prepare the returned data and headers
	overview <- data.frame(year = df.year, investment = df.investment)

	# only present the years specified
	specified <- is.element(overview$year, years)
	overview <- overview[specified,]

	# return data frame in increasing order of year
	overview[order(overview$year, decreasing = FALSE), ]


	geoNote <- if (geoType == "all") {
		c("All Geographies")
	} else if(geoType == "state" || geoType == "city") {
		c(geoName)
	} else {
		paste("Zip Code", geoName, sep = " ")
	}

	title <- paste("Annual NMTC Investment for", geoNote, sep = " ")

	# plot the bar chart
	makeBarChart(overview$investment, overview$year, title)
}