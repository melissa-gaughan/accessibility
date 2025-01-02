# accessibility
shiny app for service planning accessibility analysis

## Accessibility Analysis

-   service-planning\>analyses\>accessibility

-   Goal is to measure county-wide change in transit access to multiple types of destination across the county.

-   Uses r5r as the java based routing engine. You will need to download the Java Development Kit 21 with support from KCIT.

-   See the r5r github site for detailed documentation on the algorithms behind this analysis. <https://ipeagit.github.io/r5r/>

#### Analysis Set Up Steps

1.  To complete this analysis, you will need an extract of the Open Street Map network. Use the osmextract file to determine which OSM extract is the best fit for your study area.

2.  You will also need GTFS files for all transit providers offering service in your study area. Don't forget transit agency partners like ST and CT!

3.  Put the OSM network files and the GTFS files in a folder together. If you are evaluating multiple transit networks (i.e. current service and Metro Connects service), you should keep the files separate and have a second copy of the OSM network in each folder.

#### Analysis Steps

1.  Get blocks and block groups from most recent Census

2.  Make a hexagon grid that covers county or study area. Set cell size as desired (ft.)

3.  Remove water from the hex grid so that we are not measuring access to/from lakes and oceans.

4.  Load in park access points and community assets from geodatabases.

5.  Park access points are stored separately, as they duplicate parks and outnumber community assets. For this analysis, I am joining them to the community asset database to make data processing easier. In later steps, we will de-duplicate by park.

6.  Join community assets/parks to the hex grid via spatial intersection.

7.  Summarise and pivot data to result in a table of number of assets/park access points per hexagon.

8.  NOTE: To use a NetPlan GTFS, you will need to add a start date and end date to the data in calendar.txt. Dates need to be year-month-day with no separators.

    1.  Remove stations and places from stops.txt.

    2.  I ended up running the gtfs through a validator to flag issues. On the first attempt, I chose to not debug the shapes file, as it had 386 errors. All other errors were fixed. All other warnings were ignored. This allowed the network build to complete.

9.  Turn the hex grid into a set of centroid points and snap to the street grid. Make sure that the CRS is set to 4326 (world mercator).

10. Use r5r to route pathways on transit between all hexagons. Use pmap to iterate for multiple time periods, trip lengths, and days. I decided to use a logistic decay function to allow for journeys to take slightly longer than the given threshold. <https://docs.conveyal.com/learn-more/decay-functions>
