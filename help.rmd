# Metro Connects 2030 Scenarios Trip Change Dashboard

## About

This dashboard shows the anticipated level of change in trip volumes for high, medium and low growth scenarios of the Metro Connects network. The app compares potential changes to the Spring 2020 network using GTFS files for Spring 2020 and the three Metro Connects 2030 scenarios. We chose Spring 2020 as the baseline comparison point because it was the last service change before pandemic and workforce related suspensions were implemented. Spring 2020 was the largest network ever operated by Metro. This makes it an ideal baseline for future growth scenarios, as it indicates the transit capacity of existing capital facilities. However, there are important caveats to be aware of while using this dashboard:

-   Route alignments for the Metro Connects networks are taken from Remix. Metro staff used Remix's automatic stop generation process to add stops to new routes. This means that stops associated with Metro Connects routes cannot be used as confirmed future stops. To get around that issue, this app uses the route alignment to calculate the number of trips passing through an area. This means that areas without pedestrian access like highways will still show trip change. **Staff using this app should use their own knowledge and judgement to develop capital scenarios.**

-   Because stop data was not usable, Metro staff used the trip start time to determine the period of the trip. For trips with particularly long patterns or for trips scheduled on the cusp of a time period, this may lead to some fuzziness in the comparison of one network to another due to minor scheduling differences.

-   Network restructures that have occurred since Spring 2020 are not captured in this tool. This includes changes made through the RKAAMP and North Link restructures. When using the tool, you will see references to routes deleted in these restructures.

-   Spring 2020 data does not include partner service (CT, ST, PT), Metro school service, MetroFlex service or DART service.

## Metrics

The dashboard has several metrics available for planners. All reference the same data and can be used together to develop an understanding of the scale and size of change in an area.

**Trips/Period:** The number of trips passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard.

**Avg Trips/Hour:** The number of trips per hour passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. For routes with spans of service that only cover a portion of a period, the dashboard assumes that the average should be calculated using the total number of hours in the period. In other words, if a route has its last trip at 7:10 PM, it will have records available in the "7 PM - 10 PM" period, but the average number of trips per hour will be less than 1.

**Change in Trips/Period:** The change in number of trips passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**Change in Avg Trips/Hour:** The change in number of trips per hour passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**% Change Trips/Period:** The percent change in number of trips passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**% Change Avg Trips/Hour:** The percent change in number of trips per hour passing through the hexagon on the day (weekday, Saturday, Sunday) and period as selected in the dashboard. Change is calculated based on the Spring 2020 network.

**New Service:** This metric identifies areas that will have service in the selected Metro Connects 2030 scenario that did not have service in 2020. The goal is to identify areas that may need 100% new capital infrastructure or pathway improvements.

**Max Service:** This metric finds the highest amount of service that will be present in each hexagon across all scenarios and the Spring 2020 baseline. There are certain hexagons where the highest level of service is in Spring 2020. This metric is intended to provide an understanding of the maximum service that might serve each area.

## Metro Connects Growth Scenarios

Metro developed three hypothetical scenarios (low-, moderate-, and high-growth) based on the amount of fixed route transit service by 2030. A few assumptions are built into all three scenarios: 

-   The draft transit network in each scenario is adapted from the Metro Connects interim network to show how funding projections would correspond to service levels in 2030. 

-   Each scenario used 2019 as a baseline and assumes that Metro would restore all pre-pandemic service hours with some assumed additional service growth across the system by 2030. 2019 has a baseline of 4.17 million service hours.

-   Service investments were prioritized in accordance with Metro's Service Guidelines. 

-   Service level targets for each route were also determined by Metro's Service Guidelines. 

### Low Growth Scenario  

This scenario assumes approximately 4,234,000 total annual hours of service, which comes in below Metro's current budget expectations due to the expiration of the Seattle Transit Measure without a renewal by voters. Without any additional outside funding, Metro would be able to fund a small increase in service across the system in addition to the six new RapidRide lines. Roughly 58 routes would reach their target service levels as identified through Metro's service guidelines. 

#### Funding: 

Although Metro would still have the capacity for some growth in this scenario, the loss of the Seattle Transit Measure would slow overall service growth for many routes serving Seattle.

-   Metro: Net increase of 445,000 annual service hours would fund six RapidRide lines (G, H, I, J, K, R) and service hour investments across the transit system. 

-   Seattle Transit Measure: Expires without renewal (in 2027), resulting in a net loss of 225,000 annual service hours. 

-   Regional Funding Package: N/A - this funding source would not apply under this scenario. 

#### Geographic Emphasis: 

-   Service Growth: Service growth would be strongest in Downtown Seattle and South King County. The eastside and North Seattle would also receive modest service growth.  

#### Capital Needs:  

-   Speed and reliability improvements (S. King, Downtown Seattle)  

-   Hubs and route facilities (S. King, Eastside) 

### Moderate Growth Scenario  

This scenario assumes approximately 4,459,000 total annual hours of service, which is aligned with Metro's current budget expectations. Under the moderate growth scenario, the Seattle Transit Measure would be renewed, which would allow Metro to make additional system-wide investments in service while. Roughly 74 routes would reach their target service levels as identified through Metro's service guidelines. 

#### Funding:  

Under the moderate growth scenario, the funding is assumed to increase, primarily from the renewal of the Seattle Transit Measure. Metro's internal funding is consistent across all three scenarios. A regional ballot measure for additional county-wide funding would not apply in this scenario.  

-   Metro: Net increase of 445,000 annual service hours would fund six RapidRide lines (G, H, I, J, K, R) and service hour investments across the transit system. 

-   Seattle Transit Measure: Renewed, resulting in a net gain of zero annual service hours. 

-   Regional Funding Package: N/A - this funding source would not apply under this scenario. 

#### Geographic Emphasis: 

-   Service Growth: Service growth would be strongest in Downtown Seattle and South King County. The eastside and North Seattle would also see new service growth. Additional system-wide growth in suburban King County would address some smaller gaps within the transit system. 

#### Capital Needs:  

-   New speed and reliability investments (N. Seattle) 

-   New passenger & route facilities  

-   Moderate fleet/capacity constraints  

### High Growth Scenario 

This scenario assumes 5,000,000 total annual hours of service, which aligns with Metro Connects but exceeds current budget projections. This increase in funding would enable Metro to increase service all across the county. Roughly 107 routes would reach their target service levels as identified through Metro's service guidelines. To deliver this level of service by 2030, Metro must address operational capacity limits by facilitating historic increases in staffing and fleet size.  

#### Funding:  

These service levels would only be possible through a county-wide regional funding package. Under this scenario, the City of Seattle would be unlikely to renew the Seattle Transit Measure, but the regional funding package would offset and exceed any loss of local funding.  

-   Metro: Net increase of 445,000 annual service hours would fund six RapidRide lines (G, H, I, J, K, R) and service hour investments across the transit system. 

-   Seattle Transit Measure: Eliminated, resulting in a net loss of 225,000 annual service hours. 

-   Regional Funding Measure: Passed, resulting in a net gain of 766,000 annual service hours.  

#### Geographic Emphasis: 

-   Service Growth: Service growth would occur across the transit system, with large increases in service in Downtown Seattle, North Seattle, South King County, the eastside, and additional growth across the suburbs. This scenario would also fund service growth in North King County.  

#### Capital Needs:  

-   New speed and reliability investments (N. Seattle) 

-   New passenger & route facilities  

-   Significant fleet/capacity constraints  

## 

## More Info

This app was developed in R using shinydashboard by Melissa Gaughan. For code and technical documentation see <https://github.com/melissa-gaughan/mc-2030-trips-by-hex>
