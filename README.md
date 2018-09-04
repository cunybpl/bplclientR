### bplclientR 
--------------

An R package for interfacing with bplservices.

You need to generate a personal access token using github since its currently private. You can generate one from your github settings page.

Make sure you have ```devtools``` installed and then use the token to fetch the master branch:
```{r}
devtools::install_github('cunybpl/bplclientR', auth_token='token-string-here...')
```

You can use this to fetch data from our backend, including portfolio data for multiple fiscal years for use in various apps and projects. The dynamic bema endpoints will give you utility and/or baseline model output for the specified target_date and month range.



#### Authenticate
-----------------
First you must initialize the ```cache```. This is a protected R env that stores url routes and the jwt-token. This only needs to get called once when you start your session. The token automatically gets passed in the headers for our get and post request functions. 

```{r}
cache_init()

```
You can make get or post requests to any of our [databases](https://bpl-services-staging.herokuapp.com/api/v1) as long as you have login credentials. This method needs to be called once before you can access our routes.

```{r}
fetch_auth_token(username='username', password='password')

```
The token is automatically stored in the cache on a successful request and as long as the developer uses our interface, will not have to worry about appending the auth token to request headers.



#### Fetch requests
-------------------

With the jwt-token cached you can make filterable get requests to any of our list-view urls. The fetch_request method performs the request, checks for errors and returns parsed content. Any data records will be automatically converted into dataframes and can be retrieved from the resulting list 

```{r}
# return the building 1033
content <- fetch_request('dcasdb/buildings/, query_params=list(bdbid=1033))

# get the df -- data is always on a "result" key in the list
bidf <- content$result

# return changepoint models for building 1033 fy 2017
content <- fetch_request('portfolios/changepoint-models/, query_params=list(bdbid=1033, fiscal_year=2017))

# get the df
models <- content$result

```

This will always return max 100 records. In order to make bigger requests you need to use the paginator endpoint. 
This stores all the paginated results in a list that you can combine the dfs in your app.

```{r}
# this will return over 700 records... needs the paginator 
contents <- paginator_fetch_request('dcasdb/consumption-records/, query_params=list(bdbid=1033))

page1df = contents[[1]]$result
page2df = contents[[2]]$result 

# etc...

```

#### Bema requests 
------------------

Make a request to online-bema requires two requests. The first is a post request to one of the bema routes with the correct input. A 200 response should give the user a 'task_id'. This is passed into the ```polling_request``` function along with the results endpoint. This route pings the result url to get the result data.

For bema endpoints the target_date is always given as string %Y-%m-%d. The utility generator will look back the number of months in period starting from the month prior to date specified. 

```{r}
# make a utility file request for 24 months of fy data for 2016/17
payload <- list(bdbid=1033, target_date='2017-07-01, period=24, no_sqft=TRUE)
task <- post_request('bema/utility/whole-facility', payload=payload)

# pass the task_id into this method... 
# will poll for results.. 10 tries at an interval of 3 sec in between requests
result <- polling_request('bema/results/, task$task_id, tries=10, polling_interval=3)

```

The speed at which this returns will vary based on network inter-connectivity from our web-worker and its redis broker. Occassionally worker/redis-connections on heroku will drop and the results need to be refetched. Make sure your code can handle these situations. The default will timeout the client request at around 30 seconds, but the process may still be active in redis and need more time to sync up.








