-- example HTTP POST script which demonstrates setting the
-- HTTP method, body, and adding a header

wrk.method = "PUT"
wrk.body   = "{\"logLatitude\":30.267153,\"logIdVarchar\":\"0\",\"logVenueID\":\"austin-venue\",\"logLongitude\":-97.7430608,\"logDatetime\":\"2015-06-04T00:00:00.000000000000Z\"}"
wrk.headers["Content-Type"] = "application/json"
