# Error Handling

There a few different types of errors that we can get when calling the CoinBene API:

1. Exchange returns HTTP status `200 OK` but describes error condition in response. We get this, for example, for "insufficient funds" when trying to place an order.
2. Exchange returns HTTP error code. For example: Status code "502 Bad Gateway"
3. We get an error on the network before/after reaching the exchange

We will treat these errors as follows:

Errors from (1) will be caught when parsing the response and obtaining a `RespError` inside pure code in `decodeResponse` the returned `Left (ExchangeError ...)` will always imply the operation failed. Accordingly, we will always retry. The assumption is that no such API request will succeed or partially succeed. For, this assumption holds.

Errors from (2) will be caught by looking at the status code in the response from the call to `http`. Each of these errors will be looked at differently. It makes no sense to retry on a status code `tooManyRequests429`, but it makes total sense to retry upon a `badGateway502`. For now, all we do is to retry on a 502, other cases are not treated.

Finally, errors from (3) will be detected by catching any exceptions thrown by `httpLBS`. These are usually things like "connection reset by peer" (i.e. RST packets) or network timeouts. We can safely retry API calls with these errors if the are idempotent, but not otherwise. This is what we currently do. 