## About

This is a library that facilitates sending email via AWS SES using the
background processor `Hworker`. In particular, it handles rate
limiting (for sending rate currently, not daily quotas), as SES does
not queue messages.

## Rate limiting

Aside from sending emails in the background, the main thing that
`hworker-ses` provides is rate limiting. The rate limiting is only on
a per-process (most likely, per server) basis, as it is controlled via
in-memory storage. This means that you should take into account how
many workers you are likely to have when setting the
messages-per-second rate on the workers.

For example, if your rate limit is 30 messages per second, and you expect
to have at most 5 workers (you can organize them however you like, but a
simple strategy is just to start a worker with each application process),
then if you set the rate limit to be 6 messages per second, things will be
fine.

Note that if you do run over, it won't mean that your messages actually
don't get delivered. That error code will trigger a retry on the message,
so eventually it will go out. But it's wasteful (as you can know in advance
if a given message will actually be able to be sent), and if you really
mess it up (ie, set an absurdly high limit), you potentially will have
your API calls rate limited, which isn't good.
