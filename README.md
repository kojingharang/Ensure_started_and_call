What is this
======
Sample code: How to ensure gen_server started and call it

Summary
======
If your gen_server is supposed to stop sometimes, it might happen between ensure started and gen_server:call. Retry call!

Run
======
```
make start

> sample:a().

```
