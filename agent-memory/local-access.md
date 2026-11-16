---
name: Local access
description: The app serves on loopback and requires a URL token for API calls.
---

The server binds to 127.0.0.1 and API requests require the token supplied in the launch URL fragment, forwarded in `X-Dualdag-Token`. Host and Origin are checked too. This restricts the app to local access and authenticated same-user requests. `start_server` and `check_request` in `R/server.R`, and URL construction in `run.R`, implement this.
