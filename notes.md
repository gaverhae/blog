# Notes To My Future Self

## Required Env Vars

Deploying this blog requires the following env vars:

- `TF_VAR_cloudflare_s3_{access_key,secret_key,endpoint}` for the Terraform
  State stored in CloudFlare R2 masquerading as S3.
- `AWS_ACCESS_KEY_ID` & `AWS_SECRET_ACCESS_KEY` for the AWS resources.
- `DNSIMPLE_ACCOUNT` and `DNSIMPLE_TOKEN` for DNS name management.
- `CLOUDFLARE_API_TOKEN` for CloudFlare workers.
- `TF_VAR_cloudflare_account_id` because apparently CloudFlare needs it.
- `CLOUDFLARE_ACCOUNT_ID` because `wrangler` also needs it (same value as above).
- `TF_VAR_r2_token` because apparently the R2 API needs its own, separate token.

## Wrangler

I'm using `wrangler` to deploy the site to CloudFlare Workers. `wrangler` is
able to use the `CLOUDFLARE_API_TOKEN` from above, so no special action needs
to be taken to login, as attested by `wrangler whoami`.

However, `wrangler` does need to be told the account ID explicitly through the
`CLOUDFLARE_ACCOUNT_ID` env var, despite `wrangler whoami` reporting it.

I couldn't find a practical way to manage the "CloudFlare Worker" through
Terraform; I can create the worker, but I did not find any good way to push the
actual content through Terraform, so I would just end up duplicating the data
in `wrangler.confc`.
