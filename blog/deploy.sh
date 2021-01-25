#/usr/bin/env bash

set -euo pipefail

lein run
( cd public && tar czf ../public.tar.gz . )
VERSION=$(git log -n1 --format=%cd-$(git rev-list --count HEAD)-%h --date=format:%Y%m%d --abbrev=8)

aws s3 cp public.tar.gz s3://cuddly-octo-palm-tree/public/$VERSION.tar.gz
rm public.tar.gz
( cd tf && terraform apply -var="blog_version=$VERSION" )
