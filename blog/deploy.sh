#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$DIR"

SHA=$(git log -n1 --format=%H .)
COUNT=$(git rev-list --count $SHA)
VERSION=$(git show -s --format=%cd-$COUNT-%h --date=format:%Y%m%d --abbrev=8 $SHA)
S3_FILE=s3://cuddly-octo-palm-tree/public/$VERSION.tar.gz

update_deploy_set() {
    local jq_expr
    jq_expr="$1"

    jq -c --arg version $VERSION "$jq_expr" < tf/deployed > tf/deployed.tmp
    mv tf/deployed.tmp tf/deployed
}

deploy() {
    local var
    var="-var=deployed_json=$(cat tf/deployed)"
    ( cd tf && tofu plan "$var" && tofu apply "$var" -auto-approve)
}

wait_for() {
    while ! { RESP=$(curl --fail \
                          $(if [ "$1" == "background" ]; then
                              echo "--connect-to cuddly-octo-palm-tree.com:443:$(cat tf/out):443"
                            fi) \
                          https://cuddly-octo-palm-tree.com/version.txt -s) \
          && [[ "$VERSION" == "$RESP" ]] ;}; do
        echo -n .
        sleep 5
    done
    echo
}

if ! [ -z "$(git status --porcelain)" ]; then
    echo "Not working on dirty worktree."
    exit 1
fi

if aws s3 ls $S3_FILE; then
    echo "File already exists, no need to build."
else
    # clean-up
    rm -rf public

    # compile static site
    lein run

    # resize images
    find public/img \
         -not -path '*/\.*' \
         -type f \
         -exec bash -c '
         ext=$(echo $1 | grep -o "\.[^.]*")
         base=$(echo $1 | sed "s/\(.*\)\.[^.]*/\1/")
         convert "$1" -resize 800x2000 "$base.tmp$ext" && mv "$base.tmp$ext" "$1"
         ' -s '{}' \;

    # create tar
    ( cd public && tar czf ../public.tar.gz . )

    # push to S3
    aws s3 cp public.tar.gz $S3_FILE
    rm public.tar.gz
fi

update_deploy_set '. + [{version: $version, ami: null}]'
deploy
wait_for background

update_deploy_set 'reverse'
deploy
wait_for redirect

update_deploy_set '.[0:1]'
deploy
