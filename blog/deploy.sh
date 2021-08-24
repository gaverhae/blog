#/usr/bin/env bash

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
    ( cd tf && terraform apply -var="blog_version=$(cat deployed)" )
}

wait_for() {
    while ! { RESP=$(curl --fail \
                          $(if [ "$1" == "background" ]; then
                              echo "--connect-to cuddly-octo-palm-tree.com:443:$(cat tf/out):443"
                            fi) \
                          https://cuddly-octo-palm-tree.com/version.txt) \
          && [[ "$VERSION" == "$RESP" ]] ;}; do
        echo $RESP
        sleep 5
    done
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

    # resize images
    find content/img \
         -not -path '*/\.*' \
         -type f \
         -exec bash -c \
         "convert '{}' -resize 800x2000 '{}.out.jpg' && mv '{}.out.jpg' '{}'" \;

    # compile static site
    lein run

    # create tar
    ( cd public && tar czf ../public.tar.gz . )

    # push to S3
    aws s3 cp public.tar.gz $S3_FILE
    rm public.tar.gz
fi

update_deploy_set '. + [$version]'
deploy
wait_for background

update_deploy_set 'reverse'
deploy
wait_for redirect

update_deploy_set '.[0:1]'
deploy

git add tf/deployed
git commit -m "auto: deployed $VERSION"
git push
