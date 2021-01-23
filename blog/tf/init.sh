#!/usr/bin/env bash
set -euo pipefail

apt-get update
apt-get upgrade -q -y

apt-get install -q -y nginx certbot python3-certbot-nginx awscli

DOMAIN=cuddly-octo-palm-tree.com
mkdir -p /var/www/$DOMAIN/html

cat <<CONFIG > /etc/nginx/sites-available/$DOMAIN
server {
  listen 80;
  listen [::]:80;

  root /var/www/$DOMAIN/html;
  index index.html;

  server_name $DOMAIN www.$DOMAIN;

  location / {
    try_files \$uri \$uri/ =404;
  }
}
CONFIG

ln -s /etc/nginx/sites-available/$DOMAIN /etc/nginx/sites-enabled/

TARBALL=manual-2021-01-23.tar.gz
S3=s3://cuddly-octo-palm-tree-blog
if aws s3 ls $S3/$TARBALL; then
    aws s3 cp $S3/$TARBALL /tmp/blog
    tar xzf /tmp/blog -C /var/www/$DOMAIN/html/
else
    cat <<'INDEX' > /var/www/$DOMAIN/html/index.html
<html>
<body>
Website not initialized yet.
</body>
</html>
INDEX
fi

chown -R ubuntu:ubuntu /var/www/$DOMAIN/html
systemctl restart nginx
certbot run --nginx \
            --non-interactive \
            --agree-tos \
            --email gary.verhaegen@gmail.com \
            --redirect \
            -d $DOMAIN \
            -d www.$DOMAIN
