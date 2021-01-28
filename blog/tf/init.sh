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

S3=s3://cuddly-octo-palm-tree
# This is a Terraform substitution!
TARBALL=public/${version}.tar.gz
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

CERT=cert/current.tar.gz
if aws s3 ls $S3/$CERT; then
    aws s3 cp $S3/$CERT /tmp/cert
    tar xzf /tmp/cert -C /etc/letsencrypt
fi

certbot run --nginx \
            --reinstall \
            --non-interactive \
            --agree-tos \
            --email gary.verhaegen@gmail.com \
            --redirect \
            -d $DOMAIN \
            -d www.$DOMAIN

cd /etc/letsencrypt
tar czf /tmp/new-cert .
aws s3 cp /tmp/new-cert $S3/cert/current.tar.gz
TOKEN=$(curl -X PUT "http://169.254.169.254/latest/api/token" -H "X-aws-ec2-metadata-token-ttl-seconds: 21600")
INSTANCE_ID=$(curl -s -H "X-aws-ec2-metadata-token: $TOKEN" http://169.254.169.254/latest/meta-data/instance-id)
aws s3 cp /tmp/new-cert $S3/cert/$(date -u +'%Y-%m-%dT%H-%M-%S')-$INSTANCE_ID.tar.gz

cat <<LOGROTATE > /etc/logrotate.d/nginx
/var/log/nginx/*.log {
  rotate 14
  create 0640 www-data adm
  compress
  daily
  dateext
  dateformat -%Y-%m-%d-$INSTANCE_ID
  nomail
  missingok
  sharedscripts
  prerotate
    if [ -d /etc/logrotate.d/httpd-prerotate ]; then
      run-parts /etc/logrotate.d/httpd-prerotate
    fi
  endscript
  postrotate
    invoke-rc.d nginx rotate >/dev/null 2>&1
  endscript
  lastaction
    cd /var/log/nginx
    for f in *.gz; do
      aws s3 cp \$f $S3/logs/\$f
    done
  endscript
}
LOGROTATE
